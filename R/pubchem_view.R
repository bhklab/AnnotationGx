#' Get annotation headings (name only) based on type and heading criteria.
#'
#' @param type The type of annotation headings to retrieve.
#' Options include "Compound", "Gene", "Taxonomy", "Element", "Assay", "Protein", "Cell", "Pathway", or "all" (default).
#' @param heading The specific heading to filter the results by. Defaults to NULL, which retrieves all headings.
#'
#' @return A `data.table` containing the annotation headings and types.
#'
#' @examples
#' getPubchemAnnotationHeadings()
#' getPubchemAnnotationHeadings(type = "Compound")
#' getPubchemAnnotationHeadings(heading = "ChEMBL*")
#' getPubchemAnnotationHeadings(type = "Compound", heading = "ChEMBL*")
#'
#' @export
getPubchemAnnotationHeadings <- function(
    type = "all", heading = NULL) {
  funContext <- .funContext("getPubchemAnnotationHeadings")
  .debug(funContext, " type: ", type, " heading: ", heading)

  # TODO:: messy...
  checkmate::assert(
    checkmate::test_choice(
      tolower(type), tolower(c(
        "Compound", "Gene", "Taxonomy", "Element",
        "Assay", "Protein", "Cell", "Pathway"
      ))
    ) || type == "all"
  )

  ann_dt <- .get_all_heading_types()
  .debug(funContext, " ann_dt: ", utils::capture.output(utils::str(ann_dt)))
  if (type != "all") {
    ann_dt <- ann_dt[grepl(type, ann_dt$Type, ignore.case = T), ]
  }
  if (!is.null(heading)) {
    ann_dt <- ann_dt[grepl(heading, ann_dt$Heading, ignore.case = F), ]
  }

  if (nrow(ann_dt) == 0) {
    .warn(
      funContext, " No headings found for type: `", type, "` and heading: `", heading,
      "`.\nTry getPubchemAnnotationHeadings(type = 'all') for available headings and types"
    )
  }
  ann_dt
}

#' Annotate PubChem Compound
#'
#' This function retrieves information about a PubChem compound based on the provided compound ID (CID).
#'
#' @param cids The compound ID (CID) of the PubChem compound.
#' @param heading The type of information to retrieve. Default is "ChEMBL ID".
#' @param source The data source to use. Default is NULL.
#' @param parse_function A custom parsing function to process the response. Default is the identity function.
#' @param query_only Logical indicating whether to return the query URL only. Default is FALSE.
#' @param raw Logical indicating whether to return the raw response. Default is FALSE.
#'
#' @return The annotated information about the PubChem compound.
#'
#' @examples
#' annotatePubchemCompound(cid = 2244)
#' annotatePubchemCompound(cid = c(2244, 67890), heading = "CAS")
#'
#' @export
annotatePubchemCompound <- function(
    cids, heading = "ChEMBL ID", source = NULL, parse_function = identity,
    query_only = FALSE, raw = FALSE) {
  funContext <- .funContext("annotatePubchemCompound")

  requests <- lapply(cids, function(cid) {
    .build_pubchem_view_query(
      id = cid, record = "compound", heading = heading,
      output = "JSON", source = source
    )
  })

  .debug(funContext, paste0("query:", sapply(requests, `[[`, i = "url")))

  if (query_only) {
    return(requests)
  }

  resp_raw <- httr2::req_perform_sequential(requests, on_error = "continue")
  if (raw) {
    return(resp_raw)
  }

  responses <- lapply(resp_raw, .parse_resp_json)

  # apply the parse function to each response depending on heading
  parsed_responses <- .bplapply(responses, function(response) {
    switch(heading,
      "ChEMBL ID" = .parseCHEMBLresponse(response),
      "CAS" = .parseCASresponse(response),
      "NSC Number" = .parseNSCresponse(response),
      "ATC Code" = .parseATCresponse(response),
      "Drug Induced Liver Injury" = .parseDILIresponse(response),
      tryCatch(
        {
          parse_function(response)
        },
        error = function(e) {
          .warn(
            funContext, "The parseFUN function failed: ", e,
            ". Returning unparsed results instead. Please test the parseFUN
                  on the returned data."
          )
          response
        }
      )
    )
  })

  sapply(parsed_responses, .replace_null)

}

# helper function to replace NULL with NA
.replace_null <- function(x) {
  ifelse(is.null(x), NA_character_, x)
}