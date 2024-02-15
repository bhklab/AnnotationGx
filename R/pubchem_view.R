
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
    type = "all", heading = NULL
    ){
    funContext <- .funContext("getPubchemAnnotationHeadings")
    .debug(funContext, " type: ", type, " heading: ", heading)

    # TODO:: messy...
    checkmate::assert(
        checkmate::test_choice(
            tolower(type), tolower(c("Compound", "Gene", "Taxonomy", "Element",
            "Assay", "Protein", "Cell", "Pathway"))
        ) || type == "all"
    )

    ann_dt <- .get_all_heading_types()
    .debug(funContext, " ann_dt: ", utils::capture.output(utils::str(ann_dt)))
    if(type != "all"){
        ann_dt <- ann_dt[grepl(type, ann_dt$Type, ignore.case = T),]
    }
    if(!is.null(heading)){
        ann_dt <- ann_dt[grepl(heading, ann_dt$Heading, ignore.case = F),]
    }

    if(nrow(ann_dt) == 0){
        .warn(funContext, " No headings found for type: `", type, "` and heading: `", heading,
            "`.\nTry getPubchemAnnotationHeadings(type = 'all') for available headings and types")
    }
    ann_dt
}

#' Annotate PubChem Compound
#'
#' This function retrieves information about a PubChem compound based on the provided compound ID (CID).
#'
#' @param cid The compound ID (CID) of the PubChem compound.
#' @param heading The type of information to retrieve. Default is "ChEMBL ID".
#' @param source The data source to use. Default is NULL.
#' @param parse_function A custom parsing function to process the response. Default is the identity function.
#'
#' @return The annotated information about the PubChem compound.
#'
#' @examples
#' annotatePubchemCompound(cid = 12345)
#' annotatePubchemCompound(cid = 67890, heading = "CAS")
#'
#' @export
annotatePubchemCompound <- function(
    cid, heading = "ChEMBL ID", source = NULL, parse_function = identity
    ){
    funContext <- .funContext("annotatePubchemCompound")
    validHeaders <-  getPubchemAnnotationHeadings("Compound")$Heading
    if(!checkmate::test_choice(heading, validHeaders))
        .err(funContext, "Invalid heading: ", heading,
            ". Use getPubchemAnnotationHeadings() to get valid headings.")

    url <- .build_pubchem_view_query(
        id = cid, record = "compound", heading = heading,
        output = "JSON", source = source)

    .debug(funContext, " query: ", url)

    response <- .build_pubchem_request(url) |>
        httr2::req_perform() |> .parse_resp_json()

    switch(heading,
        'ChEMBL ID' = .parseCHEMBLresponse(response),
        'CAS' = .parseCASresponse(response),
        'NSC Number' = .parseNSCresponse(response),
        'ATC Code' = .parseATCresponse(response),
        'Drug Induced Liver Injury' = .parseDILIresponse(response),
        tryCatch({
            parse_function(response)
        }, error = function(e){
            .warn(funContext, 'The parseFUN function failed: ', e,
                '. Returning unparsed results instead. Please test the parseFUN
                on the returned data.')
            response
        }))
}


#' Get all available annotation headings
#'
#' https://pubchem.ncbi.nlm.nih.gov/rest/pug/annotations/headings/JSON will return a list of all available headings
#'
#' @keywords internal
#' @noRd
.get_all_heading_types <- function(){
    url <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug/annotations/headings/JSON"
    req <- .build_pubchem_request(url)
    response <- httr2::req_perform(req) |> .parse_resp_json()
    .asDT(response[[1]][[1]])
}


#' Build a PubChem REST query URL
#'
#' pubchem<DOT>ncbi<DOT>nlm<DOT>nih<DOT>gov/rest/pug_view/<ANNOTATION>/<RECORD>/<ID>/<OUTPUT><?OPTIONS>
#'
#' Optional Parameters:
#'   - annotation: data, index, annotations, categories, neighbors, literature, structure, image, qr, linkout
#'  - record: compound, substance, assay, cell, gene, protein
#' - filter (Parameter, Value, Description):
#'      - Page, n <integer>, The page number to retrieve. Retrieves the nth page of the output data when the data is paginated.
#'          This option is useful when downloading the annotations under a specific heading for all compounds
#'          By default, retrieve all pages of the output data.
#'      - Version, n <int> for substance | n.m <int.int> for assay, data for a particular version of a substance or assay record
#'      - Heading, (Sub)heading name, The name of the annotation heading or subheading to retrieve
#'      - Source, source name, The name of the annotation source to retrieve from a specified source
#'
#' @param id The identifier for the query.
#' @param annotation The type of annotation to retrieve. Options include "data", "index", "annotations", "categories", "neighbors", "literature", "structure", "image", "qr", or "linkout".
#' @param record The type of record to retrieve. Options include "compound", "substance", "assay", "cell", "gene", or "protein".
#' @param page The page number to retrieve. Retrieves the nth page of the output data when the data is paginated. By default, retrieve all pages of the output data.
#' @param version The version of the record to retrieve. For substance, this is an integer. For assay, this is a string in the format "n.m".
#' @param heading The name of the annotation heading or subheading to retrieve.
#' @param source The name of the annotation source to retrieve from a specified source.
#' @param output The desired output format. Options are "JSON", "XML", "SDF", "TXT", "CSV".
#'
#' @return The query URL
#'
#' @keywords internal
#' @noRd
.build_pubchem_view_query <- function(
    id, annotation = 'data', record = 'compound', page = NULL, version = NULL, heading = NULL, source = NULL, output = 'JSON', ...
    ){
    funContext <- .funContext(".build_pubchem_view_query")


    checkmate::assert_choice(annotation,
        c('data', 'index', 'annotations', 'categories', 'neighbors', 'literature', 'structure', 'image', 'qr', 'linkout'))
    checkmate::assert_choice(record, c('compound', 'substance', 'assay', 'cell', 'gene', 'protein'))

    opts_ = list()
    if(!is.null(heading)){
        if(record == "substance") {
            .debug(funContext,
                " fyi: https://pubchem.ncbi.nlm.nih.gov/rest/pug/annotations/headings/JSON
                 has no substance headings")
        } else {
            check <- checkmate::check_character(
                unique(getPubchemAnnotationHeadings(record, heading)$Heading),
                min.chars = 1, min.len = 1)
            if(!isTRUE(check)) .err(funContext, "Invalid heading: ", heading,
                ". Use getPubchemAnnotationHeadings() to get valid headings.")
        }
        opts_ <- c(opts_, list(heading = heading))
    }
    if(!is.null(version)){
        if(record == "substance") {
            checkmate::assert_string(version, min.chars = 1)
        } else {
            checkmate::assert_numeric(version, lower = 1)
        }
        opts_ <- c(opts_, list(version = version))
    }

    if(!is.null(source)){
        checkmate::assert_string(source, min.chars = 1)
        opts_ <- c(opts_, list(source = source))
    }

    if(!is.null(page)){
        checkmate::assert_numeric(page, lower = 1)
        opts_ <- c(opts_, list(page = page))
    }



    base_url <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug_view"
    url <- httr2::url_parse(base_url)
    url$path <- .buildURL(url$path, annotation, record, id, output)
    url$query <- opts_

    url |> httr2::url_build()
}


