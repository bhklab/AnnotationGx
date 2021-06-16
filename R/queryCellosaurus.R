## =========================================
## Parse Cellosaurus xml
## -----------------------------------------

#' Access and parse Cellosaurus xml for developers
#'
#'
#' @description
#' This function reads Cellosaurus XML and parses parent node for each cell line, returning an XML document.
#' This enables parsing any child node of interest.
#'
#'
#' @details
#'
#' @return An `XML` document of Cellosaurus
#' @references
#' Bairoch A.The Cellosaurus, a cell line knowledge resource.J. Biomol. Tech. 29:25-38(2018) DOI: 10.7171/jbt.18-2902-002; PMID: 29805321; PMCID: PMC5945021
#' @param url is cellosaurus link to xml. This should be a valid link to Cellosaurus xml. Default is `https://ftp.expasy.org/databases/cellosaurus/cellosaurus.xml`.
#' @param cellline_input is any cell line identifier. Cell line name(s) or `Cellosaurus ID (CVCL ID)`` can be provided as the input.
#' @param namespace is either cell line name (default) or Cellosaurus ID (CVCL ID).
#' @param verbose is TRUE by default
#'
#'
#'
#' @md
#' @importFrom xml2 read_xml xml_find_all xml_find_first xml_text xml_add_child
#' @export

queryCellosaurus <-
    function(url = "https://ftp.expasy.org/databases/cellosaurus/cellosaurus.xml",
             cellline_input,
             namespace = "name",
             verbose = TRUE) {
      if (namespace != "name" & namespace != "cvclid") {
        if (verbose) {
          message("invalid input. Please provide a valid namespace : 'name' or 'cvclid'")
        }
        return(NULL)
      }
      if (verbose) {
        message(paste(
          "xml read started from",
          url,
          format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        ))
      }
        main_xml <- read_xml(url)
        if (verbose) {
          message(paste(
            "xml read completed at",
            format(Sys.time(), "%Y-%m-%d %H:%M:%S")
          ))
        }
        root_node <- xml_new_root("cell-line-list")
        cellline_input <- cellline_input[!is.na(cellline_input)]
        if (namespace == "name") {
          for (ip in 1:length(cellline_input)) {
            xmlObject <-
              xml_find_first(
                main_xml,
                paste(
                  "//cell-line/name-list/name[normalize-space(text()) = '",
                  cellline_input[ip],
                  "']/../..",
                  sep = ""
                )
              )
            if (length(xmlObject) > 0) {
          xml_add_child(root_node, xmlObject)
        }
      }
    }
    else if (namespace == "cvclid") {
      for (ip in 1:length(cellline_input)) {
        xmlObject <-
          xml_find_first(
            main_xml,
            paste(
              "//cell-line/accession-list/accession[@type = 'primary'][normalize-space(text()) = '",
              cellline_input[ip],
              "']/../..",
              sep = ""
            )
          )
        if (length(xmlObject) > 0) {
          xml_add_child(root_node, xmlObject)
          }
        }
    } 
    if   (verbose) {
        message(paste(
        "completed fetching nodesets for",
        length(cellline_input),
        "cell line(s)"
      ))
    }
    return(root_node)
    }
