## =========================================
## Parse Cellosaurus xml
## -----------------------------------------

#' Access and parse Cellosaurus xml for regular users
#'
#' @description
#' This function reads Cellosaurus XML and parses nodes for cell line annotations based on input parameters, returning a data.table object
#'
#' @details This function is a wrapper for the `getCellosaurus` function, 
#' which is a wrapper for the `getInfoFromCelllineInput` function, 
#' which is a wrapper for the `cleanCellnames` function, 
#' which is a wrapper for the `getCelloxml` function.
#'
#' @return An `XML` document of Cellosaurus
#' @references
#' Bairoch A.The Cellosaurus, a cell line knowledge resource.J. Biomol. Tech. 29:25-38(2018) DOI: 10.7171/jbt.18-2902-002; PMID: 29805321; PMCID: PMC5945021
#' @param url is cellosaurus link to xml. This should be a valid link to Cellosaurus xml. Default is `https://ftp.expasy.org/databases/cellosaurus/cellosaurus.xml`.
#' @param verbose is TRUE by default
#'
#'
#' @md
#' @importFrom xml2 read_xml
#' @export
getCelloxml <- memoise::memoise(function(url = "https://ftp.expasy.org/databases/cellosaurus/cellosaurus.xml", verbose = TRUE) {
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
    return(main_xml)
  }
)

#' Clean Cell Names
#' 
#' @description 
#' TODO::
#' 
#' @md
#' @importFrom xml2 read_xml xml_find_all
#' @export
#########ADD DOCS
cleanCellnames <- function(main_xml, verbose = TRUE) {
  if (verbose) {
    message(paste(
      "Started removing special characters from cell line names in the xml",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ))
  }
  matching <- xml_find_all(main_xml, "//cell-line/name-list/name/text()")
  # A raw string, added in R 4.0 will excape characters for you: r"{ <string> }"
  badchars <- r"{[\xb5]|[]|[ ,]|[;]|[:]|[-]|[+]|[*]|[%]|[$]|[#]|[{]|[}]|[[]|[]]|[|]|[^]|[/]|[\]|[.]|[_]|[ ]|[(]|[)]}"
  for(i in 1:length(matching)){
    node1 <- matching[[i]]
    node1text <- xml_text(node1)
    xml_par <- xml_find_first(node1, "parent::*")
    xml_set_attr(xml_par, "cleanname", gsub(badchars,"",ignore.case = TRUE, node1text))
  }
  if (verbose) {
    message(paste(
      "Removed special characters from cell line names in the xml",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ))
  }
  return(main_xml)
}

#' Filter parent node cell-line and parse child nodes for required annotations
#' @param cell_ip is either cell name or cvcl id.
#' @param main_xml is read xml object from getCelloxml
#' @param input_type is either cell line name(s) denoted by `name` or Cellosaurus ID (CVCL ID) denoted by `cvclid`.
#' @return A `list` of xml child nodes
#'
#' @md
#' @importFrom xml2 xml_find_all xml_find_first xml_text
#' @export
getInfoFromCelllineInput <-
    function(cell_ip, main_xml, input_type, remove = FALSE) {
      if (input_type == "name") {
        if(remove == TRUE){
          xmlObject <-
            xml_find_first(
              main_xml,
              paste(
                "//cell-line/name-list/name[@cleanname = '",
                cell_ip,
                "']/../..",
                sep = ""
              )
            )
        }
        else {
          xmlObject <-
            xml_find_first(
              main_xml,
              paste(
                "//cell-line/name-list/name[normalize-space(text()) = '",
                cell_ip,
                "']/../..",
                sep = ""
              )
            )
        }
      }
      else if (input_type == "cvclid") {
        xmlObject <-
          xml_find_first(
            main_xml,
            paste(
              "//cell-line/accession-list/accession[@type = 'primary'][normalize-space(text()) = '",
              cell_ip,
              "']/../..",
              sep = ""
            )
          )
      }
    #xml_exist <- xml_find_first(xmlObject, ".//name-list/name[@type = 'identifier']")
    if(class(xmlObject) == "xml_missing"){
        message(paste(
          "Cell line '", cell_ip, "' not found in xml",
          sep = ""
        ))
      op_list <-
        list(
          std_name = cell_ip,
          syno = NA,
          cvcl = NA,
          dis = NA,
          ncit = NA,
          cat = NA,
          dep = NA,
          sex = NA,
          age = NA,
          meta = NA
        )
      return(op_list)
    }
    std_name <-
      xml_text(xml_find_first(xmlObject, ".//name-list/name[@type = 'identifier']"))
    syno_list <-
      xml_text(xml_find_all(xmlObject, ".//name-list/name[@type = 'synonym']"))
    syno <- paste(shQuote(unlist(syno_list)), collapse = ",")
    cvcl <-
      xml_text(xml_find_first(xmlObject, ".//accession-list/accession[@type = 'primary']"))
    dis <-
      xml_text(xml_find_first(xmlObject, ".//disease-list/cv-term"))
    #tis <- xml_text(xml_find_first(xmlObject, ".//disease-list/cv-term"))#have to extract from disease
    ncit <-
      xml_text(xml_find_first(xmlObject, ".//disease-list/cv-term/@accession"))
    cat <- xml_text(xml_find_first(xmlObject, "./@category"))
    dep <-
      xml_text(xml_find_first(
        xmlObject,
        ".//xref-list/xref[@database = 'DepMap']/@accession"
      ))
    sex <- xml_text(xml_find_first(xmlObject, "./@sex"))
    age <- xml_text(xml_find_first(xmlObject, "./@age"))
    meta <-
      xml_text(
        xml_find_first(
          xmlObject,
          ".//comment-list/comment[@category = 'Derived from metastatic site']"
        )
      )
    op_list <-
      list(
        std_name = std_name,
        syno = syno,
        cvcl = cvcl,
        dis = dis,
        ncit = ncit,
        cat = cat,
        dep = dep,
        sex = sex,
        age = age,
        meta = meta
      )
    return(op_list)
    }

## ============================
## getCellosaurus wrapper methods
## ----------------------------

## These methods further specialize the getCellosaurus function to provide
## a simple user interface that does not require knowledge of the parsing cellosaurus xml

#' Build a `data.table` of annotations from query list.
#' @param cellline_input is either cell name or cvcl id (identifiers or synonyms)
#' @param url is cellosaurus link to xml
#' @param namespace is either cell line name(s) which is default, denoted by `name` or Cellosaurus ID (CVCL ID) denoted by `cvclid`.
#' @param verbose is TRUE by default
#' @return A `data.table` object with the results of the input cell line(s)
#'
#' @md
#' @importFrom data.table data.table
#' @export
getCellosaurus <-
    function(cellline_input,
             namespace = "name",
             url = "https://ftp.expasy.org/databases/cellosaurus/cellosaurus.xml",
             remove_char = FALSE,
             verbose = TRUE) {
      if (namespace != "name" & namespace != "cvclid") {
        if (verbose) {
          message("invalid input. Please provide a valid namespace : 'name' or 'cvclid'")
        }
        return(NULL)
      }
      if (verbose) {
        message(paste(
          "omitting",
          sum(!complete.cases(cellline_input)),
          "missing/NA value(s) from input",
          sep = " "
        ))
      }
      cellline_input <- cellline_input[!is.na(cellline_input)]
      main_xml <- getCelloxml(url, verbose)
      if(remove_char == TRUE){
        main_xml <- cleanCellnames(main_xml, verbose)
        badchars <- "[\xb5]|[]|[ ,]|[;]|[:]|[-]|[+]|[*]|[%]|[$]|[#]|[{]|[}]|[[]|[]]|[|]|[\\^]|[/]|[\\]|[.]|[_]|[ ]|[(]|[)]"
        cellline_input <- gsub(badchars,"",ignore.case = TRUE, cellline_input)
      }
      op_dt <-
        data.table(
          standard_name = character(),
          synonyms = character(),
          cvcl_id = character(),
          disease = character(),
          ncit_id = character(),
          category = character(),
          depmap_id = character(),
          sex = character(),
          age = numeric(),
          metastatic_site = character()
        )
      if (verbose) {
        message("fetching cell line data from xml")
      }
      for (nm in 1:length(cellline_input)) {
        if (namespace == "name") {
          op_list <-
            getInfoFromCelllineInput(cell_ip = cellline_input[nm],
                                     main_xml = main_xml,
                                     input_type = "name", remove = remove_char)
      }
      else if (namespace == "cvclid") {
        op_list <-
          getInfoFromCelllineInput(cell_ip = cellline_input[nm],
                                   main_xml = main_xml,
                                   input_type = "cvclid")
      }
      op_sub_dt = data.table(
        standard_name = op_list$std_name,
        synonyms = op_list$syno,
        cvcl_id = op_list$cvcl,
        disease = op_list$dis,
        #tis = op_list$tis,
        ncit_id = op_list$ncit,
        category = op_list$cat,
        depmap_id = op_list$dep,
        sex = op_list$sex,
        age = op_list$age,
        metastatic_site = op_list$meta
      )
      op_dt <- rbind(op_dt, op_sub_dt)
    }
    if (verbose) {
      message(paste(
        "completed fetching data for",
        length(cellline_input),
        "cell line(s)"
      ))
    }
    return(op_dt)
    }

#' Fetches the complete Cellosaurus data
#' 
#' This function fetches the complete Cellosaurus data and converts it 
#' into a data table using the Cellosaurus package. 
#' 
#' @export
#' @return A data table containing the complete Cellosaurus data
getCompleteCellosaurusObject <- function() {

  message("Fetching Cellosaurus Data")
  message("This may take up to 15 minutes and it is recommended to save 
          the output for future use")
  Cellosaurus::Cellosaurus()
}