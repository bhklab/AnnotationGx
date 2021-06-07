## =========================================
## Parse Cellosaurus xml
## -----------------------------------------

#' Access and parse Cellosaurus xml 
#' 
#' 
#' @description
#' This function reads Cellosaurus XML and parses nodes for cell line annotations based on input parameters, returning a data.table object
#' 
#' 
#' @details
#' 
#' @return A data.table object with the results of the input cell line(s)
#' @references
#' Bairoch A.The Cellosaurus, a cell line knowledge resource.J. Biomol. Tech. 29:25-38(2018) DOI: 10.7171/jbt.18-2902-002; PMID: 29805321; PMCID: PMC5945021
#' @param url is cellosaurus link to xml
#' 
#' 
#' 
#' @md
#' @importFrom xml2 read_xml
#' @export

getCelloxml <- function(url = "https://ftp.expasy.org/databases/cellosaurus/cellosaurus.xml"){
    print(paste("xml read started from", url, format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
    main_xml <- read_xml(url)
    print(paste("xml read completed at", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
    return(main_xml)
}

#' Filter parent node cell-line and parse child nodes for required annotations
#' @param cell_nm is cell name
#' @param main_xml is read xml object from getCelloxml
#' 
#' 
#' @md
#' @importFrom xml2 xml_find_all xml_find_first xml_text
#' @export
getInfoFromCellID <- function(cell_nm, main_xml) {
    xmlObject <- xml_find_first(main_xml,paste("//cell-line/name-list/name[normalize-space(text()) = '", cell_nm, "']/../..", sep = ""))
    std_name <- xml_text(xml_find_first(xmlObject, ".//name-list/name[@type = 'identifier']"))
    syno_list <- xml_text(xml_find_all(xmlObject, ".//name-list/name[@type = 'synonym']"))
    syno <- paste(shQuote(unlist(syno_list)), collapse=",")
    cvcl <- xml_text(xml_find_first(xmlObject, ".//accession-list/accession[@type = 'primary']"))
    dis <- xml_text(xml_find_first(xmlObject, ".//disease-list/cv-term"))
    #tis <- xml_text(xml_find_first(xmlObject, ".//disease-list/cv-term"))#have to extract from disease
    ncit <- xml_text(xml_find_first(xmlObject, ".//disease-list/cv-term/@accession"))
    cat <- xml_text(xml_find_first(xmlObject, "./@category"))
    dep <- xml_text(xml_find_first(xmlObject, ".//xref-list/xref[@database = 'DepMap']/@accession"))
    sex <- xml_text(xml_find_first(xmlObject, "./@sex"))
    age <- xml_text(xml_find_first(xmlObject, "./@age"))
    meta <- xml_text(xml_find_first(xmlObject, ".//comment-list/comment[@category = 'Derived from metastatic site']"))
    op_list <- list(std_name = std_name,syno = syno, cvcl = cvcl, dis = dis, ncit = ncit, cat = cat, dep = dep, sex = sex, age = age, meta = meta )
    return(op_list)
}

## ============================
## getCellosaurus wrapper methods
## ----------------------------

## These methods further specialize the getCellosaurus function to provide
## a simple user interface that does not require knowledge of the parsing cellosaurus xml 

#' Build a `data.table` of annotations from query list.
#' @param cellnames is the list of cell names (identifiers and synonyms)
#' @param url is cellosaurus link to xml 
#' 
#' 
#' @md
#' @importFrom data.table data.table
#' @export
getCellosaurus <- function(cellnames, url = "https://ftp.expasy.org/databases/cellosaurus/cellosaurus.xml"){
    cellnames <- cellnames[!is.na(cellnames)]
    main_xml <- getCelloxml(url)
    op_dt <- data.table(standard_name = character(), synonyms = character(), cvcl_id = character(), disease = character(), ncit_id = character(), category = character(), depmap_id = character(), sex = character(), age = numeric(), metastatic_site = character()) 
    print("fetching cell line data from xml")
    for(nm in 1:length(cellnames)){
      op_list <- getInfoFromCellID(cell_nm = cellnames[nm], main_xml = main_xml)
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
    print(paste("completed fetching data for",length(cellnames), "cell lines"))
    return(op_dt)
}


