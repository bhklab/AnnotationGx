## =========================================
## Parse Cellosaurus xml
## -----------------------------------------

#' Access and read xml 
#' @param url is cellosaurus link to xml
#' 
#' 
#' 
getCelloxml <- function(url = "https://ftp.expasy.org/databases/cellosaurus/cellosaurus.xml"){
  print(paste("xml read started from", url, format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  main.xml <- read_xml(url)
  print(paste("xml read completed at", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  return(main.xml)
}

#' Filter parent node cell-line and parse child nodes for required annotations
#' @param cell.nm is cell name
#' @param main.xml is read xml object from getCelloxml
#' 
#' 
#' 
getInfoFromCellID <- function(cell.nm, main.xml){
  xmlObject <- xml_find_first(main.xml,paste("//cell-line/name-list/name[normalize-space(text()) = '", cell.nm, "']/../..", sep = ""))
  std.name <- xml_text(xml_find_first(xmlObject, ".//name-list/name[@type = 'identifier']"))
  syno.list <- xml_text(xml_find_all(xmlObject, ".//name-list/name[@type = 'synonym']"))
  syno <- paste(shQuote(unlist(syno.list)), collapse=",")
  cvcl <- xml_text(xml_find_first(xmlObject, ".//accession-list/accession[@type = 'primary']"))
  dis <- xml_text(xml_find_first(xmlObject, ".//disease-list/cv-term"))
  #tis <- xml_text(xml_find_first(xmlObject, ".//disease-list/cv-term"))#have to extract from disease
  ncit <- xml_text(xml_find_first(xmlObject, ".//disease-list/cv-term/@accession"))
  cat <- xml_text(xml_find_first(xmlObject, "./@category"))
  dep <- xml_text(xml_find_first(xmlObject, ".//xref-list/xref[@database = 'DepMap']/@accession"))
  sex <- xml_text(xml_find_first(xmlObject, "./@sex"))
  age <- xml_text(xml_find_first(xmlObject, "./@age"))
  meta <- xml_text(xml_find_first(xmlObject, ".//comment-list/comment[@category = 'Derived from metastatic site']"))
  op.list <- list(std.name = std.name,syno = syno, cvcl = cvcl, dis = dis, ncit = ncit, cat = cat, dep = dep, sex = sex, age = age, meta = meta )
  return(op.list)
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
#' 
getCellosaurus <- function(cellnames, url = "https://ftp.expasy.org/databases/cellosaurus/cellosaurus.xml"){
  library(XML); library(httr); library(xml2); library(data.table)
  cellnames <- cellnames[!is.na(cellnames)]
  main.xml <- getCelloxml(url)
  op.dt <- data.table(standard.name = character(), synonyms = character(), cvcl.id = character(), disease = character(), ncit.id = character(), category = character(), depmap.id = character(), sex = character(), age = numeric(), metastatic.site = character()) 
  print("fetching cell line data from xml")
  for(nm in 1:length(cellnames)){
    op.list <- getInfoFromCellID(cell.nm = cellnames[nm], main.xml = main.xml)
    op.sub.dt = data.table(
      standard.name = op.list$std.name,
      synonyms = op.list$syno,
      cvcl.id = op.list$cvcl,
      disease = op.list$dis, 
      #tis = op.list$tis, 
      ncit.id = op.list$ncit, 
      category = op.list$cat, 
      depmap.id = op.list$dep, 
      sex = op.list$sex, 
      age = op.list$age, 
      metastatic.site = op.list$meta
    )
    op.dt <- rbind(op.dt, op.sub.dt)
  }
  print(paste("completed fetching data for",length(cellnames), "cell lines"))
  return(op.dt)
}


