#'
#'
#'
#'
#'
#' @export
getDrugBank <- function(url='https://go.drugbank.com/releases/5-1-7/downloads/all-full-database',
    ...)
{
    message("Not implemented yet :(")
}

#' Read in DrugBank database from an XML file as a list of data.tables
#'
#' **WARNING**: Due to the size of the database dump it is likely this
#'   function will crash your computer if you have less than 32 GB of RAM!
#'
#' @param xmlPath
#' @param outPath `character`
#' @param ...
#'
#' @import xml2
#' @export
parseDrugBankXML <- function(xmlPath, outPath)
{
    drugBank <- read_xml(filePath)

    getNodePathway <- function(node) getPathways(xml2::as_list(node))
    a <- Sys.time()
    pathwayL <- bplapply(xml_children(drugBank), getNodePathway)
    b <- Sys.time()
    diff <- b - a
    diff

    pathwayDT <- rbindlist(pathwayL, fill=TRUE)
    pathwayDT <- pathwayDT[, lapply(.SD, unlist)]

    if (!missing(outPath)) {
        fwrite(pathwayDT, outPath)
    }

    return(pathwayDT)
}

#' Collect repeated list item names into a single list item
#'
#' @param dataList A `list` with repeated names.
#' @return A `list` with unique names, collecting repeats into sublists.
#'
#' @export
groupListByName <- function(dataList) {

    uniqueNames <- unique(names(dataList))
    newList <- vector(mode='list', length(uniqueNames))
    names(newList) <- uniqueNames

    for (name in uniqueNames) {
        newList[[name]] <- dataList[name]
    }
    return(newList)
}

#' Extract the `pathway` item from a node in the DrugBank .xml file
#'
#' @param nodeL A `list` produced by calling `xml2::as_list` on a single node
#'   in the DrugBank database .xml file.
#' @return A `data.table` containing the pathway data for `nodeL`
#'
#' @export
getPathways <- function(nodeL) {
    pathwayL <- nodeL$pathways$pathway
    pathDT <- as.data.table(pathwayL)
    rm(pathwayL)
    pathDT <- pathDT[, lapply(.SD, listColToDT)]
    setnames(pathDT,
        old=c("smpdb.id.V1", "name.V1", "category.V1", "drugs.drugbank.id",
            "drugs.name", "enzymes.V1"),
        new=c('smpdb_id', 'pathway', 'category', 'drugbank_id', 'drug_name',
            'uniprot_id'),
        skip_absent=TRUE)
    return(pathDT)
}


#' Convert a list column in a `data.table` to a `data.table`
#'
#' @param col A `list` column in a `data.tabel`
#' @return A `data.table` produced by calling `as.data.table` on the list column
#'
#' @export
listColToDT <- function(col) {
    rbindlist(lapply(col, as.data.table))
}

if (sys.nframe() == 0) {
    library(xml2)
    library(data.table)
    library(BiocParallel)
    library(jsonlite)
    library(httr)
    filePath <- 'data/full database.xml'

    DT <- parseDrugBankXML(filePath)
}