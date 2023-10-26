#'
#'
#'
#'
#'
getDrugBank <- function(url='https://go.drugbank.com/releases/5-1-7/downloads/all-full-database',
    ...)
{
    stop("Not implemented yet!")
}


#' Extract the `pathway` item from a node in the DrugBank .xml file
#'
#' @param nodeL A `list` produced by calling `xml2::as_list` on a single node
#'   in the DrugBank database .xml file.
#' @return A `data.table` containing the pathway data for `nodeL`
#'
getPathways <- function(nodeL)
{
    stop("This function hasn't been implemented yet!")
}

#' Extract all drug targets from the `target` node of the DrugBank .XML file
#'
#' @param filePath `character(1)` Path to the Drug Bank .XML file.
#' @param progressbars `logical(1)` Should progress bars be shown for 
#'    parallelized parts of this functions execution.
#' @param ... Force subequent parameters to be named. Not used.
#' @param BPPARM Configurate parallelization backend. See 
#'   [`BiocParallel::bpparam`] for details.
#' 
#' @return A `data.table` containing all drug target data from the .XML file
#'   as a long format table. List columns are collapsed into string where items
#'   are delimited by '|' and names are captured as 'name=value' for each
#'   item. 
#'
#' @importFrom BiocParallel bplapply bpmapply bpparam bpprogressbar
#' @importFrom data.table data.table as.data.table rbindlist merge.data.table
#' @importFrom xml2 read_xml xml_find_all as_list
#' @export
getDrugTargets <- function(filePath, progressbars=TRUE, ..., BPPARAM=bpparam())
{
    ## TODO: Add informative message that document function exectution
    ## TODO: Refactor into more helper methods; make helper methods available
    ##>in the packge
    
    # -- configure progressbars
    if (progressbars) tryCatch({ bpprogressbar(BPPARAM) <- TRUE },
        error=function(e) warning('Setting progressbar failed: ', e))

    # load the xml file
    drugBank <- read_xml(filePath)

    # -- helper methods
    listItemsAsDT <- function(list, items=names(list)) as.data.table(lapply(list[items], unlist))
    sublistItemsAsDT <- function(list, ...)
        lapply(list, listItemsAsDT, ...)
    rbindSublistItemsAsDT <- function(list, ..., fill=TRUE)
        rbindlist(sublistItemsAsDT(list, ...), fill=fill)
    subsublistItemsAsDT <- function(list, ...) lapply(list, FUN=rbindSublistItemsAsDT, ...)
    extractSublistItem <- function(list, item='') lapply(list, FUN=`[[`, i=item)
    
    # -- get drug information
    # NOTE: namespace of interest is d1, see xml_ns(drugBank)
    drugs <- xml_find_all(drugBank, 'd1:drug')
    drug_primary_dbid <- unlist(as_list(xml_find_all(drugs, 'd1:drugbank-id[@primary="true"]')))
    drug_name <- unlist(as_list(xml_find_all(drugs, 'd1:name')))
    drug_fda_status <- unlist(lapply(
        lapply(as_list(xml_find_all(drugs, 'd1:groups')), FUN=unlist), 
        FUN=paste0, collapse='|')
        )

    # -- extract exteral identifiers
    # extract and parse the identifiers into a character vector
    drug_ext_ids_list <- bplapply(xml_find_all(drugs, 'd1:external-identifiers'), 
        FUN=as_list, BPPARAM=BPPARAM)
    drug_ext_ids_DTs <- subsublistItemsAsDT(drug_ext_ids_list)
    drug_ext_ids <- unlist(lapply(drug_ext_ids_DTs, 
        FUN=function(x) paste0(paste(x$resource, x$identifier, sep='='), 
            collapse='|')))
    
    # -- build the drug table
    drug_DT <- data.table(
        drugbank_id=drug_primary_dbid,
        drug_name=drug_name,
        drug_external_ids=drug_ext_ids,
        drug_fda_status=drug_fda_status)

    # -- get target information
    drug_targets <- xml_find_all(drugBank, 'd1:drug/d1:targets')

    targetList <- bplapply(drug_targets, FUN=as_list, BPPARAM=BPPARAM)

    # -- unnested items
    target_tables <- subsublistItemsAsDT(targetList, items=c('id', 'name', 
        'organism', 'known-action'))

    # -- nested items
    target_actions <-
        lapply(targetList, FUN=function(x) {
            action_list <- lapply(x, FUN=`[[`, i='actions')
            lapply(action_list, FUN=function(x) paste0(unlist(x), collapse='|'))
        })

    # -- build the target tables
    target_tables <- mapply(FUN=`[[<-`, x=target_tables, i='actions', value=target_actions)
    target_tables <- lapply(target_tables, 
        FUN=function(x) { colnames(x) <- paste0('target_', colnames(x)); x})

    # -- parse polypeptide tables
    polypeptideListList <- lapply(targetList,
        FUN=function(x) lapply(x, FUN=`[[`, i='polypeptide'))

    # -- unnested items
    polypetpide_items <- c('name', 'general-function', 'specific-function', 
        'gene-name', 'chromosome-location', 'locus', 'cellular-location', 
        'organism')
    polypetideTables <- subsublistItemsAsDT(polypeptideListList, 
        items=polypetpide_items)

    # -- nested items
    polypeptide_ext_id_list <- lapply(polypeptideListList, 
        FUN=extractSublistItem, item='external-identifiers')
    polypeptide_ext_DTs <- lapply(polypeptide_ext_id_list, 
        FUN=subsublistItemsAsDT)
    .collapseToString <- function(x) { 
        if (length(x) > 1)
            paste0(unlist(paste(x[[1]], x[[2]], sep='=')), collapse='|') 
        else 
            unlist(x) 
    }
    polypeptide_ext_ids <- lapply(polypeptide_ext_DTs, 
        FUN=function(x) unlist(lapply(x, .collapseToString)))

    polypeptide_pfam_list <- lapply(polypeptideListList, 
        FUN=extractSublistItem, item='pfams')
    polypeptide_pfam_DTs <- lapply(polypeptide_pfam_list, FUN=subsublistItemsAsDT)
    polypeptide_pfams <- lapply(polypeptide_pfam_DTs, 
        FUN=function(x) unlist(lapply(x, .collapseToString)))

    polypeptide_GO_list <- lapply(polypeptideListList, 
        FUN=extractSublistItem, item='go-classifiers')
    polypeptide_GO_DTs <- lapply(polypeptide_GO_list, FUN=subsublistItemsAsDT)
    polypeptide_go_classes <- lapply(polypeptide_GO_DTs, 
        FUN=function(x) unlist(lapply(x, .collapseToString)))

    polypeptideNestedTables <- mapply(data.table, 
        peptide_ext_ids=polypeptide_ext_ids, pfams=polypeptide_pfams, 
        go_classifers=polypeptide_go_classes)
    
    # -- assemble drug data.table
    polypeptideTableList <- mapply(cbind, polypetideTables, 
        polypeptideNestedTables)
    polypeptideTableList <- lapply(polypeptideTableList, 
        FUN=function(x) { if (ncol(x) > 0) { 
            colnames(x) <- paste0('peptide_', colnames(x)); x} else x})

    targetTableList <- mapply(cbind, target_tables, polypeptideTableList)

    names(targetTableList) <- drug_DT$drugbank_id
    targetDT <- rbindlist(targetTableList, fill=TRUE, idcol='drugbank_id')

    # -- merge the target data with the drug annotations
    drugTargetDT <- merge.data.table(targetDT, drug_DT, by='drugbank_id')
    colnames(drugTargetDT) <- gsub('-', '_', colnames(drugTargetDT))

    # -- remove any straggler list columns
    drugTargetDT <- drugTargetDT[, 
        lapply(.SD, unlist), .SDcols=is.list, 
        by=names(which(!vapply(drugTargetDT, is.list, logical(1))))]
    
    return(drugTargetDT)
}

#' Convert a list column in a `data.table` to a `data.table`
#'
#' @param col A `list` column in a `data.tabel`
#' @return A `data.table` produced by calling `as.data.table` on the list column
#'
#' @export
listColToDT <- function(col) {
    if (is.list(col)) {
        rbindlist(lapply(col, as.data.table), fill=TRUE)
    } else {
        col
    }
}

# Function testing scripts, not run unless this file is called as a script
# if (sys.nframe() == 0) {
#     library(AnnotationGx)
#     library(xml2)
#     library(data.table)
#     library(BiocParallel)

#     drugTargetDT <- getDrugTargets('local_data/drugbank.xml')
    
# }