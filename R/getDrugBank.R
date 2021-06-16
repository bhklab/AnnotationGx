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
#' @param xmlPath a
#' @param outPath `character`
#' @param ... a
#'
#' @import xml2
#' @export
parseDrugBankXML <- function(xmlPath, table=c('pathways', 'targets'),
    outPath=tempdir(), ..., nodeFun)
{
    if (missing(nodeFun))
        extractFun <- switch(table,
            'pathways'=getPathways,
            'targets'=getTargets)
    else
        extractFun <- nodeFun

    drugBank <- read_xml(xmlPath)

    getTableFromNode <- function(node) extractFun(xml2::as_list(node))
    dataL <- bptry(bplapply(xml_children(drugBank), getTableFromNode))

    dataL <- dataL[bpok(dataL)]
    qsave(dataL, file.path(outPath, 'targetL.qs'))

    dataDT <- rbindlist(dataL, fill=TRUE)
    # Get rid of list columns
    ifNotCharAsChar <- function(col)
        if (!is.character(col)) as.character(col) else col
    dataDT <- dataDT[, lapply(.SD, ifNotCharAsChar)]
    # Fix "NULL" strings
    for (colIdx in seq_len(ncol(dataDT))) {
        set(dataDT, j=colIdx,
            value=fifelse(dataDT[[colIdx]] == 'NULL' |
                dataDT[[colIdx]] == 'list(NULL)', NA_character_,
                    dataDT[[colIdx]]))
    }

    ## TODO:: add table specific post processing (e.g., filters on columns)

    if (!missing(outPath)) {
        fwrite(dataDT, file.path(outPath, paste0(table, '.csv')))
    }

    rm(drugBank); gc()
    return(dataDT)
}

#' Extract the `pathway` item from a node in the DrugBank .xml file
#'
#' @param nodeL A `list` produced by calling `xml2::as_list` on a single node
#'   in the DrugBank database .xml file.
#' @return A `data.table` containing the pathway data for `nodeL`
#'
#' @export
getPathways <- function(nodeL)
{
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

#' Extract the `target` item from a node in the DrugBank .xml file
#'
#' @param nodeL A `list` produced by calling `xml2::as_list` on a single node
#'   in the DrugBank database .xml file.
#' @return A `data.table` containing the target data for `nodeL`
#'
#' @export
getTargets <- function(nodeL)
{
    # -- handle edge case where nodeL is wrapped in unnamed list
    if (is.null(names(nodeL))) nodeL <- nodeL[[1]]
    if (is.null(names(nodeL))) return(data.table())

    # -- get the drug annotations for the node
    drugName <- unlist(nodeL$name)
    print(drugName)
    drugBankID <- unlist(nodeL$`drugbank-id`)

    # -- extract the desired list
    targetL <- nodeL$targets$target
    #rm(nodeL); gc()
    if (is.null(targetL)) return(data.table())

    # -- define local helpers
    .listUnique <- function(...) list(unique(...))

    # -- preprocess the peptide list
    isDuplicated <- duplicated(names(targetL))
    names(targetL)[isDuplicated] <- paste0(names(targetL)[isDuplicated],
        seq_len(sum(isDuplicated)))

    ## TODO:: refactor this mess
    peptides <- targetL[grepl('polypeptide', names(targetL))]
    targetL <- targetL[!isDuplicated]
    for (peptideL in peptides) {
        if (length(targetL$polypeptide) > 0) {
            peptItemLength <- vapply(peptideL, length, numeric(1))
            peptideL[peptItemLength > 1] <- lapply(peptideL[peptItemLength > 1], data.table)
            peptideL[peptItemLength > 1] <- lapply(peptideL[peptItemLength > 1], `[`, j=listColToDT(V1))

            # check for nested lists and unlist them
            listItemIsList <- function(list) if (length(list) >= 1) is.list(list[[1]]) else FALSE
            peptItemIsList <- vapply(peptideL, listItemIsList, logical(1))
            isListLenEq1 <- peptItemLength == 1 & !peptItemIsList
            peptideL[isListLenEq1] <- lapply(peptideL[isListLenEq1], unlist)

            # preprocess specific list items
            if (length(peptideL$`external-identifiers`) > 0) {
                if (!is.data.table(peptideL$`external-identifiers`)) {
                    peptideL$`external-identifiers` <-
                        data.table(peptideL$`external-identifiers`)
                    peptideL$`external-identifiers` <-
                        peptideL$`external-identifiers`[, listColToDT(V1)]
                }
                peptideL$`external-identifiers` <-
                    peptideL$`external-identifiers`[, lapply(.SD, unlist)]
                peptideL$`external-identifiers`[, resource := gsub(' ', '', resource)]
                peptideL$`external-identifiers` <-
                    dcast(peptideL$`external-identifiers`, . ~ resource,
                        value.var='identifier')[, . := NULL]
            }
            if (length(peptideL$`go-classifiers`) > 0) {
                if (!is.data.table(peptideL$`go-classifiers`)) {
                    peptideL$`go-classifiers` <- data.table(peptideL$`go-classifiers`)
                    peptideL$`go-classifiers` <- peptideL$`go-classifiers`[, listColToDT(V1)]
                }
                peptideL$`go-classifiers` <- peptideL$`go-classifiers`[, lapply(.SD, unlist)]
                peptideL$`go-classifiers` <- dcast(peptideL$`go-classifiers`, ... ~ category,
                    value.var='description', fun.aggregate=list)[, . := NULL]
            }
            if (length(peptideL$pfams) > 0) {
                if (!is.data.table(peptideL$pfams)) {
                    peptideL$pfams <- data.table(peptideL$pfams)
                    peptideL$pfams <- peptideL$pfams[, listColToDT(V1)]
                }
                peptideL$pfams <- peptideL$pfams[,
                    list(id=mapply(paste, identifier, name, MoreArgs=list(sep=' = '),
                        SIMPLIFY=FALSE))]
            }

            # coerce to data.table
            peptideDT <- as.data.table(peptideL)
            peptideDT <- peptideDT[, lapply(.SD, unlist)] # remove any list columns

            # cast to a single row
            peptideDT <- dcast(peptideDT, name ~ ...,
                value.var=setdiff(colnames(peptideDT), 'name'),
                fun.aggregate=.listUnique)

            if (is.data.table(targetL$polypeptide)) {
                targetL$polypeptide <-
                    rbindlist(list(targetL$polypeptide, peptideDT), fill=TRUE)
            } else {
                targetL$polypeptide <- peptideDT
            }
            rm(peptideDT); gc()
        }
    }

    ## -- preprocess the reference list
    if (length(targetL$references$articles) > 0) {
        refDT <- data.table(targetL$references$articles)[, listColToDT(V1)]
        targetL$references <- refDT$`pubmed-id`
        targetL$citation <- refDT$citation
        rm(refDT)
    } else {
        targetL$citation <- NULL
    }

    ## -- preprocess actions
    if (length(targetL$actions) > 0)
        targetL$actions <- unique(unlist(targetL$actions))

    ## -- convert to data.table
    targetDT <- as.data.table(targetL)
    rm(targetL)
    targetDT <- targetDT[, lapply(.SD, unlist, recursive=FALSE)]
    if (any(isDuplicated)) {
        targetDT <- dcast(targetDT, polypeptide.name ~ ...,
            value.var=setdiff(colnames(targetDT), c('polypetide.name')),
            fun.aggregate=.listUnique)
    } else {
        targetDT <- dcast(targetDT, id + name ~ ...,
            value.var=setdiff(colnames(targetDT), c('id', 'name')),
            fun.aggregate=.listUnique)
    }

    # -- handle edge case where column name is wrong
    setnames(targetDT, 'polypeptide.name.1', 'polypeptide.name', skip_absent=TRUE)

    # --
    targetDT[, `:=`(drugName=drugName, drugBankID=drugBankID)]
    return(targetDT)
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

if (sys.nframe() == 0) {
    library(AnnotationGx)
    library(xml2)
    library(data.table)
    library(BiocParallel)

    # -- read in the data

    # load the xml file
    filePath <- 'local_data/drugbank.xml'
    drugBank <- read_xml(filePath)

    # helper function
    .find_all_as_list <- function(...) as_list(xml_find_all(...))

    # -- get drug information
    # NOTE: namespace of interest is d1, see xml_ns(drugBank)
    drugs <- xml_find_all(drugBank, 'd1:drug')
    drug_primary_dbid <- xml_find_all(drugs, 'd1:drugbank-id[@primary="true"]') |>
        as_list() |> unlist()
    drug_name <- xml_find_all(drugs, 'd1:name') |> as_list() |> unlist()
    drug_fda_status <- xml_find_all(drugs, 'd1:groups') |> as_list() |> 
        lapply(FUN=unlist) |> lapply(FUN=paste0, collapse='|') |> unlist()

    # -- extract exteral identifiers
    # configure paralellization
    bpparam <- MulticoreParam(progressbar=TRUE)
    # extract and parse the identifiers into a character vector
    drug_ext_ids_list <- xml_find_all(drugs, 'd1:external-identifiers') |> 
        bplapply(FUN=as_list, BPPARAM=bpparam)
    drug_ext_ids_DTs <- drug_ext_ids_list |>
        lapply(FUN=\(x) { rbindlist(lapply(x, as.data.table)) })
    drug_ext_ids <- unlist(lapply(drug_ext_ids_DTs, 
        FUN=\(x) paste0(paste(x$resource, x$identifier, sep='='), collapse='|')))

    # -- build the drug table
    drug_DT <- data.table(
        drugbank_id=drug_primary_dbid, 
        name=drug_name, 
        external_ids=drug_ext_ids, 
        fda_status=drug_fda_status)

    # -- get target information
    drug_targets <- xml_find_all(drugBank, 'd1:drug/d1:targets')

    targetList <- drug_targets |> bplapply(FUN=as_list, BPPARAM=bpparam)

    # extract non-nested items
    id <- drug_targets |> 
        bplapply(FUN=.find_all_as_list, 'd1:target/d1:id', BPPARAM=bpparam)
    name <- .find_all_as_list(drug_targets, 'd1:target/d1:name') |> unlist()
    organism <- .find_all_as_list(drug_targets, 'd1:target/d1:organism') |> unlist()
    known_action <- .find_all_as_list(drug_targets, 'd1:target/d1:known-action') |> 
        unlist()

    # extract nested items
    actions <- .find_all_as_list(target, 'd1:actions')
    references <- .find_all_as_list(target, 'd1:references')
    polypeptide <- .find_all_as_list(target, 'd1:polypeptide')

    # -- polypeptide for each target
    name <- unlist(.find_all_as_list(target, 'd1:name'))

}