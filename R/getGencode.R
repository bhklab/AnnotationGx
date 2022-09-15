#' Retrieve the index table for the Gencode FTP web page
#'
#' @param url `character(1)` Address of Gencode FTP web page. Default is page
#'   for Gencode Human files.
#' @param recursive `logical(1)` Should the file tables be recursively retrieved
#'   for directories? Default is `FALSE`, setting to `TRUE` will be be slow due
#'   to the large number of directories to scrape.
#'
#' @return `data.table` Table of files and directories available on the
#'   Gencode FTP web page.
#'
#' @importFrom data.table as.data.table
#' @export
getGencodeFTPTable <- function(
        url="https://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human",
        recursive=FALSE) {
    scrapeRemoteFTPFilesTable(url=url, recursive=recursive)
}


#' Retrieve a table listing all files available from Gencode for a specific
#'   database release.
#'
#' @param version `character(1)` Version number of Gencode annotations to
#'   retrieve file list for. Defaults to "latest", which is the most recent
#'   release. Use `getGencodeFTPTable()` to view available releases.
#'
#' @return `data.table` Table listing all files available for the selected
#'   `version`, along with URLs to download each file from.
#'
#' @importFrom checkmate assertCharacter
#' @importFrom data.table as.data.table rbindlist %ilike%
#' @export
getGencodeFilesTable <- function(version="latest",
        url="https://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human") {
    stopifnot(version == "latest" || !is.na(as.integer(version)))
    url <- gsub("/$", "", url) # deal with trailing slash on url
    gencode_ftp <- getGencodeFTPTable()
    version_dir <- gsub("/$", "", gencode_ftp[Name %ilike% version, Name])
    version_url <- paste0(url, "/", version_dir)
    checkmate::assertCharacter(version_url, max.len=1, any.missing=FALSE)
    file_table <- scrapeRemoteFTPFilesTable(url=version_url, recursive=TRUE)
    file_table$gencode_version <- version_dir
    return(file_table)
}


#' Download files from the Gencode FTP site and load them as the appropriate
#'   Bioconductor classes.
#'
#' @description
#' File descriptions are available at https://www.gencodegenes.org/human/.
#'
#' @param file `character(1)` Character vector of files to download from the
#' Gencode FTP site. Defaults to "comprehensive_chr". See details for options.
#' @param type `character(1)` One of "GTF", "FASTA" or "metadata". Defaults to
#'   "GTF".
#' @param version `character(1)` Gencode version to download for.
#'   Defaults to "latest". See `getGencodeFTPTable()` for options.
#' @param dir `character(1)` Path to download the file to. Defaults to
#'   `tempdir()`. When this value is `tempdir()` the downloaded file is
#'   automatically deleted when the function exits.
#' @param url `character(1)` Address of Gencode FTP web page. Default is page
#'   for Gencode Human files.
#'
#' @details
#'
#' ## Options for `file`
#'
#' When type="GTF" or "GFF":
#' * comprehensive_all
#' * comprehensive_chr
#' * basic_all
#' * basic_chr
#' * long_non_coding
#' * poly_a
#' * consensus_pseudogenes
#' * predicted_trna
#'
#' When type="FASTA":
#' * transcript
#' * protein_coding
#' * long_non_coding
#' * genome_all
#' * genome_primary
#'
#' When type="metadata":
#' * remarks
#' * entrez
#' * exon_evidence
#' * gene_source
#' * pdb
#' * poly_a
#' * pubmed_id
#' * ref_seq
#' * selenocyteine
#' * swiss_prot
#' * transcript_source
#' * transcipt_evidence
#' * trembl
#'
#' @return `GenomcRanges` object when type="GTF", `DNAStringSet` when type="FASTA",
#'   or ``
#'
#' @importFrom BiocIO import
#' @importFrom checkmate assert_subset
#' @importFrom data.table fread
#' @export
getGencodeFile <- function(
        file="comprehensive_chr",
        type="GTF",
        version="latest",
        dir=tempdir(),
        url="https://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human") {
    # validate input
    valid_files <- .valid_gencode_files(type)
    checkmate::assert_character(file, max.len=1, any.missing=FALSE)
    checkmate::assert_subset(file, names(valid_files), empty.ok=FALSE)
    # delete the file on exit if user doesn't provide a custom directory
    if (dir == tempdir()) on.exit(unlink(destfile))
    # download available files for selected Gencode version
    gencode_files <- getGencodeFilesTable(version=version, url=url)
    # find the file name and url
    pattern <- valid_files[[file]]
    match_row <- gencode_files[Name %ilike% pattern, .(Name, download_url)]
    file_name <- match_row[["Name"]]
    download_url <- match_row[["download_url"]]
    destfile <- file.path(dir, file_name)
    # download and load the file
    download.file(download_url, destfile=destfile)
    gencode_data <- switch(type,
        "GTF"=BiocIO::import(destfile),
        "FASTA"=BiocIO::import(destfile),
        "metadata"=tryCatch({
            data.table::fread(destfile)
        }, error=function(e) {
            warning("Failed to read with data.table::fread, falling back to readLines: ", e)
            readLines(gzfile(destfile))
        }),
        stop("Unknown type: ", type)
    )
    return(gencode_data)
}

#' Retrieve valid file arguments for getGencodeFile
#'
#' @keywords internal
.valid_gencode_files <- function(type=c("GTF", "FASTA", "metadata")) {
    type <- match.arg(type)
    valid_files <- list(
        "GTF"=list(
            "comprehensive_all"="",
            "comprehensive_chr"="gencode\\..{2,3}.\\.annotation\\.gtf\\.gz",
            "basic_all"="",
            "basic_chr"="",
            "long_non_coding"="",
            "poly_a"="",
            "consensus_pseudogenes"="",
            "predicted_trna"=""
        ),
        "FASTA"=list(
            "transcript"="",
            "protein_coding"="",
            "long_non_coding"="",
            "genome_all"="",
            "genome_primary"=""
        ),
        "metadata"=list(
            "remarks"="",
            "entrez"="",
            "exon_evidence"="",
            "gene_source"="",
            "pdb"="",
            "poly_a"="",
            "pubmed_id"="",
            "ref_seq"="",
            "selenocyteine"="",
            "swiss_prot"="",
            "transcript_source"="",
            "transcipt_evidence"="",
            "trembl"=""
        )
    )
    return(valid_files[[type]])
}