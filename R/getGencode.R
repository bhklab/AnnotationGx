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
getGencodeFTPTable <- function(url="https://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human", recursive=FALSE) {
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
#' @importFrom checkmate assertIntegerish
#' @importFrom data.table as.data.table rbindlist
#' @export
getGencodeVersionFiles <- function(version="latest", url="https://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human") {
    stopifnot(version == "latest" || !is.na(as.integer(version)))
    url <- gsub("/$", "", url) # deal with trailing slash on url
    gencode_ftp <- getGencodeFTPTable()
    version_dir <- gsub("/$", "", gencode_ftp[Name %ilike% version, Name])
    version_url <- paste0(url, "/", version_dir)
    stopifnot(version_url)
    file_table <- getRemoteFTPFilesTable(url=version_url, recursive=TRUE)
    file_table$gencode_version <- version_dir
}
