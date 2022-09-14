#' @importFrom rvest read_html html_elements html_table
#' @importFrom checkmate assert_character
NULL


#' @export
grep_directory_names <- function(x) {
    grep(".*/$", x, value=TRUE)
}

#' @param x `character()` vector of file or directory paths
#' @param extensions `character(1)` regex of file extensions to match. Defaults
#'   to files whcih end with 2 to 5 alphanumeric character preceded by a dot.
#' @export
grep_file_names <- function(x, extensions="[[:alnum:]]{2,5}$") {
    checkmate::assert_character(extensions, max.len=1)
    grep(paste0(".*\\.", extensions), x, value=TRUE)
}

#' @param url `character(1)` URL to scrape all HTML tables from.
#' @export
get_remote_table <- function(url) {
    read_html(url) |>
        html_elements("body") |>
        html_table()
}

#' Recursively find file URLs from URL with an embedded HTML table (a remote directory)
#'
#' @param url `character(1)` A valid URL to scrape file data from. It is assumed
#'   that the returned HTML has a table in it which indicates the remote
#'   directory contents.
#' @param column `character(1)` Name of the column in the returned HTML table
#'   to match files and directories from. The `url` is automatically prepended
#'   to the values in the column, so they should be relative paths to other files
#'   in the remote directory.
#' @param extensions `character()` vector with one of more file extensions to
#'   to scrape from `url`. This should be the file extension only, with no dot.
#'   It could also be a valid regex expression. Please note that all values will
#'   be appended with "$" to match on the end of files and collapsed together
#'   with the "|" (logical or) regex operator. The default matches any
#'   alphanumeric file extensions between two and five character long.
#'
#' @return `character()` vector of remote file URLs to download from.
#'
#' @export
find_remote_files_recursive <- function(url, column="Name", extensions="[[:alnum:]]{2,5}") {
    # input validation
    checkmate::assert_character(url, min.chars=5, max.len=1)
    checkmate::assert_character(column, min.chars=1, max.len=1)
    checkmate::assert_character(extensions, min.chars=2, min.len=1)
    url <- gsub("/$", "", url)  # remove trailing slashes
    exts <- paste0(paste0(gsub("\\.", "", extensions), "$"), collapse="|")
    # extract all HTML tables
    tables <- get_remote_table(url)
    # extract directory and file names
    directories <- gsub("/$", "",
        unlist(lapply(tables, FUN=\(x) grep_directory_names(x[[column]]))))
    files <- unlist(lapply(tables, FUN=\(x) grep_file_names(x[[column]], exts)))
    # append files and directories to current url
    directories <- file.path(url, directories)
    files <- file.path(url, files)
    if (length(directories > 0)) {
        # directories become new URL for recursive calls
        new_files <- unlist(lapply(directories, FUN=find_remote_files_recursive))
        files <- c(files, new_files)
    } else {
        return(files)
    }
}

#' Retrieve a table of files available from an FTP download site by scraping
#'   the first HTML table at the specified URL.
#'
#' @param url `character(1)`
#' @param column `character(1)` Name of the column containing the FTP file paths.
#'   Defaults to "Name".
#' @param recursive `logical(1)` Should remote directories be searched recursively?
#'   Defaults to `FALSE`.
#'
#' @return `data.table` Table of available files and their download URLs and
#'  any additional metadata available in the HTML table at `url`.
#'
#' @importFrom data.table as.data.table rbindlist
#' @importFrom checkmate assert_character assert_logical
#' @export
scrapeRemoteFTPFilesTable <- function(url, column="Name", recursive=FALSE) {
    checkmate::assert_character(url, min.chars=5, max.len=1)
    checkmate::assert_logical(recursive, any.missing=FALSE)
    url <- gsub("/$", "", url)  # remove trailing slashes
    raw_table <- get_remote_table(url)[[1]]
    parsed_table <- data.table::as.data.table(raw_table)
    # drop artifactual file names from format of FTP tables
    parsed_table <- parsed_table[
        !(parsed_table[[column]]) %in% c("", "Parent Directory"),
    ]
    parsed_table$download_url <- paste0(url, "/", parsed_table$Name)
    directories <- parsed_table[[column]][grepl("/$", parsed_table[[column]])]
    if (length(directories) > 0 && isTRUE(recursive)) {
        for (dir in directories) {
            new_table <- getRemoteFTPFilesTable(
                url=paste0(url, "/", dir),
                recursive=recursive
            )
            new_table[, Name := paste0(dir, Name)]
            parsed_table <- rbindlist(list(
                parsed_table[Name != dir, ],
                new_table
            ), fill=TRUE, use.names=TRUE)
        }
    }
    drop_columns <- vapply(parsed_table,
        FUN=function(x) all(is.na(x)),
        FUN.VALUE=logical(1)
    )
    parsed_table <- parsed_table[, .SD, .SDcols=!drop_columns]
    return(parsed_table)
}