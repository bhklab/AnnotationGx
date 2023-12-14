
## ---- Public utilities

#' Download a compressed file from a remote URL and extract it.
#'
#' @param url `character(1)` URL of the compressed file to download.
#' @param extract_fun `character(1)` or `function` to unzip the downloaded
#'   file with. Default is `utils::unzip`.
#' @param ... Fall through arguments to `extract_fun`. See documentation of
#'   the specified `extract_fun` for more details.
#'
#' @return `character` vector of unzipped file paths when `extract_fun` is
#'   `unzip` (default), otherwise the return value of the specified
#'   `extract_fun`.
#'
#' @seealso
#' [utils::unzip], [utils::untar], [R.utils::gunzip], [R.utils::bunzip2]
#'
#' @importFrom checkmate assertCharacter assertFunction
#' @export
downloadAndExtract <- function(url, extract_fun=unzip, ...) {
    assertCharacter(url, max.len=1)
    if (is.character(extract_fun)) extract_fun <- get(extract_fun)
    assertFunction(extract_fun)

    # download to a temporary file
    temp <- tempfile()
    download.file(url, destfile=temp)
    extract_fun_result <- extract_fun(temp, ...)
    unlink(temp)
    return(extract_fun_result)
}

#' Convert a character string to a named vector.
#'
#' This function takes a character string in the format "name=value|name=value|..."
#' and converts it into a named vector, where each name-value pair is separated by
#' a pipe symbol (|) and the name and value are separated by an equals sign (=).
#'
#' @param x A character string in the format "name=value|name=value|..."
#' @return A named vector with the names and values extracted from the input string.
#' @export
characterToNamedVector <- function(x) {
    Reduce(c, lapply(strsplit(unlist(strsplit(x, '\\|')), '='),
            FUN=\(x) structure(x[2], .Names=x[1]))) }

#' Get failure messages from an object
#'
#' This function retrieves failure messages from an object that has a 'failed' attribute.
#'
#' @param x An object with a 'failed' attribute
#' @return A data frame containing the query and corresponding failure messages
#' @export
getFailureMessages <- function(x) {
    if (is.null(attributes(x)$failed)) stop("There is no 'failed' attribute?")
    DtL <- Map(as.data.table, attributes(x)$failed)
    DT <- rbindlist(DtL, fill=TRUE)
    failedDT <- rbindlist(lapply(DT$failure, as.data.table), fill=TRUE)
    return(cbind(DT[, 'query'], failedDT))
}
#' Get the failed attribute of an object
#'
#' This function retrieves the "failed" attribute of an object.
#'
#' @param x An object
#' @return The "failed" attribute of the object
#' @export
getFailed <- function(x) attributes(x)$failed


#' Get the failed IDs from an object
#'
#' This function retrieves the "failed" IDs from an object by accessing the "query" element of the "failed" attribute.
#'
#' @param x An object
#' @return A vector of failed IDs
#' @export
getFailedIDs <- function(x) unlist(lapply(getFailed(x), `[[`, i='query'))


## ---- Private utilites

#' Helper function to connect
#'
#' @param ... Arguments passed through to `paste`. The `sep` arguement is
#'  already set to '/'.
#'
#' @return A `character` vector with the URL.
#'
#' @export
.buildURL <- function(...) paste0(na.omit(unlist(list(...))), collapse='/')

#' Collect repeated list item names into a single list item
#'
#' @param dataList A `list` with repeated names.
#' @return A `list` with unique names, collecting repeats into sublists.
#'
#' @export
.groupListByName <- function(dataList)
{
    uniqueNames <- unique(names(dataList))
    newList <- vector(mode='list', length(uniqueNames))
    names(newList) <- uniqueNames

    for (name in uniqueNames) {
        newList[[name]] <- dataList[name]
    }
    return(newList)
}


##FIXME:: Remove these in final package build
.collapse <- function(..., collapse = " ") paste0(..., collapse=collapse)

#' @importFrom crayon blue bold
.message <- function(...) {
    optionName <- paste0(packageName(), ".verbose")
    optionIsTRUE <- !is.null(getOption(optionName)) && getOption(optionName)
    verboseIsTRUE <- getOption("verbose")
    if (optionIsTRUE || verboseIsTRUE)
        message(blue$bold(.formatMessage(...)))
}

#' @importFrom crayon magenta blue cyan bold
.formatMessage <- function(..., collapse = ", ") {
    paste0(strwrap(paste0(..., collapse = collapse)), collapse = "\n")
}

#' @importFrom crayon magenta bold
.error <- function(...) {
    stop(magenta$bold(.formatMessage(...)), call. = FALSE)
}

#' @importFrom crayon cyan bold
.warning <- function(...) {
    warning(cyan$bold(.formatMessage(...)), call. = FALSE)
}

.funContext <- function(funName) paste0("[", packageName(), funName, "]\n")

#' Return the name of the function and the name of the package that function
#'   is in when called within an R function.
#'
#' For providing context in user messages, warnings and errors
#'
#' @param n `integer` How far up the call stack to look for context. Defaults to
#'   2 since it is assumed this function will be used inside of `message`,
#'   `warning` or `stop`.
#'
#' @return `list`:
#' - fun: `character` The name of the function where `.getExectutionContext()`
#' was called
#' - pkg: `character` The name of the package `fun` is from, if applicable.
#'
#' @md
#' @keywords internal
#' @importFrom rlang trace_back
#' @noRd
#' @aliases .context
.getExecutionContext <- function(n=2) {

    # name of function which called this function
    callStack <- rlang::trace_back()$calls
    context <- deparse(callStack[[length(callStack) - n]])

    # remove function arguments
    context <- gsub('\\(.*\\)', '', context)

    return(paste0('\n[', context, '] ', collapse='::'))
}
#' @noRd
.context <- .getExecutionContext
