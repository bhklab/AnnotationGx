#' Helper function to connect
#'
#' @param ... Arguments passed through to `paste`. The `sep` arguement is
#'  already set to '/'.
#'
#' @return A `character` vector with the URL.
#'
#' @export
.buildURL <- function(...) paste(..., sep='/')

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
.collapse <- CoreGx::.collapse

.errorMsg <- CoreGx::.errorMsg

.warnMsg <- CoreGx::.warnMsg

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