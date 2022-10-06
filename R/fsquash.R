#' Recursively flatten a `data.frame` while correctly handling nested lists
#'   of `data.frame`s within `list` columns.
#'
#' @param x `data.frame` Table to squash, recursively flattening list columns
#'   and nested `data.frame`s.
#'
#' @return `data.frame` The data in `x`, but with only atomic column types.
#'   Values are repeated and mappings retained if the nested colums contain
#'   multiple entrier per row.
#'
#' @importFrom data.table is.data.table as.data.table rbindlist merge.data.table
#' @export
fsquash <- function(x) {
    stopifnot(is.data.frame(x))
    is_dt  <- data.table::is.data.table(x)
    if (!is_dt) {
        rn <- rownames(x)
        x <- data.table::as.data.table(x)
    }
    list_cols <- names(which(vapply(x, is.list, logical(1))))
    if (length(list_cols) == 0) return(x)
    for (i in list_cols) {
        col <- x[[i]]
        # add names as index to prevent loss of NULL values
        names(col) <- seq_along(col)
        # find any list columns
        list_class <- unique(vapply(col,
            FUN=function(x) class(x)[1L],
            FUN.VALUE=character(1)))
        if (length(list_class) > 1) {
            if (!("NULL" %in% list_class))
                stop("Heterogeneous list classes!", list_class)
            list_class <- setdiff(list_class, "NULL")
        }
        # recursively flatten list columns
        if (list_class %in% c("data.frame", "data.table", "list")) {
            df_ <- tryCatch({
                data.table::rbindlist(lapply(col, fsquash), fill=TRUE,
                    idcol=TRUE)
                },
                error=function(e) {
                    warning(e)
                    data.table::rbindlist(col, fill=TRUE, idcol=TRUE)
                })
            still_list_cols <- vapply(df_, is.list, logical(1))
            while (any(still_list_cols)) {
                df_ <- fsquash(df_)
                still_list_cols <- vapply(df_, is.list, logical(1))
            }
        }
        # use an index to handle cases of NULL values in list columns
        colnames(df_)[-1] <- paste0(i, ".", colnames(df_)[-1])
        x[[".id"]] <- as.character(seq_len(nrow(x)))
        x <- merge(x, df_, by=".id", all.x=TRUE)
        x[[i]] <- NULL
        x[[".id"]] <- NULL
    }
    return(if (is_dt) x else as.data.frame(x, row.names=rn))
}