#' Match inside nested elements
#' 
#' @export
#'
#' @details
#' Intentionally only performs exact matching. Refer to `filterNested` function
#' for partial matching support with regular expressions.
#'
#' @param x
#' The values to be matched.
#'
#' @param table
#' The values to be matched against.
#' Applies across rows for `DataFrame` method.
#' 
#' @param keep_duplicates
#' A logical value indicating whether to keep duplicates.
#' 
#' @return `integer`.
#' A positional vector corresponding to values defined in `table` the same
#' size as `x`.
#'
#' @examples
#' showMethods("matchNested")
setGeneric(
    name = "matchNested",
    def = function(x, table, ..., keep_duplicates = FALSE) standardGeneric("matchNested"),
    signature = c("x", "table", "keep_duplicates")
)

`matchNested,list` <- 
    function(x, table, keep_duplicates){
        dt <- lapply(table, unlistNested) |> 
            .convert_nested_list_to_dt() 

        if (!keep_duplicates){
            dt <- dt[!duplicated(dt$value), , drop = FALSE]
        }

        dt[dt[["value"]] == x]$idx
}

`matchNested,data.table` <- 
    function(x, table, ...){
        checkmate::assert_data_table(table, min.rows = 1)

        dt <- apply(
            X = table,
            MARGIN = 1L,
            FUN = unlistNested,
            simplify = FALSE
            ) |>
            .convert_nested_list_to_dt() 

        if (!keep_duplicates){
            dt <- dt[!duplicated(dt$value), , drop = FALSE]
        }

        dt[dt[["value"]] == x]$idx
}


`matchNested,data.frame`  <-
    function(x, table, ...){
        checkmate::assert_data_frame(table, min.rows = 1)

        dt <- apply(
            X = table,
            MARGIN = 1L,
            FUN = unlistNested,
            simplify = FALSE
            ) |>
            .convert_nested_list_to_dt() 

        if (!keep_duplicates){
            dt <- dt[!duplicated(dt$value), , drop = FALSE]
        }

        dt[dt[["value"]] == x]$idx
}

#' @rdname matchNested
#' @export
setMethod(
    f = "matchNested",
    signature = signature(
        x = "character",
        table = "list"
    ),
    definition = `matchNested,list`
)

#' @rdname matchNested
#' @export
setMethod(
    f = "matchNested",
    signature = signature(
        x = "numeric",
        table = "list"
    ),
    definition = `matchNested,list`
)


#' @rdname matchNested
#' @export
setMethod(
    f = "matchNested",
    signature = signature(
        x = "character",
        table = "data.table"
    ),
    definition = `matchNested,data.table`
)

#' @rdname matchNested
#' @export
setMethod(
    f = "matchNested",
    signature = signature(
        x = "numeric",
        table = "data.table"
    ),
    definition = `matchNested,data.table`
)

#' @rdname matchNested
#' @export
setMethod(
    f = "matchNested",
    signature = signature(
        x = "character",
        table = "data.frame"
    ),
    definition = `matchNested,data.frame`
)

#' @rdname matchNested
#' @export
setMethod(
    f = "matchNested",
    signature = signature(
        x = "character",
        table = "data.frame"
    ),
    definition = `matchNested,data.frame`
)

#' Convert Nested List to Data Table
#'
#' This function converts a nested list into a data table with two columns: "idx" and "value".
#'
#' @param unlisted_elements A nested list to be converted into a data table.
#' @return A data table with two columns: "idx" and "value".
#' @noRd 
#' @keywords internal
.convert_nested_list_to_dt <- function(unlisted_elements){
    idx <- rep(seq_along(unlisted_elements), times = lengths(unlisted_elements))
    elements <- unlist(unlisted_elements, recursive = FALSE, use.names = FALSE)
    data.table::data.table("idx" = idx, "value" = elements)
}


#' Unlists a nested list and removes NA values and duplicates.
#'
#' This function takes a nested list as input and unlists it recursively. 
#' It then removes any NA values and duplicates from the resulting vector.
#'
#' @param element The nested list to be unlisted.
#' @return A vector with NA values and duplicates removed.
#' @examples
#' nested_list <- list(a = list(1, 2, NA), b = list(3, 4, 5))
#' unlistNested(nested_list)
#' # Output: [1] 1 2 3 4 5
#'
#' @export
unlistNested <- function(element){
    unlist(element, recursive = TRUE, use.names = FALSE) |>
        na.omit() |>
        unique()
}