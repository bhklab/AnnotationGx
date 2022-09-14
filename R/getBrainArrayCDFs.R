#' Retrieve a table of all available CDF files from BrainArray
#'
#' @param `character(1)` URL to a page listing CDF files on BrainArray. Defaults
#'   to the ENSG version for Ensembl IDs.
#'
#' @examples
#' brain_array <- getBrainArrayTable()
#'
#' @importFrom rvest read_html html_table
#' @importFrom data.table as.data.table tstrsplit
#' @export
getBrainArrayTable <- function(url="http://brainarray.mbni.med.umich.edu/Brainarray/Database/CustomCDF/25.0.0/ensg.asp") {

    # fetch the HTML from the URL
    content <- url |> read_html()

    # extract modes containing an HTML table
    content |>
        html_elements("table") -> # select the html tables unprocessed
        table_html

    # extract the table as a tibble; but it doesn't get the links for table items
    #  and is therefore pretty much useless in this case
    # will use it for the table column names
    template_table <- html_table(table_html[[2]])

    table_html[2] |>
        html_elements("tr") |> # pull out the rows
        lapply(html_elements, xpath=".//td") |>
        lapply(as.character) ->
        row_list

    dt <- do.call(rbind, row_list[-c(1, 2)]) |>
        as.data.table()

    # remove the header
    brain_array <- dt[-(1:2), -1, with=FALSE]
    # parse the header into nice column names
    .format_html_header <- function(x) {
        x |>
        gsub("<[^<>]*>|\n", "", x=_) |>
        gsub(" ", "", x=_) |>
        gsub("%", "Pct", x=_) |>
        gsub("#", "Num", x=_)
    }
    top_header <-  unlist(template_table[1, ]) |>
        .format_html_header()

    lower_header <- unlist(template_table[2, ]) |>
        .format_html_header()

    header <- Map(
        function(x, y) paste0(unique(c(x, y)), collapse="."),
        top_header,
        lower_header
    )

    colnames(brain_array) <- unlist(header)[-1]
    brain_array <- brain_array[,
        lapply(.SD, FUN=gsub, pattern="<td>|<.td>|\n", replacement="")
    ]
    brain_array <- brain_array[,
        lapply(.SD, strsplit, split="<a href=", fixed=TRUE)
    ]
    suppressWarnings({
    brian_array_final <- brain_array[,
        lapply(.SD, function(x) {
            x <- na.omit(unlist(x))
            x <- x[x != ""]
            type.convert(gsub('^[^\\"]*\\"|\\"[^\\"]*$|^.*href=', "", x))
        })
    ]
    })
    brain_array_final <- brian_array_final[,
        lapply(.SD, function(x) paste0(unique(x), collapse=";")),
        by=CustomCDFName
    ]
    brain_array_final[,
        c("RSourcePackage.C", "RsourcePackge.P") :=
            tstrsplit(RSourcePackage.CP, split=";")
    ]
    brain_array_final[,
        c("BinaryPackage.C", "BinaryPackage.P") :=
            tstrsplit(RBinaryPackage.CP, split=";")
    ]
    brain_array_final[,
        c("RSourcePackage.CP", "RBinaryPackage.CP") := NULL
    ]
    return(brain_array_final[])
}