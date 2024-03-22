library(AnnotationGx)
library(testthat)
library(checkmate)

ids <- c("HT")
to = cellosaurus_fields(common=T)
from <- "idsy"
fuzzy <- FALSE
numResults <- 1000
sort = "ac"
parsed = FALSE
keep_duplicates = FALSE
query <- AnnotationGx:::.create_cellosaurus_queries(ids, from, fuzzy)
names(query) <- ids
requests <- AnnotationGx:::.build_cellosaurus_request(
    query = query,
    to = to,
    numResults = numResults,
    sort = sort,
    output = "TXT",
    fuzzy = fuzzy
)
responses <- AnnotationGx:::.perform_request_parallel(list(requests))
names(responses) <- as.character(ids)

lines <- httr2::resp_body_string(responses[[ids[1]]]) |>
    strsplit("\n") |>
    unlist()

# Test case 1: Test with a valid cell line name
lines <- readRDS(system.file("extdata", "cellosaurus_HT_raw_lines.RDS", package = "AnnotationGx"))

parsed_lines <- 
    Map(
    f = function(lines, i, j) {
        lines[i:(j - 1L)]
    },
    i = grep(pattern = "^ID\\s+", x = lines, value = FALSE),
    j = grep(pattern = "^//$", x = lines, value = FALSE),
    MoreArgs = list("lines" = lines),
    USE.NAMES = FALSE
)



requiredKeys = c("AC", "CA", "DT", "ID")
nestedKeys = c("DI", "DR", "HI", "OI", "OX", "WW")
optionalKeys = c("AG", "SX", "SY", "ACAS", "DIN", "DIO", "CH", "DTC", "DTU", "DTV", "FROM", "GROUP")
specialKeys = c("CC")

x <- strSplit(parsed_lines[[1]], split = "   ")
x <- split(x[, 2L], f = x[, 1L])


test_that(".formatComments works as expected", {
    # cc_column <- AnnotationGx:::.formatComments(x)

    dt <- data.table::data.table(rbind(x))

    da(); .formatComments(dt)

})

