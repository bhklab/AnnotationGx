

.parseQueryToDT <- function(resp){
    data.table::as.data.table(resp[[1]][[1]])
}


