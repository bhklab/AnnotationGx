.clean_parsed_annotation <- function(result) {
  # If returned value is a list, concatenate the elements into a single string with ";"
  if (length(result) > 1) {
    return(paste(result, collapse = "; "))
  }
  return(result)
}

.parseCHEMBLresponse <- function(result) {
  gsub("Compound::", "", result[["Record"]][["Reference"]][["SourceID"]]) |>
    .clean_parsed_annotation()
}

.parseCASresponse <- function(result) {
  df <- result[["Record"]][["Reference"]]
  df[df$SourceName == "CAS Common Chemistry", "SourceID"] |>
    .clean_parsed_annotation()
}

.parseNSCresponse <- function(result) {
  df <- result[["Record"]][["Reference"]]
  df[df$SourceName == "DTP/NCI", "SourceID"] |>
    .clean_parsed_annotation()
}

.parseATCresponse <- function(result) {
  df <- result[["Record"]][["Reference"]]
  df[df$SourceName == "WHO Anatomical Therapeutic Chemical (ATC) Classification", "SourceID"] |>
    .clean_parsed_annotation()
}

.parseDILIresponse <- function(result) {
  df <- result[["Record"]][["Reference"]]
  df[df$SourceName == "Drug Induced Liver Injury Rank (DILIrank) Dataset", "SourceID"] |>
    .clean_parsed_annotation()
}
