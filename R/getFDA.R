#' Download and extract the FDA OrangeBook files from the FDA website
#'
#' @param url `character(1)` URL to download the OrangeBook files from.
#' @param output_dir ``
#'
#' @return `character` vector of paths to the FDA OrangeBook files.
#'
#' @export
downloadFDAOrangeBook <- function(url="https://www.fda.gov/media/76860/download",
        output_dir=tempdir()) {
    file_paths <- downloadAndExtract(url, extract_fun=unzip, exdir=output_dir)
    return(file_paths)
}


#' Download and load the FDA OrangeBook products file
#'
#' @param file_path `character` Optional path to the FDA OrangeBook products
#'   file. If excluded the file is downloaded from the FDA website.
#'
#' @return `data.table` FDA OrangeBook products table.
#'
#' @export
getFDAOrangeBookProducts <- function(file_path, ...) {
    if (missing(file_path))
        file_path <- grep("products.txt", downloadFDAOrangeBook(), value=TRUE)
    products_df <- fread(file_path)
    return(products_df)
}