#' @import checkmate
#' @importFrom httr POST PUT content upload_file warn_for_status progress
NULL


#' Build the required metadata for uploading a file to zenodo
#'
#' @param title `character(1)` A character vector with length greater than 3.
#'   Indicates the name of the Zenodo data entry for a new upload.
#' @param upload_type `character(1)` Type of Zenodo entry to create. Default
#'   is "dataset".
#' @param description `character(1)` Body of the Description field on the
#'   Zenodo entry.
#' @param names `character` One or more author names, formatted like
#'   "Last Name, First Name". Defaults to "Haibe-Kains, Benjamin"
#' @param affiliations `list(1)` or `list(length(names))` List of character
#'   vectors specifying author affiliations. May be length one, in which
#'   case the character vector of affiliations is recycled for all authors, or
#'   specify different vectors for each author name in `names`.
#' @param ... Additional valid metadata items for Zenodo. No validation is done
#'   yet, so make sure you know what you are doing before using this.
#'
#' @return `list` Named list with properly formatted metadata
#'
#' @md
#' @aliases zenodo_metadata
#' @export
zenodoMetadata <- function(title="New Upload", upload_type="dataset",
        description="Default description",
        names="Haibe-Kains, Benjamin",
        affiliations=list(c("Princess Margaret Cancer Centre")),
        ...) {
    checkmate::assert_character(title, len=1)
    checkmate::assert_character(upload_type, len=1)
    checkmate::assert_character(names, min.len=1)
    checkmate::assert(
        checkmate::check_list(affiliations, types="character", len=1),
        checkmate::check_list(affiliations, types="character",
            len=length(names))
    )

    c(list(title=title, upload_type=upload_type, description=description,
        creators=unname(Map(f=list, name=names, affiliation=affiliations))),
    ...)
}


#' Upload and optionally publish data to Zenodo
#'
#' @description
#' Make the PUT and POST request necessary to upload and optionally publish
#'   a file to Zenodo.
#'
#' @param file_path `character(1)` Path to the file to upload to Zenodo.
#' @param metadata `list` Named list of metadata required to publish publish
#'   a Zenodo entry, as returned by the `zenodoMetadata` function. See
#'   `?zenodoMetadata` for documentation of the correct format.
#' @param publish `logical(1)` Should the Zenodo entry be published, assuming
#'   the upload succeeds? Default is `FALSE`.
#' @param ... Fall through parameters to the `httr::POST` and `httr::PUT`
#'   method calls inside this function. Pass `httr::verbose()` to get detailed
#'   information about your API requests, useful for debugging.
#' @param url `character(1)` URL string for the API. Do not change unless you
#' know what you are doing.
#' @param access_token `character(1)` Zenodo access token. By deafult tries to
#' read this from the ZENODO_TOKEN environmet variable via `Sys.getenv`.
#' See details for more information.
#'
#' @return If all HTTP requests succeed, returns the details of the Zenodo entry
#'   as a list invisiblty. If any of the requests fail, it returns the failed
#'   request object for ease of debugging.
#'
#' @details
#'
#' ## Caveats
#' This function is an early prototype and is not very polished. The current
#'   version will create a new entry everytime the function is called, even
#'   if some of the steps succeeded. For now, you will need to manually delete
#'   these entries from you Zenodo account. However, this will be corrected in
#'   future updates.
#'
#' ## `access_token`
#'
#' API documentation is available at https://developers.zenodo.org/.
#'
#' To use this function you need a Zenodo access token for your account. A link
#' is available in the API documentation to create one. It is assumed you have
#' added the access token to your operating system environment in the
#' ZENODO_TOKEN variable.
#'
#' If you have not, you can specify an access token. For this function to
#' work the token needs to provide both upload and publish permissions on your
#' Zenodo account.
#'
#' ## Trouble Shooting
#' - *405 Errors*: Ensure that you have verified your Zenodo account email.
#' - *403 Errors*: Ensure you have checked both the `publish:actions` and
#'   `publish:write` boxes for your Zenodo access token.
#'
#' @seealso
#' [httr::POST], [httr::upload_file]
#'
#' @md
#' @aliases deposit_zenodo
#' @export
depositZenodo <- function(file_path, metadata=zenodoMetadata(), publish=FALSE,
        ...,
        url="https://zenodo.org/api/deposit/depositions",
        access_token=Sys.getenv("ZENODO_TOKEN")) {

    checkmate::assert_character(file_path, len=1)
    checkmate::assert_file_exists(file_path)
    checkmate::assert_character(url, len=1)
    checkmate::assert_character(access_token, len=1)
    checkmate::assert_list(metadata)

    # POST request to create Zenodo entry with associated metadata
    query <- list(access_token=access_token)
    message("Creating Zenodo entry...")
    create_entry_request <- POST(url, query=query,
        body=setNames(list(), character(0)),
        encode="json",
        ...
    )
    if (!http_status(create_entry_request)$category == "Success") {
        warn_for_status(create_entry_request)
        return(create_entry_request)
    }
    message("...COMPLETE\n")
    zenodo_entry <- content(create_entry_request, "parsed")

    # PUT request to upload the data for the entry
    deposit_file <- basename(file_path)
    bucket_url <- zenodo_entry$links$bucket
    # FIXME:: more robust URL parsing!
    bucket_file <- paste0(bucket_url, "/", deposit_file)
    message("Uploading data file to Zenodo...")
    upload_request <- PUT(bucket_file,
        query=query,
        body=upload_file(file_path),
        if (...length()) ... else progress("up")
    )
    message("...COMPLETE\n")

    # PUT request to create the object metadata
    deposition_url <- zenodo_entry$links$self
    message("Updating Zenodo entry metadata...")
    metadata_request <- PUT(deposition_url, query=query,
        body=list(metadata=metadata), encode="json",
        ...
    )
    if (!http_status(metadata_request)$category == "Success") {
        warn_for_status(metadata_request)
        return(metadata_request)
    }
    message("...COMPLETE\n")

    # Optional POST request to publish the dataset
    if (isTRUE(publish)) {
        publish_url <- zenodo_entry$links$publish
        message("Publishing Zenodo entry...")
        publish_request <- POST(publish_url, query=query)
        if (!http_status(publish_request)$category == "Success") {
            warn_for_status(publish_request)
            warning("Publication failed!")
            return(zenodo_entry)
        }
        message("...COMPLETE!\n")
    }

    # Inform user of status
    if (isTRUE(publish)) {
        message("Zenodo publication successful! You can view it at:\n  ",
            zenodo_entry$links$html)
    } else {
        message("Zenodo upload successful! You can view it at:\n  ",
            zenodo_entry$links$html)
    }

    return(invisible(zenodo_entry))
}


#'
#'
#'
#'
getZenodoFiles <- function() {

}