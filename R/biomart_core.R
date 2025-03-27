#' BioMart Information Class
#'
#' @description
#' Class that stores information about a BioMart database.
#'
#' @details
#' This class encapsulates metadata about a BioMart database, including
#' its name, display name, description, configuration, and other properties.
MartInfo <- R6::R6Class("MartInfo",
  public = list(
    #' @field name The internal name of the mart
    name = NULL,
    #' @field displayName The human-readable name of the mart
    displayName = NULL,
    #' @field description A textual description of the mart
    description = NULL,
    #' @field config Configuration identifier for the mart
    config = NULL,
    #' @field isHidden Flag indicating if the mart is hidden
    isHidden = NULL,
    #' @field operation Operation mode of the mart
    operation = NULL,
    #' @field meta Additional metadata for the mart
    meta = NULL,
    #' @field group Group the mart belongs to
    group = NULL,

    #' @description
    #' Initialize a new MartInfo object
    #'
    #' @param name Character, the internal name of the mart
    #' @param displayName Character, the human-readable name
    #' @param description Character, the mart description
    #' @param config Character, the configuration identifier
    #' @param isHidden Logical, whether the mart is hidden
    #' @param operation Character, the operation mode
    #' @param meta List, additional metadata
    #' @param group Character, the group the mart belongs to
    #' @return A new MartInfo object
    initialize = function(name, displayName, description, config,
                          isHidden, operation, meta, group) {
      self$name <- name
      self$displayName <- displayName
      self$description <- description
      self$config <- config
      self$isHidden <- isHidden
      self$operation <- operation
      self$meta <- meta
      self$group <- group
    },

    #' @description
    #' Print method for MartInfo objects
    #'
    #' @param ... Additional arguments (not used)
    #' @return Invisibly returns self
    print = function(...) {
      cat("<MartInfo>", "\n")
      cat("  name       :", self$name, "\n")
      cat("  config     :", self$config, "\n")
      cat("  displayName:", self$displayName, "\n")
      invisible(self)
    }
  )
)

#' BioMart Dataset Information Class
#'
#' @description
#' Class that stores information about a dataset within a BioMart database.
#'
#' @details
#' This class encapsulates metadata about a specific dataset in a
#'  BioMart database,
#' including its name, description, display name, and a reference
#'  to the parent mart.
DatasetInfo <- R6::R6Class("DatasetInfo",
  public = list(
    #' @field name The internal name of the dataset
    name = NULL,
    #' @field description A textual description of the dataset
    description = NULL,
    #' @field displayName The human-readable name of the dataset
    displayName = NULL,
    #' @field mart The MartInfo object this dataset belongs to
    mart = NULL,

    #' @description
    #' Initialize a new DatasetInfo object
    #'
    #' @param name Character, the internal name of the dataset
    #' @param description Character, the dataset description
    #' @param displayName Character, the human-readable name
    #' @param mart MartInfo object, the parent mart
    #' @return A new DatasetInfo object
    initialize = function(name, description, displayName, mart) {
      self$name <- name
      self$description <- description
      self$displayName <- displayName
      self$mart <- mart
    },

    #' @description
    #' Print method for DatasetInfo objects
    #'
    #' @param ... Additional arguments (not used)
    #' @return Invisibly returns self
    print = function(...) {
      cat("<DatasetInfo>\n")
      cat("  name       :", self$name, "\n")
      cat("  mart config:", self$mart$config, "\n")
      cat("  displayName:", self$displayName, "\n")
      invisible(self)
    }
  )
)

#' BioMart Filter Information Class
#'
#' @description
#' Represents a filter from a BioMart dataset.
#'
#' @details
#' This class encapsulates information about a filter available in a BioMart dataset.
#' Filters are used to specify conditions for querying and subsetting data from BioMart.
FilterInfo <- R6::R6Class("FilterInfo",
  public = list(
    #' @field name The internal name of the filter used in BioMart queries
    name = NULL,
    #' @field displayName The human-readable name of the filter shown in user interfaces
    displayName = NULL,
    #' @field description A textual description explaining the filter's purpose
    description = NULL,
    #' @field type The data type of the filter (e.g., "string", "boolean", "list")
    type = NULL,
    #' @field isHidden Logical flag indicating if the filter should be hidden in user interfaces
    isHidden = NULL,
    #' @field values Possible values for the filter if it has a fixed/enumerated set of options
    values = NULL,

    #' @description
    #' Initialize a new FilterInfo object
    #'
    #' @param name Character, the internal name of the filter used in API calls
    #' @param displayName Character, the human-readable name shown to users
    #' @param description Character, the filter description explaining its purpose
    #' @param type Character, the data type of the filter (e.g., "string", "boolean")
    #' @param isHidden Logical, whether the filter should be hidden in UIs
    #' @param values List or vector, possible values for the filter if applicable
    #' @return A new FilterInfo object
    initialize = function(name, displayName = NULL, description = NULL,
                          type = NULL, isHidden = NULL, values = NULL) {
      self$name <- name
      self$displayName <- displayName
      self$description <- description
      self$type <- type
      self$isHidden <- isHidden
      self$values <- values
    },

    #' @description
    #' Print method for FilterInfo objects
    #'
    #' @param ... Additional arguments passed to print methods (not used)
    #' @return Invisibly returns self for method chaining
    print = function(...) {
      cat("<FilterInfo>\n")
      cat("  name       :", self$name, "\n")
      cat("  type       :", self$type, "\n")
      cat("  displayName:", self$displayName, "\n")
      invisible(self)
    }
  )
)


#' BioMart Attribute Information Class
#'
#' @description
#' Represents an attribute from a BioMart dataset.
#'
#' @details
#' This class encapsulates information about an attribute available in a BioMart dataset.
#' Attributes are data fields that can be selected for retrieval in BioMart query results.
AttributeInfo <- R6::R6Class("AttributeInfo",
  public = list(
    #' @field name The internal name of the attribute used in BioMart queries
    name = NULL,
    #' @field displayName The human-readable name of the attribute shown in user interfaces
    displayName = NULL,
    #' @field description A textual description explaining what the attribute represents
    description = NULL,
    #' @field linkURL URL that provides additional information about the attribute
    linkURL = NULL,
    #' @field isHidden Logical flag indicating if the attribute should be hidden in user interfaces
    isHidden = NULL,

    #' @description
    #' Initialize a new AttributeInfo object
    #'
    #' @param name Character, the internal name of the attribute used in API calls
    #' @param displayName Character, the human-readable name shown to users
    #' @param description Character, the attribute description explaining what it represents
    #' @param linkURL Character, URL for additional information about the attribute
    #' @param isHidden Logical, whether the attribute should be hidden in UIs
    #' @return A new AttributeInfo object
    initialize = function(name, displayName = NULL, description = NULL,
                          linkURL = NULL, isHidden = NULL) {
      self$name <- name
      self$displayName <- displayName
      self$description <- description
      self$linkURL <- linkURL
      self$isHidden <- isHidden
    },

    #' @description
    #' Print method for AttributeInfo objects
    #'
    #' @param ... Additional arguments passed to print methods (not used)
    #' @return Invisibly returns self for method chaining
    print = function(...) {
      cat("<AttributeInfo>\n")
      cat("  name       :", self$name, "\n")
      cat("  displayName:", self$displayName, "\n")
      invisible(self)
    }
  )
)

#' BioMart Attribute Set Class
#'
#' @description
#' Represents a set of attributes from a BioMart dataset.
AttributeSet <- R6::R6Class("AttributeSet",
  public = list(
    #' @field attributes List of AttributeInfo objects
    attributes = NULL,

    #' @description
    #' Initialize a new AttributeSet object
    #'
    #' @param attributes List of AttributeInfo objects
    initialize = function(attributes) {
      self$attributes <- attributes
    },
    #' @param display_names Character vector of display names
    #' @return List of matching AttributeInfo objects
    get_by_display_name = function(display_names) {
      tmp <- self$attributes[
        sapply(
          self$attributes, function(attr) attr$displayName %in% display_names
        )
      ]
      AttributeSet$new(tmp)
    },
    #' @description Print method for AttributeSet objects
    #'
    #' @param ... Additional arguments passed to print methods (not used)
    #' @return Invisibly returns self
    print = function(...) {
      cat("<AttributeSet>\n")
      cat("  Attributes:\n")
      for (attr in self$attributes) {
        cat("    -", attr$displayName, "\n")
      }
      invisible(self)
    },
    #' @description Convert AttributeSet to list
    #' @return List of attributes DisplayName as strings
    as.list = function() {
      sapply(self$attributes, function(attr) attr$displayName)
    },
    #' @description Filter attributes based on regex pattern
    #' @param pattern Regular expression pattern to match against display names
    #' @param exclude Logical, if TRUE excludes matching patterns, if FALSE includes them
    #' @return A new AttributeSet with filtered attributes
    filter = function(pattern, exclude = FALSE) {
      matches <- grepl(
        pattern, sapply(self$attributes, function(attr) attr$displayName)
      )
      if (exclude) matches <- !matches
      AttributeSet$new(self$attributes[matches])
    }
  )
)

#' BioMart API Client
#'
#' @description
#' Client class for interacting with the BioMart REST API.
#'
#' @details
#' This class provides methods for querying BioMart REST API endpoints,
#' retrieving information about available marts, datasets, attributes, and filters.
#'
#' @examples
#' \dontrun{
#' # Create a client for Ensembl BioMart
#' client <- BioMartClient$new("https://www.ensembl.org")
#'
#' # Get available marts
#' marts <- client$get_marts()
#'
#' # Select a mart and get its datasets
#' ensembl <- marts[[1]]
#' datasets <- client$get_datasets(ensembl)
#' }
#'
#' @export
BioMartClient <- R6::R6Class("BioMartClient",
  public = list(
    #' @field base_url Base URL of the BioMart service
    base_url = NULL,
    #' @field path Path to the BioMart API on the server
    path = NULL,

    #' @description
    #' Initialize a new BioMartClient
    #'
    #' @param base_url Character, the base URL of the BioMart service
    #' @param path Character, the API path, defaults to "/biomart"
    #' @return A new BioMartClient object
    initialize = function(base_url, path = "/biomart") {
      self$base_url <- sub("/+$", "", base_url)
      self$path <- sub("^/+", "", path)
    },

    #' @description
    #' Retrieve available marts from the BioMart service
    #'
    #' @return List of MartInfo objects
    get_marts = function() {
      step <- cli::cli_progress_step("[Fetch] Fetching available marts")
      on.exit(cli::cli_progress_done(step), add = TRUE)

      res <- private$.request("marts.json") |>
        httr2::req_perform() |>
        httr2::resp_body_json()

      lapply(res, function(mart) {
        do.call(MartInfo$new, mart)
      })
    },

    #' @description
    #' Retrieve available datasets for a given mart
    #'
    #' @param mart MartInfo object, the mart to query
    #' @return List of DatasetInfo objects
    get_datasets = function(mart) {
      stopifnot(inherits(mart, "MartInfo"))
      step <- cli::cli_progress_step("[Dataset] Fetching datasets for {.val {mart$config}}")
      on.exit(cli::cli_progress_done(step), add = TRUE)

      res <- private$.request("datasets.json") |>
        httr2::req_url_query(config = mart$config) |>
        httr2::req_perform() |>
        httr2::resp_body_json()

      lapply(res, function(d) {
        DatasetInfo$new(
          name = d$name,
          description = d$description,
          displayName = d$displayName,
          mart = mart
        )
      })
    },

    #' @description
    #' Retrieve available attributes for a given dataset
    #'
    #' @param dataset DatasetInfo object, the dataset to query
    #' @return List of attribute information
    get_attributes = function(dataset) {
      stopifnot(inherits(dataset, "DatasetInfo"))
      step <- cli::cli_progress_step("[Attributes] Fetching attributes for {.val {dataset$name}} ({.val {dataset$mart$config}})")
      on.exit(cli::cli_progress_done(step), add = TRUE)

      res <- private$.request("attributes.json") |>
        httr2::req_url_query(datasets = dataset$name, config = dataset$mart$config) |>
        httr2::req_perform() |>
        httr2::resp_body_json()

      attrs <- lapply(res, function(a) {
        AttributeInfo$new(
          name = a$name,
          displayName = a$displayName,
          description = a$description,
          linkURL = a$linkURL,
          isHidden = a$isHidden
        )
      })

      AttributeSet$new(attrs)
    },

    #' @description
    #' Retrieve available filters for a given dataset
    #'
    #' @param dataset DatasetInfo object, the dataset to query
    #' @return List of filter information
    get_filters = function(dataset) {
      stopifnot(inherits(dataset, "DatasetInfo"))
      step <- cli::cli_progress_step("[Filters] Fetching filters for {.val {dataset$name}} ({.val {dataset$mart$config}})")
      on.exit(cli::cli_progress_done(step), add = TRUE)

      res <- private$.request("filters.json") |>
        httr2::req_url_query(datasets = dataset$name, config = dataset$mart$config) |>
        httr2::req_perform() |>
        httr2::resp_body_json()

      lapply(res, function(f) {
        FilterInfo$new(
          name = f$name,
          displayName = f$displayName,
          description = f$description,
          type = f$type,
          isHidden = f$isHidden,
          values = f$values
        )
      })
    }
  ),
  private = list(
    .request = function(endpoint) {
      full_url <- paste0(self$base_url, "/", self$path, "/", endpoint)
      httr2::request(full_url)
    }
  )
)
#' Build BioMart Query XML
#'
#' @param dataset A DatasetInfo object representing the BioMart dataset to query
#' @param filters List of filters, either:
#'   - Named list where names are filter names and values are filter values
#'   - List of FilterInfo objects with added 'value' field
#' @param attributes AttributeSet, list of AttributeInfo objects, or character vector
#'        specifying the attributes to retrieve
#' @param client_name Character, name of client for query identification (default: "biomartclient")
#' @param processor Character, output format processor type (default: "TSV")
#' @param header Logical, whether to include header row in results (default: TRUE)
#' @param limit Integer, maximum number of rows to return, -1 for unlimited (default: -1)
#'
#' @return Character string containing the formatted XML BioMart query
bm_query_builder <- function(dataset,
                             filters = list(),
                             attributes = character(),
                             client_name = "biomartclient",
                             processor = "TSV",
                             header = TRUE,
                             limit = -1) {
  stopifnot(inherits(dataset, "DatasetInfo"))

  # Handle AttributeInfo objects
  if (all(sapply(attributes, inherits, "AttributeInfo"))) {
    attributes <- vapply(attributes, function(a) a$name, character(1))
  } else if (inherits(attributes, "AttributeSet")) {
    attributes <- vapply(attributes$attributes, function(a) a$name, character(1))
  } else if (!is.character(attributes)) {
    stop("attributes must be a character vector or AttributeSet")
  } else {
    attributes <- as.character(attributes)
  }

  attr_xml <- paste0(
    sprintf('<Attribute name="%s"/>', attributes),
    collapse = ""
  )

  # Handle FilterInfo objects OR named list
  filter_xml <- ""

  if (length(filters) > 0) {
    if (all(sapply(filters, inherits, "FilterInfo"))) {
      # If user passed FilterInfo objects, extract value from each
      filter_xml <- paste(
        vapply(filters, function(f) {
          if (is.null(f$value)) {
            stop(sprintf("Filter '%s' is missing a value", f$name))
          }
          val <- paste(f$value, collapse = ",")
          # if the f$name has _text at the end, then we remove it
          name <- sub("_text$", "", f$name)
          sprintf('<Filter name="%s" value="%s"/>', name, val)
        }, character(1)),
        collapse = ""
      )
    } else {
      # Otherwise assume a named list: name -> value
      filter_xml <- paste(
        vapply(names(filters), function(name) {
          val <- paste(filters[[name]], collapse = ",")
          # if the f$name has _text at the end, then we remove it
          name <- sub("_text$", "", name)
          sprintf(
            '<Filter name="%s" value="%s" filter_list=""/>',
            name, val
          )
        }, character(1)),
        collapse = ""
      )
    }
  }
  xml <- sprintf(
    '<!DOCTYPE Query><Query client="%s" processor="%s" header="%d" limit="%d">
    <Dataset name="%s" config="%s">%s%s</Dataset></Query>',
    client_name, processor, as.integer(header), as.integer(limit),
    dataset$name, dataset$mart$config, filter_xml, attr_xml
  )
  xml <- gsub('"', "'", xml)
  xml <- gsub("\n\\s+", "", xml) # Remove newlines and indentation
}

#' Query HGNC BioMart by Gene Symbols
#'
#' @description
#' Retrieves data from the HGNC BioMart for a list of gene symbols.
#'
#' @details
#' This function connects to the HGNC BioMart service and retrieves specified
#' attributes for a list of gene symbols. It automatically handles the selection
#' of marts and datasets based on provided parameters.
#'
#' @param genes Character vector of HGNC gene symbols.
#' @param attributes Character vector of attribute display names to return.
#' @param mart_name Character, optional name or display name of the mart to use
#'   (default: first available mart).
#' @param dataset_name Character, optional name or display name of the dataset
#'   to use (default: first available dataset).
#'
#' @return A data.table with results for matching gene symbols and attributes.
#' @export
query_hgnc_by_genes <- function(genes, attributes, mart_name = NULL,
                                dataset_name = NULL) {
  stopifnot(is.character(genes), is.character(attributes))

  client <- BioMartClient$new("https://biomart.genenames.org")
  marts <- client$get_marts()
  # TODO:: since this function is specific for genes,
  # we shouldnt be allowing user to choose mart and dataset
  # extract this selection to a separate function
  # and make this function only for hgnc genes
  # this way we can also maybe use something else?

  # Select mart: either by name/displayName or default to first one
  if (!is.null(mart_name)) {
    mart_idx <- which(sapply(marts, function(m) {
      m$name == mart_name || m$displayName == mart_name
    }))
    if (length(mart_idx) == 0) {
      available_marts <- vapply(marts, function(m) {
        paste0(m$name, " (", m$displayName, ")")
      }, character(1))
      stop(
        "Invalid mart name: '", mart_name, "'. Available marts: ",
        paste(available_marts, collapse = ", ")
      )
    }
    mart <- marts[[mart_idx[1]]]
  } else {
    mart <- marts[[1]]
  }

  datasets <- client$get_datasets(mart)

  # Select dataset: either by name/displayName or default to first one
  if (!is.null(dataset_name)) {
    dataset_idx <- which(sapply(datasets, function(d) {
      d$name == dataset_name || d$displayName == dataset_name
    }))
    if (length(dataset_idx) == 0) {
      available_datasets <- vapply(datasets, function(d) {
        paste0(d$name, " (", d$displayName, ")")
      }, character(1))
      stop(
        "Invalid dataset name: '", dataset_name,
        "'. Available datasets: ",
        paste(available_datasets, collapse = ", ")
      )
    }
    dset <- datasets[[dataset_idx[1]]]
  } else {
    dset <- datasets[[1]]
  }

  attrset <- client$get_attributes(dset)

  valid_attrs <- attrset$get_by_display_name(attributes)
  if (length(valid_attrs$attributes) != length(attributes)) {
    available <- vapply(attrset$attributes, \(a) a$displayName, character(1))
    missing <- setdiff(attributes, available)
    errmsg <- paste("Invalid attribute(s):", paste(missing, collapse = ", "))
    # show valid attributes if available
    if (length(available) > 0) {
      errmsg <- paste(
        errmsg, "\nAvailable attributes:\n\t-",
        paste(available, collapse = "\n\t- ")
      )
    }
    stop(errmsg)
  }

  q <- bm_query_builder(
    dataset = dset,
    attributes = valid_attrs,
    filters = list(
      hgnc_gene__approved_symbol_1010_text = paste(genes, collapse = ",")
    )
  )

  resp <- httr2::request("https://biomart.genenames.org/martservice/results") |>
    httr2::req_url_query(query = q) |>
    httr2::req_perform() |>
    .parse_resp_tsv()

  data.table::as.data.table(resp)
}
