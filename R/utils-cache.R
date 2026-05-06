#' Get the package cache directory.
#'
#' @return Character(1) path to the AnnotationGx cache directory.
#' @keywords internal
#' @noRd
.annotationgx_cache_dir <- function() {
  cache_dir <- getOption("annotationgx.cache.dir")

  if (is.null(cache_dir) || length(cache_dir) != 1L || !nzchar(cache_dir)) {
    cache_dir <- tools::R_user_dir("AnnotationGx", which = "cache")
  }

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_dir
}


#' Get the package cache handle.
#'
#' @return A `BiocFileCache` object.
#' @keywords internal
#' @noRd
.annotationgx_cache <- function() {
  BiocFileCache::BiocFileCache(.annotationgx_cache_dir(), ask = FALSE)
}


#' Check whether persistent caching is enabled.
#'
#' @return Logical(1) indicating whether persistent caching is enabled.
#' @keywords internal
#' @noRd
.cache_enabled <- function() {
  isTRUE(getOption("annotationgx.cache.use", TRUE))
}


#' Check whether cached values should be refreshed.
#'
#' @return Logical(1) indicating whether cache refresh is enabled.
#' @keywords internal
#' @noRd
.cache_refresh_enabled <- function() {
  isTRUE(getOption("annotationgx.cache.refresh", FALSE))
}


#' Hash cache inputs into a stable key fragment.
#'
#' @param x Arbitrary R object.
#' @return Character(1) hash string.
#' @keywords internal
#' @noRd
.cache_hash <- function(x) {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)

  saveRDS(x, file = tmp, version = 2)
  unname(tools::md5sum(tmp))
}


#' Build a cache key.
#'
#' @param namespace Character(1) namespace for the cached value.
#' @param params List of inputs describing the cached value.
#' @return Character(1) cache key.
#' @keywords internal
#' @noRd
.cache_key <- function(namespace, params = list()) {
  paste(
    "AnnotationGx",
    as.character(utils::packageVersion("AnnotationGx")),
    namespace,
    .cache_hash(params),
    sep = "/"
  )
}


#' Look up a cache record identifier.
#'
#' @param bfc A `BiocFileCache` object.
#' @param key Character(1) cache key.
#' @return Character(1) cache record id or `NULL`.
#' @keywords internal
#' @noRd
.cache_rid <- function(bfc, key) {
  hits <- BiocFileCache::bfcquery(
    bfc,
    key,
    field = "rname",
    exact = TRUE
  )

  if (nrow(hits) == 0L) {
    return(NULL)
  }

  hits$rid[[1L]]
}


#' Read a cached R object if present.
#'
#' @param key Character(1) cache key.
#' @return Cached object or `NULL` if unavailable.
#' @keywords internal
#' @noRd
.cache_read <- function(key) {
  if (!.cache_enabled()) {
    return(NULL)
  }

  tryCatch(
    {
      bfc <- .annotationgx_cache()
      rid <- .cache_rid(bfc, key)

      if (is.null(rid)) {
        return(NULL)
      }

      path <- BiocFileCache::bfcrpath(bfc, rids = rid)
      if (!file.exists(path)) {
        return(NULL)
      }

      readRDS(path)
    },
    error = function(e) {
      .debug(
        .funContext("AnnotationGx:::.cache_read"),
        "Cache read failed for key `",
        key,
        "`: ",
        conditionMessage(e)
      )
      NULL
    }
  )
}


#' Write an R object to the package cache.
#'
#' @param key Character(1) cache key.
#' @param value Arbitrary R object to cache.
#' @return Invisibly returns `value`.
#' @keywords internal
#' @noRd
.cache_write <- function(key, value) {
  if (!.cache_enabled()) {
    return(invisible(value))
  }

  tryCatch(
    {
      bfc <- .annotationgx_cache()
      rid <- .cache_rid(bfc, key)

      path <- if (is.null(rid)) {
        unname(BiocFileCache::bfcnew(bfc, rname = key, ext = ".rds"))
      } else {
        BiocFileCache::bfcrpath(bfc, rids = rid)
      }

      saveRDS(value, file = path, version = 2)
      invisible(value)
    },
    error = function(e) {
      .debug(
        .funContext("AnnotationGx:::.cache_write"),
        "Cache write failed for key `",
        key,
        "`: ",
        conditionMessage(e)
      )
      invisible(value)
    }
  )
}


#' Fetch a value from cache or compute and persist it.
#'
#' @param namespace Character(1) cache namespace.
#' @param params List of cache inputs.
#' @param FUN Function used to compute the value when there is a cache miss.
#' @param refresh Logical(1) indicating whether to force recomputation.
#' @return Cached or computed value.
#' @keywords internal
#' @noRd
.cache_fetch <- function(
  namespace,
  params = list(),
  FUN,
  refresh = .cache_refresh_enabled()
) {
  if (!.cache_enabled()) {
    return(FUN())
  }

  key <- .cache_key(namespace, params)

  if (!isTRUE(refresh)) {
    cached <- .cache_read(key)
    if (!is.null(cached)) {
      return(cached)
    }
  }

  value <- FUN()
  .cache_write(key, value)
  value
}
