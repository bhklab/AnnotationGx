cache_dir <- file.path(tempdir(), "AnnotationGx-test-cache")
unlink(cache_dir, recursive = TRUE, force = TRUE)
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

options(
  annotationgx.cache.dir = cache_dir,
  annotationgx.cache.refresh = FALSE,
  annotationgx.cache.use = TRUE
)
