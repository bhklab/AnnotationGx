library(AnnotationGx)
library(testthat)

test_that(".cache_fetch persists values across calls", {
  counter <- 0L

  first <- .cache_fetch(
    namespace = "tests/cache-persist",
    params = list(id = "persist"),
    FUN = function() {
      counter <<- counter + 1L
      list(value = counter)
    }
  )

  second <- .cache_fetch(
    namespace = "tests/cache-persist",
    params = list(id = "persist"),
    FUN = function() {
      counter <<- counter + 1L
      list(value = counter)
    }
  )

  expect_equal(counter, 1L)
  expect_equal(first, second)
  expect_equal(first$value, 1L)
})


test_that(".cache_fetch respects the refresh option", {
  old_refresh <- getOption("annotationgx.cache.refresh")
  on.exit(options(annotationgx.cache.refresh = old_refresh), add = TRUE)

  options(annotationgx.cache.refresh = TRUE)

  counter <- 0L

  first <- .cache_fetch(
    namespace = "tests/cache-refresh",
    params = list(id = "refresh"),
    FUN = function() {
      counter <<- counter + 1L
      counter
    }
  )

  second <- .cache_fetch(
    namespace = "tests/cache-refresh",
    params = list(id = "refresh"),
    FUN = function() {
      counter <<- counter + 1L
      counter
    }
  )

  expect_equal(first, 1L)
  expect_equal(second, 2L)
})
