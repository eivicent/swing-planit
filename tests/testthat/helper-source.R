# testthat auto-sources helper-*.R before each test file.
# This loads the scraper helpers without requiring the project to be a real
# R package.

project_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
source(file.path(project_root, "R", "scraper.R"))

# A no-op logger so tests don't write to disk.
silent_log <- function(level, msg, url = NA_character_) invisible(NULL)

#' Temporarily replace a named binding in `envir` for the lifetime of the
#' current `test_that()` frame. Cleanup runs via `withr::defer()` so it fires
#' even on test failure. Used instead of `testthat::local_mocked_bindings()`
#' because that helper requires a real R package context (pkgload), which this
#' project intentionally does not have.
local_bind <- function(name, value, envir = globalenv(), frame = parent.frame()) {
  had <- exists(name, envir = envir, inherits = FALSE)
  old <- if (had) get(name, envir = envir, inherits = FALSE) else NULL
  assign(name, value, envir = envir)
  withr::defer(
    {
      if (had) {
        assign(name, old, envir = envir)
      } else {
        rm(list = name, envir = envir)
      }
    },
    envir = frame
  )
  invisible(NULL)
}
