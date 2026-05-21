# Entry point for `Rscript tests/testthat.R` (run from project root).
# This project is not a real R package, so we cannot use testthat::test_check().
# `helper-source.R` inside `tests/testthat/` loads R/scraper.R for each test.

library(testthat)
test_dir("tests/testthat", stop_on_failure = TRUE)
