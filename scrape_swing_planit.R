#!/usr/bin/env Rscript
# Daily SwingPlanit scrape entry point.
#
# All extraction and HTTP logic lives in R/scraper.R so it can be unit-tested
# with stored HTML fixtures. This file is intentionally short.

source("R/scraper.R")

main <- function() {
  log <- make_logger()

  parallel <- !identical(Sys.getenv("SWING_PLANIT_PARALLEL", "true"), "false")
  max_active <- as.integer(Sys.getenv("SWING_PLANIT_MAX_ACTIVE", "4"))
  if (is.na(max_active) || max_active < 1) max_active <- 4L

  snapshot <- scrape_all(parallel = parallel, max_active = max_active, log = log)

  out_path <- write_daily_snapshot(snapshot, log = log)
  log("INFO", paste("snapshot written rows=", nrow(snapshot), " path=", out_path))
  invisible(out_path)
}

if (!interactive()) main()
