# swing-planit

Scrapes SwingPlanit daily, computes festival statistics, and serves a minimal website to inspect each festival's edition count and views per day.

## Project structure

- `scrape_swing_planit.R`: daily scraper that saves snapshots in `daily_parse_data/`.
- `build_metrics.R`: transforms snapshots into analytics datasets in `processed_data/`.
- `app.R`: minimal Shiny app to explore one festival at a time.
- `.github/workflows/scrap_swingplanit.yml`: scheduled automation for scrape + metrics refresh.

## Run locally

Install dependencies:

```r
install.packages(c("rvest", "dplyr", "tibble", "stringr", "purrr", "readr", "tidyr", "shiny", "ggplot2", "scales"))
```

Run pipeline:

```bash
Rscript scrape_swing_planit.R
Rscript build_metrics.R
```

Run website:

```bash
Rscript -e "shiny::runApp('app.R')"
```

## Output datasets

`build_metrics.R` writes:

- `processed_data/festival_timeseries.csv`: daily time series per festival with deltas and rolling average views/day.
- `processed_data/festival_latest.csv`: latest metrics per festival including `edition_count_estimate`.
- `processed_data/quality_report.csv`: basic pipeline health metrics.
