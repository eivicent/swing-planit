# swing-planit

Scrapes SwingPlanit daily, computes festival statistics, and serves a minimal
website to inspect each festival's edition count and views per day.

## Project structure

- `R/scraper.R` — testable scraper helpers (HTTP, parsing, logging).
- `scrape_swing_planit.R` — thin entry point for the daily scrape.
- `build_metrics.R` — builds analytics datasets from daily snapshots.
- `app.R` — minimal Shiny app to explore one festival at a time.
- `.github/workflows/scrap_swingplanit.yml` — scheduled daily scrape + metrics
  refresh, pushes results to the `data` branch.
- `.github/workflows/bootstrap_data_branch.yml` — one-time workflow to seed
  the `data` branch from the current snapshot directory.
- `DESCRIPTION` — declares R package deps so CI can cache them.

## Branches and where the data lives

- `main` — code only (this branch).
- `data` — orphan branch, auto-managed by CI. Holds:
  - `daily_parse_data/` — one CSV per scrape day.
  - `processed_data/` — derived analytics tables.

Splitting code from data keeps `main` history clean and makes the repo small
to clone for code work.

## Run locally

Install dependencies (or use `pak::pkg_install(".")` if you have pak):

```r
install.packages(c(
  "rvest", "dplyr", "tibble", "stringr", "purrr", "readr", "tidyr",
  "httr2", "shiny", "ggplot2", "scales"
))
```

To work with the historical data locally, fetch the `data` branch into the
expected paths:

```bash
git worktree add .data_branch origin/data
ln -sfn .data_branch/daily_parse_data daily_parse_data
ln -sfn .data_branch/processed_data processed_data
```

Run pipeline:

```bash
Rscript scrape_swing_planit.R
Rscript build_metrics.R
```

Run the website:

```bash
Rscript -e "shiny::runApp('app.R')"
```

## Output datasets

`build_metrics.R` writes:

- `processed_data/festival_timeseries.csv` — daily time series per festival.
  Columns include the legacy `views`, `daily_views_delta`, `views_per_day`,
  `avg_views_per_day_7d`, plus `edition_index` and `edition_reset_flag`
  produced by edition-reset detection.
- `processed_data/festival_latest.csv` — latest metrics per festival including
  `edition_count_estimate` and `current_edition_index`.
- `processed_data/quality_report.csv` — pipeline health metrics, including
  `edition_reset_count`.

## Scraper configuration

Environment variables consumed by `scrape_swing_planit.R`:

- `SWING_PLANIT_PARALLEL` — `true` (default) or `false`. Toggles parallel
  fetch of event detail pages.
- `SWING_PLANIT_MAX_ACTIVE` — integer, default `4`. Max concurrent requests
  when parallel.

Per-run logs are written to `daily_parse_data/_logs/<date>.log`.

## CI bootstrap (one-time)

After merging the workflow changes for the first time:

1. Manually dispatch `Bootstrap data branch` from the Actions tab. This
   creates the `data` branch from the current `daily_parse_data/` and
   `processed_data/` directories.
2. Untrack those directories on `main`:

   ```bash
   git rm -r --cached daily_parse_data processed_data
   git commit -m "Untrack data dirs (now on data branch)"
   git push
   ```

3. From then on the daily workflow only writes to the `data` branch.
