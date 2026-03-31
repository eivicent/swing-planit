---
name: swingplanit-scraper
description: Maintain and improve the SwingPlanit scraper and ingestion workflow in R. Use when editing scrape_swing_planit.R, updating selectors, changing daily snapshot schema, or debugging scraping failures and missing festival rows.
---

# SwingPlanit Scraper

## Quick start

Use this workflow when changing the scraper in `scrape_swing_planit.R`.

1. Read current selectors and extracted fields.
2. Keep output compatible with files in `daily_parse_data/`.
3. Run one scrape and verify row count and key columns.
4. Check edge cases (missing tags, missing website, special characters).
5. Commit only scraper-related changes.

## Required output columns

Daily CSV snapshots must include:

- `month`
- `starting_date`
- `views`
- `name`
- `country`
- `cities`
- `tags`
- `websites`
- `swingplanit_link`
- `observation_date`

When introducing new columns, append them without removing existing ones unless user asks for a breaking change.

## Selector change protocol

If page structure changes:

1. Identify the failing selector.
2. Prefer a more stable selector based on semantic class names.
3. Keep fallback logic for optional fields:
   - return `NA` or empty string instead of crashing
   - log extraction failures with festival URL
4. Re-run scrape and compare output shape with latest CSV.

## Data quality checks

After any scraper change, validate:

- no duplicated rows for the same `swingplanit_link` within a run
- `views` is numeric and mostly non-missing
- `observation_date` is today for all rows
- row count is in expected range vs recent days

## Common fixes

- Encoding artifacts in `cities` or `name`: normalize strings consistently.
- Missing event details page: skip row with warning and continue.
- Slow nested requests: batch politely and avoid unnecessary full-page parsing.

## Example tasks

- "Update scraper because `.viewsplease` changed"
- "Add robust handling for missing website links"
- "Investigate sudden drop in daily festival count"
