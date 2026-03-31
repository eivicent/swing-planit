---
name: swingplanit-metrics
description: Build and validate festival statistics from daily_parse_data snapshots. Use when computing trends, editions, views per day, time-series tables, or preparing datasets for dashboard and website visualizations.
---

# SwingPlanit Metrics

## Scope

Use this skill for analytics tasks over `daily_parse_data/*.csv` and derived tables.

## Metric definitions

Use consistent definitions unless the user requests alternatives:

- **total_views**: latest observed `views` for a festival.
- **daily_views_delta**: `views(today) - views(previous_observation)`.
- **avg_views_per_day_7d**: rolling mean of `daily_views_delta` over last 7 observations.
- **edition_count_estimate**: number of distinct event-year occurrences for a festival lineage.

## Festival identity

Default identifier priority:

1. normalized `swingplanit_link`
2. normalized `name` fallback when links differ across years

When fuzzy logic is used, explicitly report assumptions and uncertainty.

## Standard workflow

1. Load all daily CSV snapshots.
2. Parse event date fields and `observation_date`.
3. Build a canonical festival key.
4. Compute longitudinal metrics by festival key.
5. Validate anomalies before final output.

## Validation checklist

- No duplicate `(observation_date, festival_key)` rows.
- Flag negative `daily_views_delta` values.
- Flag unusually large spikes in views.
- Confirm latest observation date exists in source files.
- Report number of festivals with insufficient history for 7-day averages.

## Output contract for frontend

When preparing data for UI/API, include:

- `festival_key`
- `festival_name`
- `observation_date`
- `views`
- `daily_views_delta`
- `avg_views_per_day_7d`
- `edition_count_estimate`

Prefer stable, machine-friendly column names (`snake_case`).

## Example tasks

- "Compute views/day for each festival"
- "Estimate how many editions a festival has had"
- "Prepare a dataset for one-festival detail page chart"
