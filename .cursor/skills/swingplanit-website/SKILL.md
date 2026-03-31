---
name: swingplanit-website
description: Implement and evolve the minimal festival website that shows edition count and views-per-day trends. Use when creating UI pages, data endpoints, chart interactions, and lightweight deployable frontend behavior for this project.
---

# SwingPlanit Website

## Goal

Build a minimal website where a user selects one festival and sees:

1. estimated number of editions in dataset
2. views-per-day trend
3. latest cumulative views

## Minimum product requirements

- Festival selector (search or dropdown)
- Summary cards:
  - `edition_count_estimate`
  - `latest_views`
  - `avg_views_per_day_7d`
- Time-series chart for views and daily deltas
- "Last updated" timestamp from latest data snapshot

## Data contract assumptions

The frontend should consume a prepared dataset/API with at least:

- `festival_key`
- `festival_name`
- `observation_date`
- `views`
- `daily_views_delta`
- `avg_views_per_day_7d`
- `edition_count_estimate`

Avoid running expensive raw-data transforms in the UI layer.

## Implementation workflow

1. Confirm available data fields and freshness.
2. Build selector + one festival detail view.
3. Add summary metrics and trend chart.
4. Test with:
   - a festival with long history
   - a festival with sparse history
5. Verify empty/loading/error states.

## UX and reliability rules

- Keep page load fast; pre-aggregate on backend where possible.
- Show clear message when a metric cannot be computed.
- Keep chart labels and date formatting consistent.
- Do not block render on optional metrics.

## Example tasks

- "Add a festival detail page with views/day line chart"
- "Display how many editions this festival had in the data"
- "Wire UI to precomputed metrics dataset"
