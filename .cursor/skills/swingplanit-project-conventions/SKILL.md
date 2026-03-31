---
name: swingplanit-project-conventions
description: Apply repository-specific conventions for scraping, historical data compatibility, and validation-first changes. Use when making any non-trivial change in this project, especially data model, automation, or analytics updates.
---

# SwingPlanit Project Conventions

## Core principles

1. Preserve historical compatibility with `daily_parse_data/*.csv`.
2. Prefer additive schema changes over breaking ones.
3. Validate outputs after each substantive change.
4. Keep implementations small and observable.

## Change safety checklist

Before finalizing any change:

- confirm expected files still exist (`scrape_swing_planit.R`, `daily_parse_data/`, dashboard or site files)
- verify no accidental destructive operations on historical data
- document assumptions for inferred metrics (especially editions)
- ensure naming consistency (`snake_case` for derived datasets)

## Data compatibility policy

- Do not rename or remove existing raw snapshot columns without explicit request.
- If a new canonical field is introduced, keep legacy field available during transition.
- Prefer deriving new analytics in separate transformed datasets rather than mutating raw history.

## CI/automation policy

- Keep scheduled scrape jobs idempotent.
- Avoid creating commits when data did not change.
- Log meaningful failures to help debug selector or network issues.

## Communication style for project tasks

When reporting results:

- include what changed
- include validation performed
- include known risks and next step

## Example tasks

- "Refactor scraper but keep existing CSV compatibility"
- "Add new metric while preserving downstream dashboard behavior"
- "Improve workflow reliability without changing business logic"
