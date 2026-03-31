library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tidyr)

safe_date_from_month_start <- function(month_label, day_value) {
  month_start <- as.Date(paste("1", month_label), format = "%d %B %Y")
  if (is.na(month_start) || is.na(day_value)) {
    return(as.Date(NA))
  }
  as.Date(sprintf("%s-%02d", format(month_start, "%Y-%m"), as.integer(day_value)))
}

festival_id_from_values <- function(link, name) {
  slug <- link %>% str_extract("(?<=/event/)[^/?#]+")
  if (!is.na(slug) && slug != "") {
    return(str_to_lower(slug))
  }
  name %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "-") %>%
    str_replace_all("(^-+|-+$)", "")
}

rolling_mean_last_n <- function(values, n = 7) {
  out <- numeric(length(values))
  for (ii in seq_along(values)) {
    window <- tail(values[seq_len(ii)], n)
    avg <- mean(window, na.rm = TRUE)
    out[[ii]] <- ifelse(is.nan(avg), NA_real_, avg)
  }
  out
}

input_dir <- "./daily_parse_data"
output_dir <- "./processed_data"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

csv_files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)
if (length(csv_files) == 0) {
  stop("No CSV files found in ./daily_parse_data")
}

raw_data <- map_dfr(csv_files, ~ read_csv(.x, show_col_types = FALSE, name_repair = "minimal")) %>%
  select(-any_of("...1"))

if (!"festival_id" %in% names(raw_data)) {
  raw_data <- raw_data %>%
    mutate(festival_id = NA_character_)
}

raw_data <- raw_data %>%
  mutate(
    observation_date = as.Date(observation_date),
    views = as.numeric(views),
    starting_date = as.integer(starting_date),
    festival_id = coalesce(
      na_if(str_trim(str_to_lower(as.character(festival_id))), ""),
      map2_chr(swingplanit_link, name, festival_id_from_values)
    ),
    festival_name = name,
    event_start_date = map2_chr(month, starting_date, ~ as.character(safe_date_from_month_start(.x, .y))) %>% as.Date(),
    event_year = as.integer(format(event_start_date, "%Y"))
  )

timeseries <- raw_data %>%
  group_by(observation_date, festival_id) %>%
  summarise(
    festival_name = first(na.omit(festival_name)),
    swingplanit_link = first(na.omit(swingplanit_link)),
    country = first(na.omit(country)),
    city = first(na.omit(cities)),
    month = first(na.omit(month)),
    event_start_date = first(na.omit(event_start_date)),
    views = max(views, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(views = ifelse(is.infinite(views), NA_real_, views)) %>%
  arrange(festival_id, observation_date) %>%
  group_by(festival_id) %>%
  mutate(
    previous_observation_date = lag(observation_date),
    previous_views = lag(views),
    days_since_previous = as.numeric(observation_date - previous_observation_date),
    daily_views_delta = views - previous_views,
    views_per_day = ifelse(!is.na(days_since_previous) & days_since_previous > 0, daily_views_delta / days_since_previous, NA_real_),
    avg_views_per_day_7d = rolling_mean_last_n(views_per_day, n = 7)
  ) %>%
  ungroup()

edition_counts <- raw_data %>%
  mutate(event_year = map2_chr(month, starting_date, ~ as.character(safe_date_from_month_start(.x, .y))) %>% as.Date() %>% format("%Y") %>% as.integer()) %>%
  distinct(festival_id, event_year) %>%
  filter(!is.na(event_year)) %>%
  count(festival_id, name = "edition_count_estimate")

timeseries <- timeseries %>%
  left_join(edition_counts, by = "festival_id") %>%
  mutate(edition_count_estimate = replace_na(edition_count_estimate, 1L))

latest_metrics <- timeseries %>%
  group_by(festival_id) %>%
  filter(observation_date == max(observation_date, na.rm = TRUE)) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  transmute(
    festival_id,
    festival_name,
    swingplanit_link,
    country,
    city,
    latest_observation_date = observation_date,
    latest_views = views,
    latest_views_per_day = views_per_day,
    avg_views_per_day_7d,
    edition_count_estimate
  )

quality_report <- tibble(
  metric = c(
    "rows_timeseries",
    "festivals",
    "negative_daily_delta_count",
    "missing_views_count",
    "latest_observation_date"
  ),
  value = c(
    nrow(timeseries),
    n_distinct(timeseries$festival_id),
    sum(timeseries$daily_views_delta < 0, na.rm = TRUE),
    sum(is.na(timeseries$views)),
    as.character(max(timeseries$observation_date, na.rm = TRUE))
  )
)

write_csv(timeseries, file.path(output_dir, "festival_timeseries.csv"))
write_csv(latest_metrics, file.path(output_dir, "festival_latest.csv"))
write_csv(quality_report, file.path(output_dir, "quality_report.csv"))
