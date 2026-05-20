#' SwingPlanit scraper helpers
#'
#' Small, testable units used by `scrape_swing_planit.R`. Splitting the script
#' into named functions keeps selector logic easy to fixture-test and lets us
#' swap the HTTP layer (#6, #7) and logging (#8) in one place.

suppressPackageStartupMessages({
  library(rvest)
  library(dplyr)
  library(tibble)
  library(stringr)
  library(purrr)
  library(readr)
  library(httr2)
})

USER_AGENT <- "swing-planit-scraper/0.1 (+https://github.com/eivicent/swing-planit)"
BASE_URL <- "https://www.swingplanit.com"

# ---- logging (#8) -----------------------------------------------------------

#' Create a per-run logger that appends to `daily_parse_data/_logs/<date>.log`.
#'
#' Returns a function `log(level, msg, url = NA)`. Levels are free-form strings
#' (e.g. "WARN", "INFO", "ERROR"). The logger is fail-soft: if the log file
#' cannot be written we fall back to `message()`.
make_logger <- function(log_dir = "./daily_parse_data/_logs", today = Sys.Date()) {
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  log_path <- file.path(log_dir, paste0(today, ".log"))

  function(level, msg, url = NA_character_) {
    line <- sprintf(
      "%s [%s] %s%s",
      format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      level,
      msg,
      if (is.na(url) || url == "") "" else paste0(" url=", url)
    )
    ok <- tryCatch({
      cat(line, "\n", file = log_path, append = TRUE, sep = "")
      TRUE
    }, error = function(e) FALSE)
    if (!ok) message(line)
    invisible(line)
  }
}

# ---- HTTP layer (#6, #7) ----------------------------------------------------

#' Build an httr2 request with our UA, retries, and throttle settings.
build_request <- function(url, throttle_rate = 5) {
  httr2::request(url) |>
    httr2::req_user_agent(USER_AGENT) |>
    httr2::req_retry(
      max_tries = 3,
      backoff = function(attempt) 2^attempt,
      is_transient = function(resp) httr2::resp_status(resp) %in% c(408, 429, 500, 502, 503, 504)
    ) |>
    httr2::req_throttle(rate = throttle_rate / 1) |>
    httr2::req_timeout(30)
}

#' Fetch a single URL and return parsed HTML, or NULL on failure.
fetch_html <- function(url, log = NULL) {
  result <- tryCatch(
    httr2::req_perform(build_request(url)),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    if (!is.null(log)) log("WARN", paste("fetch failed:", conditionMessage(result)), url)
    return(NULL)
  }
  body <- tryCatch(
    httr2::resp_body_string(result),
    error = function(e) NULL
  )
  if (is.null(body) || !nzchar(body)) {
    if (!is.null(log)) log("WARN", "empty body", url)
    return(NULL)
  }
  tryCatch(
    rvest::read_html(body),
    error = function(e) {
      if (!is.null(log)) log("WARN", paste("parse failed:", conditionMessage(e)), url)
      NULL
    }
  )
}

#' Fetch many URLs in parallel with the same retry/throttle settings.
#'
#' Returns a list aligned with `urls`; entries are `xml_document` or NULL.
fetch_html_many <- function(urls, log = NULL, max_active = 4, throttle_rate = 5) {
  if (length(urls) == 0) return(list())
  reqs <- lapply(urls, build_request, throttle_rate = throttle_rate)
  responses <- httr2::req_perform_parallel(
    reqs,
    max_active = max_active,
    on_error = "continue"
  )
  purrr::map2(responses, urls, function(resp, url) {
    if (inherits(resp, "error")) {
      if (!is.null(log)) log("WARN", paste("parallel fetch failed:", conditionMessage(resp)), url)
      return(NULL)
    }
    if (httr2::resp_is_error(resp)) {
      if (!is.null(log)) log("WARN", paste0("HTTP ", httr2::resp_status(resp)), url)
      return(NULL)
    }
    body <- tryCatch(httr2::resp_body_string(resp), error = function(e) NULL)
    if (is.null(body) || !nzchar(body)) {
      if (!is.null(log)) log("WARN", "empty body", url)
      return(NULL)
    }
    tryCatch(
      rvest::read_html(body),
      error = function(e) {
        if (!is.null(log)) log("WARN", paste("parse failed:", conditionMessage(e)), url)
        NULL
      }
    )
  })
}

# ---- parsing helpers --------------------------------------------------------

safe_text <- function(node, css) {
  out <- node |> rvest::html_element(css = css)
  if (length(out) == 0 || anyNA(out)) {
    return(NA_character_)
  }
  rvest::html_text2(out) |> stringr::str_trim()
}

normalize_event_link <- function(link, base_url = BASE_URL) {
  if (is.na(link) || link == "") {
    return(NA_character_)
  }
  absolute <- if (stringr::str_starts(link, "http")) link else paste0(base_url, link)
  absolute |>
    stringr::str_remove("\\?.*$") |>
    stringr::str_remove("/+$")
}

festival_id_from_link <- function(link, name) {
  slug <- link |> stringr::str_extract("(?<=/event/)[^/?#]+")
  if (!is.na(slug) && slug != "") {
    return(stringr::str_to_lower(slug))
  }
  name |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "-") |>
    stringr::str_replace_all("(^-+|-+$)", "")
}

# ---- structured <li> extraction (#9) ----------------------------------------

#' Parse the event detail `<li>` block into a key/value tibble.
#'
#' SwingPlanit event pages list metadata as `<li>Label: value</li>`. Instead of
#' substring matching on a fixed prefix (which breaks on whitespace, casing or
#' label drift), we split on the first colon and normalize the key.
parse_event_details <- function(page) {
  if (is.null(page)) {
    return(tibble::tibble(key = character(), value = character()))
  }
  lis <- page |>
    rvest::html_elements("li") |>
    rvest::html_text2() |>
    stringr::str_trim()
  lis <- lis[stringr::str_detect(lis, ":")]
  if (length(lis) == 0) {
    return(tibble::tibble(key = character(), value = character()))
  }
  key <- stringr::str_extract(lis, "^[^:]+") |>
    stringr::str_to_lower() |>
    stringr::str_trim() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_replace_all("(^_+|_+$)", "")
  value <- stringr::str_remove(lis, "^[^:]+:\\s*") |>
    stringr::str_trim()
  tibble::tibble(key = key, value = value) |>
    dplyr::filter(!is.na(.data$key), .data$key != "", !is.na(.data$value))
}

#' Look up a field from `parse_event_details()` output by canonical key.
event_field <- function(details, key) {
  if (nrow(details) == 0) {
    return(NA_character_)
  }
  hit <- details |> dplyr::filter(.data$key == !!key)
  if (nrow(hit) == 0) {
    return(NA_character_)
  }
  hit$value[[1]]
}

# ---- views parsing ----------------------------------------------------------

extract_views <- function(page) {
  if (is.null(page)) return(NA_real_)
  raw <- safe_text(page, ".viewsplease")
  if (is.na(raw)) return(NA_real_)
  value <- raw |>
    stringr::str_remove("^Views:\\s*") |>
    stringr::str_replace_all(",", "") |>
    as.numeric()
  if (is.na(value)) NA_real_ else value
}

# ---- homepage / month parsing -----------------------------------------------

#' Read the homepage and return month nodes + labels.
read_homepage <- function(url = BASE_URL, log = NULL) {
  page <- fetch_html(url, log = log)
  if (is.null(page)) {
    if (!is.null(log)) log("ERROR", "homepage fetch failed", url)
    stop("Failed to fetch SwingPlanit homepage at ", url)
  }
  month_nodes <- page |> rvest::html_elements(".homepagelist")
  month_labels <- page |> rvest::html_elements(".swingtag") |> rvest::html_text2() |> stringr::str_trim()
  month_count <- min(length(month_nodes), length(month_labels))
  list(
    page = page,
    month_nodes = month_nodes[seq_len(month_count)],
    month_labels = month_labels[seq_len(month_count)]
  )
}

#' Extract the festival cards listed under one month section.
parse_month_cards <- function(month_node) {
  cards <- month_node |> rvest::html_elements(".color-shape")
  if (length(cards) == 0) {
    return(tibble::tibble(
      starting_date = integer(),
      name = character(),
      country = character(),
      tags = character(),
      swingplanit_link = character()
    ))
  }
  starting_date <- purrr::map_dbl(cards, function(card) {
    raw_day <- safe_text(card, ".daycalendar")
    day_number <- raw_day |> stringr::str_extract("\\d+") |> as.numeric()
    if (is.na(day_number)) NA_real_ else day_number
  })
  swingplanit_link <- purrr::map_chr(cards, function(card) {
    card |>
      rvest::html_element("a") |>
      rvest::html_attr("href") |>
      normalize_event_link()
  })
  name <- purrr::map_chr(cards, ~ safe_text(.x, ".maintitle2"))
  country <- purrr::map_chr(cards, ~ safe_text(.x, ".pins"))
  tags <- purrr::map_chr(cards, ~ safe_text(.x, ".circledetails"))

  tibble::tibble(
    starting_date = starting_date,
    name = name,
    country = country,
    tags = tags,
    swingplanit_link = swingplanit_link
  )
}

#' For a month tibble (output of `parse_month_cards`), enrich each row with the
#' details extracted from its event page.
enrich_with_event_pages <- function(cards, log = NULL, parallel = TRUE, max_active = 4) {
  if (nrow(cards) == 0) return(cards)
  urls <- cards$swingplanit_link
  pages <- if (parallel) {
    fetch_html_many(urls, log = log, max_active = max_active)
  } else {
    purrr::map(urls, fetch_html, log = log)
  }

  if (!is.null(log)) {
    missing_idx <- which(vapply(pages, is.null, logical(1)))
    for (i in missing_idx) log("WARN", "no event page parsed", urls[[i]])
  }

  details_list <- purrr::map(pages, parse_event_details)

  cards |>
    dplyr::mutate(
      cities = purrr::map_chr(details_list, ~ event_field(.x, "town")),
      websites = purrr::map_chr(details_list, ~ event_field(.x, "website")),
      views = purrr::map_dbl(pages, extract_views)
    )
}

# ---- main pipeline ----------------------------------------------------------

#' End-to-end scrape returning the daily snapshot tibble.
scrape_all <- function(base_url = BASE_URL,
                       parallel = TRUE,
                       max_active = 4,
                       log = make_logger()) {
  log("INFO", paste("scrape start parallel=", parallel, " max_active=", max_active))
  home <- read_homepage(base_url, log = log)
  per_month <- purrr::map2(
    home$month_nodes,
    home$month_labels,
    function(node, label) {
      cards <- parse_month_cards(node)
      if (nrow(cards) == 0) {
        log("WARN", paste("no festival cards for month", label))
        return(cards)
      }
      enrich_with_event_pages(cards, log = log, parallel = parallel, max_active = max_active) |>
        dplyr::mutate(month = label)
    }
  )
  out <- dplyr::bind_rows(per_month) |>
    dplyr::mutate(
      tags = .data$tags |> stringr::str_replace_all("\\s+", " ") |> stringr::str_trim(),
      observation_date = Sys.Date(),
      festival_id = purrr::map2_chr(.data$swingplanit_link, .data$name, festival_id_from_link)
    ) |>
    # Preserve historical CSV column order (additive: new columns may be added
    # at the end without breaking downstream consumers).
    dplyr::relocate(
      "month",
      "starting_date",
      "views",
      "name",
      "festival_id",
      "country",
      "cities",
      "tags",
      "websites",
      "swingplanit_link",
      "observation_date"
    )

  log("INFO", paste("scrape end rows=", nrow(out), " missing_views=", sum(is.na(out$views))))
  out
}

#' Persist a daily snapshot to `daily_parse_data/<date>.csv`.
write_daily_snapshot <- function(df, dir = "./daily_parse_data", today = Sys.Date(), log = NULL) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(dir, paste0(today, ".csv"))
  if (file.exists(out_path) && !is.null(log)) {
    log("INFO", paste("overwriting existing snapshot", out_path))
  }
  readr::write_csv(df, out_path)
  out_path
}
