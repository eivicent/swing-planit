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
#' (e.g. "WARN", "INFO", "ERROR"). Every line is also emitted with `message()`
#' so GitHub Actions captures the reason a fetch failed (file-only logs were
#' invisible in CI on 2026-07-15 / 2026-07-24). File writes remain fail-soft.
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
    message(line)
    tryCatch(
      cat(line, "\n", file = log_path, append = TRUE, sep = ""),
      error = function(e) invisible(NULL)
    )
    invisible(line)
  }
}

# ---- HTTP layer (#6, #7) ----------------------------------------------------

#' TRUE iff `url` is a non-NA, non-empty `http(s)://` string.
#'
#' Used to short-circuit malformed/missing links before they reach
#' `httr2::request()` (which aborts on NA input and would take down the whole
#' scrape on a single bad card).
is_fetchable_url <- function(url) {
  !is.na(url) & nzchar(url) & stringr::str_starts(url, "https?://")
}

#' Build an httr2 request with our UA, retries, and throttle settings.
#'
#' `retry_on_failure = TRUE` is important: by default `httr2::req_retry()`
#' only retries HTTP-level transient codes, NOT curl-level errors. The 2026-05-21
#' production failure was exactly such a case (`open.connection: cannot open
#' the connection`), which would have been absorbed by a single retry. We
#' opt in to retry curl errors too since the SwingPlanit domain is known-stable.
build_request <- function(url, throttle_rate = 5, max_tries = 4) {
  httr2::request(url) |>
    httr2::req_user_agent(USER_AGENT) |>
    httr2::req_retry(
      max_tries = max_tries,
      backoff = function(attempt) 2^attempt,
      is_transient = function(resp) httr2::resp_status(resp) %in% c(408, 429, 500, 502, 503, 504),
      retry_on_failure = TRUE
    ) |>
    httr2::req_throttle(rate = throttle_rate) |>
    httr2::req_timeout(30)
}

#' Parse an httr2 response object into an `xml_document`, or NULL on failure.
#' Shared by `fetch_html()` and `fetch_html_many()` so single-URL and parallel
#' paths fail identically.
response_to_html <- function(resp, url, log = NULL) {
  if (inherits(resp, "error") || inherits(resp, "condition")) {
    if (!is.null(log)) log("WARN", paste("fetch failed:", conditionMessage(resp)), url)
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
}

#' Fetch a single URL and return parsed HTML, or NULL on failure.
fetch_html <- function(url, log = NULL) {
  if (!is_fetchable_url(url)) {
    if (!is.null(log)) {
      log("WARN", "invalid url", if (is.na(url)) NA_character_ else url)
    }
    return(NULL)
  }
  resp <- tryCatch(
    httr2::req_perform(build_request(url)),
    error = function(e) e
  )
  response_to_html(resp, url, log = log)
}

#' Fetch many URLs in parallel with the same retry/throttle settings.
#'
#' Returns a list aligned positionally with `urls`. Entries are `xml_document`
#' or NULL. Invalid URLs (NA, empty, non-http) are skipped without making any
#' request and produce NULL entries at the corresponding positions.
fetch_html_many <- function(urls, log = NULL, max_active = 4, throttle_rate = 5, max_tries = 4) {
  if (length(urls) == 0) return(list())

  valid <- is_fetchable_url(urls)
  result <- vector("list", length(urls))

  if (!is.null(log)) {
    for (i in which(!valid)) {
      log("WARN", "invalid url", if (is.na(urls[[i]])) NA_character_ else urls[[i]])
    }
  }
  if (!any(valid)) return(result)

  valid_urls <- urls[valid]
  reqs <- lapply(valid_urls, build_request, throttle_rate = throttle_rate, max_tries = max_tries)
  responses <- httr2::req_perform_parallel(
    reqs,
    max_active = max_active,
    on_error = "continue"
  )
  parsed <- purrr::map2(responses, valid_urls, function(resp, url) {
    response_to_html(resp, url, log = log)
  })
  result[valid] <- parsed
  result
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
#'
#' Outer retries cover failure modes httr2 does not treat as transient (empty
#' body, non-5xx/429 status, brief upstream blips). Both 2026-07-15 and
#' 2026-07-24 CI failures died in ~1s on the homepage fetch — too fast for the
#' configured httr2 backoff — so a small sleep-and-retry here is intentional.
read_homepage <- function(url = BASE_URL,
                          log = NULL,
                          max_attempts = 3L,
                          retry_wait_secs = 5) {
  page <- NULL
  for (attempt in seq_len(max_attempts)) {
    page <- fetch_html(url, log = log)
    if (!is.null(page)) break
    if (attempt < max_attempts) {
      if (!is.null(log)) {
        log(
          "WARN",
          paste0(
            "homepage fetch attempt ", attempt, "/", max_attempts,
            " failed; retrying in ", retry_wait_secs, "s"
          ),
          url
        )
      }
      Sys.sleep(retry_wait_secs)
    }
  }
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

#' Canonical empty snapshot tibble with the schema downstream consumers expect.
#'
#' Used when the homepage has no parseable month sections. Returning a typed
#' zero-row tibble (instead of `bind_rows(list())`, which yields a 0x0 frame)
#' keeps `dplyr::mutate(.data$tags, ...)` and downstream metrics code from
#' crashing on the empty path.
empty_snapshot <- function() {
  tibble::tibble(
    month = character(),
    starting_date = numeric(),
    views = numeric(),
    name = character(),
    festival_id = character(),
    country = character(),
    cities = character(),
    tags = character(),
    websites = character(),
    swingplanit_link = character(),
    observation_date = as.Date(character())
  )
}

#' End-to-end scrape returning the daily snapshot tibble.
scrape_all <- function(base_url = BASE_URL,
                       parallel = TRUE,
                       max_active = 4,
                       log = make_logger()) {
  log("INFO", paste("scrape start parallel=", parallel, " max_active=", max_active))
  home <- read_homepage(base_url, log = log)

  if (length(home$month_nodes) == 0) {
    log("ERROR", "homepage parsed but contains zero month sections", base_url)
    return(empty_snapshot())
  }

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

  out_bare <- dplyr::bind_rows(per_month)
  if (nrow(out_bare) == 0) {
    log("WARN", "no festival rows scraped across any month section")
    return(empty_snapshot())
  }

  out <- out_bare |>
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
