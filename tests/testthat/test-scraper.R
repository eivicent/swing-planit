test_that("build_request opts into retrying curl-level failures", {
  # Regression for 2026-05-21 production failure: open.connection() error on
  # main's pre-refactor scraper. httr2::req_retry()'s default retry_on_failure
  # is FALSE, which would absorb HTTP 5xx but NOT a transient DNS / connection
  # failure. We explicitly opt in so curl errors are retried.
  req <- build_request("https://example.com")
  expect_true(isTRUE(req$policies$retry_on_failure))
  expect_gte(req$policies$retry_max_tries, 3)
  expect_equal(req$options$timeout_ms, 30000)
})

test_that("is_fetchable_url accepts http(s) URLs and rejects NA/empty/non-http", {
  expect_true(is_fetchable_url("https://example.com"))
  expect_true(is_fetchable_url("http://example.com/event/foo"))
  expect_false(is_fetchable_url(NA_character_))
  expect_false(is_fetchable_url(""))
  expect_false(is_fetchable_url("javascript:void(0)"))
  expect_false(is_fetchable_url("/relative/path"))

  expect_equal(
    is_fetchable_url(c("https://a", NA, "", "ftp://x", "http://b")),
    c(TRUE, FALSE, FALSE, FALSE, TRUE)
  )
})

test_that("fetch_html short-circuits on NA/empty URL without hitting the network", {
  expect_null(fetch_html(NA_character_, log = silent_log))
  expect_null(fetch_html("", log = silent_log))
  expect_null(fetch_html("not-a-url", log = silent_log))
})

test_that("fetch_html_many tolerates a mix of valid and invalid URLs", {
  # Regression for prior crash: a single NA in the input vector aborted the
  # whole batch via httr2::request().
  result <- fetch_html_many(
    c(NA_character_, "", "javascript:void(0)"),
    log = silent_log
  )
  expect_length(result, 3)
  expect_true(all(vapply(result, is.null, logical(1))))
})

test_that("fetch_html_many preserves positional alignment with input urls", {
  # All-invalid input must still return a list of NULLs of the right length.
  urls <- c(NA_character_, "not-http", "")
  result <- fetch_html_many(urls, log = silent_log)
  expect_length(result, length(urls))
})

test_that("scrape_all returns canonical empty tibble when homepage has zero month sections", {
  # Regression for prior crash: bind_rows(list()) -> 0x0 frame -> mutate on
  # `.data$tags` errored with "column not found".
  local_bind("read_homepage", function(url = BASE_URL, log = NULL) {
    list(page = NULL, month_nodes = list(), month_labels = character())
  })

  snapshot <- scrape_all(log = silent_log)

  expect_s3_class(snapshot, "tbl_df")
  expect_equal(nrow(snapshot), 0L)
  required_cols <- c(
    "month", "starting_date", "views", "name", "country",
    "cities", "tags", "websites", "swingplanit_link", "observation_date"
  )
  expect_true(all(required_cols %in% names(snapshot)))
})

test_that("scrape_all returns empty snapshot when all months yield zero cards", {
  fake_node <- xml2::read_html("<div class='homepagelist'></div>") |>
    rvest::html_element(".homepagelist")

  local_bind("read_homepage", function(url = BASE_URL, log = NULL) {
    list(
      page = NULL,
      month_nodes = list(fake_node, fake_node),
      month_labels = c("June 2026", "July 2026")
    )
  })

  snapshot <- scrape_all(log = silent_log)
  expect_equal(nrow(snapshot), 0L)
  expect_true("tags" %in% names(snapshot))
})

test_that("normalize_event_link handles NA, empty, absolute and relative inputs", {
  expect_true(is.na(normalize_event_link(NA_character_)))
  expect_true(is.na(normalize_event_link("")))
  expect_equal(
    normalize_event_link("/event/foo/?utm=1"),
    paste0(BASE_URL, "/event/foo")
  )
  expect_equal(
    normalize_event_link("https://other.example/event/bar/"),
    "https://other.example/event/bar"
  )
})

test_that("festival_id_from_link prefers the URL slug, falls back to a name slug", {
  expect_equal(
    festival_id_from_link("https://www.swingplanit.com/event/herrang-2026", "Herräng 2026"),
    "herrang-2026"
  )
  expect_equal(
    festival_id_from_link(NA_character_, "Some Festival Name!"),
    "some-festival-name"
  )
})

test_that("parse_event_details splits on the first colon and normalizes keys", {
  page <- xml2::read_html(
    "<html><body><ul>
       <li>Town: Berlin</li>
       <li>Website: https://example.com</li>
       <li>Notes: contains: a colon</li>
       <li>just-a-list-item</li>
     </ul></body></html>"
  )
  details <- parse_event_details(page)

  expect_true(all(c("town", "website", "notes") %in% details$key))
  expect_equal(event_field(details, "town"), "Berlin")
  expect_equal(event_field(details, "website"), "https://example.com")
  expect_equal(event_field(details, "notes"), "contains: a colon")
  expect_true(is.na(event_field(details, "missing")))
})

test_that("parse_event_details handles NULL page and pages with no list items", {
  expect_equal(nrow(parse_event_details(NULL)), 0L)

  empty_page <- xml2::read_html("<html><body><p>no lists</p></body></html>")
  expect_equal(nrow(parse_event_details(empty_page)), 0L)
})

test_that("extract_views parses comma-separated counts and missing views", {
  page_views <- xml2::read_html(
    "<html><body><span class='viewsplease'>Views: 12,345</span></body></html>"
  )
  expect_equal(extract_views(page_views), 12345)

  page_no_views <- xml2::read_html("<html><body></body></html>")
  expect_true(is.na(extract_views(page_no_views)))

  expect_true(is.na(extract_views(NULL)))
})
