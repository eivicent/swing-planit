library(rvest)
library(dplyr)
library(tibble)
library(stringr)
library(purrr)
library(readr)

base_url <- "https://www.swingplanit.com"

safe_text <- function(node, css) {
  out <- node %>% html_element(css = css)
  if (length(out) == 0 || anyNA(out)) {
    return(NA_character_)
  }
  html_text2(out) %>% str_trim()
}

normalize_event_link <- function(link) {
  if (is.na(link) || link == "") {
    return(NA_character_)
  }
  absolute <- if (str_starts(link, "http")) link else paste0(base_url, link)
  absolute %>%
    str_remove("\\?.*$") %>%
    str_remove("/+$")
}

festival_id_from_link <- function(link, name) {
  slug <- link %>% str_extract("(?<=/event/)[^/?#]+")
  if (!is.na(slug) && slug != "") {
    return(str_to_lower(slug))
  }
  name %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "-") %>%
    str_replace_all("(^-+|-+$)", "")
}

extract_li_value <- function(li_values, label) {
  candidate <- li_values[str_starts(li_values, fixed(label))]
  if (length(candidate) == 0) {
    return(NA_character_)
  }
  str_remove(candidate[[1]], fixed(label)) %>% str_trim()
}

safe_read_event_page <- purrr::safely(function(url) read_html(url), otherwise = NULL)

website <- read_html(base_url)

month_nodes <- website %>% html_elements(".homepagelist")
month_labels <- website %>% html_elements(".swingtag") %>% html_text2() %>% str_trim()
month_count <- min(length(month_nodes), length(month_labels))

clean_festival_list <- vector(mode = "list", length = month_count)

for (ii in seq_len(month_count)) {
  festivals_ii <- month_nodes[[ii]] %>% html_elements(".color-shape")
  if (length(festivals_ii) == 0) {
    clean_festival_list[[ii]] <- tibble()
    next
  }

  starting_date <- map_dbl(festivals_ii, function(card) {
    raw_day <- safe_text(card, ".daycalendar")
    day_number <- raw_day %>% str_extract("\\d+") %>% as.numeric()
    ifelse(is.na(day_number), NA_real_, day_number)
  })

  swingplanit_link <- map_chr(festivals_ii, function(card) {
    card %>%
      html_element("a") %>%
      html_attr("href") %>%
      normalize_event_link()
  })

  name <- map_chr(festivals_ii, ~ safe_text(.x, ".maintitle2"))
  country <- map_chr(festivals_ii, ~ safe_text(.x, ".pins"))
  tags <- map_chr(festivals_ii, ~ safe_text(.x, ".circledetails"))

  event_pages <- map(swingplanit_link, safe_read_event_page)

  details_li <- map(event_pages, function(page) {
    if (is.null(page$result)) {
      return(character(0))
    }
    page$result %>% html_elements("li") %>% html_text2() %>% str_trim()
  })

  cities <- map_chr(details_li, ~ extract_li_value(.x, "Town: "))
  websites <- map_chr(details_li, ~ extract_li_value(.x, "Website: "))

  views <- map_dbl(event_pages, function(page) {
    if (is.null(page$result)) {
      return(NA_real_)
    }
    value <- page$result %>%
      safe_text(".viewsplease") %>%
      str_remove("^Views:\\s*") %>%
      str_replace_all(",", "") %>%
      as.numeric()
    ifelse(is.na(value), NA_real_, value)
  })

  clean_festival_list[[ii]] <- tibble(
    month = month_labels[[ii]],
    starting_date = starting_date,
    views = views,
    name = name,
    country = country,
    cities = cities,
    tags = tags,
    websites = websites,
    swingplanit_link = swingplanit_link
  ) %>%
    mutate(
      tags = tags %>% str_replace_all("\\s+", " ") %>% str_trim(),
      observation_date = Sys.Date(),
      festival_id = map2_chr(swingplanit_link, name, festival_id_from_link)
    ) %>%
    relocate(festival_id, .after = name)
}

clean_festival <- bind_rows(clean_festival_list)
dir.create("./daily_parse_data", recursive = TRUE, showWarnings = FALSE)
write_csv(clean_festival, paste0("./daily_parse_data/", Sys.Date(), ".csv"))


