
library(rvest)
library(dplyr)
library(tibble)
library(stringr)

extract_text <- function(html_object, css_string) {
  html_object %>% html_element(css = css_string) %>% html_text()
}

website <- read_html("https://www.swingplanit.com/")

all_festivals <- website %>% html_elements(css = ".homepagelist")

dates <- website %>%
  html_elements(css = ".swingtag") %>% html_text() %>% 
  as_tibble_col(column_name = "name") %>% 
  mutate(date = as.Date(paste("1", name), format="%d %B %Y"))

clean_festival_list <- list()
for(ii in 1:nrow(dates)) {
  
  festivals_ii <- all_festivals[[ii]] %>% 
    html_elements(css = ".color-shape")
  
  starting_date <- festivals_ii %>% extract_text(".daycalendar")  %>% 
    str_extract("\\d+") %>% as.numeric()
  
  swingplanit_link <- html_elements(festivals_ii, "a") %>% 
    html_attr(name = "href")
  
  name <- festivals_ii %>% extract_text(".maintitle2")
  country <- festivals_ii %>% extract_text(".pins")
  tags <- festivals_ii %>%
    extract_text(".circledetails")
  
  aux_view <- lapply(swingplanit_link, read_html)
  aux <- lapply(aux_view, function(x) 
    x %>% 
      html_elements(css = "li") %>% html_text()
  )
  
  cities <- sapply(aux, function(x) x[3] %>% gsub("Town: ", "", .))
  websites <- sapply(aux, function(x) x[4] %>% gsub("Website: ", "", .))
  views <- sapply(aux_view,  function(x)  x %>% extract_text(".viewsplease") %>% 
                    gsub("Views: ", "", .) %>% as.numeric()
  )
  
  clean_festival_list[[ii]] <- tibble(starting_date, views, name, 
                                      country, cities, tags, 
                                      websites, swingplanit_link) %>% 
    mutate(tags = str_trim(gsub("\\s+", " ", tags)),
           month = dates$name[ii],
           observation_date = Sys.Date()) %>% 
    relocate(month, .before = starting_date)
}

clean_festival <- bind_rows(clean_festival_list)
write.csv(x = clean_festival, file = paste0("./daily_parse_data/",Sys.Date(),".csv"))


