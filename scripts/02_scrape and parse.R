library(rvest)
library(tidyverse)


# import file if not still in environment
all_pages <- readRDS("data/links")

# scrape the content of each url
pages <- all_pages$full %>%
  map(read_html)

saveRDS(pages, "data/pages.rds")

# grab title  
title <- pages %>% 
  map(. %>% 
        html_nodes("h4") %>% 
        html_text()
  )
# grab address
address <- pages %>% 
  map(. %>% 
        html_nodes("p:nth-child(4)") %>% 
        html_text()
  )
# grab inspection date
date <- pages %>% 
  map(. %>% 
        html_nodes("p:nth-child(6)") %>% 
        html_text() %>% 
        as.character()
  )
# grab infraction box results
infractions <- pages %>% 
  map(. %>% 
        html_nodes(".question, .table-striped p") %>% 
        html_text()
  )
# combine into dataframe
new_df <- data_frame(name = as.character(title), 
                     address = as.character(address), 
                     date = as.character(date), infractions)

# flatten list of infractions
new_df %>% 
  unnest(infractions, .drop = FALSE) -> new_df

# tidy
new_df %>% 
  separate(address, into = c("street", "city"), sep=", ") %>% 
  separate(date, into = c("inspect", "visit"), sep=": ") -> tidy_df

library(lubridate)

# tidy
tidy_df$visit <- mdy(tidy_df$visit)
tidy_df$name <- str_trim(tidy_df$name)
tidy_df$street <- str_trim(tidy_df$street)
tidy_df$infractions <- str_trim(tidy_df$infractions)

glimpse(tidy_df)

# write file
tidy_df %>% 
  select(-inspect) %>% 
  saveRDS("data/tidy.RDS")

