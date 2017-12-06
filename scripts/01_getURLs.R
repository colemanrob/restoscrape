# each inspection report is served as it's own URL.  so first we have to scrape all possible URLs 
# the site requires an active session which uses a token system

library(rvest)
library(tidyverse)

url <- "http://www.algomapublichealth.com/inspections-environment/food-safety/restaurant-inspection-reports/"

# open active session
session <- html_session(url)

# function to scrape the url
get_inspection_urls <- function(results) {
  read_html(results) %>% 
    html_nodes(".date-inspected a") %>% 
    html_attr("href") %>% 
    tibble()
  
}

# scrape the form
form <- html_form(session)[[2]]

# fill in the form using the token assigned from the session header
set_values(form, Establishment = '',
           Address = '',
           City = '',
           Page = '',
           ufprt = form$fields$ufprt$value)
# submit the form
results <- submit_form(session, form, submit="action")

# inspect the results
page1 <- get_inspection_urls(results)

# success - getting the form again
form2 <- html_form(results)[[3]]

# setting values
set_values(form2, Establishment = '',
           Address = '',
           City = '',
           Page = form2$fields$Page$value,
           ufprt = form2$fields$ufprt$value)

# submit the form
results <- submit_form(session, form2)

# scrape the links
page2 <- get_inspection_urls(results)


# from here on in the form html is uniform so we can use a function
# to get the next page of results
get_next_page <- function(results) {
  form3 <<- html_form(results)[[4]]
  set_values(form3, Establishment = '',
             Address = '',
             City = '',
             Page = form3$fields$Page$value,
             ufprt = form3$fields$ufprt$value)
  results <<- submit_form(session, form3)
}

# it works
get_next_page(results)
page3 <- get_inspection_urls(results)

# another function to scrape the rest of the urls
scrape <- function(results, n) {
  #Sys.sleep(1) 
  if(!is.na(results)){
    map_df(1:n, ~{ #add results to dataframe
      results <<- get_next_page(results)
      links <- get_inspection_urls(results) 
      data.frame(links = links)
    })
  }}

# hammer time
page_145 <- scrape(results, 143)

# check
page_145 %>% 
  n_distinct()

# check no more Next values
results %>% 
  html_form()

# combine the pages together
all_pages <- rbind(page_145, page3, page1, page2)

# check
all_pages %>% 
  n_distinct()

# clean up
colnames(all_pages) <- "links"

# add the rest of the url
all_pages %>% 
  mutate(full = paste(url, .$links, sep="")) -> all_pages

# save the data to a file
all_pages %>% 
  saveRDS("data\\links")

#--- end part 1 ---- 
