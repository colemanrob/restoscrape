library(yelp)
library(tidyverse)

access_token <- get_access_token('piGBaQuicYwKuj-fVclRTA', '5QL6rgBxoXfHYPuARUaUUo2hsAO0WgmvnqCxDq9SDsDDOsuj4hi3UuoZkyl0jLmH')

store_access_token(access_token)

rawdata <- readRDS("data/tidy.RDS")

rawdata %>% 
  filter(city == "Sault Ste. Marie") %>% 
  group_by(name, street) %>% 
  summarise(visits = n_distinct(visit),
            infractions = sum(infractions!="No infractions found at the time of inspection"),
            infraction_per_visit = round(infractions / visits, 2)) %>% 
  arrange(desc(infraction_per_visit)) %>% 
  head(n=5) %>% 
  View()

yelp_data <- top_5 %>% 
  map(business_search($name, "Sault Ste. Marie, ON"))

ssm_resto_data1 <- reviews("Shabby Motley", "Sault Ste. Marie, ON", locale = 'en_US')
ssm_resto_data2 <- business_search("restaurants", "Sault Ste. Marie, ON", limit = 50, offset = 50)
ssm_resto_data3 <- business_search("restaurants", "Sault Ste. Marie, ON", limit = 50, offset = 100)
ssm_resto_data4 <- business_search("restaurants", "Sault Ste. Marie, ON", limit = 50, offset = 150)
ssm_resto_data5 <- business_search("restaurants", "Sault Ste. Marie, ON", limit = 50, offset = 200)




yelpdata <- rbind(ssm_resto_data, ssm_resto_data1, ssm_resto_data2)

yelpdata %>% 
  filter(country == "CA") %>% 
  select(name, rating) -> yelp_small


