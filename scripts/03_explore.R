library(tidyverse)
library(lubridate)

library(extrafont)
extrafont::loadfonts(device="win")





rawdata <- readRDS("data/tidy.RDS")

glimpse(rawdata)

# the old ssm name problem
sum(rawdata$city == "Sault Ste Marie")
sum(rawdata$city == "Sault Ste. Marie")

# let's fix
rawdata$city <- as.factor(rawdata$city)
rawdata$city[rawdata$city == "Sault Ste Marie"] <- "Sault Ste. Marie"

# how many cities -  wow 45

rawdata %>% 
  count(city) %>% 
  arrange(desc(n))

# how many visits per year
rawdata %>% 
  group_by(year = year(visit)) %>% 
  summarise(total = n()) 

# how many unique resto's in the Sault
rawdata %>% 
  filter(city == "Sault Ste. Marie") %>% 
  group_by(name) %>% 
  summarise(uniques = n_distinct(name)) %>% 
  select(-name) %>% 
  colSums()

# how many infractions?
rawdata %>% 
  filter(city == "Sault Ste. Marie") %>% 
  group_by(name, street) %>% 
  summarise(visits = n_distinct(visit),
            infractions = sum(infractions!="No infractions found at the time of inspection"),
            infraction_per_visit = round(infractions / visits, 2)) %>% 
  arrange(desc(infraction_per_visit)) %>% 
  View()


# what are the infractions?

rawdata %>% 
  filter(city == "Sault Ste. Marie") %>% 
  filter(infractions !="No infractions found at the time of inspection") %>% 
  count(infractions) %>% 
  arrange(desc(n)) %>% 
  View()

# here's an unusual bunch
# eggs?
rawdata %>% 
  filter(str_detect(infractions, "eggs")) %>% 
  View()

# sleeping?
rawdata %>% 
  filter(str_detect(infractions, "sleep")) %>% 
  View()

# live animals?
rawdata %>% 
  filter(str_detect(infractions, "live animal")) %>% 
  View()

# hunting meat? not sure who teen challenge is - wonder what the challenge was
rawdata %>% 
  filter(str_detect(infractions, "hunting")) %>% 
  View()

# toxic?
rawdata %>% 
  filter(str_detect(infractions, "Toxic")) %>% 
  View()

# hygiene?
rawdata %>% 
  filter(str_detect(infractions, "hygiene")) %>% 
  View()

# this one seems like a general mark of untidyness
rawdata %>% 
  filter(city == "Sault Ste. Marie") %>% 
  filter(infractions == "General housekeeping is satisfactory") %>% 
  count(name) %>% 
  arrange(desc(n)) %>% 
  head(n = 10) %>% 
  View()


# frequency
rawdata %>% 
  filter(city == "Sault Ste. Marie") %>% 
  filter(infractions !="No infractions found at the time of inspection") %>% 
  count(infractions) %>% 
  ggplot(aes(reorder(infractions, n), n)) +
  geom_col() +
  coord_flip()
  
# how many visits per year
rawdata %>% 
  group_by(year = year(visit), city) %>% 
  summarise(total = n()) %>% 
  filter(total > 10) %>% 
  ggplot(aes(x=reorder(city, total), y=total)) +
  geom_col(fill="#1BA5B8", alpha=0.8) +
  coord_flip() +
  theme_ipsum_rc(grid = "X")


import_roboto_condensed()



rawdata %>% 
  group_by(visit, name) %>% 
  summarise(total_infractions = sum(infractions != "No infractions found at the time of inspection")) %>% 
  filter(total_infractions > 0) %>% 
  arrange(desc(total_infractions)) %>% 
  View()

rawdata %>% 
  filter(city == "Sault Ste. Marie") %>% 
  group_by(name, street) %>% 
  summarise(visits = n_distinct(visit),
            infractions = sum(infractions!="No infractions found at the time of inspection"),
            infraction_score = round(infractions / visits, 2)) %>% 
  arrange(desc(infraction_score)) %>% 
  head(10) %>% 
  pull(name) -> top10 
View()


# ------ top 20 ---------

library(ggalt)
library(hrbrthemes)
rawdata %>% 
  filter(city == "Sault Ste. Marie") %>% 
  group_by(name, street) %>% 
  summarise(visits = n_distinct(visit),
            infractions = sum(infractions!="No infractions found at the time of inspection"),
            infraction_score = round(infractions / visits, 2)) %>% 
  arrange(desc(infraction_score)) %>% 
  head(20) %>% 
  ggplot(aes(x=reorder(name, infractions), infractions)) +
  geom_lollipop(point.colour="#1BA5B8", point.size=2) +
  coord_flip() +
  labs(title = "Top 20 Sault Ste Marie Restaurants",
       x = NULL,
       y = "# of health infractions",
       subtitle = "by Algoma Public Health Restaurant Inspection Report Infractions",
       caption = "Data from Algoma Public Health Inspection Reports \n http://www.algomapublichealth.com/inspections-environment/food-safety/restaurant-inspection-reports/ \n more analysis @ robcoleman.ca") +
  theme_minimal(base_family="Roboto") +
  theme(panel.grid.major.y=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(axis.line.y=element_line(color="#2b2b2b", size=0.15)) +
  theme(axis.text.y=element_text(margin=margin(r=0, l=0))) +
  theme(plot.margin=unit(rep(30, 4), "pt")) +
  theme(plot.title=element_text(face="bold")) +
  theme(plot.subtitle=element_text(margin=margin(b=10))) +
  theme(plot.caption=element_text(size=8, margin=margin(t=10)))

ggsave("test.png", plot = last_plot(), height = 5, width=8)



rawdata %>% 
  filter(infractions != "No infractions found at the time of inspection") %>% 
  count(infractions) %>% 
  filter(n > 0) %>% 
  ggplot(aes(reorder(infractions, n), n)) +
  geom_col(fill="#1BA5B8", alpha=0.8) +
  coord_flip() +
  labs(title="Top infraction types by frequency",
       x= "",
       y=  "# of infractions") +
  theme(legend.position="none") +
  theme_ipsum_rc(grid="X") 


