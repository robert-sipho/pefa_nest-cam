library(lubridate)
library(readr)
library(magrittr)
library(tidyverse)
library(readtext)
library(stringr)
library(exifr)
library(dtplyr)
library(nuwcru)
library(lme4)
library(zoo)


adult_pres <- read_csv("data/02_2017_prop.csv")
kfsurv <- read_csv("data/00_kfsurv.csv") #%>% filter( year == 2017)
datasheet <- read_csv("data/hatch_14.csv") %>% rename("yday_hatch" = "yday")
model <- arrow::read_parquet("data/02_clean_parquets/02_alldata_2017.parquet")

brood <- read_csv("data/03_daily_survival.csv") %>% select(-X1)

x <- brood %>%
  filter(!duplicated(ind)) %>%
  group_by(year, site) %>%
  tally() 

brood <- brood %>% 
  mutate(yday = yday(date)) %>%
  group_by(site, year, yday) %>% 
  summarize(b_size = sum(alive))




# visualize nestling counts -----------------------------------------------



unique(brood$site)
brood %<>% filter(year == 2017)

# count
nestling_count <- model %>% 
  mutate(day = date(date)) %>%
  group_by(site, day) %>% 
  summarize(max_nestling = max(nestling)) %>% 
  mutate(yday = yday(day),
         max_nestling_prop = max_nestling/4) 


as.character(unique(brood$site)[1:10])
sites <- as.character(unique(brood$site)[10:30])

brood %>%
  filter(year == 2017) %>%
  mutate(site = as.character(site)) %>%
  left_join(nestling_count, by = c("site","yday")) %>%
  filter(site %in% sites) %>%
ggplot() +
  geom_line(aes(x = yday, y = b_size), colour = grey4) +
  geom_point(aes(x = yday, y = b_size), colour = "white", size = 3) +
  geom_segment(aes(x = yday, xend = yday, y = b_size, yend = max_nestling), colour = blue4) +
  geom_point(aes(x = yday, y = b_size), colour = "white", size = 3, alpha = 0.5) +
  geom_point(aes(x = yday, y = max_nestling), shape = 21, fill = NA, colour = blue4, size = 2) +
  geom_text(aes(x = yday, y = b_size, label = b_size), size = 2.5) +
  scale_alpha_continuous(range = c(0,1)) +
  #scale_y_continuous(limits = c(0,1.5)) +
  scale_x_continuous(limits = c(190,225)) +
  facet_grid(site~.)+
  ylab("brood size confidence") + xlab("julian day") +
  theme_nuwcru() + theme(legend.position = "none") + facet_nuwcru()
               





# visualize nestling binary -----------------------------------------------




# binary
model_summary <- model %>%
  mutate(nestling = ifelse(nestling > 0, 1, 0)) %>%
  mutate(day = date(date)) %>%
  group_by(site, day) %>%
  summarize(prop = sum(nestling) / n()) %>%
  mutate(yday = yday(day))


sites <- as.character(unique(brood$site)[10:30])
adult <- adult_pres %>%
  mutate(yday = yday(date),
         site = as.character(site)) %>%
  select(yday, site, "prop_adult" = "prop")

brood %>%
  filter(year == 2017) %>%
  mutate(site = as.character(site)) %>%
  left_join(model_summary, by = c("site","yday")) %>%
  left_join(adult, by = c("site", "yday")) %>%
  filter(site %in% sites) %>%
  ggplot() +
  geom_line(aes(x = yday, y = b_size/5), colour = grey4) +
  geom_point(aes(x = yday, y = b_size/5), colour = "white", size = 3) +
  
  geom_segment(aes(x = yday+0.15, xend = yday+0.15, y = 0, yend = prop), colour = blue4, size = 2) +
  geom_segment(data = filter(model_summary, site %in% sites), aes(x = yday, xend = yday, y = 0, yend = prop), colour = blue4, size = 2) +
  geom_segment(aes(x = yday-0.15, xend = yday-0.15, y = 0, yend = prop_adult), colour = blue5, size = 2) +
  
  geom_point(aes(x = yday, y = b_size/5), colour = "white", size = 3, alpha = 0.5) +
  geom_text(aes(x = yday, y = b_size/5, label = b_size), size = 2.5) +
  scale_alpha_continuous(range = c(0,1)) +
  scale_x_continuous(limits = c(190,225)) +
  facet_grid(site~.)+
  ylab("brood size confidence") + xlab("julian day") +
  theme_nuwcru() + theme(legend.position = "none") + facet_nuwcru()
