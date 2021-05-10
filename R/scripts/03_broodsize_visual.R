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
# clean Model data --------------------------------------------------------

unique(brood$site)
brood %<>% filter(year == 2017)

# count
nestling_count <- model %>% 
  mutate(day = date(date)) %>%
  group_by(site, day) %>% 
  summarize(max_nestling = max(nestling)) %>% 
  mutate(yday = yday(day),
         max_nestling_prop = max_nestling/4) 


# binary
model_summary <- model %>%
  mutate(nestling = ifelse(nestling > 0, 1, 0)) %>%
  mutate(day = date(date)) %>%
  group_by(site, day) %>%
  summarize(prop = sum(nestling) / n()) %>%
  mutate(yday = yday(day))

as.character(unique(brood$site)[1:10])
sites <- as.character(unique(brood$site)[1:10])

ggplot() +
  geom_line(data = filter(brood, site %in% sites & year == 2017), aes(x = yday, y = b_size), colour = grey4) +
  geom_point(data = filter(brood,site %in% sites & year == 2017), aes(x = yday, y = b_size), colour = "white", size = 3) +
  geom_text(data = filter(brood, site %in% sites & year == 2017), aes(x = yday, y = b_size, label = b_size), size = 2.5) +
  #geom_line(data = filter(model_summary, site %in% sites), aes(x = yday, y = prop), colour = red2) +
  geom_line(data = filter(nestling_count, site %in% sites), aes(x = yday, y = max_nestling), colour = red3) +
  scale_alpha_continuous(range = c(0,1)) +
  #scale_y_continuous(limits = c(0,1.5)) +
  scale_x_continuous(limits = c(190,225)) +
  facet_grid(site~.)+
  ylab("brood size confidence") + xlab("julian day") +
  theme_nuwcru() + theme(legend.position = "none") + facet_nuwcru()
               







