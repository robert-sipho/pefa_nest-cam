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


write.csv(x, "data/brood_size_summary.csv")
brood %>%
  f
  tally()
  


brood <- brood %>% 
  mutate(yday = yday(date)) %>%
  group_by(site, year, yday) %>% 
  summarize(b_size = sum(alive))
# clean Model data --------------------------------------------------------

unique(brood$site)
brood %<>% filter(year == 2017)

model_summary <- model %>% 
  mutate(nestling = as.factor(nestling)) %>%
  mutate(day = date(date)) %>%
  group_by(site, day) %>% 
  count(nestling) %>% 
  mutate(yday = yday(day)) %>%
  filter(site == 100) %>%
  group_by(day) %>%
  mutate(prop = n / sum(n))



ggplot() +
  geom_line(data = filter(brood, site == 100), aes(x = yday, y = b_size/4), colour = grey4) +
  geom_point(data = filter(brood, site == 100), aes(x = yday, y = b_size/4), colour = "white", size = 3) +
  geom_text(data = filter(brood, site == 100), aes(x = yday, y = b_size/4, label = b_size), size = 2.5) +
  geom_line(data = filter(model_summary, nestling == 1), aes(x = yday, y = prop), colour = red5) +
  geom_point(data = filter(model_summary, nestling == 1), aes(x = yday, y = prop, alpha = prop),colour = "white", size = 3) +
  geom_point(data = filter(model_summary, nestling == 1), aes(x = yday, y = prop, alpha = prop, size = prop), shape = "1", fill = "white", colour = red2) +
  geom_line(data = filter(model_summary, nestling == 2), aes(x = yday, y = prop), colour = red5) +
  geom_point(data = filter(model_summary, nestling == 2), aes(x = yday, y = prop, alpha = prop),colour = "white", size = 3) +
  geom_point(data = filter(model_summary, nestling == 2), aes(x = yday, y = prop, alpha = prop, size = prop), shape = "2", fill = "white", colour = red2) +
  geom_line(data = filter(model_summary, nestling == 3), aes(x = yday, y = prop), colour = red5) +
  geom_point(data = filter(model_summary, nestling == 3), aes(x = yday, y = prop, alpha = prop),  colour = "white", size = 2) +
  geom_point(data = filter(model_summary, nestling == 3), aes(x = yday, y = prop, alpha = prop, size = prop), shape = "3", fill = "white", colour = red2) +
  geom_line(data = filter(model_summary, nestling == 4), aes(x = yday, y = prop), colour = red5) +
  geom_point(data = filter(model_summary, nestling == 4), aes(x = yday, y = prop, alpha = prop), colour = "white", size = 2) +
  geom_point(data = filter(model_summary, nestling == 4), aes(x = yday, y = prop, alpha = prop, size = prop), shape = "4", fill = "white", colour = red2) +
  scale_alpha_continuous(range = c(0,1)) +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(limits = c(190,225)) +
  ylab("brood size confidence") + xlab("julian day") +
  theme_nuwcru() + theme(legend.position = "none")
               







