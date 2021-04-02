library(lubridate)
library(readr)
library(tidyverse)
library(readtext)
library(stringr)
library(exifr)
library(dtplyr)
library(nuwcru)
library(lme4)
library(zoo)

dat <- read_csv("data/00_kfsurv.csv") %>% 
  select(-relhatch, -hatch, -yday, -nestyear, -treatment)

test <- dat %>%
  mutate(hatch_date = lubridate::ymd(paste0(year, "-07-", july)),
         surv = ifelse(last > 29, 1, 0),
         death_date = case_when(
           surv == 1 ~ hatch_date + 70,
           surv == 0 ~ hatch_date + last
         )) %>%
  mutate(alive_time = as.numeric(difftime(death_date, hatch_date, units = "days"))) %>%
  select(-july, -year)

test %>% filter(alive_time == 29)


list <- test %>% 
  filter(alive_time > 1) %>%
  group_by(ind) %>%
  group_split()


new_list <- list()

for (i in 1:length(list)){

  start_date <- list[[i]]$hatch_date
  end_date <- list[[i]]$death_date
  new_list[[i]] <- list[[i]] %>%
    expand(date = seq(ymd(start_date), ymd(end_date), by="days"),
           ind = ind,
           site = site,
           alive = 1, 
           surv = surv) %>%
    mutate(age = cumsum(alive))
}

alive_dates <- bind_rows(new_list) %>%
  mutate(year = year(date))




b_size <- alive_dates %>%
  mutate(yday = yday(date)) %>%
  group_by(site, year, date(date)) %>%
  summarize(brood = sum(alive))



write.csv(alive_dates, "data/03_daily_survival.csv")
