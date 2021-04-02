library(tidyverse)
library(lubridate)
library(magrittr)
library(nuwcru)



d <- arrow::read_parquet("data/02_clean_parquets/02_alldata_2017.parquet")


var <- read_csv("data/00_varsens.csv")
var$date_end <- ymd_hms(paste(var$date, var$filled_end))
var <- var %>% filter(year == 2017) %>%
  filter(!is.na(date_end))



# Create new date frame with all minutes between min and max dates    
empty_dates <- tibble(date = seq(min(var$date_start, na.rm = TRUE), max(var$date_end, na.rm=TRUE), by = "1 mins")) %>%
  mutate(date = round_date(date, unit = "min"))




end <- var %>% 
  select(site, date, date_start, date_end) %>%
mutate(date_end   = round_date(date_end, unit = "minute")) %>%
mutate(date   = date_end) %>%
  select(date_end, date, site)

start <- var %>% 
  select(site, date, date_start, date_end) %>%
mutate(date_start   = round_date(date_start, unit = "minute")) %>%
mutate(date   = date_start) %>%
  select(date_start, date, site)

x <- empty_dates %>% 
  left_join(end, by = "date") %>% 
  left_join(start, by = c("date", "site"))



x %<>% 
  mutate(prov = case_when(
    !is.na(date_start) ~ 1,
    !is.na(date_end)  ~ 0)) 

#expand dates, fill NAs with previous value
for (i in 1:length(list_pres)){
  list_pres[[i]] <- empty_dates %>% 
    left_join(list_pres[[i]], by = "date") %>% 
    select(date, date_start, date_end) %>%
    mutate(prov = case_when(
      !is.na(date_start) ~ 1,
      !is.na(date_end)   ~ 0))
  list_pres[[i]] <- na.locf(list_pres[[i]], maxgap = 1000)
}
all_d <- bind_rows(list_pres)


x <- empty_dates %>% 
  left_join(list_pres[[1]], by = "date") %>% 
  select(date, date_start, date_end) %>%
  mutate(prov = case_when(
    !is.na(date_start) ~ 1,
    !is.na(date_end)   ~ 0))

x %>% filter(!is.na(date_start))

x <- zoo::na.locf(x, maxgap = 1000)



daily_split <- all_d %>% drop_na(site) %>% group_by(site, date(date)) %>% summarize(prop = sum(pres)/n()) %>%
  rename("date" = "date(date)") %>% group_split()
ga
#expand dates, fill NAs with previous value
daily <- list()
for (i in 1:length(daily_split)){
  daily[[i]] <- clim %>% left_join(daily_split[[i]], by = "date")
}

