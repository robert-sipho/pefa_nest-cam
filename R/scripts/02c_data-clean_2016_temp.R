library(lubridate)
library(tidyverse)
library(readtext)
library(stringr)
library(nuwcru)
library(zoo)
library(nuwcru)




# Load data ---------------------------------------------------------------


# load data
clim <- read_csv("data/02_weather_1981-2019.csv") %>% select(-X1)

f <- list.files("data/01_site_parquets/", full.names = TRUE, pattern = "*.parquet")
all_2016 <- do.call(rbind, 
                    lapply(f, arrow::read_parquet))


# Metadata wrangle --------------------------------------------------------

meta <- arrow::read_parquet("data/00_meta/clean_meta_2016_new.parquet")

# match patterns in yolo outs with exifs
meta$to_match <- str_sub(meta$SourceFile, 
                         str_locate(meta$SourceFile, "//")[,1]+2)
all_2016$to_match <- str_sub(all_2016$Image_name, 
                             str_locate(tolower(all_2016$Image_name), "/site")[,1]+1)


all_2016 <- all_2016 %>% 
  left_join(meta, by = "to_match") %>%
  mutate(month = month(date)
         yday = yday(date),
         hour = hour(date))

# reduce data frame to number of detections
all_t <- all_2016 %>% mutate(adult    = rowSums(.[,2:35] == "adult", na.rm = TRUE),
                             eggs     = rowSums(.[,2:35] == "eggs", na.rm = TRUE),
                             nestling = rowSums(.[,2:35] == "nestling", na.rm = TRUE),
                             sband    = rowSums(.[,2:35] == "sband", na.rm = TRUE),
                             bband    = rowSums(.[,2:35] == "bband", na.rm = TRUE))  %>% 
  # mutate(date = date + hours(2)) %>%
  select(Image_name, site, date, adult, eggs, nestling, sband, bband, eggs) #%>% drop_na(site) #%>%


arrow::write_parquet(all_t, "data/02_alldata_2016_new.parquet")




# **expand to minute resolution ---------------------------------------

# create a new datafame which rounds to minute, and summarizes presence within that minute
a <- all_t %>% 
  mutate(dateR = round_date(date, unit = "min")) %>% 
  group_by(dateR, site) %>% 
  summarize(sum_adult = sum(adult), sum_nestling = sum(nestling)) %>% 
  rename("datetime" = "dateR") %>%
  mutate(pres_adult = ifelse(sum_adult >= 1, 1, 0),
         pres_nestling = ifelse(sum_nestling >= 1, 1, 0)) %>% 
  select(-sum_adult, -sum_nestling) %>% 
  arrange(desc(site)) %>%
  mutate(date = as.Date(datetime))


