library(lubridate)
library(tidyverse)
library(readtext)
library(stringr)
library(nuwcru)
library(zoo)
library(nuwcru)
library(weathercan)
stations_search("Rankin Inlet")
weather <- weather_dl(station_ids = 51277, start = "2015-06-25", end = "2017-04-15", format = FALSE)
?weather_dl
# load data
clim <- read_csv("data/weather_1981-2019.csv") %>% select(-X1)
trt <- read_csv("data/trt.csv") %>% rename("site" = "NEST", "year" = "YEAR", "trt" = "TREATMENT")
trt$site <- as.character(trt$site)



# sergeant Drill should be an option to read parquets but I can't get it to connect. 

# load files from 2017 
f <- list.files("data/site_parquets/", full.names = TRUE, pattern = "*.parquet")
f <- f[str_detect(f, "2013")]
#f <- f[!str_detect(f, "19")] # remove sites 19 and 72, apparently some issues with labesl there.
#f <- f[!str_detect(f, "72")] # remove sites 19 and 72, apparently some issues with labesl there.

all_2017 <- do.call(rbind, lapply(f, arrow::read_parquet))




# *house keeping ----------------------------------------------------------
all_2017$site <- str_sub(all_2017$Image_name, str_locate(all_2017$Image_name, "/Site ")[,2]+1, str_locate(all_2017$Image_name, "/2017")[,1]-1)
all_2017$date <- str_sub(all_2017$Image_name, str_locate(all_2017$Image_name, "/2017")[,2]-3, str_locate(all_2017$Image_name, ".JPG")[,1]-1)

# remove motion trigger numbers
all_2017$date <- str_sub(all_2017$date, 0, 19) 


all_2017$date <- parse_date_time(all_2017$date, "y-m-d h-M-s")
all_2017$yday <- yday(all_2017$date)
all_2017$hour <- hour(all_2017$date)
all_2017$month <- month(all_2017$date)



# reduce data frame to number of detections
all_t <- all_2017 %>% mutate(adult    = rowSums(.[-39] == "adult", na.rm = TRUE),
                             eggs     = rowSums(.[-39] == "eggs", na.rm = TRUE),
                             nestling = rowSums(.[-39] == "nestling", na.rm = TRUE),
                             sband    = rowSums(.[-39] == "sband", na.rm = TRUE),
                             bband    = rowSums(.[-39] == "bband", na.rm = TRUE))  %>% 
  select(site, date, month, yday, hour, adult, nestling, sband, bband) %>% drop_na(site) #%>%
left_join(trt, by = "site")


# number of pictures taken each day
all_t %>% mutate(date = date(date)) %>% group_by(site, date) %>% summarize(sum = sum(adult), count = n()) %>% mutate(prop = sum/count) %>%
  ggplot() +
  geom_segment(aes(x = date, xend = date, y = 0, yend = count, group = site), size = 2) +
  #scale_x_date(date_breaks = "2 weeks",
  # date_labels = "%B %d") +
  facet_grid(site~.) +
  theme_nuwcru() 

# **expand to minute resolution ---------------------------------------

# create a new datafame which rounds to minute, and summarizes presence within that minute
a <- all_t %>% mutate(dateR = round_date(date, unit = "min")) %>% 
  group_by(dateR, site) %>% summarize(sum = sum(adult)) %>% rename("date" = "dateR") %>%
  mutate(pres = ifelse(sum >= 1, 1, 0)) %>% select(-sum) %>% arrange(desc(site)) 

# Create new date frame with all minutes between min and max dates    
empty_dates <- tibble(date = seq(min(all_t$date, na.rm = TRUE), max(all_t$date, na.rm=TRUE), by = "1 mins")) %>%
  mutate(date = round_date(date, unit = "min"))






# split
list_pres <- a %>% group_by(site) %>% group_split() 

#expand dates, fill NAs with previous value
for (i in 1:length(list_pres)){
  list_pres[[i]] <- empty_dates %>% left_join(list_pres[[i]], by = "date")
  list_pres[[i]] <- na.locf(list_pres[[i]], maxgap = 60)
}
all_d <- bind_rows(list_pres)




daily_split <- all_d %>% drop_na(site) %>% group_by(site, date(date)) %>% summarize(prop = sum(pres)/n()) %>%
  rename("date" = "date(date)") %>% group_split()

#expand dates, fill NAs with previous value
daily <- list()
for (i in 1:length(daily_split)){
  daily[[i]] <- clim %>% left_join(daily_split[[i]], by = "date")
}

trt_2017 <- trt %>% filter(year == 2017)
daily_df <- daily %>% bind_rows(daily) %>% drop_na(site) %>% left_join(trt_2017, by = "site")


daily_df %>% mutate(prec_prop = precip/max(daily_df$precip, na.rm = TRUE),
                    daily_temp = (meanTemp + min(meanTemp))/max(meanTemp + min(meanTemp))) %>% 
  left_join(trt, by = "site") %>% #filter(site %in% c("19", "22", "23", "30", "85", "98")) %>%
  ggplot() +
  geom_segment(aes(x = date, xend = date, y = 0, yend = prec_prop), colour = blue4, size = 3) +
  #geom_line(aes(x = date, y = daily_temp),colour = blue5) +
  geom_line(aes(x = date, y = prop), colour = grey2) +
  geom_line(aes(x = date, y = prop), colour = grey2) +
  #geom_point(aes(x = date, y = prop),shape = 21, fill = "white", colour =  "black") +
  geom_point(aes(x = date, y = prop),shape = 21, fill = "black", colour =  "white") +
  facet_grid(site ~ .) +
  theme_nuwcru() + theme(axis.title.y=element_blank(),axis.text.y=element_blank(),
                         axis.title.x=element_blank())


daily_df %>% group_by(site, trt) %>% tally()
d_surv <- y
d_surv$date <- as.Date(d_surv$yday, origin = "2017-01-01")



daily_2017 <- daily
daily_2015 <- daily_df
