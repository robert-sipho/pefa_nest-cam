library(lubridate)
library(tidyverse)
library(readtext)
library(stringr)
library(nuwcru)
library(zoo)
library(nuwcru)

# Disclaimer ~~~~
# it would be really nice to have one script that can clean all years without edits
# Each year has unique ... eccentricities ... and it's my opinion that we should 
# slowly clean each year to deal with the unique characteristics. If we do a better
# job of standardizing this pipeline (from reconyx in the field to computer), we can 
# automate the data cleaning, but for now, we really need to be attentive and do this 
# slowly/thoughtfully



# load data
clim <- read_csv("data/02_weather_1981-2019.csv") %>% select(-X1)
trt <- read_csv("data/trt.csv") %>% rename("site" = "NEST", "year" = "YEAR", "trt" = "TREATMENT")
trt$site <- as.character(trt$site)



# sergeant Drill should be an option to read parquets but I can't get it to connect. 

# load files from specific year
f <- list.files("data/01_site_parquets/", full.names = TRUE, pattern = "*.parquet")
f <- f[str_detect(f, "2013")]
#f <- f[!str_detect(f, "19")] # remove sites 19 and 72, apparently some issues with labesl there.
#f <- f[!str_detect(f, "72")] # remove sites 19 and 72, apparently some issues with labesl there.

all_2013 <- do.call(rbind, lapply(f, arrow::read_parquet))
head(all_2013)



# *house keeping ----------------------------------------------------------
all_2013$site <-str_sub(all_2013$Image_name, str_locate(tolower(all_2013$Image_name), "/site ")[,2]+1, str_locate(all_2013$Image_name, "/2013-")[,1]-1)

# an example of an odd characteristic... images have been Broken up into multiple directories
# per site

all_2013$date <- str_sub(all_2013$Image_name, str_locate(all_2013$Image_name, "/2013-")[,2]-4, str_locate(all_2013$Image_name, ".JPG")[,1]-1)

# remove motion trigger numbers
all_2013$date <- str_sub(all_2013$date, 0, 19) 


all_2013$date <- parse_date_time(all_2013$date, "y-m-d h-M-s")
all_2013$yday <- yday(all_2013$date)
all_2013$hour <- hour(all_2013$date)
all_2013$month <- month(all_2013$date)



# reduce data frame to number of detections
all_t <- all_2013 %>% mutate(adult    = rowSums(.[-39] == "adult", na.rm = TRUE),
                             eggs     = rowSums(.[-39] == "eggs", na.rm = TRUE),
                             nestling = rowSums(.[-39] == "nestling", na.rm = TRUE),
                             sband    = rowSums(.[-39] == "sband", na.rm = TRUE),
                             bband    = rowSums(.[-39] == "bband", na.rm = TRUE))  %>% 
  select(site, date, month, yday, hour, adult, nestling, sband, bband) %>% drop_na(site) #%>%

arrow::write_parquet(all_t, "data/02_alldata_2013.parquet")


# *visual* number of pictures taken each day ~~~~~~~~~~~~~~~~~~~~~~~~~~
all_t %>% mutate(date = date(date)) %>% group_by(site, date) %>% summarize(sum = sum(adult), count = n()) %>% mutate(prop = sum/count) %>%
  ggplot() +
  geom_segment(aes(x = date, xend = date, y = 0, yend = count, group = site), size = 2) +
  #scale_x_date(date_breaks = "2 weeks",
  # date_labels = "%B %d") +
  facet_grid(site~.) +
  theme_nuwcru() 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

trt_2013 <- trt %>% filter(year == 2013)
daily_df <- daily %>% bind_rows(daily) %>% drop_na(site) %>% left_join(trt_2013, by = "site")

# *visual* of daily proportion nest attendance ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

daily_df %>% group_by(site, trt) %>% tally()
y2013 <- daily_df %>% select(-year.y) %>% rename("year" = "year.x")

write.csv(y2013, "data/02_2013_prop.csv")

