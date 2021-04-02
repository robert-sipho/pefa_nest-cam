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

getwd()

# load data
clim <- read_csv("data/02_weather_1981-2019.csv") %>% select(-X1)
trt <- read_csv("data/00_site_treatment_summary.csv") %>% select(-X1) %>% mutate(site = as.factor(site))

# sergeant Drill should be an option to read parquets but I can't get it to connect. 

# load files from specific year
f <- list.files("data/01_site_parquets/", full.names = TRUE, pattern = "*.parquet")
f <- f[str_detect(f, "2014")]

all_2014 <- do.call(rbind, lapply(f, arrow::read_parquet))



# *house keeping ----------------------------------------------------------
all_2014$site <- as.factor(str_sub(all_2014$Image_name, str_locate(tolower(all_2014$Image_name), "/site ")[,2]+1, str_locate(all_2014$Image_name, "/2014-")[,1]-1))
all_2014 %>% filter(site == 100)
# an example of an odd characteristic... images have been Broken up into multiple directories
# per site

all_2014 <- all_2014 %>% filter(site != "33/Site 35")
all_2014$date <- str_sub(all_2014$Image_name, str_locate(all_2014$Image_name, "/2014-")[,2]-4, str_locate(all_2014$Image_name, ".JPG")[,1]-1)

# remove motion trigger numbers
all_2014$date <- str_sub(all_2014$date, 0, 19) 


all_2014$date <- parse_date_time(all_2014$date, "y-m-d h-M-s")
all_2014$yday <- yday(all_2014$date)
all_2014$hour <- hour(all_2014$date)
all_2014$month <- month(all_2014$date)


# reduce data frame to number of detections
all_t <- all_2014 %>% mutate(adult    = rowSums(.[,2:35] == "adult", na.rm = TRUE),
                             eggs     = rowSums(.[,2:35] == "eggs", na.rm = TRUE),
                             nestling = rowSums(.[,2:35] == "nestling", na.rm = TRUE),
                             sband    = rowSums(.[,2:35] == "sband", na.rm = TRUE),
                             bband    = rowSums(.[,2:35] == "bband", na.rm = TRUE))  %>% 
  select(site, date, month, yday, hour, adult, nestling, sband, bband) %>% drop_na(site) #%>%


levels(all_t$site)

# *visual* number of pictures taken each day ~~~~~~~~~~~~~~~~~~~~~~~~~~
all_t %>% mutate(date = date(date)) %>% 
  group_by(site, date) %>% 
  summarize(sum = sum(adult), count = n()) %>% 
  mutate(prop = sum/count) %>%
  filter(site %in% c(33, 100, 119)) %>%
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

daily_df <- daily %>% bind_rows(daily) %>% drop_na(site) %>% left_join(trt, by = c("year", "site"))

# *visual* of daily proportion nest attendance ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
daily_df %>% mutate(prec_prop = precip/max(daily_df$precip, na.rm = TRUE),
                    daily_temp = (meanTemp + min(meanTemp))/max(meanTemp + min(meanTemp))) %>% 
  left_join(trt, by = "site") %>% filter(site %in% c("72")) %>%
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
y2016 <- daily_df %>% select(-year.y) %>% rename("year" = "year.x")

write.csv(daily_df, "data/02_2014_prop.csv")

