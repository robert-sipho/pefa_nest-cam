library(lubridate)
library(tidyverse)
library(readtext)
library(stringr)
library(nuwcru)
library(zoo)
library(nuwcru)



## Updated with meta data - December 30 2020 - 


# Disclaimer ~~~~
# it would be really nice to have one script that can clean all years without edits
# Each year has unique ... eccentricities ... and it's my opinion that we should 
# slowly clean each year to deal with the unique characteristics. If we do a better
# job of standardizing this pipeline (from reconyx in the field to computer), we can 
# automate the data cleaning, but for now, we really need to be attentive and do this 
# slowly/thoughtfully



# load data
clim <- read_csv("data/02_weather_1981-2019.csv") %>% select(-X1)
trt <- read_csv("data/00_trt.csv") %>% rename("site" = "NEST", "year" = "YEAR", "trt" = "TREATMENT")
trt$site <- as.character(trt$site)

# load meta
meta <- arrow::read_parquet("data/00_meta/clean_meta_2017.parquet")

# sergeant Drill should be an option to read parquets but I can't get it to connect. 

# load files from specific year
f <- list.files("data/01_site_parquets/", full.names = TRUE, pattern = "*.parquet")
f <- f[str_detect(f, "2017")]
#f <- f[!str_detect(f, "19")] # remove sites 19 and 72, apparently some issues with labesl there.
#f <- f[!str_detect(f, "72")] # remove sites 19 and 72, apparently some issues with labesl there.

all_2017 <- do.call(rbind, lapply(f, arrow::read_parquet))




# *clean dates and site numbers  ---------------------------------------------


# dates - some missing in meta data, use filename
all_2017$Image_name <- str_replace(all_2017$Image_name, "/media/robert/", "")
meta$Image_name <- str_replace(meta$Image_name, "//", "/")
join <- all_2017 %>% left_join(meta, by = "Image_name")

join$date <- str_sub(join$Image_name, 
        str_locate(join$Image_name, "/201")[,1]+1,
        str_locate(join$Image_name, ".JPG")[,1]-1)
join$date <- str_sub(join$date, 0, 19)
join$date <- parse_date_time(join$date, "y-m-d h-M-s")

all_2017 <- join

# some sites missing in meta_data, use filename
sites_missing <- all_2017 %>% filter(is.na(meta_site)) 
sites_not_missing <- all_2017 %>% filter(!is.na(meta_site)) 

sites_missing$meta_site <- str_sub(sites_missing$Image_name, str_locate(sites_missing$Image_name, "/Site")[,1]+1)
sites_missing$meta_site <- str_sub(sites_missing$meta_site, 0, str_locate(sites_missing$meta_site, "/")[,1]-1)
sites_missing$meta_site <- as.character(as.numeric(gsub(".*?([0-9]+).*", "\\1", sites_missing$meta_site)))

all_2017 <- rbind(sites_missing, sites_not_missing)


# *house keeping ----------------------------------------------------------
### all_2017$site <- str_sub(all_2017$Image_name, str_locate(tolower(all_2017$Image_name), "/site ")[,2]+1, str_locate(all_2017$Image_name, "/201")[,1]-1)

# an example of an odd characteristic... images have been Broken up into multiple directories
# per site

# remove post hatch, so resulting pictures are clumped purely by site
### all_2017$site <- str_replace(all_2017$site, "/Post-hatch", "") 

### all_2017$date <- str_sub(all_2017$Image_name, str_locate(all_2017$Image_name, "/201")[,2]-3, str_locate(all_2017$Image_name, ".JPG")[,1]-1)

# remove motion trigger numbers
### all_2017$date <- str_sub(all_2017$date, 0, 19) 


all_2017$yday  <- yday(all_2017$date)
all_2017$hour  <- hour(all_2017$date)
all_2017$month <- month(all_2017$date)
names(all_2017)
# reduce data frame to number of detections
all_t <- all_2017 %>% mutate(adult = rowSums(.[-38:-44] == "adult", na.rm = TRUE),
                             eggs  = rowSums(.[-38:-44] == "eggs", na.rm = TRUE),
                             nestling = rowSums(.[-38:-44] == "nestling", na.rm = TRUE),
                             sband  = rowSums(.[-38:-44] == "sband", na.rm = TRUE),
                             bband = rowSums(.[-38:-44] == "bband", na.rm = TRUE))  %>% 
  select(Image_name, site = meta_site, date, month, yday, hour, adult, nestling, sband, bband) 


# Nestling Work -----------------------------------------------------------



x <- all_2017[which(all_t$nestling > 0),]
piv1 <- x %>%
  select(Class1:height6, Image_name) %>%
  mutate_at(vars(2:35), as.character) %>%
  pivot_longer(Class1:height6,
               values_transform = list(Percent1 = as.character))

nestling_info <- piv1[which(piv1$value == "nestling"),]
nestling_info[,4] <- piv1[which(piv1$value == "nestling")+1,3] # likelihood
nestling_info[,5] <- piv1[which(piv1$value == "nestling")+2,3] # leftx
nestling_info[,6] <- piv1[which(piv1$value == "nestling")+3,3] # topy
nestling_info[,7] <- piv1[which(piv1$value == "nestling")+4,3] # width
nestling_info[,8] <- piv1[which(piv1$value == "nestling")+5,3] # height


nestling_info <- nestling_info %>%
  rename(lik = 4, leftx = 5, topy = 6, width = 7, height = 8) %>%
  select(-value)

nestling_info$name <- str_replace(nestling_info$name, "Class", "nestling")

nestling_wide <- nestling_info %>%
  pivot_wider(id_cols = Image_name, names_from = name, values_from = lik:height)


nestling_2017 <- all_t %>% 
  left_join(select(nestling_wide, Image_name, starts_with("lik")), by = "Image_name") %>%
  mutate_at(vars(starts_with("lik")), ~replace_na(., 0)) 

nestling_2017[,11:ncol(nestling_2017)] <- sapply(nestling_2017[,11:ncol(nestling_2017)],as.numeric)




arrow::write_parquet(nestling_2017, "data/02_nestling_likelihoods_2017.parquet")



arrow::write_parquet(all_t, "data/02_alldata_2017.parquet")

# *visual* number of pictures taken each day


brood <- read_csv("data/03_daily_survival.csv") %>% 
  filter(year == 2017) %>%
  filter(age == 1)



all_t %>% mutate(date = date(date)) %>% group_by(site, date) %>% summarize(sum = sum(adult), count = n()) %>% mutate(prop = sum/count) %>%
  ggplot() +
  geom_segment(aes(x = date, xend = date, y = 0, yend = count, group = site), size = 2) +
  geom_point(data = brood, aes(x = date, y = 10),shape = 21, color = nuwcru::red2, size = 2) +
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
  list_pres[[i]] <- na.locf(list_pres[[i]], maxgap = 15, na.rm = FALSE)
}

all_d <- bind_rows(list_pres)
# arrow::write_parquet(all_d, "data/02_alldata_2017.parquet")

all_d %>% group_by(site, date(date)) %>% tally()


daily_split <- all_d %>% drop_na(site) %>% group_by(site, date(date)) %>% summarize(prop = sum(pres)/n()) %>%
  rename("date" = "date(date)") %>% group_split()

#expand dates, fill NAs with previous value
daily <- list()
for (i in 1:length(daily_split)){
  daily[[i]] <- clim %>% left_join(daily_split[[i]], by = "date")
}

trt_2017 <- trt %>% filter(year == 2017)
daily_df <- daily %>% bind_rows(daily) %>% drop_na(site)#  %>% left_join(trt_2017, by = "site")

# *visual* of daily proportion nest attendance
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
y2017 <- daily_df %>% select(-year.y) %>% rename("year" = "year.x")

write.csv(daily_df, "data/2017_prop.csv")
