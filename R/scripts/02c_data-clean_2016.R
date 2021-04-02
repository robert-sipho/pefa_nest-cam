library(lubridate)
library(tidyverse)
library(readtext)
library(stringr)
library(nuwcru)
library(zoo)
library(nuwcru)

## Updated with meta data - January 5 2020 - 


# Disclaimer ~~~~
# it would be really nice to have one script that can clean all years without edits
# Each year has unique ... eccentricities ... and it's my opinion that we should 
# slowly clean each year to deal with the unique characteristics. If we do a better
# job of standardizing this pipeline (from reconyx in the field to computer), we can 
# automate the data cleaning, but for now, we really need to be attentive and do this 
# slowly/thoughtfully



# load data
clim <- read_csv("data/02_weather_1981-2019.csv") %>% select(-X1)



# sergeant Drill should be an option to read parquets but I can't get it to connect. 

# load files from specific year
f <- list.files("data/01_site_parquets/", full.names = TRUE, pattern = "*.parquet")
f <- f[str_detect(f, "2016")]
#f <- f[!str_detect(f, "19")] # remove sites 19 and 72, apparently some issues with labesl there.
#f <- f[!str_detect(f, "72")] # remove sites 19 and 72, apparently some issues with labesl there.

all_2016 <- do.call(rbind, lapply(f, arrow::read_parquet))




# work with metadata so we can rename appropriate filenames ---------------
meta <- arrow::read_parquet("data/00_meta/clean_meta_2016.parquet")
head(meta)

# match patterns in yolo outs with exifs
meta$to_match <- str_sub(meta$Image_name, str_locate(meta$Image_name, "//")[,1]+2)
all_2016$to_match <- str_sub(all_2016$Image_name, str_locate(tolower(all_2016$Image_name), "/site")[,1]+1)


all_2016 <- all_2016 %>% left_join(meta, by = "to_match")
unique(all_2016$meta_site)
all_2016 %>% filter(is.na(meta_site)) # no NAs


# Date --------------------------------------------------------------------

# there are NA's peppered throughout the data across sources, so this section is a bit involved
# the strategy will be to find NAs, and fill them with sources that actually have date info
# we 

exif <- arrow::read_parquet("/Volumes/GoogleDrive/My Drive/NuWCRU/Analysis/NuWCRU/krmp_image-class/R/data/metadata/meta_2016.parquet")
exif <- exif %>% select(SourceFile, FileModifyDate)
exif$SourceFile <- str_replace(exif$SourceFile, "/Volumes/", "")
exif$SourceFile <- str_replace(exif$SourceFile, "//", "/")

# manipulate to make conversion to date easier
all_2016$meta_date <- str_replace_all(all_2016$meta_date, " AM", "")
all_2016$meta_date <- str_replace_all(all_2016$meta_date, " PM", "")


# all_2016$date <- lubridate::parse_date_time(all_2016$meta_date, "y-m-d h:M:s")
all_2016$date <- all_2016$meta_date
all_2016 %>% filter(str_detect(date, "NA"))

# Split all_2016 into 2, one df where dates are missing (a), and one where dates are present (b)

    b <- all_2016 %>% filter(!str_detect(date, "NA"))
    a <- all_2016 %>% filter(str_detect(date, "NA"))
         # parse to date
         a$date <- str_sub(a$Image_name.x, 
                                       str_locate(a$Image_name.x, "/2016")[,1]+1)
         a$date <- str_replace_all(a$date, ".JPG", "")
         a$date <- lubridate::parse_date_time(a$date, "y-m-d h-M-s")
         a$SourceFile <- str_replace(a$Image_name.x, "/media/robert/", "")
         
         join_exif_a <- a %>% inner_join(exif, by = "SourceFile")
         join_exif_a$FileModifyDate <- str_sub(join_exif_a$FileModifyDate, 0, str_locate(join_exif_a$FileModifyDate, "-")[,1]-1)
         join_exif_a$FileModifyDate <- lubridate::parse_date_time(join_exif_a$FileModifyDate, "y:m:d h:M:s")
         
         # exif dates are all 2 hours ahead of the original
         join_exif_a$FileModifyDate  <- join_exif_a$FileModifyDate - hours(2)
         join_exif_a$date <- join_exif_a$FileModifyDate
         join_exif_a <- join_exif_a %>% select(-SourceFile, -FileModifyDate)
         names(join_exif_a)
         names(b)
         test <- rbind(join_exif_a, b)
         dim(test)
         dim(all_2016)
all_2016 <- test %>% select("Image_name" = "Image_name.x", everything(), -Image_name.y)

all_2016$yday <- yday(all_2016$date)
all_2016$hour <- hour(all_2016$date)

all_2016$month <- month(all_2016$date)


names(all_2016)
# reduce data frame to number of detections
all_t <- all_2016 %>% mutate(adult    = rowSums(.[,2:35] == "adult", na.rm = TRUE),
                             eggs     = rowSums(.[,2:35] == "eggs", na.rm = TRUE),
                             nestling = rowSums(.[,2:35] == "nestling", na.rm = TRUE),
                             sband    = rowSums(.[,2:35] == "sband", na.rm = TRUE),
                             bband    = rowSums(.[,2:35] == "bband", na.rm = TRUE))  %>% 
  select(Image_name, "site" = "meta_site", date, month, yday, hour, adult, nestling, sband, bband, eggs) %>% drop_na(site) #%>%

arrow::write_parquet(all_t, "data/02_alldata_2016.parquet")



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
unique(all_d$site)



daily_split <- all_d %>% drop_na(site) %>% group_by(site, date(date)) %>% summarize(prop = sum(pres)/n()) %>%
  rename("date" = "date(date)") %>% group_split()

#expand dates, fill NAs with previous value
daily <- list()
for (i in 1:length(daily_split)){
  daily[[i]] <- clim %>% left_join(daily_split[[i]], by = "date")
}

daily_df <- daily %>% bind_rows(daily) %>% drop_na(site)

# *visual* of daily proportion nest attendance ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
daily_df %>% mutate(prec_prop = precip/max(daily_df$precip, na.rm = TRUE),
                    daily_temp = (meanTemp + min(meanTemp))/max(meanTemp + min(meanTemp))) %>% 
  # left_join(trt, by = "site") %>% #filter(site %in% c("19", "22", "23", "30", "85", "98")) %>%
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




write.csv(daily_df, "data/2016_prop.csv")

