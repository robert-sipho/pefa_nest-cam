library(lubridate)
library(tidyverse)
library(readtext)
library(stringr)
library(nuwcru)
library(zoo)
library(nuwcru)

# updated with new dates and site numbers from meta data - January 6 2021 ~~

# Disclaimer ~~~~
# it would be really nice to have one script that can clean all years without edits
# Each year has unique ... eccentricities ... and it's my opinion that we should 
# slowly clean each year to deal with the unique characteristics. If we do a better
# job of standardizing this pipeline (from reconyx in the field to computer), we can 
# automate the data cleaning, but for now, we really need to be attentive and do this 
# slowly/thoughtfully



# load data
clim <- read_csv("data/02_weather_1981-2019.csv") %>% select(-X1)
# trt <- read_csv("data/trt.csv") %>% rename("site" = "NEST", "year" = "YEAR", "trt" = "TREATMENT")
# trt$site <- as.character(trt$site)



# sergeant Drill should be an option to read parquets but I can't get it to connect. 

# load files from specific year
f <- list.files("data/01_site_parquets/", full.names = TRUE, pattern = "*.parquet")
f <- f[str_detect(f, "2015")]
#f <- f[!str_detect(f, "19")] # remove sites 19 and 72, apparently some issues with labesl there.
#f <- f[!str_detect(f, "72")] # remove sites 19 and 72, apparently some issues with labesl there.

all_2015 <- do.call(rbind, lapply(f, arrow::read_parquet))

all_2015$Image_name <- str_replace(all_2015$Image_name, "/media/robert/", "")

all_2015
# *house keeping ----------------------------------------------------------

# load site names from meta data - use metadata_check.R
meta_2015 <- arrow::read_parquet("data/00_meta/clean_meta_2015.parquet")

all_2015 <- meta_2015 %>% right_join(all_2015, by = "Image_name")
all_2015$source_site <- as.factor(str_sub(all_2015$Image_name, str_locate(tolower(all_2015$Image_name), "/site ")[,2]+1, str_locate(all_2015$Image_name, "/201")[,1]-1))


all_2015$site <- ifelse(!is.na(all_2015$meta_site), all_2015$meta_site, all_2015$source_site)
all_2015 %>% filter(is.na(site)) %>% select(site, meta_site, source_site)



# Date --------------------------------------------------------------------

# there are NA's peppered throughout the data across sources, so this section is a bit involved
# the strategy will be to find NAs, and fill them with sources that actually have date info
# we 

exif <- arrow::read_parquet("/Volumes/GoogleDrive/My Drive/NuWCRU/Analysis/NuWCRU/krmp_image-class/R/data/metadata/meta_2015.parquet")
exif <- exif %>% select(SourceFile, FileModifyDate)
exif$SourceFile <- str_replace(exif$SourceFile, "/Volumes/", "")
exif$SourceFile <- str_replace(exif$SourceFile, "//", "/")

# manipulate to make conversion to date easier
all_2015$meta_date <- str_replace_all(all_2015$meta_date, " AM", "")
all_2015$meta_date <- str_replace_all(all_2015$meta_date, " PM", "")


# all_2015$date <- lubridate::parse_date_time(all_2015$meta_date, "y-m-d h:M:s")
all_2015$date <- all_2015$meta_date
all_2015$date <- lubridate::parse_date_time(all_2015$date, "y-m-d h:M:s")
all_2015 %>% filter(is.na(date))

# Split all_2015 into 2, one df where dates are missing (a), and one where dates are present (b)

b <- all_2015 %>% filter(!is.na(date))
a <- all_2015 %>% filter(is.na(date))

# parse to date
a$date <- str_sub(a$Image_name, 
                  str_locate(a$Image_name, "/2015")[,1]+1)
a$date <- str_replace_all(a$date, ".JPG", "")
a$date <- str_sub(a$date, 0, 19)
a$date <- lubridate::parse_date_time(a$date, "y-m-d h-M-s")
a$SourceFile <- a$Image_name
a$SourceFile <- str_replace(a$SourceFile, "//", "/")

join_exif_a <- a %>% inner_join(exif, by = "SourceFile")
join_exif_a$FileModifyDate <- str_sub(join_exif_a$FileModifyDate, 0, str_locate(join_exif_a$FileModifyDate, "-")[,1]-1)
join_exif_a$FileModifyDate <- lubridate::parse_date_time(join_exif_a$FileModifyDate, "y:m:d h:M:s")

# exif dates are all 1 hour behind of the original
join_exif_a$FileModifyDate  <- join_exif_a$FileModifyDate + hours(1)
join_exif_a$date <- join_exif_a$FileModifyDate
join_exif_a <- join_exif_a %>% select(-SourceFile, -FileModifyDate)
names(join_exif_a)
names(b)
test <- rbind(join_exif_a, b)

dim(test)
dim(all_2015)

all_2015 <- test 

all_2015$yday <- yday(all_2015$date)
all_2015$hour <- hour(all_2015$date)
all_2015$month <- month(all_2015$date)




all_2015[,5:40]




# reduce data frame to number of detections
all_t <- all_2015 %>% mutate(adult    = rowSums(.[5:40] == "adult", na.rm = TRUE),
                             eggs     = rowSums(.[5:40] == "eggs", na.rm = TRUE),
                             nestling = rowSums(.[5:40] == "nestling", na.rm = TRUE),
                             sband    = rowSums(.[5:40] == "sband", na.rm = TRUE),
                             bband    = rowSums(.[5:40] == "bband", na.rm = TRUE))  %>% 
  select(site, date, month, yday, hour, adult, nestling, sband, bband, "file" = "Image_name") %>% 
  drop_na(site)




# nestling work -----------------------------------------------------------


nestling_info <- piv1[which(piv1$value == "nestling"),]
nestling_info[,4] <- piv1[which(piv1$value == "nestling")+1,3]
nestling_info[,5] <- piv1[which(piv1$value == "nestling")+2,3]
nestling_info[,6] <- piv1[which(piv1$value == "nestling")+3,3]
nestling_info[,7] <- piv1[which(piv1$value == "nestling")+4,3]
nestling_info[,8] <- piv1[which(piv1$value == "nestling")+5,3]


nestling_info <- nestling_info %>%
  rename(lik = 4, leftx = 5, topy = 6, width = 7, height = 8) %>%
  select(-value)

nestling_info$name <- str_replace(nestling_info$name, "Class", "nestling")

nestling_wide <- nestling_info %>%
  pivot_wider(id_cols = Image_name, names_from = name, values_from = lik:height)


nestling_2015 <- all_t %>% 
  rename("Image_name" = "file") %>%
  left_join(select(nestling_wide, Image_name, starts_with("lik")), by = "Image_name") %>%
  mutate_at(vars(starts_with("lik")), ~replace_na(., 0)) 

nestling_2015[,11:ncol(nestling_2015)] <- sapply(nestling_2015[,11:ncol(nestling_2015)],as.numeric)

  


arrow::write_parquet(nestling_2015, "data/02_nestling_likelihoods_2015.parquet")




arrow::write_parquet(all_t, "data/02_alldata_2015.parquet")
all_t <- arrow::read_parquet("data/02_alldata_2015.parquet")

# *visual* number of pictures taken each day ~~~~~~~~~~~~~~~~~~~~~~~~~~

brood <- read_csv("data/03_daily_survival.csv") %>% filter(year == 2015) %>%
  filter(age == 1)

all_t %>% mutate(date = date(date)) %>% group_by(site, date) %>% summarize(sum = sum(adult), count = n()) %>% mutate(prop = sum/count) %>%
  ggplot() +
  geom_segment(aes(x = date, xend = date, y = 0, yend = count, group = site), size = 2) +
  geom_point(data = brood, aes(x = date, y = 10),color = nuwcru::red2, size = 2) +
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

# expand dates, fill NAs with previous value
for (i in 1:length(list_pres)){
  list_pres[[i]] <- empty_dates %>% left_join(list_pres[[i]], by = "date")
  list_pres[[i]] <- na.locf(list_pres[[i]], maxgap = 15)
}
all_d <- bind_rows(list_pres)


# merge in climate data ---------------------------------------------------





# reduce to daily proportions ---------------------------------------------




daily_split <- all_d %>% 
  drop_na(site) %>% 
  group_by(site) %>% 
  rename("date_time" = "date") %>%
  mutate(date = date(date_time)) %>%
  group_split()

# expand dates, fill NAs with previous value
daily <- list()
for (i in 1:length(daily_split)){
  daily[[i]] <- clim %>% filter(year == 2015) %>% left_join(daily_split[[i]], by = "date")
}

trt_2015 <- trt %>% filter(year == 2015)
daily_df <- daily %>% bind_rows(daily) %>% filter(!is.na(site))

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

daily_df %>% group_by(site, trt) %>% tally()
y2015 <- daily_df %>% select(-year.y) %>% rename("year" = "year.x")

write.csv(daily_df, "data/2017_prop.csv")
daily_df <- read_csv("data/2017_prop.csv")
