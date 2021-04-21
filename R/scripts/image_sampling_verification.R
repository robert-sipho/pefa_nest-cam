library(tidyverse)
library(exifr)

# a script to 
#   1. load verified image results (manual classification)
#   2. extract identification info via exif data
#   3. match manual results with yolo predictions
#   4. calculate accuracy across settings (night, day, etc...)



# 1. load manual class data -----------------------------------------------

ver <- read.csv("data/2016_verify/verification_images.csv")

source <- "/Volumes/GoogleDrive/My Drive/NuWCRU/Analysis/emhedlin/nuwcru/cam/pefa_nest-cam/python/verification_gui/images"
results <- list.files(source, pattern = "*.txt", full.names = TRUE, recursive = TRUE)
x <- list()
for (i in 1:length(results)){
  x[[i]] <- read.delim(results[i], sep = ",", 
                       col.names = c("image", "adults", "nestlings", "eggs", "bbands", "sbands"),
                       colClasses = rep("character", 6))
  x[[i]]$path <- str_sub(results[i], 0, str_locate(results[i], "output")[,1]-1)
}
df <- bind_rows(x)
df <- df %>% filter(adults != "unk")
dim(df)


# 2. extract exif ---------------------------------------------------------

files <- paste0(df$path)
exif <- read_exif(source, recursive = TRUE)

# wrangle exif into:

# site , datetime ,
unique(exif$UserLabel)

# drop images where site info is not avaiable
exif_comment <- exif %>% 
  filter(UserLabel %in% c("NA",NA, "PC800 HYPERFIRE PRO")) %>%
  select(Comment)
  
 exif_comment %>%
    filter(!is.na(Comment)) %>%
   # Site
    mutate(site = stringr::str_split(Comment, "\r") %>% 
             map_chr(., 14) %>% 
             str_sub(.,6) %>%
             parse_number()) %>%
   # Date
    mutate(date = stringr::str_split(Comment, "\r") %>% 
             map_chr(., 3) %>% 
             str_sub(.,6,-1) %>%
           lubridate::parse_date_time(., '%y-%m-%d %I:%M:%S %p')) %>%
    filter(site != "Lbl: PC85 RAPIDFIRE PRO") %>%
    select(-Comment)

 
 
exif_no_ul <- exif %>%
  select(Directory, FileName, UserLabel, Comment) %>%
  filter(UserLabel %in% c("NA",NA, "PC800 HYPERFIRE PRO"))  %>%
  filter(!is.na(Comment))  %>%
  # Site
  mutate(site = stringr::str_split(Comment, "\r") %>% map_chr(., 14) %>% str_sub(.,6)) %>%
  filter(!str_detect(site,"RAPIDFIRE")) %>%
  mutate(site = parse_number(site)) %>%
  # Date
  mutate(date = stringr::str_split(Comment, "\r") %>% 
           map_chr(., 3) %>% 
           str_sub(.,6,-1) %>%
           lubridate::parse_date_time(., '%y-%m-%d %I:%M:%S %p')) %>%
  filter(site != "Lbl: PC85 RAPIDFIRE PRO") %>%
  select(-Comment)

x <- exif %>%
  select(Directory, FileName, UserLabel, CreateDate) %>% 
  filter(!is.na(UserLabel) & !str_detect(UserLabel, "PC")) %>%
  mutate(site = parse_number(UserLabel),
         date = lubridate::parse_date_time(CreateDate, '%Y:%m:%d %H:%M:%s')) %>%
  select(site, date)
  x
  

  
names(exif)

  


df %>%
  bind_cols(exif$UserLabel, exif$Comment)
exif %>%
  select(Directory)
 2016-08-12 6:04:27 AM