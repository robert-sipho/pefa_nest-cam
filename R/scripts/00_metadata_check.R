library(tidyverse)



# 2015 --------------------------------------------------------------------

d <- arrow::read_parquet("/Volumes/GoogleDrive/My Drive/NuWCRU/Analysis/NuWCRU/krmp_image-class/R/data/metadata/meta_2015.parquet")
head(d)


d$dir <- str_sub(d$SourceFile, str_locate(d$SourceFile, "//")[,1]+2) %>%
  str_sub(0, str_locate(., "/")[,1]-1)
unique(d$dir)

list <- d %>% group_by(dir) %>% group_split()

t <- list()
for (i in 1:length(list)){
  x <- str_split(toString(list[[i]]$Comment), "\r")
  x <- tibble(col = unlist(x))
  
  if (!str_detect(x[,1], "NA")){ #If metadata is found, create tibble
    date <- x %>% filter(str_detect(col, "Dat")) %>% mutate(date = str_sub(col, 6)) %>% select(-col)
    site <- x %>% filter(str_detect(col, "Lbl")) %>% mutate(site = str_sub(col, 6)) %>% select(-col)
    t[[i]] <- tibble(date = date$date, 
                     site = site$site, 
                     Image_name = list[[i]]$SourceFile)
  } else{ # If metadata can't be found, insert NAs for dates
    t[[i]] <- tibble(date = rep("NA", nrow(list[[i]])), 
                     site = list[[i]]$dir, 
                     Image_name = list[[i]]$SourceFile)
  }
}


head(meta_2015)
meta_2015 <- bind_rows(t)

meta_2015$Image_name <- str_replace(meta_2015$Image_name, "/Volumes/", "")
meta_2015$source_site <- str_sub(meta_2015$Image_name, 
                                 str_locate(meta_2015$Image_name, "//")[,2]+1,
                                 str_locate(meta_2015$Image_name, "/2015")[,1]-1) 
meta_2015$source_site <- str_sub(meta_2015$source_site, 0, 8)

meta_2015$source_site <- as.numeric(gsub(".*?([0-9]+).*", "\\1", meta_2015$source_site))


meta_2015$meta_site <- as.numeric(ifelse(is.na(meta_2015$date), NA, as.character(as.numeric(gsub(".*?([0-9]+).*", "\\1", meta_2015$site)))))






diff <- meta_2015 %>% mutate(diff = as.numeric(source_site) - as.numeric(meta_site)) %>%
  filter(diff != 0)

unique(diff$source_site)

meta_2015 <- meta_2015 %>% select(Image_name, -site, "meta_date" = date, meta_site, source_site)

arrow::write_parquet(meta_2015, "data/00_meta/clean_meta_2015.parquet")



# 2016 --------------------------------------------------------------------


d <- arrow::read_parquet("/Volumes/GoogleDrive/My Drive/NuWCRU/Analysis/NuWCRU/krmp_image-class/R/data/metadata/meta_2016.parquet")

d$dir <- str_sub(d$SourceFile, str_locate(d$SourceFile, "//")[,1]+2) %>%
  str_sub(0, str_locate(., "/")[,1]-1)
unique(d$dir)

list <- d %>% group_by(dir) %>% group_split()
head(d)

d %>% filter(str_detect(FileModifyDate, "NA"))

t <- list()
for (i in 1:length(list)){
  x <- str_split(toString(list[[i]]$Comment), "\r")
  x <- tibble(col = unlist(x))
  
  if (!str_detect(x[,1], "NA")){ #If metadata is found, create tibble
    date <- x %>% filter(str_detect(col, "Dat")) %>% mutate(date = str_sub(col, 6)) %>% select(-col)
    site <- x %>% filter(str_detect(col, "Lbl")) %>% mutate(site = str_sub(col, 6)) %>% select(-col)
    t[[i]] <- tibble(date = date$date, 
                     site = site$site, 
                     Image_name = list[[i]]$SourceFile)
  } else{ # If metadata can't be found, insert NAs for dates
    t[[i]] <- tibble(date = rep("NA", nrow(list[[i]])), 
                     site = list[[i]]$dir, 
                     Image_name = list[[i]]$SourceFile)
  }
}


meta_2016 <- bind_rows(t)
head(meta_2016)

meta_2016$Image_name <- str_replace(meta_2016$Image_name, "/Volumes/", "")


meta_2016$source_site <- str_sub(meta_2016$Image_name,str_locate(meta_2016$Image_name, "//")[,2]+1) 
meta_2016$source_site <- str_sub(meta_2016$source_site, 0, str_locate(meta_2016$source_site,"/")[,1]-1)

meta_2016$source_site <- as.numeric(gsub(".*?([0-9]+).*", "\\1", meta_2016$source_site))


meta_2016$meta_site <- as.numeric(ifelse(is.na(meta_2016$date), NA, as.character(as.numeric(gsub(".*?([0-9]+).*", "\\1", meta_2016$site)))))






diff <- meta_2016 %>% mutate(diff = as.numeric(source_site) - as.numeric(meta_site)) %>%
  filter(diff != 0)

unique(diff$source_site)

meta_2016 <- meta_2016 %>% select(Image_name, -site, "meta_date" = date, meta_site, source_site)

arrow::write_parquet(meta_2016, "data/00_meta/clean_meta_2016.parquet")




# 2017 --------------------------------------------------------------------

d <- arrow::read_parquet("/Volumes/GoogleDrive/My Drive/NuWCRU/Analysis/NuWCRU/krmp_image-class/R/data/metadata/meta_2017.parquet")
d$dir <- str_sub(d$SourceFile, str_locate(d$SourceFile, "//")[,1]+2) %>%
  str_sub(0, str_locate(., "/")[,1]-1)
unique(d$dir)

list <- d %>% group_by(dir) %>% group_split()

t <- list()
for (i in 1:length(list)){
    x <- str_split(toString(list[[i]]$Comment), "\r")
    x <- tibble(col = unlist(x))
    
    if (!str_detect(x[,1], "NA")){ #If metadata is found, create tibble
      date <- x %>% filter(str_detect(col, "Dat")) %>% mutate(date = str_sub(col, 6)) %>% select(-col)
      site <- x %>% filter(str_detect(col, "Lbl")) %>% mutate(site = str_sub(col, 6)) %>% select(-col)
      t[[i]] <- tibble(date = date$date, 
                       site = site$site, 
                       Image_name = list[[i]]$SourceFile)
    } else{ # If metadata can't be found, insert NAs for dates
      t[[i]] <- tibble(date = rep("NA", nrow(list[[i]])), 
                       site = list[[i]]$dir, 
                       Image_name = list[[i]]$SourceFile)
    }
}


head(meta_2017)
meta_2017 <- bind_rows(t)

meta_2017$Image_name <- str_replace(meta_2017$Image_name, "/Volumes/", "")
# meta_2017$source_site <- str_sub(meta_2017$Image_name, 
#                                  str_locate(meta_2017$Image_name, "//")[,2]+1,
#                                  str_locate(meta_2017$Image_name, "/2017")[,1]-1) 
  

# extract number
meta_2017$source_site <- gsub(".*?([0-9]+).*", "\\1", 
                              test <- str_sub(meta_2017$Image_name, 
                                      str_locate(meta_2017$Image_name, "//")[,2]+1,
                                      str_locate(meta_2017$Image_name, "/2017")[,1]-1))
unique(meta_2017$source_site)

meta_2017$meta_site <- ifelse(is.na(meta_2017$date), NA, as.character(as.numeric(gsub(".*?([0-9]+).*", "\\1", meta_2017$site))))




setdiff(meta_2017$meta_site, meta_2017$source_site)

diff <- meta_2017 %>% mutate(diff = as.numeric(source_site) - as.numeric(meta_site)) %>%
  filter(diff != 0)

unique(diff$source_site)

meta_2017 <- meta_2017 %>% select(Image_name, -site, "meta_date" = date, meta_site, source_site)

arrow::write_parquet(meta_2017, "data/00_meta/clean_meta_2017.parquet")
