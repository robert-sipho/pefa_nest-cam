library(lubridate)
library(tidyverse)
library(readtext)
library(stringr)
library(exifr)




# 1. Wranlge and save data ---------------------------------------------------
t_2000 <- readtext("data/pefa_v4_r1_site151.txt") # read in text file
t_2000 <- t_2000[,2]

# create line breaks at the word "enter", since this is where the data for each image begins
t <- str_split(toString(t_2000), "Enter ")
t <- tibble(col = unlist(t))
t <- t[2:nrow(t),]

# split the string at the first object detected
x <- data.frame(do.call('rbind', strsplit(t$col,'s.',fixed=TRUE))) 

# remove all txt line break characters
x$X2 <- str_replace_all(as.character(x$X2), "\n", "")

# maximum number of objects detected
max(sapply(strsplit(as.character(x$X2),'%'), length))

objects <- as.data.frame(str_split_fixed(x$X2, "%", 7))

results <- as.data.frame(cbind(x$X1, objects))

names(results)[1] <- "path"
results <- results[1:14100,] #bogus last line

# image name
start <- str_locate(results$path, "/2019")
end <- str_locate(results$path, ".JPG")
images <- str_sub(results$path, start[,1]+1, end[,2])

# date/other image attributes
path <- str_replace(results$path, "Image Path: ", "")
path <- str_replace(path, "/media/erik/", "/Volumes/")
path_end <- str_locate(path, ": Predicted")
path <- str_sub(path, 0, path_end[,1]-1)

#path <- na.omit(path)
exif <- read_exif(paste0(path))
date <- exif$CreateDate
tm   <- exif$TriggerMode

pefa_results <- data.frame(
  path = path,
  image = images,
  date = parse_date_time(date, "y:m:d h:M:s"),
  trigger = tm,
  ir = exif$InfraredIlluminator,
  obj1 = as.character(results$V1),
  obj2 = as.character(results$V2),
  obj3 = as.character(results$V3),
  obj4 = as.character(results$V4),
  obj5 = as.character(results$V5),
  obj6 = as.character(results$V6),
  obj7 = as.character(results$V7)
)

# convert to binary for adults
head(pefa_results)
ad <- pefa_results

# sum the occurences of object detections
ad$adult <- Reduce(`+`, lapply(ad[-1], 
                                  function(x) lengths(str_extract_all(x, "adult"))))
ad$nestling <- Reduce(`+`, lapply(ad[-1], 
                               function(x) lengths(str_extract_all(x, "nestling"))))
ad$eggs <- Reduce(`+`, lapply(ad[-1], 
                               function(x) lengths(str_extract_all(x, "eggs"))))
ad$bband <- Reduce(`+`, lapply(ad[-1], 
                               function(x) lengths(str_extract_all(x, "bband"))))
ad$sband <- Reduce(`+`, lapply(ad[-1], 
                               function(x) lengths(str_extract_all(x, "sband"))))


dat <- ad %>% select(path, image, date, trigger, ir, adult:sband)
head(dat)

write.csv(dat, "data/pefav4_151_r1.csv")


# 2. Load clean data ------------------------------------------------------


dat <- read_csv("data/pefav4_151_r1.csv") 
head(dat)

dat %>% filter(adult > 1)

# dump pictures into folder based on filter
source <- dat %>% filter(nestling > 2)
dest <- "/Volumes/NUWCRU_DATA/untitled folder"
file.copy(source$path, dest)
