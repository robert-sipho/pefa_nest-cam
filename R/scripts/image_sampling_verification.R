library(tidyverse)
library(exifr)

# a script to 
#   1. load and clean verified image results (manual classification)
#   2. run the verified images through yolo
#   3. clean yolo outs
#   4. compare yolo vs. manual classification, and output to a df that summarizes discrepancies
#   5. dump images with discrepancies into appropriate folders (adult mistakes to adult folder)
#   6. re-classify mistaken images to verify whether it was yolo or human error
#   7. Use these results to make a summary about yolo error vs. human error



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
df <- df %>% filter(adults != "unk") %>%
  mutate(source = paste0(path,image)) %>%
  filter(image != ".DS_Store")
dim(df)


# 2. Run model on verification set ---------------------------------------------------------

# Copy files over to hard drive to move to jetson
dest <- '/Volumes/NUWCRU_DATA/2016/'
file.copy(df$source,dest)


# Read in results
results <- readtext("/Volumes/NUWCRU_DATA/2016_verification_results.txt")$text # read in text file


# create line breaks at the word "enter", since this is where the data for each image begins
t <- str_split(toString(results), "Enter ")
t <- t[[1]][2:length(t[[1]])]
t <- tibble(col = unlist(t))
x <- data.frame(do.call('rbind', strsplit(t$col,'s.',fixed=TRUE))) 
x$X2 <- str_replace_all(as.character(x$X2), "\n", "")

# maximum number of objects detected
max(sapply(strsplit(as.character(x$X2),'%'), length))

objects <- as.data.frame(str_split_fixed(x$X2, "%", 8))

results <- as.data.frame(cbind(x$X1, objects))

names(results)[1] <- "path"
results <- results[1:nrow(results)-1,]

# image name
start <- str_locate(results$path, "/2016")
end <- str_locate(results$path, ".JPG")
images <- str_sub(results$path, start[,1]+1, end[,2])


# date/other image attributes
path <- str_replace(results$path, "Image Path: ", "")
path <- str_replace(path, "/media/erik/", "/Volumes/")
path_end <- str_locate(path, ": Predicted")
path <- str_sub(path, 0, path_end[,1]-1)

#path <- na.omit(path)
#exif <- read_exif(paste0(path))
#date <- exif$CreateDate
#tm   <- exif$TriggerMode

pefa_results <- data.frame(
  path = path,
  image = images,
  #date = parse_date_time(date, "y:m:d h:M:s"),
  #trigger = tm,
  #ir = exif$InfraredIlluminator,
  obj1 = as.character(results$V1),
  obj2 = as.character(results$V2),
  obj3 = as.character(results$V3),
  obj4 = as.character(results$V4),
  obj5 = as.character(results$V5),
  obj6 = as.character(results$V6),
  obj7 = as.character(results$V7),
  obj8 = as.character(results$V8)
)


ad <- pefa_results

# sum the occurences of object detections
ad$adult_pred <- Reduce(`+`, lapply(ad[-1], 
                               function(x) lengths(str_extract_all(x, "adult"))))
ad$nestling_pred <- Reduce(`+`, lapply(ad[-1], 
                                  function(x) lengths(str_extract_all(x, "nestling"))))
ad$eggs_pred <- Reduce(`+`, lapply(ad[-1], 
                              function(x) lengths(str_extract_all(x, "eggs"))))
ad$bband_pred <- Reduce(`+`, lapply(ad[-1], 
                               function(x) lengths(str_extract_all(x, "bband"))))
ad$sband_pred <- Reduce(`+`, lapply(ad[-1], 
                               function(x) lengths(str_extract_all(x, "sband"))))


dat <- ad %>% 
  select(image, adult_pred:sband_pred) %>% 
  filter(image != ".DS_Store") %>%
  mutate(image = str_replace(image, "2016/", "")) %>%
  right_join(df, by = "image") %>%
  select(image:sbands)


diff <- dat %>%
  # calculate differences between model predictions and manual classification
  mutate(adult_diff = as.numeric(adult_pred) - as.numeric(adults),
         nestling_diff = as.numeric(nestling_pred) - as.numeric(nestlings),
         eggs_diff = as.numeric(eggs_pred) - as.numeric(eggs),
         bband_diff = as.numeric(bband_pred) - as.numeric(bbands),
         sbands_diff = as.numeric(sband_pred) - as.numeric(sbands))
  
adult_diff <- diff %>% 
  filter(adult_diff != 0) %>%
  mutate(image_path = paste0("/Volumes/NUWCRU_DATA/2016/",image),
         new_image_name = paste0(adult_pred,"_","adults","_",image))

nestling_diff <- diff %>% 
  filter(nestling_diff != 0) %>%
  mutate(image_path = paste0("/Volumes/NUWCRU_DATA/2016/",image),
         new_image_name = paste0(nestling_pred,"_","nestlings",image))

bband_diff <- diff %>% 
  filter(bband_diff != 0) %>%
  mutate(image_path = paste0("/Volumes/NUWCRU_DATA/2016/",image),
         new_image_name = paste0(bband_pred,"_","bbands",image))

sband_diff <- diff %>% 
  filter(sbands_diff != 0) %>%
  mutate(image_path = paste0("/Volumes/NUWCRU_DATA/2016/",image),
         new_image_name = paste0(sband_pred,"_","sbands",image))

egg_diff <- diff %>% 
  filter(eggs_diff != 0) %>%
  mutate(image_path = paste0("/Volumes/NUWCRU_DATA/2016/",image),
         new_image_name = paste0(eggs_pred,"_","eggs",image))

# accuracy results
abs(length(which(diff$adult_diff != 0)) / nrow(diff) * 100 - 100)
abs(length(which(diff$eggs_diff != 0)) / nrow(diff) * 100 - 100)
abs(length(which(diff$bband_diff != 0)) / nrow(diff) * 100 - 100)
abs(length(which(diff$sband_diff != 0)) / nrow(diff) * 100 - 100)


dest <- '/Volumes/NUWCRU_DATA/mistakes/adults/'
dir.create(dest)
file.copy(adult_diff$image_path, dest)
file.rename(paste0(dest, adult_diff$image), paste0(dest,adult_diff$new_image_name))

dest <- '/Volumes/NUWCRU_DATA/mistakes/nestlings/'
dir.create(dest)
file.copy(nestling_diff$image_path, dest)
file.rename(paste0(dest, nestling_diff$image), paste0(dest,nestling_diff$new_image_name))

dest <- '/Volumes/NUWCRU_DATA/mistakes/bband/'
dir.create(dest)
file.copy(bband_diff$image_path, dest)
file.rename(paste0(dest, bband_diff$image), paste0(dest,bband_diff$new_image_name))

dest <- '/Volumes/GoogleDrive/My Drive/NuWCRU/Analysis/emhedlin/nuwcru/cam/pefa_nest-cam/python/verification_gui/verification_results/sband/'
dir.create(dest)
file.copy(sband_diff$image_path, dest)
file.rename(paste0(dest, sband_diff$image), paste0(dest,sband_diff$new_image_name))

dest <- '/Volumes/GoogleDrive/My Drive/NuWCRU/Analysis/emhedlin/nuwcru/cam/pefa_nest-cam/python/verification_gui/verification_results/eggs/'
dir.create(dest)
file.copy(egg_diff$image_path, dest)
file.rename(paste0(dest, egg_diff$image), paste0(dest,egg_diff$new_image_name))





# Load Verification Results -----------------------------------------------


#   * adults --------------------------------------------------------------

source <- "/Volumes/GoogleDrive/My Drive/NuWCRU/Analysis/emhedlin/nuwcru/cam/pefa_nest-cam/python/verification_gui/verification_results/adults"
adults <- list.files(source, pattern = "*.txt", full.names = TRUE, recursive = TRUE)

adult_df <- read.delim(adults, sep = ",", col.names = c("image", "adults", "nestlings", "eggs", "bbands", "sbands")) %>%
  mutate(source = paste0(source, image)) %>%
  mutate(image = str_sub(image, str_locate(adult_df$image, "adults")[,2]+2)) %>%
  filter(image != ".DS_Store") %>%
  select(image, "verified" = "adults")

adult_results <- dat %>%
  select(image, "cnn_pred"="adult_pred", "human_pred"="adults") %>%
  right_join(adult_df, by = "image") %>%
  mutate(cnn_diff  = cnn_pred - verified,
         human_diff= as.numeric(human_pred) - verified)



#  *** Stats --------------------------------------------------------------



  # true positives when we initially thought they were mistakes
TP1 <- adult_results %>%
  filter(cnn_diff == 0) %>%
  summarize(sum = sum(verified))

  # true positives from original
TP2 <- diff %>%
  filter(adult_diff == 0) %>%
  summarize(sum = sum(adult_pred))

TP <- as.numeric(TP1 + TP2)

FP <- as.numeric(adult_results %>%
  filter(cnn_diff > 0) %>%
  summarize(fp = sum(cnn_diff)))

TN1 <- adult_results %>%
  filter(cnn_diff == 0) %>%
  filter(verified == 0) %>%
  tally()
TN2 <- diff %>%
  filter(adult_diff == 0) %>%
  filter(adults == 0) %>%
  tally()
TN <- as.numeric(TN1 + TN2)

FN <- as.numeric(adult_results %>%
  filter(cnn_diff < 0) %>%
  summarize(fn = sum(abs(cnn_diff))))
  
adult_stats <- data.frame(TPR = TP / (TP + FN),
                          FPR = FP / (FP + TN),
                          F1 = (2*TP) / (2*TP) + FP + FN,
                          accuracy = (TP + TN) / (TP + TN + FP + FN))


# * bbands ----------------------------------------------------------------


source <- "/Volumes/GoogleDrive/My Drive/NuWCRU/Analysis/emhedlin/nuwcru/cam/pefa_nest-cam/python/verification_gui/verification_results/bband"
bbands <- list.files(source, pattern = "*.txt", full.names = TRUE, recursive = TRUE)

bband_df <- read.delim(bbands, sep = ",", col.names = c("image", "adults", "nestlings", "eggs", "bbands", "sbands")) %>%
  mutate(source = paste0(source, image)) %>%
  mutate(image = str_sub(image, str_locate(image, "bbands")[,2]+1)) %>%
  filter(image != ".DS_Store") %>%
  select(image, "verified" = "bbands")
names(dat)


bband_results <- tibble(dat) %>%
  select(image, "cnn_pred"="bband_pred", "human_pred"="bbands") %>%
  right_join(bband_df, by = "image") %>%
  mutate(cnn_diff  = cnn_pred - verified,
         human_diff= as.numeric(human_pred) - verified)



#  *** Stats --------------------------------------------------------------



# true positives when we initially thought they were mistakes
TP1 <- bband_results %>%
  filter(cnn_diff == 0) %>%
  summarize(sum = sum(verified))

# true positives from original
TP2 <- tibble(diff) %>%
  filter(adult_diff == 0) %>%
  summarize(sum = sum(bband_pred))

TP <- as.numeric(TP1 + TP2)

FP <- as.numeric(bband_results %>%
                   filter(cnn_diff > 0) %>%
                   summarize(fp = sum(cnn_diff)))

TN1 <- bband_results %>%
  filter(cnn_diff == 0) %>%
  filter(verified == 0) %>%
  tally()
TN2 <- diff %>%
  filter(bband_diff == 0) %>%
  filter(bbands == 0) %>%
  tally()

TN <- as.numeric(TN1 + TN2)

FN <- as.numeric(bband_results %>%
                   filter(cnn_diff < 0) %>%
                   summarize(fn = sum(abs(cnn_diff))))
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
bband_stats <- data.frame(recall = recall,
                          precision = precision,
                          FPR = FP / (FP + TN),
                          F1 = (2*(precision*recall)) / precision+recall,
                          accuracy = (TP + TN) / (TP + TN + FP + FN))




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
  select(-Comment, -UserLabel)

exif_ul <- exif %>%
  select(Directory, FileName, UserLabel, CreateDate) %>% 
  filter(!is.na(UserLabel) & !str_detect(UserLabel, "PC")) %>%
  mutate(site = parse_number(UserLabel),
         date = lubridate::parse_date_time(CreateDate, '%Y:%m:%d %H:%M:%s')) %>%
  select(Directory, FileName, site, date)

meta <- bind_rows(exif_ul,exif_no_ul) %>% 
  mutate(source = paste0(Directory,"/", FileName)) %>%
  select(-Directory,-FileName) %>%
  right_join(df, by = "source") %>%
  select(-path, -image)


# 3. match results --------------------------------------------------------

predictions <- arrow::read_parquet("data/02_alldata_2016_new.parquet") %>%
  rename("adult_pred" = "adult", "nestlings_pred" = "nestling", "eggs_pred" =  "eggs", "bbands_pred" = "bband", "sbands_pred" = "sband")

meta %>% left_join(predictions, by = c("site","date")) %>%
  filter(is.na(sbands_pred))




