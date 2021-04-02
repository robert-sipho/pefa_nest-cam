

# This script is intended to move pictures so we can manually go through them and verify the accuracy of our model


# Run 02c_data-clean_2016.R up until line 113

# 34 sites
length(unique(all_t$site))

## Verify 50 random images per site
#' 8 images with adults at night
#' 8 images with adults during the day
#' 8 images without adults at night
#' 8 images without adults during the day
#' 5 images with nestlings at night
#' 5 images with nestlings during the day
#' 5 images with bands if present at night
#' 5 images with bands if present during the day
#' 5 images with eggs during the day
#' 5 images with eggs during the night

# discretize time so that we sample evenly in day and night
all_t <- all_t %>% mutate(day_night = case_when(
                          hour > 6 & hour < 20 ~ "day",
                          hour > 0 & hour < 4 ~ "night",
                          TRUE ~ "NA"
                        ))

# sample adults -----------------------------------------------------------

adult_verify <- all_t %>%
  mutate(adults_01 = ifelse(adult > 0, 1, 0)) %>%
  filter(day_night %in% c("day", "night")) %>%
  group_by(site, adults_01, day_night) %>% 
  slice_sample(., n = 8) %>%
  select(-adults_01)



# sample nestlings --------------------------------------------------------

nestling_verify <- all_t %>%
  mutate(nestling_01 = ifelse(nestling > 0, 1, 0)) %>%
  filter(day_night %in% c("day", "night")) %>%
  group_by(site, nestling_01, day_night) %>% 
  slice_sample(., n = 5) %>% ungroup() %>%
  select(-nestling_01)


# sample bands ------------------------------------------------------------

bbands_verify <- all_t %>%
  mutate(bband_01 = ifelse(bband > 0, 1, 0)) %>%
  filter(day_night %in% c("day", "night")) %>%
  group_by(site, bband_01, day_night) %>% 
  slice_sample(., n = 5) %>% ungroup() %>%
  select(-bband_01)

sbands_verify <- all_t %>%
  mutate(sband_01 = ifelse(bband > 0, 1, 0)) %>%
  filter(day_night %in% c("day", "night")) %>%
  group_by(site, sband_01, day_night) %>% 
  slice_sample(., n = 5) %>% ungroup() %>%
  select(-sband_01) 


# sample eggs -------------------------------------------------------------
eggs_verify <- all_t %>%
  mutate(eggs_01 = ifelse(eggs > 0, 1, 0)) %>%
  filter(day_night %in% c("day", "night")) %>%
  group_by(site, eggs_01, day_night) %>% 
  slice_sample(., n = 5) %>% ungroup() %>%
  select(-eggs_01)

fs::dir_create('data/2016_verify/')
write.csv(eggs_verify, "data/2016_verify/verification_images_eggs.csv")
x <- read.csv("data/2016_verify/verification_images.csv")



x$image <- str_replace(x$Image_name, "")
x %>% filter(str_detect(Image_name, "IMG_0366 (6).JPG"))

x <- x %>% filter(site == 78)

x$image <- str_replace(x$Image_name, "/Volumes/Seagate Backup Plus Drive/Reconyx_2016/Rankin 2016/Site 78/Post-hatch/", "")
x %>% filter(str_detect(x$image, "0366"))

# combine verification data sets, and use image paths to copy into folder
verify_2016 <- rbind(adult_verify, nestling_verify, bbands_verify, sbands_verify, eggs_verify)

verify_2016$Image_name <- str_replace(verify_2016$Image_name , "/media/robert/", "/Volumes/")

source <-"/Volumes/Seagate Backup Plus Drive/Reconyx_2016"
dest <- "/Volumes/NUWCRU_DATA/2016/"

file.copy(verify_2016$Image_name, paste0(dest))

x <- list.files("/Volumes/NUWCRU_DATA/2016/images/", pattern = "*.JPG")
length(x)





# Move completed photos ---------------------------------------------------
image_source <- "/Users/erikhedlin/Desktop/2016/images/"
source <- "/Users/erikhedlin/Desktop/2016/"

x <- read.delim(paste0(image_source, "output.txt"), sep = ",", col.names = c("image", "adults", "nestlings", "eggs", "bbands", "sbands"))

completed <- x %>% filter(adults != 'unk')
fs::dir_create(paste0(source, "completed_4"))
head(paste0(source, completed$image))

# copy and remove completed images
file.copy(paste0(image_source, completed$image), paste0(source, "completed_4"))
file.copy(paste0(image_source, "output.txt"), paste0(source, "completed_4"))
file.rename(paste0(source, "completed_4/", "output.txt"), paste0(source, "completed_4/", "output_4.txt"))
file.remove(paste0(image_source, "output.txt"))
file.remove(paste0(image_source, completed$image))




# Check accuracy ----------------------------------------------------------

# Load and clean model results

model_results <- readtext("/Volumes/NUWCRU_DATA/2016_verification_results.txt")

# create line breaks at the word "enter", since this is where the data for each image begins
t <- str_split(toString(model_results), "Enter ")
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

# image name
start <- str_locate(results$path, "verification/")
end <- str_locate(results$path, ".JPG")
images <- str_sub(results$path, start[,1]+13, end[,2])

pefa_results <- data.frame(
  image = images,
  obj1 = as.character(results$V1),
  obj2 = as.character(results$V2),
  obj3 = as.character(results$V3),
  obj4 = as.character(results$V4),
  obj5 = as.character(results$V5),
  obj6 = as.character(results$V6),
  obj7 = as.character(results$V7))

ad <- pefa_results

# sum the occurences of object detections
ad$adult_model <- Reduce(`+`, lapply(ad[-1], 
                               function(x) lengths(str_extract_all(x, "adult"))))
ad$nestling_model <- Reduce(`+`, lapply(ad[-1], 
                                  function(x) lengths(str_extract_all(x, "nestling"))))
ad$eggs_model <- Reduce(`+`, lapply(ad[-1], 
                              function(x) lengths(str_extract_all(x, "eggs"))))
ad$bband_model <- Reduce(`+`, lapply(ad[-1], 
                               function(x) lengths(str_extract_all(x, "bband"))))
ad$sband_model <- Reduce(`+`, lapply(ad[-1], 
                               function(x) lengths(str_extract_all(x, "sband"))))
dat <- ad %>% select(image, adult_model:sband_model)


model_results <-dat[-nrow(dat),] # empty last row




# Manual results
files <- list.files(source, recursive = TRUE, pattern = "*.txt", full.names = TRUE)
manual_results <- do.call(rbind, lapply(files, function(x) read.csv(x, sep = ",", col.names = c("image", "adults", "nestlings", "eggs", "bbands", "sbands"))))

join <- manual_results %>% left_join(model_results, by = "image") %>%
  filter(adults != "unk")


str(join)

diff <- join %>%
  mutate(adults_diff = as.numeric(adults) - adult_model,
         nestlings_diff = as.numeric(nestlings) - nestling_model,
         eggs_diff = as.numeric(eggs) - eggs_model,
         bbands_diff = as.numeric(bbands) - bband_model,
         sbands_diff = as.numeric(sbands) - sband_model) %>%
  select(image, contains("diff"))

# 0 = correct prediction
# < 0 = false positive - model predicted an object when it wasn't there
# > 0 = false negative - model missed an object
head(diff)


# Dump mistaken pictures into another folder

# adults
names(diff)
mistakes <- diff %>% filter(adults_diff != 0 | 
                                    nestlings_diff != 0 |
                                    eggs_diff != 0 |
                                    bbands_diff != 0 |
                                    sbands_diff != 0)
mistaken_files <- list.files(paste0(source), recursive = TRUE, full.names = TRUE, pattern = "*.JPG")
fs::dir_create(paste0(source), "/mistakes/")
fs::file_copy(mistaken_files, paste0(source, "/mistakes/"))
image_labels <- model_results %>% filter(image %in% mistakes[,"image"])

file.rename(paste0(source, "mistakes/",image_labels[,1]), 
            paste0(source, "mistakes/", "adult_", image_labels[,"adult_model"], "-",
                   "nestlings_", image_labels[,"nestling_model"], "-",
                   "eggs_", image_labels[,"eggs_model"], "-",
                   "bbands_", image_labels[,"bband_model"], "-",
                   "sbands_", image_labels[,"sband_model"], ".JPG"))

names(image_labels)
# overall accuracy for adults
accuracy <- data.frame(adults = nrow(diff[which(diff$adults_diff == 0),]) / nrow(diff),
                       nestlings = nrow(diff[which(diff$nestlings_diff == 0),]) / nrow(diff),
                       eggs = nrow(diff[which(diff$eggs_diff == 0),]) / nrow(diff),
                       bbands = nrow(diff[which(diff$bbands_diff == 0),]) / nrow(diff),
                       sbands = nrow(diff[which(diff$sbands_diff == 0),]) / nrow(diff))
accuracy
nrow(diff)

# false postivies/negatives
x <- diff %>%
  mutate(adults_fp = ifelse(as.numeric(adults_diff) < 0, 1, 0),
         adults_fn = ifelse(as.numeric(adults_diff) > 0, 1, 0),
         eggs_fp = ifelse(as.numeric(eggs_diff) < 0, 1, 0),
         eggs_fn = ifelse(as.numeric(eggs_diff) > 0, 1, 0))

y <- data.frame(adults_fp = sum(x$adults_fp, na.rm = TRUE) / nrow(x),
           adults_fn = sum(x$adults_fn, na.rm = TRUE) / nrow(x))






