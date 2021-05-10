library(tidyverse)
source("scripts/utils.R")


## *manual class ----------------------------------------------------------

source <- "/Volumes/GoogleDrive/My Drive/NuWCRU/Analysis/emhedlin/nuwcru/cam/pefa_nest-cam/python/verification_gui/images"
dest <- "/Volumes/GoogleDrive/My Drive/NuWCRU/Analysis/emhedlin/nuwcru/cam/pefa_nest-cam/python/verification_gui/images/pooled_images"
results <- list.files(source, pattern = "*.txt", full.names = TRUE, recursive = FALSE)
images_full <- list.files(source, pattern = "*.JPG", full.names = TRUE, recursive = TRUE) 
images <- list.files(source, pattern = "*.JPG", full.names = FALSE, recursive = TRUE) %>%
  str_sub(., str_locate(.,"/")[,2]+1)

file.copy(images_full[images %in% df$image],dest) 

list <- list()
for (i in 1:length(results)){
  list[[i]] <- read_verif(results[i])
}
df <- bind_rows(list)


# run function ------------------------------------------------------------
files <- list.files("/Volumes/GoogleDrive/My Drive/NuWCRU/Analysis/emhedlin/nuwcru/cam/pefa_nest-cam/R/data/2016_verify",
           pattern = "*.txt", full.names = TRUE)

weights_1k <- convert_yolo_pefa(source = files[2], weights = "_k1")
weights_4k <- convert_yolo_pefa(source = files[3], weights = "_k4")
weights_5k <- convert_yolo_pefa(source = files[4], weights = "_k5")
weights_8k <- convert_yolo_pefa(source = files[5], weights = "_k8")
weights_10k <- convert_yolo_pefa(source = files[1], weights = "_k10")

dat <- df %>% 
  left_join(weights_1k, by = "image") %>%
  left_join(weights_4k, by = "image") %>%
  left_join(weights_5k, by = "image") %>%
  left_join(weights_8k, by = "image") %>%
  left_join(weights_10k, by = "image") %>%
  pivot_longer(cols = adult_human:sband_k10) %>%
  mutate(weights = str_sub(name, str_locate(name,"_")[,2]+1),
         class = str_sub(name, 0,str_locate(name,"_")[,2]-1)) %>%
  select(-name)

# nestling detection - binary outcome
nestling_dh <- dat %>%
  filter(class == "nestling") %>%
  select(-class) %>%
  pivot_wider(names_from = weights, values_from = value) %>%
    # binarize
  mutate_at(c("human","k1","k4","k5","k8","k10"), function(x) ifelse(x > 0,1,0))

# nestling count - count outcome
nestling_ch <- dat %>%
  filter(class == "nestling") %>%
  select(-class) %>%
  pivot_wider(names_from = weights, values_from = value) #%>%
    # binarize
  #mutate_at(c("human","k1","k4","k5","k8","k10"), function(x) ifelse(x > 0,1,0))

# determine images where human counts are different than our best model. Use this to hopefuly correct human count to being 100% perfect
nestling <- nestling_ch %>% 
  filter(human != k10) %>%
  pull(image)

nestling_dh %>% 
  filter(human != k10)

# Model -------------------------------------------------------------------

## Obs Conf ----------------------------------------------------------------


source <- "/Volumes/GoogleDrive/My Drive/NuWCRU/Analysis/emhedlin/nuwcru/cam/pefa_nest-cam/python/verification_gui/images"
dest <- "/Volumes/GoogleDrive/My Drive/NuWCRU/Analysis/emhedlin/nuwcru/cam/pefa_nest-cam/python/verification_gui/verification_results/nestling_dh"
files_full <- list.files(source, pattern = "*.JPG", recursive = TRUE, full.names = TRUE)
files <- list.files(source, pattern = "*.JPG", recursive = TRUE) %>%
  str_sub(., str_locate(.,"/")[,2]+1)



file.copy(files_full[files %in% (nestling_dh %>% filter(human != k10) %>% pull(image))], dest)
new_names <- paste0("h_",
                    nestling_dh %>% filter(human != k10) %>% pull(human),
                    "-CNN_",
                    nestling_dh %>% filter(human != k10) %>% pull(k10), "_",
                    nestling_dh %>% filter(human != k10) %>% mutate(row = row_number()) %>% pull(row),
                    ".JPG")
file.rename(
  list.files(dest, pattern = "*.JPG", full.names = TRUE),
  paste0(dest,"/",new_names)
)

expit <- function(x) 1/(1 + exp(-x))

# Likelihood
MultipleMethod <- function(beta,data,occ){
  psi = expit(beta[1])
  p11 = expit(beta[2])
  p10 = expit(beta[3])
  r11 = expit(beta[4])
  y = data[,1]
  w = data[,2]
  Lik = psi * dbinom(y,occ[1],p11) * dbinom(w,occ[2],r11) + (1-psi) * dbinom(y,occ[1],p10) * dbinom(w,occ[2],0)
  nLL = sum( -log(Lik) )
  return(nLL)
}


# starting values
beta.2 <- c(1,0,-1)			# starting values -- set 2 (3 parameters)	



# optimize likelihood
out.ObsConf = optim(beta.2, 
                    ObsConf, 
                    data = data.ObsConf, 
                    occ = occ.ObsConf, 
                    hessian = T) 
