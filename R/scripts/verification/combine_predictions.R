library(tidyverse)
source("scripts/utils.R")


## *manual class ----------------------------------------------------------

source <- "/Volumes/GoogleDrive/My Drive/NuWCRU/Analysis/emhedlin/nuwcru/cam/pefa_nest-cam/python/verification_gui/images"

results <- list.files(source, pattern = "*.txt", full.names = TRUE, recursive = FALSE)

list <- list()
for (i in 1:length(results)){
  list[[i]] <- read_verif(results[i])
}
df <- bind_rows(list)


# run function ------------------------------------------------------------


source <- "/Volumes/NUWCRU_DATA/2016_verification_1k.txt"
weights_1k <- convert_yolo_pefa(source = source, weights = "_1k")
source <- "/Volumes/NUWCRU_DATA/2016_verification_4k.txt"
weights_4k <- convert_yolo_pefa(source = source, weights = "_4k")
source <- "/Volumes/NUWCRU_DATA/2016_verification_5k.txt"
weights_5k <- convert_yolo_pefa(source = source, weights = "_5k")
source <- "/Volumes/NUWCRU_DATA/2016_verification_8k.txt"
weights_8k <- convert_yolo_pefa(source = source, weights = "_8k")
source <- "/Volumes/NUWCRU_DATA/2016_verification_10000.txt"
weights_10k <- convert_yolo_pefa(source = source, weights = "_10k")
source <- "/Volumes/NUWCRU_DATA/2016_verification_final.txt"
weights_final <- convert_yolo_pefa(source = source, weights = "_final")

dat <- df %>% 
  left_join(weights_1k, by = "image") %>%
  left_join(weights_4k, by = "image") %>%
  left_join(weights_5k, by = "image") %>%
  left_join(weights_8k, by = "image") %>%
  left_join(weights_10k, by = "image") %>%
  left_join(weights_final, by = "image") %>%
  pivot_longer(cols = adult_human:sband_final) %>%
  mutate(weights = str_sub(name, str_locate(name,"_")[,2]+1),
         class = str_sub(name, 0,str_locate(name,"_")[,2]-1)) %>%
  select(-name)

dat %>% filter(weights == "1k")

dat

# Model -------------------------------------------------------------------

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
