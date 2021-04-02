library(tidyverse)
library(bnstruct)


# Load and prepare data ---------------------------------------------------
dat <- do.call(rbind, 
               lapply(c("data/02_2015_prop.csv", "data/02_2016_prop.csv","data/02_2017_prop.csv","data/02_2014_prop.csv"), read_csv)) %>% 
  select(-X1, -trt) %>% mutate(yday = yday(date))
chicks <- read_csv("data/03a_chick_weights.csv")
trt <- read_csv("data/00_site_treatment_summary.csv") %>% select(-X1)

# broodsize by n, and by weight
brood_size <- chicks %>% 
  group_by(site, year, yday) %>% 
  summarize(b_size = sum(alive, na.rm = TRUE),
            b_weight = sum(m_weight, na.rm = TRUE),
            b_age = max(age, na.rm = TRUE))

# merge with proportion
dat <- dat %>% left_join(brood_size, by = c("site", "year", "yday"))
dat <- dat %>% left_join(trt, by = c("site", "year")) %>% filter(!is.na(b_size))

dat$prop <- round(dat$prop, 1)
dat <- dat %>% filter(b_age < 16)
dat$yday <- dat$yday - min(dat$yday)


BN <- dat %>% select(-date, -wind, -year:-day, -snoGrnd, -site, -trt, -b_size, -b_age, -b_weight, -yday)

dataset <- BNDataset(
  data = BN,
  discreteness = rep(F, ncol(BN)),
  variables = colnames(BN),
  node.sizes = c(50, 50, 50, 50, 10))



net  <- learn.network(dataset)




plot(net, 
    label.scale.equal=T,
    node.width = 12)
