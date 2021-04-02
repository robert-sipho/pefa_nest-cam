library(tidyverse)
library(lubridate)

# Script is to calculate chickage and weight, and merge with other data

broods <- read_csv("data/03_daily_survival.csv") %>% select(-X1)



growth <- read_csv("data/03_growth.csv") 


# calculate mean weight for all ages
mean_weight <- growth %>% 
  filter(treatment == 0) %>%
  group_by(age) %>%
  summarize(m_weight = mean(weight, na.rm = TRUE),
            sd_weight = sd(weight, na.rm = TRUE)) %>%
  mutate(age = age + 1)


nestling_weights <- broods %>% left_join(mean_weight, by = "age")


b_size <- nestling_weights %>%
  group_by(site, date) %>%
  summarize(brood = sum(alive),
            brood_weight = sum(m_weight),
            var = sqrt(sum(sd_weight^2)))




write.csv(nestling_weights, "data/03a_chick_weights.csv")
write.csv(b_size, "data/03b_brood_weights.csv")


varsens <- read_csv("data/00_varsens.csv") %>%
  select(site, date, chicks)
View(varsens %>%
  filter(site == 1 & year == 2017))
b_size %>%
  filter(year(date) == 2017 & site == 28)

x <- b_size %>% left_join(varsens, by = c("site", "date"))



b_size %>%
  filter(year(date) == 2017) %>%
  ggplot() +
  geom_line(aes(x = date, y = brood_weight, group = site)) +
  nuwcru::theme_nuwcru()