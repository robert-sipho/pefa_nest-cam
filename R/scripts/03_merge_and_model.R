library(tidyverse)
library(lubridate)
library(nuwcru)
library(R2jags)
library(brms)
library(cmdstanr)
install_cmdstan()
set_cmdstan_path()



# Load and prepare data ---------------------------------------------------


dat <- do.call(rbind, 
               lapply(c(#"data/02_2013_prop.csv","data/02_2014_prop.csv",
                 "data/02_2015_prop.csv", "data/02_2016_prop.csv","data/02_2017_prop.csv"), read_csv)) %>% 
  select(-X1, -trt) %>% mutate(yday = yday(date))



chicks <- read_csv("data/03a_chick_weights.csv") %>% select(-X1)
trt <- read_csv("data/00_trt.csv") %>% 
  rename("site" = "nest")


# broodsize by n, and by weight
brood_size <- chicks %>% 
                mutate(yday = yday(date)) %>%
                group_by(site, year, yday) %>% 
                summarize(b_size = sum(alive, na.rm = TRUE),
                          b_weight = sum(m_weight, na.rm = TRUE),
                          b_weight_var = sqrt(sum(sd_weight^2))) %>%
  mutate(plot_date = as.Date(yday, origin = "2015-01-01"))

# merge with proportion
dat <- dat %>% left_join(brood_size, by = c("site", "year", "yday"))
dat <- dat %>% 
  left_join(trt, by = c("site", "year")) %>% 
  filter(!is.na(b_size)) %>%
  mutate(date = as.Date(yday, origin = paste0(year, "-01-01")))


x <- dat %>%
  group_by(yday, plot_date, year, treatment) %>%
  summarize(pop_weight = sum(b_weight)) 

  
ggplot() +
  geom_line(data = filter(x, treatment == 0), aes(x = plot_date, y = pop_weight), colour = nuwcru::grey3) +
  geom_line(data = filter(x, treatment == 1), aes(x = plot_date, y = pop_weight), colour = nuwcru::red2) +
  facet_wrap(~year, ncol = 1) +
  theme_nuwcru() + facet_nuwcru()


# Visual ------------------------------------------------------------------

labels = tibble(
  state = c(0,1),
  year = c(2016,2016),
  text = c("no rain", "rain"),
  trt = c("control", "control")
)

t <- dat %>% filter(!is.na(precip)) %>%
  mutate(trt = as.factor(ifelse(treatment == 0, "control", "supplemented"))) 
t  %>%
  ggplot() +
  geom_point(dat = filter(dat, precip > 0 & treatment == 1), aes(x = b_weight, y = prop, size = precip+0.2), colour = nuwcru::blue4) +
  geom_point(dat = filter(dat, precip == 0 & treatment == 1), aes(x = b_weight, y = prop, size = precip+0.2)) +
  geom_text(data = filter(labels, state == 1), aes(x = 2500, y = 0.88, label = "rainy day"),  colour = nuwcru::blue4, size = 5) +
  scale_x_continuous(limits = c(0,2800)) +
  xlab("brood weight") + ylab("proportion of daily attendance") +
  theme_nuwcru() + facet_nuwcru() + theme(legend.position = "None", axis.text.x = element_text(hjust = 0.5, angle = 0))
t %>% select(treatment, trt)

t %>%
  filter(trt == "supplemented")

source <- "/Volumes/NUWCRU_DATA/camera images download/annotation/moose/main/input/"
length(list.files(source, pattern = "*.jpg"))


# functions ---------------------------------------------------------------

b <- c(2, 0.75)
x <- seq(1,20,1)
y <- rnorm(100, mean = b[1] * exp(b[2] * x))
dat1 <- data.frame(x, y)
plot(x,y)

-a*exp(-b*exp(-c * x))


a <- 2.87+1.17    # inflection
b <- 0.05 # decay rate
x <- seq(min(t$b_weight)/30, max(t$b_weight)/30, 1)

y <- 1 + -1*exp(-a*exp(-b * x))
plot((t$b_weight/30),t$prop, pch = 16, col = grey7)

lines(x, y, col = red2)




# model_fit ---------------------------------------------------------------


m <- brm(
  bf(prop ~ 1 + -1*exp(-a*exp(-b * (b_weight/30))), 
               a ~ 1 + treatment*precip + (),
               b ~ 1,
               nl = TRUE),
  data = t, 
  prior = c(
    prior(normal(5,3), nlpar = "a")#,
    #prior(normal(0.1,3), nlpar = "b")
    ),
  warmup = 5000, iter = 10000, chains = 2, cores = 4,
  control = list(adapt_delta = 0.95)
  )








