library(tidyverse)
library(lubridate)
library(nuwcru)
library(R2jags)
library(brms)
library(cmdstanr)

# install_cmdstan()
set_cmdstan_path()

getmode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

# Load and prepare data ---------------------------------------------------


dat <- do.call(rbind, 
               lapply(c(#"data/02_2013_prop.csv","data/02_2014_prop.csv",
                 "data/02_2015_prop.csv", "data/02_2016_prop.csv","data/02_2017_prop.csv"), read_csv)) %>% 
  select(-X1, -trt) %>% mutate(yday = yday(date))

test <- read_csv("data/02_2016_prop.csv")

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
dat_new <- dat %>% 
  left_join(brood_size, by = c("site", "year", "yday"))

x <- dat_new %>% 
  left_join(trt, by = c("site", "year")) %>% 
  filter(!is.na(b_size)) %>%
  mutate(date = as.Date(yday, origin = paste0(year, "-01-01")))


x_new <- x %>%
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



# functions ---------------------------------------------------------------

# model_fit ---------------------------------------------------------------

  # too few observations at brood size = 5
  # calculate brood age
t <- x %>% 
    filter(!is.na(precip)) %>%
    filter(b_size != 5) %>%
    filter(b_weight > 0) %>%
    mutate(nest_year = paste0(site, "_", year)) %>%
    group_by(nest_year) %>%
    mutate(b_age = as.numeric(date - min(date) + 1)) %>%
    mutate(precip_disc = case_when(
      precip == 0 ~ "0",
      precip > 0 & precip < 6  ~ "1_5",
      precip > 5 & precip < 11 ~ "6_10",
      precip > 10 ~ ">10")) %>%
  mutate(treatment = as.factor(treatment))



  
m <- brm(
  bf(prop ~ 1 + -1*exp(-a*exp(-b * (b_age))), 
               a ~ 1 + treatment + precip_disc + treatment:precip_disc + (1|b_size),
               b ~ 1 + treatment + precip_disc + treatment:precip_disc + (1|b_size),
               nl = TRUE),
  data = t, 
  prior = c(
    prior(normal(5,1.5), nlpar = "a"),
    prior(normal(0.1,0.5), nlpar = "b")
    ),
  warmup = 1500, iter = 5000, chains = 4, cores = 4,
  control = list(adapt_delta = 0.95),
  backend = "cmdstanr",
  threads = threading(8))


  
pred <- t %>% 
  expand(b_age = b_age, treatment = treatment, precip_disc = precip_disc, b_size = b_size) %>%
    tidybayes::add_fitted_draws(m, n = 100) %>%
    group_by(b_age, treatment, precip_disc, b_size) %>%
    summarize(mode = getmode(.value),
              sd = sd(.value)) %>%
    mutate(upper95 = mode + (sd*1.96),
           upper80 = mode + (sd*1.282),
           upper50 = mode + (sd*0.674),
           lower95 = mode - (sd*1.96),
           lower80 = mode - (sd*1.282),
           lower50 = mode - (sd*0.674)) %>%
  mutate(upper95 = ifelse(upper95 > 1, 1, upper95),
         upper80 = ifelse(upper80 > 1, 1, upper80),
         upper50 = ifelse(upper50 > 1, 1, upper50))


unique(post$precip_disc)
# subset and create labels for plotting
precip <- ">10"
precip_label <- " = 6 - 10mm"
clutch <- 4

control <- filter(post, treatment == 0 & 
                        precip_disc %in% precip & 
                        b_size %in% clutch)
supplemented <- filter(post, treatment == 1 & 
                             precip_disc %in% precip & 
                             b_size %in% clutch)

raw_supplemented <- filter(t, treatment == 1 & 
                             precip_disc %in% precip & 
                             b_size %in% clutch)
raw_control <- filter(t, treatment == 0 & 
                        precip_disc %in% precip & 
                        b_size %in% clutch)
# plot
  ggplot() +
    
    geom_point(data = raw_control,
               aes(x = b_age, y = prop), colour = signal_blue8) +
    geom_point(data = raw_supplemented,
               aes(x = b_age, y = prop), colour = signal_yellow6) +
    # control
    geom_segment(data = control, 
                aes(x = b_age+0.2, xend = b_age+0.2, y = lower95, yend = upper95), colour = signal_blue6, alpha = 0.2) + 
    geom_segment(data = control, 
                aes(x = b_age+0.2, xend = b_age+0.2, y = lower80, yend = upper80), colour = signal_blue6, alpha = 0.2, size = 1.5) + 
    geom_segment(data = control, 
                aes(x = b_age+0.2, xend = b_age+0.2, y = lower50, yend = upper50), colour = signal_blue8, alpha = 1, size = 2) + 
    geom_point(data = control,
              aes(x = b_age+0.2, y = mode), colour = signal_blue1, size = 1) +
    
    # supplemented attendance
    geom_segment(data = supplemented, 
                 aes(x = b_age, xend = b_age, y = lower95, yend = upper95), colour = signal_yellow6, alpha = 0.2) + 
    geom_segment(data = supplemented, 
                 aes(x = b_age, xend = b_age, y = lower80, yend = upper80), colour = signal_yellow6, alpha = 0.2, size = 1.5) + 
    geom_segment(data = supplemented, 
                 aes(x = b_age, xend = b_age, y = lower50, yend = upper50), colour = signal_yellow8, alpha = 1, size = 2) + 
    geom_point(data = supplemented,
              aes(x = b_age, y = mode), colour = signal_yellow1, size = 1) +
    geom_text(aes(x = 23, y = 0.95), 
              label = paste0("clutch size = ",clutch),
              colour = signal_blue7,
              family = "Times New Roman",
              size = 4,
              hjust = 0) +
    geom_text(aes(x = 23, y = 0.9), 
              label = paste0("rain",precip_label),
              colour = signal_blue7,
              family = "Times New Roman",
              size = 4,
              hjust = 0) +
  xlab("brood age") + ylab("daily nest attendance (proportion)") +
    scale_x_continuous(limits = c(0,35)) + scale_y_continuous(limits = c(0,1)) +
    hedlin_theme() + 
    theme(axis.title.x = element_text(size = 12, colour = signal_blue5),
          axis.title.y = element_text(size = 12, colour = signal_blue5))
  

# nestling mortality rates ----
  
  d <- read_csv("data/03_daily_survival.csv") %>% 
    select(-X1) 
  
  test <- d %>% expand(ind = ind, age = seq(1,30,1)) %>% 
    left_join(select(d, ind, age, alive), by = c("ind", "age")) %>%
    mutate(alive = ifelse(is.na(alive), 0,alive))
  
  View(test %>% filter(ind %in% unique(test$ind)[3]))

  
  library(rstan)
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  set.seed(123)
  
  ## Read data
  ## The data generation code is in bpa-code.txt, available at
  ## http://www.vogelwarte.ch/de/projekte/publikationen/bpa/complete-code-and-data-files-of-the-book.html
  stan_data <- list(
    y = 
      nind = 
      n_occasions = 
      x = 
      max_age = 
  )
  
  ## Parameters monitored
  params <- c("beta")
  
  ## MCMC settings
  ni <- 2000
  nt <- 1
  nb <- 1000
  nc <- 4
  
  ## Initial values
  inits <- function() list(beta = runif(2, 0, 1))
  
  ## Call Stan from R
  cjs_age  <- stan("cjs_age.stan",
                   data = stan_data, init = inits, pars = params,
                   chains = nc, iter = ni, warmup = nb, thin = nt,
                   seed = 1,
                   open_progress = FALSE)
  
  ## Summarize posteriors
  print(cjs_age, digits = 3)
  