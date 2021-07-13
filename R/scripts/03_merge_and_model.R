library(tidyverse)
library(lubridate)
library(nuwcru)
library(cmdstanr)

# install_cmdstan()
set_cmdstan_path()

getmode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

# Load and prepare data ---------------------------------------------------


d_surv <- read_csv("data/03_daily_survival.csv") %>% 
  select(-X1) 

# merge weather in with survival data
dat <- do.call(rbind, 
               lapply(c(#"data/02_2013_prop.csv","data/02_2014_prop.csv",
                 "data/02_2015_prop.csv", "data/02_2016_prop.csv","data/02_2017_prop.csv"), read_csv)) %>% 
  dplyr::select(-X1, -trt) %>%
  mutate(yday = yday(date))

dat <- d_surv %>% 
  select(-year) %>%
  right_join(dat, by = c("date", "site")) %>%
  mutate(unique_id = paste(date, ind, sep = "_")) %>%
  filter(!duplicated(unique_id)) %>%
  select(-unique_id)

# merge in in weather station data, and replace ECCC NA's with weather station data
dat <- do.call(rbind, 
                      lapply(c(#"data/02_2013_prop.csv","data/02_2014_prop.csv",
                        "data/00_weather_station_2015.csv", 
                        "data/00_weather_station_2016.csv",
                        "data/00_weather_station_2017.csv"), read_csv)) %>%
  group_by(DATE) %>%
  summarize(daily_rain_ws = sum(RAINF)) %>%
  rename("date" = "DATE") %>%
  right_join(dat, by = "date") %>%
  mutate(rainfall = ifelse(is.na(precip), daily_rain_ws, precip))





chicks <- read_csv("data/03a_chick_weights.csv") %>% select(-X1)
trt <- read_csv("data/00_trt.csv") %>% 
  rename("site" = "nest")


# broodsize by n, and by weight
dat <- chicks %>% 
                mutate(yday = yday(date)) %>%
                group_by(site, year, yday) %>% 
                summarize(b_size = sum(alive, na.rm = TRUE),
                          b_weight = sum(m_weight, na.rm = TRUE),
                          b_weight_var = sqrt(sum(sd_weight^2))) %>%
  mutate(plot_date = as.Date(yday, origin = "2015-01-01")) %>%
    # merge with nest attendance data
  right_join(dat, by = c("site", "year", "yday")) %>%
    # join in treatment allocations
  left_join(trt, by = c("site", "year")) %>%
  filter(!is.na(b_size)) 








# population weight by day/year
x_new <- dat %>%
  group_by(yday, plot_date, year, treatment) %>%
  summarize(pop_weight = sum(b_weight)) 

  
ggplot() +
  geom_line(data = filter(x_new, treatment == 0), aes(x = plot_date, y = pop_weight), colour = nuwcru::grey3) +
  geom_line(data = filter(x_new, treatment == 1), aes(x = plot_date, y = pop_weight), colour = nuwcru::red2) +
  facet_wrap(~year, ncol = 1) +
  theme_nuwcru() + facet_nuwcru()


# Visual ------------------------------------------------------------------

labels = tibble(
  state = c(0,1),
  year = c(2016,2016),
  text = c("no rain", "rain"),
  trt = c("control", "control")
)



# model_fit ---------------------------------------------------------------

  # too few observations at brood size = 5
  # calculate brood age
nonlin_dat <- dat %>% 
    # filter(!is.na(precip)) %>%
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
  data = nonlin_dat, 
  prior = c(
    prior(normal(5,1.5), nlpar = "a"),
    prior(normal(0.1,0.5), nlpar = "b")
    ),
  warmup = 1500, iter = 5000, chains = 4, cores = 4,
  control = list(adapt_delta = 0.95),
  backend = "cmdstanr",
  threads = threading(8))


  
pred <- nonlin_dat %>% 
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

raw_supplemented <- filter(nonlin_dat, treatment == 1 & 
                             precip_disc %in% precip & 
                             b_size %in% clutch)
raw_control <- filter(nonlin_dat, treatment == 0 & 
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
  


# Known Fate Survival -----------------------------------------------------
  # https://tobiasroth.github.io/BDAEcology/dailynestsurv.html#check-convergence
  
  library(cmdstanr)
  set_cmdstan_path()
  
  obs_hist <- dat %>% 
    expand(ind = ind, age = seq(1,30,1)) %>% 
    left_join(select(d, ind, age, alive), by = c("ind", "age")) %>%
    mutate(alive = ifelse(is.na(alive), 0,alive))
  
  y <- obs_hist %>%
    pivot_wider(names_from = age, values_from = alive)
  
  # Observation matrix
  y_mat <- as.matrix(y[,2:ncol(y)])
  dim(y_mat)
  
year <- obs_hist %>%
  filter(!duplicated(ind)) %>%
  mutate(year = as.numeric(str_sub(ind, -2)) - 12) %>%
  pull(year)


last <- rowSums(y_mat)
last <- ifelse(last < 30, last+1, last)

  stan_data <- list(
    y      = as.matrix(y[,2:ncol(y)]),
    n_ind  = nrow(y),
    n_years = 5,
    year = year,
    last   = last,
    first  = rep(1, nrow(y)),
    age    = 1:30,
    max_age = 30)
  
  
  ## Parameters monitored
  params <- c("b0", "b_age")
  

  ## Initial values
  inits <- function() list(b0 = runif(1, 0, 1),
                           b_age = runif(1, 0, 1))
  
  
  file <- file.path("models","known_fate.stan")
  mod <- cmdstan_model(file)
  mod_recompile <- mod$compile(force_recompile = TRUE, cpp_options = list(stan_threads = TRUE))
  
  fit_mcmc2 <- mod_recompile$sample(
    data = stan_data,
    seed = 123,
    chains = 2,
    iter_warmup = 500,
    iter_sampling = 1000,
    threads_per_chain = 8,
    thin = 1,
    parallel_chains = 2,
    adapt_delta = 0.99,
    max_treedepth = 11)
  
  draws <- fit_mcmc2$draws() %>% 
    as_draws_df()
  
  
  

# known-fate proportion ---------------------------------------------------

  # some missing rain data in the ECCC historicals, so work with our weather stations
  
  
  
  ws_weather <- do.call(rbind, 
                 lapply(c(#"data/02_2013_prop.csv","data/02_2014_prop.csv",
                   "data/00_weather_station_2015.csv", 
                   "data/00_weather_station_2016.csv",
                   "data/00_weather_station_2017.csv"), read_csv))  
  ws_rain <- ws_weather %>% 
    group_by(DATE) %>%
    summarize(daily_rain_ws = sum(RAINF)) %>%
    rename("date" = "DATE")
  
  combined <- d_weather %>%
    left_join(ws_rain, by = "date") %>%
    mutate(rainfall = ifelse(is.na(precip), daily_rain_ws, precip))
  
  combined[which(combined$rainfall == max(combined$rainfall)),]
  
  ws_weather %>% filter(DATE == as.Date("2016-07-16"))
  
  
  # merge weather in with survival data
  d_weather <- d %>% 
    select(-year) %>%
    right_join(ws_rain, by = "date") %>%
    mutate(unique_id = paste(date, ind, sep = "_")) %>%
    filter(!duplicated(unique_id)) %>%
    select(-unique_id)
  
  obs_hist <- d_weather %>% 
    expand(ind = ind, age = seq(1,30,1)) %>% 
    left_join(select(d_weather, ind, age, alive), by = c("ind", "age")) %>%
    mutate(alive = ifelse(is.na(alive), 0,alive))
  
  
  y <- obs_hist %>%
    pivot_wider(names_from = age, values_from = alive)
  
  y_mat <- as.matrix(y[,2:ncol(y)])
  
  
  #weather <- 
  d_weather %>% 
    expand(ind = ind, age = seq(1,30,1)) %>% 
    left_join(select(d_weather, ind, age, precip), by = c("ind", "age")) %>%
    pivot_wider(names_from = age, values_from = precip)
  
  
  d_weather %>%
    filter(ind == "100bk16")
  
  