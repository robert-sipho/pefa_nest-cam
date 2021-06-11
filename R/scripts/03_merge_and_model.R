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
dim(t)


t <- x %>% filter(!is.na(precip)) %>%
  mutate(id = paste0(date,site)) %>%
  filter(!duplicated(id))




t  %>%
  ggplot() +
  geom_point(dat = filter(dat, precip > 0 & treatment == 1), aes(x = b_weight, y = prop, size = precip+0.2), colour = nuwcru::blue4) +
  geom_point(dat = filter(dat, precip == 0 & treatment == 1), aes(x = b_weight, y = prop, size = precip+0.2)) +
  geom_text(data = filter(labels, state == 1), aes(x = 2500, y = 0.88, label = "rainy day"),  colour = nuwcru::blue4, size = 5) +
  scale_x_continuous(limits = c(0,2800)) +
  xlab("brood weight") + ylab("proportion of daily attendance") +
  theme_nuwcru() + facet_nuwcru() + theme(legend.position = "None", axis.text.x = element_text(hjust = 0.5, angle = 0))
t %>% select(treatment, trt)


# functions ---------------------------------------------------------------

b <- c(2, 0.75)
x <- seq(1,20,1)
y <- rnorm(100, mean = b[1] * exp(b[2] * x))
dat1 <- data.frame(x, y)
plot(x,y)

-a*exp(-b*exp(-c * x))




inv_logit <- function(x) 1 / (1 + exp(-x))


asymlogit <- 5
mid <- 5
scale <- 0.02

x <- seq(min(t$b_weight)/30, max(t$b_weight)/30, 1)
y <- -1 * inv_logit(asymlogit) * 1/(1 + exp((mid - x) * exp(scale)))

plot(x,y)

model_formula <- bf(
  # Logistic curve
  prop ~ inv_logit(asymlogit) * 1/(1 + exp((mid - age) * exp(scale))),
  # Each term in the logistic equation gets a linear model
  asymlogit ~ 1,
  mid ~ 1,
  scale ~ 1,
  # Precision
  phi ~ 1,
  # This is a nonlinear Beta regression model
  nl = TRUE, 
  family = Beta(link = identity)
)

prior_fixef <- c(
  # Point of steepest growth is age 4 plus/minus 2 years
  prior(normal(48, 12), nlpar = "mid", coef = "Intercept"),
  prior(normal(1.25, .75), nlpar = "asymlogit", coef = "Intercept"),
  prior(normal(-2, 1), nlpar = "scale", coef = "Intercept")
)

prior_phi <- c(
  prior(normal(2, 1), dpar = "phi", class = "Intercept")
)











a <- 4.75   # inflection
b <- 0.16 # decay rate
x <- seq(min(t$b_weight)/30, max(t$b_weight)/30, 1)
y <- 1 + -1*exp(-a*exp(-b * x))

plot((t$b_weight/30),t$prop, pch = 16, col = grey7)
lines(x, y, col = grey2, lwd = 2)

a <- 4.75+0.59   # inflection
b <- 0.16 # decay rate
x <- seq(min(t$b_weight)/30, max(t$b_weight)/30, 1)
y <- 1 + -1*exp(-a*exp(-b * x))
lines(x, y, col = grey3, lwd = 2)

a <- 4.57   # inflection
b <- 0.16-0.11 # decay rate
x <- seq(min(t$b_weight)/30, max(t$b_weight)/30, 1)
y <- 1 + -1*exp(-a*exp(-b * x))
lines(x, y, col = grey4, lwd = 2)

a <- 2.87+1.29+0.47   # inflection
b <- 0.05 # decay rate
x <- seq(min(t$b_weight)/30, max(t$b_weight)/30, 1)
y <- 1 + -1*exp(-a*exp(-b * x))
lines(x, y, col = blue2, lwd = 2)


x %>% 
  mutate(id = paste(year, site, yday, sep = "_")) %>%
  filter(id == "2016_59_219")
  filter(year == 2016 & site == 59 & yday > 218) %>%
  select(year, site, yday, id)

# model_fit ---------------------------------------------------------------

  # too few observations at brood size = 5
  # calculate brood age
t <- t %>% 
    filter(b_size != 5) %>%
    filter(b_weight > 0) %>%
    mutate(nest_year = paste0(site, "_", year)) %>%
    group_by(nest_year) %>%
    mutate(b_age = as.numeric(date - min(date) + 1)) %>%
    mutate(precip_disc = case_when(
      precip < 6 ~ "0_5",
      precip > 5 & precip < 11 ~ "6_10",
      precip > 10 ~ "<10"
    ))

  
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

print(brmsformula(m))

  
post <- t %>% expand(b_age = b_age, treatment = treatment, precip_disc = precip_disc, b_size = b_size) %>%
    tidybayes::add_fitted_draws(m, n = 100) %>%
    group_by(b_age, treatment, precip_disc, b_size) %>%
    summarize(mode = getmode(.value),
              sd = sd(.value)) %>%
    mutate(upper95 = mode + (sd*1.96),
           upper80 = mode + (sd*1.282),
           upper50 = mode + (sd*0.674),
           lower95 = mode - (sd*1.96),
           lower80 = mode - (sd*1.282),
           lower50 = mode - (sd*0.674))



precip <- "<10"
precip_label <- "10mm"
clutch <- 1

control <- filter(post, treatment == 0 & 
                        precip_disc %in% precip & 
                        b_size %in% clutch)
supplemented <- filter(post, treatment == 1 & 
                             precip_disc %in% precip & 
                             b_size %in% clutch)

  ggplot() +
    
    # control
    geom_ribbon(data = control, 
                aes(x = b_age, ymin = lower95, ymax = upper95), fill = signal_blue6, alpha = 0.2) + 
    geom_ribbon(data = control, 
                aes(x = b_age, ymin = lower80, ymax = upper80), fill = signal_blue6, alpha = 0.2) + 
    geom_ribbon(data = control, 
                aes(x = b_age, ymin = lower50, ymax = upper50), fill = signal_blue6, alpha = 0.2) + 
    geom_line(data = control,
              aes(x = b_age, y = mode), colour = signal_blue1) +
    
    # supplemented attendance
    geom_ribbon(data = supplemented, 
                aes(x = b_age, ymin = lower95, ymax = upper95), fill = signal_yellow6, alpha = 0.2) + 
    geom_ribbon(data = supplemented, 
                aes(x = b_age, ymin = lower80, ymax = upper80), fill = signal_yellow6, alpha = 0.2) + 
    geom_ribbon(data = supplemented, 
                aes(x = b_age, ymin = lower50, ymax = upper50), fill = signal_yellow6, alpha = 0.2) + 
    geom_line(data = supplemented,
              aes(x = b_age, y = mode), colour = signal_yellow1) +
    geom_text(aes(x = 23, y = 0.95), 
              label = paste0("clutch size = ",clutch),
              colour = signal_blue7,
              family = "Times New Roman",
              size = 4,
              hjust = 0) +
    geom_text(aes(x = 23, y = 0.9), 
              label = paste0("rain > ",precip_label),
              colour = signal_blue7,
              family = "Times New Roman",
              size = 4,
              hjust = 0) +
  xlab("brood age") + ylab("daily nest attendance (proportion)") +
    scale_x_continuous(limits = c(0,35)) + scale_y_continuous(limits = c(0,1)) +
    hedlin_theme() + 
    theme(axis.title.x = element_text(size = 12, colour = signal_blue5),
          axis.title.y = element_text(size = 12, colour = signal_blue5))
  
