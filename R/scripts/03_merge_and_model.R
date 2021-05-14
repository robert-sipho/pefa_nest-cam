library(tidyverse)
library(lubridate)
library(nuwcru)
library(R2jags)
library(brms)

install_cmdstan()
library(cmdstanr)
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
dat

x <- dat %>%
  group_by(yday, plot_date, year, treatment) %>%
  summarize(pop_weight = sum(b_weight)) 

View(dat %>%
  filter(plot_date > as.Date("2015-08-08")))
  
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
str(dat)
t <- dat %>% filter(!is.na(precip)) %>%
  mutate(trt = as.factor(ifelse(treatment == 0, "control", "supplemented"))) 
t %>% filter(trt == "supplemented") %>%
  ggplot() +
  geom_point(dat = filter(dat, precip > 0), aes(x = b_weight, y = prop, size = precip+0.2), colour = nuwcru::blue4) +
  geom_point(dat = filter(dat, precip == 0), aes(x = b_weight, y = prop, size = precip+0.2)) +
  #geom_text(data = filter(labels, state == 1), aes(x = 2400, y = 0.85, label = "rainy day"),  colour = nuwcru::blue4, size = 5) +
  #facet_grid(trt~.) +
  xlab("brood weight") + ylab("proportion of the day an adult was at the nest") +
  theme_nuwcru() + facet_nuwcru() + theme(legend.position = "None")


t %>%
  filter(trt == "supplemented")


# m1. precip ------------------------------------------------------------------

dat_na <- dat %>% 
  filter(!is.na(precip) & b_size > 0) %>%
  mutate(minutes = prop * 1440+1)

hist(log(dat_na$minutes))


dat_na %>% 
  group_by(site, year) %>% 
  arrange(site, yday) %>%
  mutate(precip_lag1 = )

dat_na$minutes <- dat_na$prop * 1440
hist(dat_na$minutes)

m1 <- brms::brm(
          minutes ~ precip + b_weight + (1 | year) + (1 | site),
          data = dat_na,
          family = Gamma(),
          warmup = 1000, 
          iter = 2500, 
          chains = 2,
          control = list(adapt_delta = 0.99),
          cores = 2)


pp_check(m1)
  
# data for Jags model
model_data = list(N = nrow(dat_na), 
                  y = dat_na$prop, 
                  precip = dat_na$precip)



model_code = '
model
{
  # Likelihood
  for (i in 1:N) {
    y[i] ~ dbeta(a[i], b[i])
    a[i] <- mu[i] * theta
    b[i] <- (1 - mu[i]) * theta
    logit(mu[i]) <- alpha + beta_precip * precip[i] 
  }

  # Priors
  alpha ~ dnorm(0, 10^-2)
  beta_precip ~ dnorm(0, 10^-2)
  theta ~ dunif(0, 10)
}
'

model_parameters =  c("alpha",
                      "beta_precip",
                      "theta")

model_run   <- jags(data    = model_data,
                    inits      = NULL,     
                    parameters = model_parameters,
                    model.file = textConnection(model_code),
                    n.thin     = 10, 
                    n.chains   = 3,
                    n.burnin   = 4000,
                    n.iter     = 5000)

m2 <- update(model_run, n.iter = 10000, n.thin = 10) 

mcmc <- as.mcmc(m2)



# * model diagnostics -------------------------------------------------------

# mixing
mcmc %>%
  window(thin=10) %>% 
  
  # sigma = the yearly residual variance, a = random intercept for site, g = random intercept for yearsite
  tidybayes::gather_draws(beta_precip, theta, alpha) %>%
  
  # change this filter to look at mixing for the parameter of interest. See above line for options
  # filter(.variable == "alpha") %>% 
  ungroup() %>%
  #mutate(term = ifelse(is.na(i), .variable, paste0(.variable,"[",i,"]"))) %>%
  ggplot(aes(x=.iteration, y=.value, color=as.factor(.chain))) +
  scale_color_manual(values=c("#461220", "#b23a48", "#fcb9b2")) +
  geom_line(alpha=0.5) +
  facet_grid(.variable~., scale="free_y") +
  labs(color="chain", x="iteration") +
  theme_nuwcru()


# m2. precip + b_weight ---------------------------------------------------------



dat$prop <- ifelse(dat$prop == 0, dat$prop+0.001, dat$prop)
dat$prop <- ifelse(dat$prop == 1, dat$prop-0.001, dat$prop)
dat_na <- dat %>% filter(!is.na(precip))

dat_na$b_weight
length(scale(dat_na$b_weight))

# data for Jags model
model_data = list(N = nrow(dat_na), 
                  y = dat_na$prop, 
                  precip = as.vector(scale(dat_na$precip)),
                  weight = as.vector(scale(dat_na$b_weight)))



model_code = '
model
{
  # Likelihood
  for (i in 1:N) {
    y[i] ~ dbeta(a[i], b[i])
    a[i] <- mu[i] * theta
    b[i] <- (1 - mu[i]) * theta
    logit(mu[i]) <- alpha + beta_precip * precip[i] + beta_weight * weight[i]
  }

  # Priors
  alpha ~ dnorm(0, 10^-2)
  beta_precip ~ dnorm(0, 10^-2)
  beta_weight ~ dnorm(0, 10^-2)
  theta ~ dunif(0, 10)
}
'

model_parameters =  c("alpha",
                      "beta_precip",
                      "beta_weight",
                      "theta")

model_run   <- jags(data    = model_data,
                    inits      = NULL,     
                    parameters = model_parameters,
                    model.file = textConnection(model_code),
                    n.thin     = 10, 
                    n.chains   = 3,
                    n.burnin   = 4000,
                    n.iter     = 5000)

m2 <- update(model_run, n.iter = 10000, n.thin = 10) 

mcmc <- as.mcmc(m2)



# * model diagnostics -------------------------------------------------------

# mixing
mcmc %>%
  window(thin=10) %>% 
  
  # sigma = the yearly residual variance, a = random intercept for site, g = random intercept for yearsite
  tidybayes::gather_draws(beta_precip, beta_weight, theta, alpha) %>%
  
  # change this filter to look at mixing for the parameter of interest. See above line for options
  # filter(.variable == "alpha") %>% 
  ungroup() %>%
  #mutate(term = ifelse(is.na(i), .variable, paste0(.variable,"[",i,"]"))) %>%
  ggplot(aes(x=.iteration, y=.value, color=as.factor(.chain))) +
  scale_color_manual(values=c("#461220", "#b23a48", "#fcb9b2")) +
  geom_line(alpha=0.5) +
  facet_grid(.variable~., scale="free_y") +
  labs(color="chain", x="iteration") +
  theme_nuwcru()

mcmc %>%
  tidybayes::gather_draws(beta_weight, beta_precip) %>%
  group_by(.variable) %>%
  summarize(mean = mean(.value), sd = sd(.value)) %>%
  mutate(upper95 = mean + (1.96 * sd), lower95 = mean - (1.96 * sd),
         upper80 = mean + (1.282 * sd), lower80 = mean - (1.282 * sd),
         upper50 = mean + (0.674 * sd), lower50 = mean - (0.674 * sd)) %>%
  ggplot() +
  geom_vline(xintercept = 0, colour = grey6, linetype = "dashed") +
  geom_segment(aes(x = lower95, xend = upper95, y = .variable, yend = .variable), colour = "#DFEBF7", size = 2) +
  geom_segment(aes(x = lower80, xend = upper80, y = .variable, yend = .variable), colour = "#A5CADF", size = 2) +
  geom_segment(aes(x = lower50, xend = upper50, y = .variable, yend = .variable), colour = "#4A84BD", size = 2) +
  #geom_point(aes(y = .variable, x = mean), shape = 21, fill = "white", colour = "black", size = 4) +
  scale_x_continuous(limits = c(-1.5,1.5)) +
  #scale_y_continuous(breaks = 1) +
  ylab("") + xlab("") + 
  theme_nuwcru() + 
  theme(panel.border = element_blank(),
        #axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())


# m3. random ------------------------------------------------------------------



dat$prop <- ifelse(dat$prop == 0, dat$prop+0.001, dat$prop)
dat$prop <- ifelse(dat$prop == 1, dat$prop-0.001, dat$prop)
dat_na <- dat %>% filter(!is.na(precip))

dat_na$b_weight
length(scale(dat_na$b_weight))

# data for Jags model
model_data = list(N      = nrow(dat_na), 
                  y      = dat_na$prop, 
                  precip = as.vector(scale(dat_na$precip)),
                  weight = as.vector(scale(dat_na$b_weight)),
                  year   = as.factor(dat_na$year),
                  nyears = length(unique(dat_na$year)))



model_code = '
model
{
  # Likelihood
  for (i in 1:N) {
    y[i] ~ dbeta(a[i], b[i])
    a[i] <- mu[i] * theta
    b[i] <- (1 - mu[i]) * theta
    logit(mu[i]) <- a_ran[year[i]] + beta_precip * precip[i] + beta_weight * weight[i]
    
  # store predicted values at each iteration in y_pred
      y_pred[i] ~ dbeta(a[i], b[i])
  }

    # prior for random intercepts, a = site, g = yearsite
        for (i in 1:nyears) {a_ran[i] ~ dnorm(a_bar, sigma_year)}
        a_bar ~ dnorm(0, 10^-2)
        sigma_year ~ dexp(1)

  
  # Priors
  beta_precip ~ dnorm(0, 10^-2)
  beta_weight ~ dnorm(0, 10^-2)
  theta ~ dunif(0, 10)
}
'

model_parameters =  c("y_pred",
                      "a_ran",
                      "a_bar",
                      "sigma_year",
                      "beta_precip",
                      "beta_weight",
                      "theta")

model_run   <- jags(data    = model_data,
                    inits      = NULL,     
                    parameters = model_parameters,
                    model.file = textConnection(model_code),
                    n.thin     = 10, 
                    n.chains   = 3,
                    n.burnin   = 4000,
                    n.iter     = 5000)

m2 <- update(model_run, n.iter = 10000, n.thin = 10) 

mcmc <- as.mcmc(m2)



# * model diagnostics -------------------------------------------------------

# mixing
mcmc %>%
  window(thin=10) %>% 
  
  # sigma = the yearly residual variance, a = random intercept for site, g = random intercept for yearsite
  tidybayes::gather_draws(beta_precip, beta_weight, theta, a_bar, sigma_year) %>%
  
  # change this filter to look at mixing for the parameter of interest. See above line for options
  # filter(.variable == "alpha") %>% 
  ungroup() %>%
  #mutate(term = ifelse(is.na(i), .variable, paste0(.variable,"[",i,"]"))) %>%
  ggplot(aes(x=.iteration, y=.value, color=as.factor(.chain))) +
  scale_color_manual(values=c("#461220", "#b23a48", "#fcb9b2")) +
  geom_line(alpha=0.5) +
  facet_grid(.variable~., scale="free_y") +
  labs(color="chain", x="iteration") +
  theme_nuwcru()



mcmc %>%
  tidybayes::gather_draws(beta_weight, beta_precip) %>%
  group_by(.variable) %>%
  summarize(mean = mean(.value), sd = sd(.value)) %>%
  mutate(upper95 = mean + (1.96 * sd), lower95 = mean - (1.96 * sd),
         upper80 = mean + (1.282 * sd), lower80 = mean - (1.282 * sd),
         upper50 = mean + (0.674 * sd), lower50 = mean - (0.674 * sd)) %>%
  ggplot() +
  geom_vline(xintercept = 0, colour = grey6, linetype = "dashed") +
  geom_segment(aes(x = lower95, xend = upper95, y = .variable, yend = .variable), colour = "#DFEBF7", size = 2) +
  geom_segment(aes(x = lower80, xend = upper80, y = .variable, yend = .variable), colour = "#A5CADF", size = 2) +
  geom_segment(aes(x = lower50, xend = upper50, y = .variable, yend = .variable), colour = "#4A84BD", size = 2) +
  #geom_point(aes(y = .variable, x = mean), shape = 21, fill = "white", colour = "black", size = 4) +
  scale_x_continuous(limits = c(-1.5,1.5)) +
  #scale_y_continuous(breaks = 1) +
  ylab("") + xlab("") + 
  theme_nuwcru() + 
  theme(panel.border = element_blank(),
        #axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())

x <- data.frame(m2$BUGSoutput$summary)
dat_na$pred_mean <- x[10:nrow(x),1]
dat_na$pred_sd <- x[10:nrow(x),2]

dat_na %>%
  ggplot() +
  geom_point(aes(x = precip, y = pred_mean), colour = red2) +
  #geom_point(aes(x = precip, y = prop), shape = 21) +
  theme_nuwcru()


# m4. nested random ------------------------------------------------------------------
dat <- read_csv("data/03_0-20.csv") %>% select(-X1)
dat <- dat %>% filter(b_age < 16)

# dat$prop <- ifelse(dat$prop == 0, dat$prop+0.001, dat$prop)
# dat$prop <- ifelse(dat$prop == 1, dat$prop-0.001, dat$prop)


# create precip lag
dat$precip_lag1 <- rep(NA, nrow(dat))
for (i in 2:nrow(dat)){
  dat$precip_lag1[i] <- dat$precip[i-1]
}

# data for Jags model
model_data = list(N      = nrow(dat), 
                  y      = dat$prop, 
                  precip = as.vector(scale(dat$precip)),
                  precip_lag1 = dat$precip_lag1,
                  weight = as.vector(scale(dat$b_weight)),
                  year   = as.factor(dat$year),
                  nyears = length(unique(dat$year)))

model_code = '
model
{
  # Likelihood
  for (i in 1:N) {
    y[i] ~ dbeta(a[i], b[i])
    a[i] <- mu[i] * theta
    b[i] <- (1 - mu[i]) * theta
    logit(mu[i]) <- a_ran[year[i]] + beta_precip * precip[i] + beta_preciplag * precip_lag1[i] + beta_weight * weight[i]
    
  # store predicted values at each iteration in y_pred
      # y_pred[i] ~ dbeta(a[i], b[i])
  }
  
  # for missing covariate data
  for (i in 1:N) {precip_lag1[i] ~ dnorm(0, 10^-2)} 
  for (i in 1:N) {precip[i] ~ dnorm(0, 10^-2)} 
  
  # prior for random intercepts, a = site, g = yearsite
    for (i in 1:nyears) {a_ran[i] ~ dnorm(a_bar, sigma_year)}
        a_bar ~ dnorm(0, 10^-2)
        sigma_year ~ dexp(1)
  
  # Priors
    beta_precip ~ dnorm(0, 10^-2)
    beta_preciplag ~ dnorm(0, 10^-2)
    beta_weight ~ dnorm(0, 10^-2)
    theta ~ dunif(0, 10)
}
'

model_parameters = c("a_ran",
                     "a_bar",
                     "sigma_year",
                     "beta_precip",
                     "beta_weight",
                     "beta_preciplag",
                     "theta")

# inits <- function (){
#   list (beta_precip    = rnorm(1), 
#         beta_weight = runif(1),
#         beta_preciplag = rnorm(0),
#         sigma_year = rnorm(0),
#         a_bar = rnorm(0),
#         theta = rnorm(3))
# }

model_run <- jags(data       = model_data,
                  inits      = NULL,     
                  parameters = model_parameters,
                  model.file = textConnection(model_code),
                  n.thin     = 10, 
                  n.chains   = 3,
                  n.burnin   = 4000,
                  n.iter     = 5000)

m2 <- update(model_run, 
             n.iter = 10000, 
             n.thin = 10) 

mcmc <- as.mcmc(m2)


# * model diagnostics -------------------------------------------------------

# mixing
mcmc %>%
  window(thin=10) %>% 
  tidybayes::gather_draws(beta_precip, beta_preciplag, beta_weight, theta, a_bar, sigma_year) %>%
  ungroup() %>%
  ggplot(aes(x=.iteration, y=.value, color=as.factor(.chain))) +
  scale_color_manual(values=c("#461220", "#b23a48", "#fcb9b2")) +
  geom_line(alpha=0.5) +
  facet_grid(.variable~., scale="free_y") +
  labs(color="chain", x="iteration") +
  theme_nuwcru()


# mixing
x <- mcmc %>%
  window(thin=10) %>% 
  tidybayes::gather_draws(mu[i], a_ran[i]) %>%
  
  # change this filter to look at mixing for the parameter of interest. See above line for options
  filter(.variable == "mu") %>% 
  ungroup() %>%
  mutate(term = ifelse(is.na(i), .variable, paste0(.variable,"[",i,"]"))) %>%
  ggplot(aes(x=.iteration, y=.value, color=as.factor(.chain))) +
  scale_color_manual(values=c("#461220", "#b23a48", "#fcb9b2")) +
  geom_line(alpha=0.5) +
  facet_grid(term~., scale="free_y") +
  labs(color="chain", x="iteration") +
  theme_nuwcru()











mcmc %>%
  tidybayes::gather_draws(beta_weight, beta_precip, beta_preciplag) %>%
  group_by(.variable) %>%
  summarize(mean = mean(.value), sd = sd(.value)) %>%
  mutate(upper95 = mean + (1.96 * sd), lower95 = mean - (1.96 * sd),
         upper80 = mean + (1.282 * sd), lower80 = mean - (1.282 * sd),
         upper50 = mean + (0.674 * sd), lower50 = mean - (0.674 * sd)) %>%
  ggplot() +
  geom_vline(xintercept = 0, colour = grey6, linetype = "dashed") +
  geom_segment(aes(x = lower95, xend = upper95, y = .variable, yend = .variable), colour = "#DFEBF7", size = 2) +
  geom_segment(aes(x = lower80, xend = upper80, y = .variable, yend = .variable), colour = "#A5CADF", size = 2) +
  geom_segment(aes(x = lower50, xend = upper50, y = .variable, yend = .variable), colour = "#4A84BD", size = 2) +
  scale_x_continuous(limits = c(-1.5,1.5)) +
  ylab("") + xlab("") + 
  theme_nuwcru() + 
  theme(panel.border = element_blank(),
        #axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())


x <- data.frame(m2$BUGSoutput$summary)
dat_na$pred_mean <- x[10:nrow(x),1]
dat_na$pred_sd <- x[10:nrow(x),2]
head(x)
dat_na %>%
  ggplot() +
  geom_point(aes(x = precip, y = pred_mean), colour = red2) +
  #geom_point(aes(x = precip, y = prop), shape = 21) +
  theme_nuwcru()


y[i] ~ dbeta(a[i], b[i])
a[i] <- mu[i] * theta
b[i] <- (1 - mu[i]) * theta


mcmc %>%
  tidybayes::gather_draws(mu, phi) %>%
  group_by(.variable) %>%
  summarize(mean = mean(.value), sd = sd(.value))


pbar <- 0.9
theta <- 20
curve( rethinking::dbeta2(x, pbar, theta) , from = 0 , to = 1 ,
       xlab="probability" , ylab="Density" )



# m4. ZOIB nested random ------------------------------------------------------------------
dat <- read_csv("data/03_0-20.csv") %>% select(-X1)
dat <- dat %>% filter(b_age < 16)

# dat$prop <- ifelse(dat$prop == 0, dat$prop+0.001, dat$prop)
# dat$prop <- ifelse(dat$prop == 1, dat$prop-0.001, dat$prop)


# create precip lag
dat$precip_lag1 <- rep(NA, nrow(dat))
for (i in 2:nrow(dat)){
  dat$precip_lag1[i] <- dat$precip[i-1]
}

# data for Jags model
model_data = list(N      = nrow(dat), 
                  y      = dat$prop, 
                  precip = as.vector(scale(dat$precip)),
                  precip_lag1 = dat$precip_lag1,
                  weight = as.vector(scale(dat$b_weight)),
                  year   = as.factor(dat$year),
                  nyears = length(unique(dat$year)))


# zero one inflated beta
# write model
cat(
  "
  model{
  # priors
  a0 ~ dnorm(0, .001)
  a1 ~ dnorm(0, .001)
  a2 ~ dnorm(0, .001)
  b0 ~ dnorm(0, .001)
  b1 ~ dnorm(0, .001)
  t0 ~ dnorm(0, .01)
  tau <- exp(t0)

  # likelihood for alpha
  for (i in 1:n){
    logit(alpha[i]) <- a0 + a1 * x[i] + a2 * x[i] ^ 2
    y.discrete[i] ~ dbern(alpha[i])
  }

  # likelihood for gamma
  for (i in 1:n.discrete){
    y.d[i] ~ dbern(mu[i])
    logit(mu[i]) <- b0 + b1 * x.d[i]
  }

  # likelihood for mu and tau
  for (i in 1:n.cont){
    y.c[i] ~ dbeta(p[i], q[i])
    p[i] <- mu2[i] * tau
    q[i] <- (1 - mu2[i]) * tau
    logit(mu2[i]) <- b0 + b1 * x.c[i]
  }
  }  
  ", file="beinf.txt"
)



model_code = '
model
{
  # Likelihood
  for (i in 1:N) {
    y[i] ~ dbeta(a[i], b[i])
    a[i] <- mu[i] * theta
    b[i] <- (1 - mu[i]) * theta
    logit(mu[i]) <- a_ran[year[i]] + beta_precip * precip[i] + beta_preciplag * precip_lag1[i] + beta_weight * weight[i]
    
  # store predicted values at each iteration in y_pred
      # y_pred[i] ~ dbeta(a[i], b[i])
  }
  
  # for missing covariate data
  for (i in 1:N) {precip_lag1[i] ~ dnorm(0, 10^-2)} 
  for (i in 1:N) {precip[i] ~ dnorm(0, 10^-2)} 
  
  # prior for random intercepts, a = site, g = yearsite
    for (i in 1:nyears) {a_ran[i] ~ dnorm(a_bar, sigma_year)}
        a_bar ~ dnorm(0, 10^-2)
        sigma_year ~ dexp(1)
  
  # Priors
    beta_precip ~ dnorm(0, 10^-2)
    beta_preciplag ~ dnorm(0, 10^-2)
    beta_weight ~ dnorm(0, 10^-2)
    theta ~ dunif(0, 10)
}
'

model_parameters = c("a_ran",
                     "a_bar",
                     "sigma_year",
                     "beta_precip",
                     "beta_weight",
                     "beta_preciplag",
                     "theta")

# inits <- function (){
#   list (beta_precip    = rnorm(1), 
#         beta_weight = runif(1),
#         beta_preciplag = rnorm(0),
#         sigma_year = rnorm(0),
#         a_bar = rnorm(0),
#         theta = rnorm(3))
# }

model_run <- jags(data       = model_data,
                  inits      = NULL,     
                  parameters = model_parameters,
                  model.file = textConnection(model_code),
                  n.thin     = 10, 
                  n.chains   = 3,
                  n.burnin   = 4000,
                  n.iter     = 5000)

m2 <- update(model_run, 
             n.iter = 10000, 
             n.thin = 10) 

mcmc <- as.mcmc(m2)


# * model diagnostics -------------------------------------------------------

# mixing
mcmc %>%
  window(thin=10) %>% 
  tidybayes::gather_draws(beta_precip, beta_preciplag, beta_weight, theta, a_bar, sigma_year) %>%
  ungroup() %>%
  ggplot(aes(x=.iteration, y=.value, color=as.factor(.chain))) +
  scale_color_manual(values=c("#461220", "#b23a48", "#fcb9b2")) +
  geom_line(alpha=0.5) +
  facet_grid(.variable~., scale="free_y") +
  labs(color="chain", x="iteration") +
  theme_nuwcru()


# mixing
x <- mcmc %>%
  window(thin=10) %>% 
  tidybayes::gather_draws(mu[i], a_ran[i]) %>%
  
  # change this filter to look at mixing for the parameter of interest. See above line for options
  filter(.variable == "mu") %>% 
  ungroup() %>%
  mutate(term = ifelse(is.na(i), .variable, paste0(.variable,"[",i,"]"))) %>%
  ggplot(aes(x=.iteration, y=.value, color=as.factor(.chain))) +
  scale_color_manual(values=c("#461220", "#b23a48", "#fcb9b2")) +
  geom_line(alpha=0.5) +
  facet_grid(term~., scale="free_y") +
  labs(color="chain", x="iteration") +
  theme_nuwcru()











mcmc %>%
  tidybayes::gather_draws(beta_weight, beta_precip, beta_preciplag) %>%
  group_by(.variable) %>%
  summarize(mean = mean(.value), sd = sd(.value)) %>%
  mutate(upper95 = mean + (1.96 * sd), lower95 = mean - (1.96 * sd),
         upper80 = mean + (1.282 * sd), lower80 = mean - (1.282 * sd),
         upper50 = mean + (0.674 * sd), lower50 = mean - (0.674 * sd)) %>%
  ggplot() +
  geom_vline(xintercept = 0, colour = grey6, linetype = "dashed") +
  geom_segment(aes(x = lower95, xend = upper95, y = .variable, yend = .variable), colour = "#DFEBF7", size = 2) +
  geom_segment(aes(x = lower80, xend = upper80, y = .variable, yend = .variable), colour = "#A5CADF", size = 2) +
  geom_segment(aes(x = lower50, xend = upper50, y = .variable, yend = .variable), colour = "#4A84BD", size = 2) +
  scale_x_continuous(limits = c(-1.5,1.5)) +
  ylab("") + xlab("") + 
  theme_nuwcru() + 
  theme(panel.border = element_blank(),
        #axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())


x <- data.frame(m2$BUGSoutput$summary)
dat_na$pred_mean <- x[10:nrow(x),1]
dat_na$pred_sd <- x[10:nrow(x),2]
head(x)
dat_na %>%
  ggplot() +
  geom_point(aes(x = precip, y = pred_mean), colour = red2) +
  #geom_point(aes(x = precip, y = prop), shape = 21) +
  theme_nuwcru()


y[i] ~ dbeta(a[i], b[i])
a[i] <- mu[i] * theta
b[i] <- (1 - mu[i]) * theta


mcmc %>%
  tidybayes::gather_draws(mu, phi) %>%
  group_by(.variable) %>%
  summarize(mean = mean(.value), sd = sd(.value))


pbar <- 0.9
theta <- 20
curve( rethinking::dbeta2(x, pbar, theta) , from = 0 , to = 1 ,
       xlab="probability" , ylab="Density" )

