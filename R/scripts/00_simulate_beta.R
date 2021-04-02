library(rethinking)
library(tidyverse)

# Beta distribution

#   y^a-1 * (1-y)^b-1
#   ------------------
#        B(a, b)

# where B is the beta function which computes a normalizing constant



# the mean of this distribution is:
#          a
# mu =  -------
#        a + b


# the spread of the distribution is:
#          k = a + b

# SD of the beta distribution is:
#     sigma = sqrt( mu(1-mu) / (k + 1) )


# in statistical rethinking, Mcelreath refers to the mean as "pbar", and the spread as "theta"
# in that context:
#               a         
#   pbar =   -------          
#             a + b       
# and 
# 
#
#  theta = a + b
#
#


# code to convert mu kappa (mean and spread) to proper beta distribution parameters a and b
mukappa_ab <- function(mu, kappa) {
  if (mu <= 0 | mu >= 1) stop("must have 0 < mu < 1")
  if (kappa <= 0) stop("kappa must be > 0")
  a <- mu * kappa
  b <- (1.0 - mu) * kappa
  return(list(a = a, b = b))
}

mu <- 0.8
kappa <- 0.2

mukappa_ab(mu, kappa)


# simulate proportion data ------------------------------------------------

# visualize different values of pbar / theta / mu / kappa
tibble(pbar = c(.25, .5, .75)) %>% 
  tidyr::expand(pbar, theta = c(5, 15, 30)) %>% 
  tidyr::expand(nesting(pbar, theta), x = seq(from = 0, to = 1, length.out = 100)) %>% 
  mutate(density = rethinking::dbeta2(x, pbar, theta),
         mu      = str_c("mu == ", pbar %>% str_remove(., "0")),
         kappa   = str_c("kappa == ", theta)) %>% 
  mutate(kappa = factor(kappa, levels = c("kappa == 30", "kappa == 15", "kappa == 5"))) %>%
  ggplot() +
  geom_ribbon(aes(x    = x, 
                  ymin = 0, 
                  ymax = density)) +
  scale_x_continuous("probability space", breaks = c(0, .5, 1)) +
  scale_y_continuous(NULL, labels = NULL) +
  theme_nuwcru() +
  facet_grid(kappa ~ mu, labeller = label_parsed)





# model
# A_i ∼ BetaBinomial(N_i, mu_i, theta) # N = number of samples, 
# logit(mu_i) = a_gid[i] 
# a_j ∼ Normal(0, 1.5) 
# theta ∼ Exponential(1)


install.packages("brms")
library(brms)


# beta distribution for proportions. data can't equal 1 or 0. Ensure that there isn't a 1 in the data. 

set.seed(1234)
eta <- c(1, -0.2)
gamma <- c(1.8, 0.4)
N <- 200 # sample size
x <- rnorm(N, 2, 2)
z <- rnorm(N, 0, 2)
mu <- binomial(link = logit)$linkinv(eta[1] + eta[2]*x)
phi <- binomial(link = log)$linkinv(gamma[1] + gamma[2]*z)
y <- rbeta(N, mu * phi, (1 - mu) * phi)
dat <- data.frame(cbind(y, x, z))
hist(dat$y, col = "darkgrey", border = F, main = "Distribution of Outcome Variable", xlab = "y", breaks = 20, freq = F)


