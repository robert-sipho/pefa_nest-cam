
d <- read_csv("data/03_daily_survival.csv") %>% 
  select(-X1) 

obs_hist <- d %>% expand(ind = ind, age = seq(1,30,1)) %>% 
  left_join(select(d, ind, age, alive), by = c("ind", "age")) %>%
  mutate(alive = ifelse(is.na(alive), 0,alive))

y <- obs_hist %>%
  #select(-age) %>%
  pivot_wider(names_from = age, values_from = alive)

View(obs_hist %>%
       group_by(age) %>%
       summarize(prop = sum(alive)/360))

age <- obs_hist %>%
  #select(-age) %>%
  pivot_wider(alive)
as.matrix
matrix(rep(1:30, each = 360), ncol = 30)

as.matrix(y[,2:ncol(y)])


y_mat <- as.matrix(y[,2:ncol(y)])
last <- rowSums(y_mat)
last <- ifelse(last < 30, last, last)




#  load covariates into list for the model
jags.dat <- list(y = y_mat,
                 age =matrix(rep(1:30, each = nrow(y_mat)), ncol = 30),
                 first = rep(1,nrow(y)),
                 last = last,
                 nind = nrow(y),
                 site = as.factor(d %>% filter(!duplicated(ind)) %>% pull(site)),
                 n_site = length(unique(as.factor(d %>% filter(!duplicated(ind)) %>% pull(site)))))






sink("model.txt")
cat("
        model {

        # priors for betas
          b0~dunif(-5,5)
         # b.rh~dunif(-5,5)
         # b.trt~dunif(-5,5)
          #b.rain~dunif(-5,5)
          b_age~dunif(-5,5)
        
        # priors for sigma
          sigmaplot~dunif(0,7)
          tauplot <- sqrt(1/(sigmaplot*sigmaplot))
        
        # priors for random effects
          for(i in 1:n_site){alpha[i]~dnorm(0,tauplot)}
        
        # Likelihood, for ith individual, and jth time interval # 
          for(i in 1:nind){
          for(j in (first[i]+1):last[i]){
            logit(phi[i,j]) <- b0  + b_age*age[i,j] + alpha[site[i]]

            mu[i,j] <- phi[i,j] * y[i,j-1]
            y[i,j]  ~ dbern(mu[i,j])

            }
          }
        }

        ",fill=TRUE)
sink()


inits <- function (){
  list("b0"     = dunif(1,0,.2),
       #"b.rh"      = dunif(1,0,.2),
       #"b.trt"     = dunif(1,0,.2),
       #"b.rain"    = dunif(1,0,.2),
       "b_age"     = dunif(1,0,.2),
       "sigmaplot" = runif(1,1,2),
       "alpha"     = rnorm(47))
}

# Note, you could also monitor "ranef" which would give you the random effects
# for each site.
params <- c("b0", # intercept
            #"b.rh",  # b*relhatch 
            #"b.trt", # b*treatment
            # "b.rain",# b*rain
            "b_age", # b*age
            "sigmaplot") #sigma random effects

m_jags   <- jags(data      = jags.dat,
                 inits      = inits,
                 parameters = params,
                 model      = "model.txt",
                 n.thin     = 10, 
                 n.chains   = 3,
                 n.burnin   = 4000,
                 n.iter     = 7000)  

m_jags2 <- update(m_jags, n.iter = 10000, n.thin = 10) 

m_jags <- as.mcmc(m_jags2) #change back to 4 later 



m_jags %>%
  window(thin=10) %>% 
  
  # sigma = the yearly residual variance, a = random intercept for site, g = random intercept for yearsite
  tidybayes::gather_draws(b_age[i]) %>%
  
  # change this filter to look at mixing for the parameter of interest. See above line for options
  filter(.variable == "b_age") %>% 
  ungroup() %>%
  mutate(term = ifelse(is.na(i), .variable, paste0(.variable,"[",i,"]"))) %>%
  ggplot(aes(x=.iteration, y=.value, color=as.factor(.chain))) +
  scale_color_manual(values=c("#461220", "#b23a48", "#fcb9b2")) +
  geom_line(alpha=0.5) +
  facet_grid(term~., scale="free_y") +
  labs(color="chain", x="iteration") +
  theme_nuwcru()
