# author: Pia Schoknecht
# date: 12.07.2024

# script to analyze reading times in the critical region

# load packages
library(brms)


# load data
crit_trim <- read.csv("pandora_spr_774_crit.csv")
crit_trim$c_trial <- crit_trim$trial/100


# priors
priors_s <- c(
  prior(normal(6, 0.6), class = Intercept),
  prior(normal(0, 0.01), class = b, coef="syn"),
  prior(normal(0, 0.01), class = b, coef="sem"),
  prior(normal(0, 0.01), class = b, coef="syn:sem"),
  prior(normal(0, 0.1),  class = b, coef="c_trial"),
  prior(normal(0, 0.5), class = sigma),
  prior(normal(0, 0.1), class = sd)
) 

priors_m <- c(
  prior(normal(6, 0.6), class = Intercept),
  prior(normal(0, 0.05), class = b, coef="syn"),
  prior(normal(0, 0.05), class = b, coef="sem"),
  prior(normal(0, 0.05), class = b, coef="syn:sem"),
  prior(normal(0, 0.1),  class = b, coef="c_trial"),
  prior(normal(0, 0.5), class = sigma),
  prior(normal(0, 0.1), class = sd)
) 

priors_l <- c(
  prior(normal(6, 0.6), class = Intercept),
  prior(normal(0, 0.1), class = b, coef="syn"),
  prior(normal(0, 0.1), class = b, coef="sem"),
  prior(normal(0, 0.1), class = b, coef="syn:sem"),
  prior(normal(0, 0.1),  class = b, coef="c_trial"),
  prior(normal(0, 0.5), class = sigma),
  prior(normal(0, 0.1), class = sd)
) 

# priors on the ms scale
# intercept
exp(extraDistr::qtnorm(0.025,mean=6, sd=0.6)) # intercept normal(6, 0.6) lower bound
exp(extraDistr::qtnorm(0.975,mean=6, sd=0.6)) # intercept normal(6, 0.6) upper bound

# Normal(0, 0.01) prior
prior_mean <- 0
prior_sd <- 0.01
exp(6+ (prior_mean - 2*prior_sd) /2)- exp(6- (prior_mean - 2*prior_sd) /2)
exp(6+ (prior_mean + 2*prior_sd) /2)- exp(6- (prior_mean + 2*prior_sd) /2)

# Normal(0, 0.05) prior
prior_mean <- 0
prior_sd <- 0.05
exp(6+ (prior_mean - 2*prior_sd) /2)- exp(6- (prior_mean - 2*prior_sd) /2)
exp(6+ (prior_mean + 2*prior_sd) /2)- exp(6- (prior_mean + 2*prior_sd) /2)


# Normal(0, 0.1) prior
prior_mean <- 0
prior_sd <- 0.1
exp(6+ (prior_mean - 2*prior_sd) /2)- exp(6- (prior_mean - 2*prior_sd) /2)
exp(6+ (prior_mean + 2*prior_sd) /2)- exp(6- (prior_mean + 2*prior_sd) /2)

# models
m_s_full <- brm(rt ~ 1 + syn * sem * c_trial +
                 (1 + syn * sem * c_trial || participant) +
                 (1 + syn * sem * c_trial || item),
                data = crit_trim,
                family = lognormal(),
                prior = priors_s,
                warmup = 2000,
                iter = 20000,
                cores = 4,
                save_pars = save_pars(all = TRUE),
                sample_prior="yes"
)
save(m_s_full, file = paste("model_fits/Fit_s_Full_crit_slopes_symmetrical.Rda"))

m_m_full <- brm(rt ~ 1 + syn * sem * c_trial +
                  (1 + syn * sem * c_trial || participant) +
                  (1 + syn * sem * c_trial || item),
                data = crit_trim,
                family = lognormal(),
                prior = priors_m,
                warmup = 2000,
                iter = 20000,
                cores = 4,
                save_pars = save_pars(all = TRUE),
                sample_prior="yes"
)
save(m_m_full, file = paste("model_fits/Fit_m_Full_crit_slopes_symmetrical.Rda"))

m_l_full <- brm(rt ~ 1 + syn * sem * c_trial +
                  (1 + syn * sem * c_trial || participant) +
                  (1 + syn * sem * c_trial || item),
                data = crit_trim,
                family = lognormal(),
                prior = priors_l,
                warmup = 2000,
                iter = 20000,
                cores = 4,
                save_pars = save_pars(all = TRUE),
                sample_prior="yes"
)
save(m_l_full, file = paste("model_fits/Fit_l_Full_crit_slopes_symmetrical.Rda"))
