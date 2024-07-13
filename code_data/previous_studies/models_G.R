# author: Pia Schoknecht
# date: 21.05.2024

# script to run models (to later compute Bayes Factors) for the previous studies

# Mertzen et al., (2024) English and German eye-tracking data
# for comparability with self-paced reading times, we use the total fixation times (TFT) eye-tracking measure


library(brms)


#### load Mertzen et al. GERMAN data ####
df <- read.table("dataRetroDE_processed.txt")
df2 <- subset(df, df$region %in% c("precrit", "crit", "postcrit"))

# contrast coding
df2$syn <- ifelse(df2$condition%in%c("a","b"),-0.5,0.5)
df2$sem <- ifelse(df2$condition%in%c("b","d"),0.5,-0.5)

# trim all rts below 80
range(df2$TFT)
df2_trim <- subset(df2, df2$TFT > 80)
range(df2_trim$TFT)


# same priors as for the present study
priors_s <- c(
  prior(normal(6, 0.6), class = Intercept),
  prior(normal(0, 0.01), class = b, coef="syn"),
  prior(normal(0, 0.01), class = b, coef="sem"),
  prior(normal(0, 0.01), class = b, coef="syn:sem"),
  prior(normal(0, 0.5), class = sigma),
  prior(normal(0, 0.1), class = sd)
) 

priors_m <- c(
  prior(normal(6, 0.6), class = Intercept),
  prior(normal(0, 0.05), class = b, coef="syn"),
  prior(normal(0, 0.05), class = b, coef="sem"),
  prior(normal(0, 0.05), class = b, coef="syn:sem"),
  prior(normal(0, 0.5), class = sigma),
  prior(normal(0, 0.1), class = sd)
) 

priors_l <- c(
  prior(normal(6, 0.6), class = Intercept),
  prior(normal(0, 0.1), class = b, coef="syn"),
  prior(normal(0, 0.1), class = b, coef="sem"),
  prior(normal(0, 0.1), class = b, coef="syn:sem"),
  prior(normal(0, 0.5), class = sigma),
  prior(normal(0, 0.1), class = sd)
) 

##### models ####
##### small prior ####
m_s_full_precrit <- brm(TFT ~ 1 + syn * sem +
                  (1 + syn * sem|| subject) +
                  (1 + syn * sem|| item),
               data = subset(df2_trim, df2_trim$region == "precrit"),
               family = lognormal(),
               prior = priors_s,
               warmup = 2000,
               iter = 20000,
               cores = 4,
               save_pars = save_pars(all = TRUE),
               sample_prior="yes"
               )
save(m_s_full_precrit, file = paste("model_fits_G/Fit_s_Full_precrit.Rda"))

m_s_full_crit <- brm(TFT ~ 1 + syn * sem +
                          (1 + syn * sem|| subject) +
                          (1 + syn * sem|| item),
                        data = subset(df2_trim, df2_trim$region == "crit"),
                        family = lognormal(),
                        prior = priors_s,
                        warmup = 2000,
                        iter = 20000,
                        cores = 4,
                        save_pars = save_pars(all = TRUE),
                        sample_prior="yes"
)
save(m_s_full_crit, file = paste("model_fits_G/Fit_s_Full_crit.Rda"))

m_s_full_postcrit <- brm(TFT ~ 1 + syn * sem +
                          (1 + syn * sem|| subject) +
                          (1 + syn * sem|| item),
                        data = subset(df2_trim, df2_trim$region == "postcrit"),
                        family = lognormal(),
                        prior = priors_s,
                        warmup = 2000,
                        iter = 20000,
                        cores = 4,
                        save_pars = save_pars(all = TRUE),
                        sample_prior="yes"
)
save(m_s_full_postcrit, file = paste("model_fits_G/Fit_s_Full_postcrit.Rda"))

##### medium prior ####
m_m_full_precrit <- brm(TFT ~ 1 + syn * sem +
                          (1 + syn * sem|| subject) +
                          (1 + syn * sem|| item),
                        data = subset(df2_trim, df2_trim$region == "precrit"),
                        family = lognormal(),
                        prior = priors_m,
                        warmup = 2000,
                        iter = 20000,
                        cores = 4,
                        save_pars = save_pars(all = TRUE),
                        sample_prior="yes"
)
save(m_m_full_precrit, file = paste("model_fits_G/Fit_m_Full_precrit.Rda"))

m_m_full_crit <- brm(TFT ~ 1 + syn * sem +
                       (1 + syn * sem|| subject) +
                       (1 + syn * sem|| item),
                     data = subset(df2_trim, df2_trim$region == "crit"),
                     family = lognormal(),
                     prior = priors_m,
                     warmup = 2000,
                     iter = 20000,
                     cores = 4,
                     save_pars = save_pars(all = TRUE),
                     sample_prior="yes"
)
save(m_m_full_crit, file = paste("model_fits_G/Fit_m_Full_crit.Rda"))

m_m_full_postcrit <- brm(TFT ~ 1 + syn * sem +
                           (1 + syn * sem|| subject) +
                           (1 + syn * sem|| item),
                         data = subset(df2_trim, df2_trim$region == "postcrit"),
                         family = lognormal(),
                         prior = priors_m,
                         warmup = 2000,
                         iter = 20000,
                         cores = 4,
                         save_pars = save_pars(all = TRUE),
                         sample_prior="yes"
)
save(m_m_full_postcrit, file = paste("model_fits_G/Fit_m_Full_postcrit.Rda"))

##### large prior ####
m_l_full_precrit <- brm(TFT ~ 1 + syn * sem +
                          (1 + syn * sem|| subject) +
                          (1 + syn * sem|| item),
                        data = subset(df2_trim, df2_trim$region == "precrit"),
                        family = lognormal(),
                        prior = priors_l,
                        warmup = 2000,
                        iter = 20000,
                        cores = 4,
                        save_pars = save_pars(all = TRUE),
                        sample_prior="yes"
)
save(m_l_full_precrit, file = paste("model_fits_G/Fit_l_Full_precrit.Rda"))

m_l_full_crit <- brm(TFT ~ 1 + syn * sem +
                       (1 + syn * sem|| subject) +
                       (1 + syn * sem|| item),
                     data = subset(df2_trim, df2_trim$region == "crit"),
                     family = lognormal(),
                     prior = priors_l,
                     warmup = 2000,
                     iter = 20000,
                     cores = 4,
                     save_pars = save_pars(all = TRUE),
                     sample_prior="yes"
)
save(m_l_full_crit, file = paste("model_fits_G/Fit_l_Full_crit.Rda"))

m_l_full_postcrit <- brm(TFT ~ 1 + syn * sem +
                           (1 + syn * sem|| subject) +
                           (1 + syn * sem|| item),
                         data = subset(df2_trim, df2_trim$region == "postcrit"),
                         family = lognormal(),
                         prior = priors_l,
                         warmup = 2000,
                         iter = 20000,
                         cores = 4,
                         save_pars = save_pars(all = TRUE),
                         sample_prior="yes"
)
save(m_l_full_postcrit, file = paste("model_fits_G/Fit_l_Full_postcrit.Rda"))
