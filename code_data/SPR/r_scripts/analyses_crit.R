# author: Pia Schoknecht
# date: 22.07.2023

library(tidyverse)
library(lme4)
library(brms)
library(ggplot2)
library(bayesplot)
library(truncnorm)


# load data
crit_trim <- read.csv("../data/pandora_spr_774_crit.csv")


# Inferential statistics
# truncated varying priors 
# (only positive effects -- longer reading times for high interference)

priors_s_tr <- c(
  prior(normal(6, 0.6), class = Intercept),
  prior(normal(0, 0.01), class = b, lb=0),
  prior(normal(0, 0.5), class = sigma),
  prior(normal(0, 0.1), class = sd)
)  

priors_m_tr <- c(
  prior(normal(6, 0.6), class = Intercept),
  prior(normal(0, 0.05), class = b, lb=0),
  prior(normal(0, 0.5), class = sigma),
  prior(normal(0, 0.1), class = sd)
) 

priors_l_tr <- c(
  prior(normal(6, 0.6), class = Intercept),
  prior(normal(0, 0.1), class = b, lb=0),
  prior(normal(0, 0.5), class = sigma),
  prior(normal(0, 0.1), class = sd)
) 

# models
m_s_full <- brm(rt ~ 1 + syn * sem +
                 (1 | participant) +
                 (1 | item),
               data = crit_trim,
               family = lognormal(),
               prior = priors_s_tr,
               warmup = 2000,
               iter = 20000,
               cores = 4,
               save_pars = save_pars(all = TRUE)
               )
save(m_s_full, file = paste("model_fits/Fit_s_Full_crit.Rda"))

m_m_full <- brm(rt ~ 1 + syn * sem +
                  (1 | participant) +
                  (1 | item),
                data = crit_trim,
                family = lognormal(),
                prior = priors_m_tr,
                warmup = 2000,
                iter = 20000,
                cores = 4,
                save_pars = save_pars(all = TRUE)
)
save(m_m_full, file = paste("model_fits/Fit_m_Full_crit.Rda"))

m_l_full <- brm(rt ~ 1 + syn * sem +
                  (1 | participant) +
                  (1 | item),
                data = crit_trim,
                family = lognormal(),
                prior = priors_l_tr,
                warmup = 2000,
                iter = 20000,
                cores = 4,
                save_pars = save_pars(all = TRUE)
)
save(m_l_full, file = paste("model_fits/Fit_l_Full_crit.Rda"))

# null modells
# without sem
m_s_nosem <- brm(rt ~ 1 + syn + syn:sem +
                  (1 | participant) +
                  (1 | item),
                data = crit_trim,
                family = lognormal(),
                prior = priors_s_tr,
                warmup = 2000,
                iter = 20000,
                cores = 4,
                save_pars = save_pars(all = TRUE)
)
save(m_s_nosem, file = paste("model_fits/Fit_s_nosem_crit.Rda"))

m_m_nosem <- brm(rt ~ 1 + syn + syn:sem +
                   (1 | participant) +
                   (1 | item),
                 data = crit_trim,
                 family = lognormal(),
                 prior = priors_m_tr,
                 warmup = 2000,
                 iter = 20000,
                 cores = 4,
                 save_pars = save_pars(all = TRUE)
)
save(m_m_nosem, file = paste("model_fits/Fit_m_nosem_crit.Rda"))

m_l_nosem <- brm(rt ~ 1 + syn + syn:sem +
                    (1 | participant) +
                    (1 | item),
                  data = crit_trim,
                  family = lognormal(),
                  prior = priors_l_tr,
                  warmup = 2000,
                  iter = 20000,
                  cores = 4,
                  save_pars = save_pars(all = TRUE)
)
save(m_l_nosem, file = paste("model_fits/Fit_l_nosem_crit.Rda"))


# without syn
m_s_nosyn <- brm(rt ~ 1 + sem + syn:sem +
                   (1 | participant) +
                   (1 | item),
                 data = crit_trim,
                 family = lognormal(),
                 prior = priors_s_tr,
                 warmup = 2000,
                 iter = 20000,
                 cores = 4,
                 save_pars = save_pars(all = TRUE)
)
save(m_s_nosyn, file = paste("model_fits/Fit_s_nosyn_crit.Rda"))

m_m_nosyn <- brm(rt ~ 1 + sem + syn:sem +
                    (1 | participant) +
                    (1 | item),
                  data = crit_trim,
                  family = lognormal(),
                  prior = priors_m_tr,
                  warmup = 2000,
                  iter = 20000,
                  cores = 4,
                  save_pars = save_pars(all = TRUE)
 )
save(m_m_nosyn, file = paste("model_fits/Fit_m_nosyn_crit.Rda"))

m_l_nosyn <- brm(rt ~ 1 + sem + syn:sem +
                    (1 | participant) +
                    (1 | item),
                  data = crit_trim,
                  family = lognormal(),
                  prior = priors_l_tr,
                  warmup = 2000,
                  iter = 20000,
                  cores = 4,
                  save_pars = save_pars(all = TRUE)
 )
save(m_l_nosyn, file = paste("model_fits/Fit_l_nosyn_crit.Rda"))

# without interaction
m_s_noint <- brm(rt ~ 1 + sem + sem +
                   (1 | participant) +
                   (1 | item),
                 data = crit_trim,
                 family = lognormal(),
                 prior = priors_s_tr,
                 warmup = 2000,
                 iter = 20000,
                 cores = 4,
                 save_pars = save_pars(all = TRUE)
)
save(m_s_noint, file = paste("model_fits/Fit_s_noint_crit.Rda"))

m_m_noint <- brm(rt ~ 1 + sem + sem +
                   (1 | participant) +
                   (1 | item),
                 data = crit_trim,
                 family = lognormal(),
                 prior = priors_m_tr,
                 warmup = 2000,
                 iter = 20000,
                 cores = 4,
                 save_pars = save_pars(all = TRUE)
)
save(m_m_noint, file = paste("model_fits/Fit_m_noint_crit.Rda"))

m_l_noint <- brm(rt ~ 1 + sem + sem +
                   (1 | participant) +
                   (1 | item),
                 data = crit_trim,
                 family = lognormal(),
                 prior = priors_l_tr,
                 warmup = 2000,
                 iter = 20000,
                 cores = 4,
                 save_pars = save_pars(all = TRUE)
)
save(m_l_noint, file = paste("model_fits/Fit_l_noint_crit.Rda"))
 
load("model_fits/Fit_s_full_crit.Rda")
load("model_fits/Fit_s_nosem_crit.Rda")
load("model_fits/Fit_s_nosyn_crit.Rda")
load("model_fits/Fit_s_noint_crit.Rda")

load("model_fits/Fit_m_full_crit.Rda")
load("model_fits/Fit_m_nosem_crit.Rda")
load("model_fits/Fit_m_nosyn_crit.Rda")
load("model_fits/Fit_m_noint_crit.Rda")

load("model_fits/Fit_l_full_crit.Rda")
load("model_fits/Fit_l_nosem_crit.Rda")
load("model_fits/Fit_l_nosyn_crit.Rda")
load("model_fits/Fit_l_noint_crit.Rda")

# bfs
bf_s_sem1 <- bayes_factor(m_s_full, m_s_nosem)$bf
bf_s_sem2 <- bayes_factor(m_s_full, m_s_nosem)$bf
bf_s_syn1 <- bayes_factor(m_s_full, m_s_nosyn)$bf
bf_s_syn2 <- bayes_factor(m_s_full, m_s_nosyn)$bf
bf_s_int1 <- bayes_factor(m_s_full, m_s_noint)$bf
bf_s_int2 <- bayes_factor(m_s_full, m_s_noint)$bf

bf_m_sem1 <- bayes_factor(m_m_full, m_m_nosem)$bf
bf_m_sem2 <- bayes_factor(m_m_full, m_m_nosem)$bf
bf_m_syn1 <- bayes_factor(m_m_full, m_m_nosyn)$bf
bf_m_syn2 <- bayes_factor(m_m_full, m_m_nosyn)$bf
bf_m_int1 <- bayes_factor(m_m_full, m_m_noint)$bf
bf_m_int2 <- bayes_factor(m_m_full, m_m_noint)$bf

bf_l_sem1 <- bayes_factor(m_l_full, m_l_nosem)$bf
bf_l_sem2 <- bayes_factor(m_l_full, m_l_nosem)$bf
bf_l_syn1 <- bayes_factor(m_l_full, m_l_nosyn)$bf
bf_l_syn2 <- bayes_factor(m_l_full, m_l_nosyn)$bf
bf_l_int1 <- bayes_factor(m_l_full, m_l_noint)$bf
bf_l_int2 <- bayes_factor(m_l_full, m_l_noint)$bf

# save BFs in df
df.bf <- data.frame(matrix(ncol=6,nrow=0))
colnames(df.bf) <- c("Region", "Effect","Prior","truncated", "BF10.1", "BF10.2")
df.bf[nrow(df.bf)+1,] <- c("critical", "semantic","Normal(0, 0.01)", "yes", round(bf_s_sem1,2), round(bf_s_sem2,2))
df.bf[nrow(df.bf)+1,] <- c("critical", "semantic","Normal(0, 0.05)","yes", round(bf_m_sem1,2), round(bf_m_sem2,2))
df.bf[nrow(df.bf)+1,] <- c("critical", "semantic","Normal(0, 0.1)","yes", round(bf_l_sem1,2), round(bf_l_sem2,2))

df.bf[nrow(df.bf)+1,] <- c("critical", "syntactic","Normal(0, 0.01)", "yes", round(bf_s_syn1,2), round(bf_s_syn2,2))
df.bf[nrow(df.bf)+1,] <- c("critical", "syntactic","Normal(0, 0.05)","yes", round(bf_m_syn1,2), round(bf_m_syn2,2))
df.bf[nrow(df.bf)+1,] <- c("critical", "syntactic","Normal(0, 0.1)","yes", round(bf_l_syn1,2), round(bf_l_syn2,2))

df.bf[nrow(df.bf)+1,] <- c("critical", "interaction","Normal(0, 0.01)", "yes", round(bf_s_int1,2), round(bf_s_int2,2))
df.bf[nrow(df.bf)+1,] <- c("critical", "interaction","Normal(0, 0.05)","yes", round(bf_m_int1,2), round(bf_m_int2,2))
df.bf[nrow(df.bf)+1,] <- c("critical", "interaction","Normal(0, 0.1)","yes", round(bf_l_int1,2), round(bf_l_int2,2))

save(df.bf, file = paste("BFs_spr_pooled_774_crit.Rda"))