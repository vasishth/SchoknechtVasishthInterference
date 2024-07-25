# author: Pia Schoknecht
# 10.07.2024

library(tidyverse)
library(brms)
library(bayesplot)
library(ggplot2)
library(cowplot)

### Plot Posteriors ###

# Pre-critical region

# load model fits
load("E://model_fits/Fit_m_Full_precrit_slopes_symmetrical.Rda")
m_precrit <- m_m_full
rm(list = c("m_m_full"))

# extract samples
samples_precrit <- as_draws_df(m_precrit)
rm(list = c("m_precrit"))

# backtransform estimates to ms scale
syn_eff_precrit <- exp(samples_precrit$b_Intercept + samples_precrit$b_syn) - exp(samples_precrit$b_Intercept - samples_precrit$b_syn)
sem_eff_precrit <- exp(samples_precrit$b_Intercept + samples_precrit$b_sem) - exp(samples_precrit$b_Intercept - samples_precrit$b_sem)
int_eff_precrit <- exp(samples_precrit$b_Intercept + samples_precrit$`b_syn:sem`) - exp(samples_precrit$b_Intercept - samples_precrit$`b_syn:sem`)

# credible intervals
round(c(mean = mean(syn_eff_precrit), quantile(syn_eff_precrit, probs = c(.025, .975))))
round(c(mean = mean(sem_eff_precrit), quantile(sem_eff_precrit, probs = c(.025, .975))))
round(c(mean = mean(int_eff_precrit), quantile(int_eff_precrit, probs = c(.025, .975))))

# add back-transformed values to samples df
samples_precrit$syntactic <- syn_eff_precrit
samples_precrit$semantic <- sem_eff_precrit
samples_precrit$interaction <- int_eff_precrit

# Plot
posteriors_precrit <- mcmc_areas(
                          samples_precrit,
                          pars = c("syntactic", "semantic", "interaction"),
                          prob = 0.8, # 80% intervals
                          #prob_outer = 0.99, # 99%
                          point_est = "mean")+
                          labs(title= "Pre-critical", x = "effect in ms")+
                          vline_0(linetype="dashed")+
                          scale_x_continuous(breaks=(seq(-30, 50, 10)), limits = c(-30, 50)) +
                          theme_bw(base_size=12) +
                          annotate("text", x = 35, y = 3.8, label = "-4 — 3 — 10", size=3) +
                          annotate("text", x = 35, y = 2.8, label = "7 — 14 — 21", size=3) +
                          annotate("text", x = 35, y = 1.8, label = "-5 — 8 — 22", size=3) 

# Critical region
load("E://model_fits/Fit_m_Full_crit_slopes_symmetrical.Rda")
m_crit <- m_m_full
rm(list = c("m_m_full"))

samples_crit <- as_draws_df(m_crit)
rm(list = c("m_crit"))

syn_eff_crit <- exp(samples_crit$b_Intercept + samples_crit$b_syn) - exp(samples_crit$b_Intercept - samples_crit$b_syn)
sem_eff_crit <- exp(samples_crit$b_Intercept + samples_crit$b_sem) - exp(samples_crit$b_Intercept - samples_crit$b_sem)
int_eff_crit <- exp(samples_crit$b_Intercept + samples_crit$`b_syn:sem`) - exp(samples_crit$b_Intercept - samples_crit$`b_syn:sem`)

round(c(mean = mean(syn_eff_crit), quantile(syn_eff_crit, probs = c(.025, .975))))
round(c(mean = mean(sem_eff_crit), quantile(sem_eff_crit, probs = c(.025, .975))))
round(c(mean = mean(int_eff_crit), quantile(int_eff_crit, probs = c(.025, .975))))

samples_crit$syntactic <- syn_eff_crit
samples_crit$semantic <- sem_eff_crit
samples_crit$interaction <- int_eff_crit


posteriors_crit <- mcmc_areas(
                      samples_crit,
                      pars = c("syntactic", "semantic", "interaction"),
                      prob = 0.8, # 80% intervals
                      #prob_outer = 0.99, # 99%
                      point_est = "mean")+
                      labs(title= "Critical", x = "effect in ms")+
                      vline_0(linetype="dashed")+
                      scale_x_continuous(breaks=(seq(-30, 50, 10)), limits = c(-30, 50)) +
                      theme_bw(base_size=12) +
                      annotate("text", x = 35, y = 3.8, label = "-11 — -2 — 7", size=3) +
                      annotate("text", x = 35, y = 2.8, label = "1 — 10 — 19", size=3) +
                      annotate("text", x = 35, y = 1.8, label = "-3 — 14 — 31", size=3) 

# Spill-over region
load("E://model_fits/Fit_m_Full_spill_slopes_symmetrical.Rda")
m_spill <- m_m_full
rm(list = c("m_m_full"))

samples_spill <- as_draws_df(m_spill)
rm(list = c("m_spill"))

syn_eff_spill <- exp(samples_spill$b_Intercept + samples_spill$b_syn) - exp(samples_spill$b_Intercept - samples_spill$b_syn)
sem_eff_spill <- exp(samples_spill$b_Intercept + samples_spill$b_sem) - exp(samples_spill$b_Intercept - samples_spill$b_sem)
int_eff_spill <- exp(samples_spill$b_Intercept + samples_spill$`b_syn:sem`) - exp(samples_spill$b_Intercept - samples_spill$`b_syn:sem`)

round(c(mean = mean(syn_eff_spill), quantile(syn_eff_spill, probs = c(.025, .975))))
round(c(mean = mean(sem_eff_spill), quantile(sem_eff_spill, probs = c(.025, .975))))
round(c(mean = mean(int_eff_spill), quantile(int_eff_spill, probs = c(.025, .975))))

samples_spill$syntactic <- syn_eff_spill
samples_spill$semantic <- sem_eff_spill
samples_spill$interaction <- int_eff_spill


posteriors_spill <- mcmc_areas(
                        samples_spill,
                        pars = c("syntactic", "semantic", "interaction"),
                        prob = 0.8, # 80% intervals
                        #prob_outer = 0.99, # 99%
                        point_est = "mean")+
                        labs(title= "Spill-over", x = "effect in ms")+
                        vline_0(linetype="dashed")+
                        scale_x_continuous(breaks=(seq(-30, 50, 10)), limits = c(-30, 50)) +
                        theme_bw(base_size=12) +
                        annotate("text", x = 35, y = 3.8, label = "1 — 8 — 15", size=3) +
                        annotate("text", x = 35, y = 2.8, label = "7 — 14 — 21", size=3) +
                        annotate("text", x = 35, y = 1.8, label = "-6 — 8 — 21", size=3)
                      

plot_grid(posteriors_precrit, posteriors_crit, posteriors_spill, ncol=3, labels="AUTO")
ggsave("plots/posteriors_spr_pooled_774.png", width = 21, height = 10, units = "cm", dpi=300)

### Trial effect ###

trial_eff_precrit <- exp(samples_precrit$b_Intercept + samples_precrit$b_c_trial) - exp(samples_precrit$b_Intercept - samples_precrit$b_c_trial)
trial_eff_crit <- exp(samples_crit$b_Intercept + samples_crit$b_c_trial) - exp(samples_crit$b_Intercept - samples_crit$b_c_trial)
trial_eff_spill <- exp(samples_spill$b_Intercept + samples_spill$b_c_trial) - exp(samples_spill$b_Intercept - samples_spill$b_c_trial)

round(c(mean = mean(trial_eff_precrit), quantile(trial_eff_precrit, probs = c(.025, .975))))
round(c(mean = mean(trial_eff_crit), quantile(trial_eff_crit, probs = c(.025, .975))))
round(c(mean = mean(trial_eff_spill), quantile(trial_eff_spill, probs = c(.025, .975))))

# precrit
#mean  2.5% 97.5% 
#-219  -233  -204 

#crit
#mean  2.5% 97.5% 
#  -296  -317  -276 

#spill
# mean  2.5% 97.5% 
#-246  -260  -233 

### Plot BFs ###

# load BFs
load("BFs_schoknecht.Rda")

# prepare
df.bf_schoknecht$Prior2 <- ifelse(df.bf_schoknecht$Prior == "Normal(0, 0.01)", "N(0, 0.01)", 
                        ifelse(df.bf_schoknecht$Prior == "Normal(0, 0.05)", "N(0, 0.05)", 
                              ("N(0, 0.1)")))

df.bf_schoknecht$BF10 <- as.numeric(df.bf_schoknecht$BF10)
df.bf_schoknecht$effect <- factor(df.bf_schoknecht$Effect, levels=c("semantic", "syntactic", "interaction"))
df.bf_schoknecht$region <- factor(df.bf_schoknecht$Region, levels=c("pre-critical", "critical", "spill-over"))

# Plot all BFs
ggplot(df.bf_schoknecht, aes(x = Prior2, y = BF10, group = effect)) +
  geom_point(aes(color=effect)) +
  geom_line(aes(color=effect)) +
  facet_grid(. ~region)+
  geom_hline(yintercept = 1, linetype="dashed") +
  theme_bw(base_size = 12)+
  xlab("Prior")+
  theme(legend.position = "top")+
  scale_y_log10("Bayes factor (BF10)",
                breaks =  c(0.05, 0.1, 0.25, 0.5, 1, 2, 3, 5, 10, 25, 50, 100, 200 ),
                labels = c(0.05, 0.1, 0.25, 0.5, 1, 2, 3, 5, 10, 25, 50, 100, 200))

ggsave("plots/BF_plot_spr_774_allregions.png", width = 18, height = 10, units = "cm", dpi=300)
