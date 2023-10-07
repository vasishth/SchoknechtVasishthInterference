# author: Pia Schoknecht
# 27.07.2023

library(tidyverse)
library(brms)
library(bayesplot)
library(ggplot2)
library(ggbreak)
library(ggimage) 
library(cowplot)



### Plot Posteriors ###

# load model fits
load("model_fits/Fit_m_Full_precrit.Rda")
m_precrit <- m_m_full
load("model_fits/Fit_m_Full_crit.Rda")
m_crit <- m_m_full
load("model_fits/Fit_m_Full_spill.Rda")
m_spill <- m_m_full

m_precrit
m_crit
m_spill

samples_precrit <- as_draws_df(m_precrit)
samples_crit <- as_draws_df(m_crit)
samples_spill <- as_draws_df(m_spill)

syn_eff_precrit <- exp(samples_precrit$b_Intercept + samples_precrit$b_syn) - exp(samples_precrit$b_Intercept - samples_precrit$b_syn)
sem_eff_precrit <- exp(samples_precrit$b_Intercept + samples_precrit$b_sem) - exp(samples_precrit$b_Intercept - samples_precrit$b_sem)
int_eff_precrit <- exp(samples_precrit$b_Intercept + samples_precrit[,4]) - exp(samples_precrit$b_Intercept - samples_precrit[,4])

round(c(mean = mean(syn_eff_precrit), quantile(syn_eff_precrit, probs = c(.025, .975))))
round(c(mean = mean(sem_eff_precrit), quantile(sem_eff_precrit, probs = c(.025, .975))))
round(c(mean = mean(int_eff_precrit[-1,]), quantile(int_eff_precrit[,1], probs = c(.025, .975))))

samples_precrit$syntactic <- syn_eff_precrit
samples_precrit$semantic <- sem_eff_precrit
samples_precrit$interaction <- int_eff_precrit


posteriors_precrit <- mcmc_areas(
                          samples_precrit,
                          pars = c("syntactic", "semantic", "interaction"),
                          prob = 0.8, # 80% intervals
                          #prob_outer = 0.99, # 99%
                          point_est = "mean")+
                          labs(title= "Pre-critical", x = "effect in ms")+
                          vline_0(linetype="dashed")+
                          xlim(0,35)+
                          #scale_x_continuous(n.breaks = 3)+
                          theme_bw(base_size=12) +
                          annotate("text", x = 7.5, y = 3.5, label = "0 — 1 — 2", size=3) +
                          annotate("text", x = 11.5, y = 2.35, label = "8 — 11 — 14", size=3) +
                          annotate("text", x = 7.5, y = 1.25, label = "1 — 7 — 13", size=3) 

syn_eff_crit <- exp(samples_crit$b_Intercept + samples_crit$b_syn) - exp(samples_crit$b_Intercept - samples_crit$b_syn)
sem_eff_crit <- exp(samples_crit$b_Intercept + samples_crit$b_sem) - exp(samples_crit$b_Intercept - samples_crit$b_sem)
int_eff_crit <- exp(samples_crit$b_Intercept + samples_crit[,4]) - exp(samples_crit$b_Intercept - samples_crit[,4])

round(c(mean = mean(syn_eff_crit), quantile(syn_eff_crit, probs = c(.025, .975))))
round(c(mean = mean(sem_eff_crit), quantile(sem_eff_crit, probs = c(.025, .975))))
round(c(mean = mean(int_eff_crit[-1,]), quantile(int_eff_crit[,1], probs = c(.025, .975))))

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
                      xlim(0,35)+
                      #scale_x_continuous(n.breaks = 3)+
                      theme_bw(base_size=12) +
                      annotate("text", x = 9, y = 3.5, label = "0 — 2 — 4", size=3) +
                      annotate("text", x = 8.5, y = 2.65, label = "4 — 8 — 12", size=3) +
                      annotate("text", x = 14.5, y = 1.4, label = "7 — 14 — 22", size=3) 

syn_eff_spill <- exp(samples_spill$b_Intercept + samples_spill$b_syn) - exp(samples_spill$b_Intercept - samples_spill$b_syn)
sem_eff_spill <- exp(samples_spill$b_Intercept + samples_spill$b_sem) - exp(samples_spill$b_Intercept - samples_spill$b_sem)
int_eff_spill <- exp(samples_spill$b_Intercept + samples_spill[,4]) - exp(samples_spill$b_Intercept - samples_spill[,4])

round(c(mean = mean(syn_eff_spill), quantile(syn_eff_spill, probs = c(.025, .975))))
round(c(mean = mean(sem_eff_spill), quantile(sem_eff_spill, probs = c(.025, .975))))
round(c(mean = mean(int_eff_spill[-1,]), quantile(int_eff_spill[,1], probs = c(.025, .975))))

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
                        xlim(0,35)+
                        #scale_x_continuous(n.breaks = 3)+
                        theme_bw(base_size=12) +
                        annotate("text", x = 9, y = 3.5, label = "0 — 1 — 4", size=3) +
                        annotate("text", x = 8, y = 2.65, label = "5 — 8 — 11", size=3) +
                        annotate("text", x = 8, y = 1.4, label = "1 — 7 — 13", size=3)
                      

plot_grid(posteriors_precrit, posteriors_crit, posteriors_spill, ncol=3, labels="AUTO")
ggsave("plots/posteriors_spr_pooled_774.png", width = 20, height = 10, units = "cm", dpi=300)


### Plot BFs ###

# load region-wise BFs
load("BFs_spr_pooled_774_precrit.Rda")
bf_precrit <- df.bf
load("BFs_spr_pooled_774_crit.Rda")
bf_crit <- df.bf
load("BFs_spr_pooled_774_spill.Rda")
bf_spill <- df.bf

# combine
df.bf <- rbind(bf_precrit, bf_crit, bf_spill)
df.bf$Prior2 <- ifelse(df.bf$Prior == "Normal(0, 0.01)", "N+(0, 0.01)", 
                        ifelse(df.bf$Prior == "Normal(0, 0.05)", "N+(0, 0.05)", 
                              ("N+(0, 0.1)")))

df.bf$BF10 <- as.numeric(df.bf$BF10.1)
df.bf$Effect <- factor(df.bf$Effect, levels=c("semantic", "interaction", "syntactic"))
df.bf$Region <- factor(df.bf$Region, levels=c("precritical", "critical", "spillover"))

# Plot all BFs
# Plot different BFs
plot_BFs <- ggplot(df.bf, aes(x = Prior2, y = BF10, group = Effect)) +
  geom_point(aes(color=Effect)) +
  geom_line(aes(color=Effect)) +
  facet_grid(. ~Region)+
  geom_hline(yintercept = 1, linetype="dashed") +
  theme_bw(base_size = 12)+
  xlab("Truncated Prior")+
  scale_y_log10("Bayes factor (BF10)",
                breaks =  c(1/100, 1/10, 1 / 3, 1, 10, 50, 100, 500, 1000, 5000, 1000, 10000, 20000, 100000000, 500000000, 1000000000),
                labels =MASS::fractions(c(1/100, 1/10, 1 / 3, 1, 10, 50, 100, 500, 1000, 5000, 1000,10000, 20000,  100000000, 500000000, 1000000000))) +
  scale_y_break(c(23000, 4e+08),scales=0.2)

plot_BFs + theme(legend.position = "top")
  
ggsave("plots/BF_plot_spr_774_allregions.png", width = 24, height = 12, units = "cm", dpi=300)


# Plot different BFs
plot_BF_precrit <- ggplot(filter(df.bf, Region =="precritical"), aes(x = Prior, y = BF10, group = Effect)) +
                      geom_point(aes(color=Effect)) +
                      geom_line(aes(color=Effect)) +
                      geom_hline(yintercept = 1, linetype="dashed") +
                    
                      scale_y_log10("Bayes factor (BF10)",
                                    breaks =  c(1/100, 1/10, 1 / 3, 1,  100000000, 1000000000),
                                    labels =MASS::fractions(c(1/100, 1/10, 1 / 3, 1, 100000000, 1000000000))) +
                      theme_bw(base_size = 8)+
                      theme(legend.position = "none")+
                      xlab("Truncated Prior")
                    
## ggbreak plot
plot_BF_precrit2 <- plot_BF_precrit + scale_y_break(c(1.5, 1e+07), scales=1.5)
plot_BF_precrit2
ggsave("plots/BF_plot_spr_774_precrit.png", width = 12, height = 6, units = "cm", dpi=300)


plot_BF_crit <- ggplot(filter(df.bf, Region =="critical"), aes(x = Prior, y = BF10, group = Effect)) +
  geom_point(aes(color=Effect)) +
  geom_line(aes(color=Effect)) +
  geom_hline(yintercept = 1, linetype="dashed") +
  
  scale_y_log10("Bayes factor (BF10)",
                breaks =  c(1/100, 1/10, 1 / 3, 1, 20, 50, 200, 400, 1000, 1400),
                labels =MASS::fractions(c(1/100, 1/10, 1 / 3, 1, 20, 50,  200,400, 1000, 1400))) +
  theme_bw(base_size = 8)+
  theme(legend.position = "none")+
  xlab("Truncated Prior")

## ggbreak plot
plot_BF_crit2 <- plot_BF_crit + scale_y_break(c(50, 200), scales=1.5)
plot_BF_crit2
ggsave("plots/BF_plot_spr_774_crit.png", width = 12, height = 6, units = "cm", dpi=300)



plot_BF_spill <- ggplot(filter(df.bf, Region =="spillover"), aes(x = Prior, y = BF10, group = Effect)) +
  geom_point(aes(color=Effect)) +
  geom_line(aes(color=Effect)) +
  geom_hline(yintercept = 1, linetype="dashed") +
  
  scale_y_log10("Bayes factor (BF10)",
                breaks =  c(1/100, 1/10, 1 / 3, 1, 2, 5500, 8500,22000),
                labels =MASS::fractions(c(1/100, 1/10, 1 / 3, 1, 2, 5500, 8500, 22000))) +
  theme_bw(base_size = 8)+
  theme(legend.position = "none")+
  xlab("Truncated Prior")

## ggbreak plot
plot_BF_spill2 <- plot_BF_spill + scale_y_break(c(2, 5000), scales=1.5)
plot_BF_spill2
ggsave("plots/BF_plot_spr_774_spill.png", width = 12, height = 6, units = "cm", dpi=300)
