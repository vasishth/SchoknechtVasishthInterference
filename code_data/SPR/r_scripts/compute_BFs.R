# author: Pia Schoknecht
# date: 09.07.2024

# script to compute Bayes Factors with the Savage-Dickey method
library(tidyverse)
library(lme4)
library(brms)
library(ggplot2)
library(bayesplot)
library(truncnorm)
library(ggbreak)
library(ggimage) 
library(cowplot)

# varying priors 
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

load("model_fits/Fit_s_Full_precrit_slopes_symmetrical.Rda")
load("model_fits/Fit_m_Full_precrit_slopes_symmetrical.Rda")
load("model_fits/Fit_l_Full_precrit_slopes_symmetrical.Rda")


# bfs
# model_s
summary(m_s_full)
h <- hypothesis(m_s_full, "syn = 0")
SD_bf10.m_s_syn <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_s_full, "sem = 0")
SD_bf10.m_s_sem <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_s_full, "syn:sem = 0")
SD_bf10.m_s_int <- 1 / h$hypothesis$Evid.Ratio

# model_m
h <- hypothesis(m_m_full, "syn = 0")
SD_bf10.m_m_syn <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_m_full, "sem = 0")
SD_bf10.m_m_sem <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_m_full, "syn:sem = 0")
SD_bf10.m_m_int <- 1 / h$hypothesis$Evid.Ratio

# model_l
h <- hypothesis(m_l_full, "syn = 0")
SD_bf10.m_l_syn <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_l_full, "sem = 0")
SD_bf10.m_l_sem <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_l_full, "syn:sem = 0")
SD_bf10.m_l_int <- 1 / h$hypothesis$Evid.Ratio


# save BFs in df
df.bf_schoknecht <- data.frame(matrix(ncol=7,nrow=0))
colnames(df.bf_schoknecht) <- c("BF_method", "Language", "Region", "Effect","Prior","truncated", "BF10")

df.bf_schoknecht[nrow(df.bf_schoknecht)+1,] <- c("S-D", "German", "pre-critical", "syntactic","Normal(0, 0.01)","no", round(SD_bf10.m_s_syn,2))
df.bf_schoknecht[nrow(df.bf_schoknecht)+1,] <- c("S-D", "German", "pre-critical", "syntactic","Normal(0, 0.05)","no", round(SD_bf10.m_m_syn,2))
df.bf_schoknecht[nrow(df.bf_schoknecht)+1,] <- c("S-D", "German", "pre-critical", "syntactic","Normal(0, 0.1)","no", round(SD_bf10.m_l_syn,2))

df.bf_schoknecht[nrow(df.bf_schoknecht)+1,] <- c("S-D", "German", "pre-critical", "semantic","Normal(0, 0.01)","no", round(SD_bf10.m_s_sem,2))
df.bf_schoknecht[nrow(df.bf_schoknecht)+1,] <- c("S-D", "German", "pre-critical", "semantic","Normal(0, 0.05)","no", round(SD_bf10.m_m_sem,2))
df.bf_schoknecht[nrow(df.bf_schoknecht)+1,] <- c("S-D", "German", "pre-critical", "semantic","Normal(0, 0.1)","no", round(SD_bf10.m_l_sem,2))

df.bf_schoknecht[nrow(df.bf_schoknecht)+1,] <- c("S-D", "German", "pre-critical", "interaction","Normal(0, 0.01)","no", round(SD_bf10.m_s_int,2))
df.bf_schoknecht[nrow(df.bf_schoknecht)+1,] <- c("S-D", "German", "pre-critical", "interaction","Normal(0, 0.05)","no", round(SD_bf10.m_m_int,2))
df.bf_schoknecht[nrow(df.bf_schoknecht)+1,] <- c("S-D", "German", "pre-critical", "interaction","Normal(0, 0.1)","no", round(SD_bf10.m_l_int,2))
save(df.bf_schoknecht, file = paste("BFs_schoknecht_precrit.Rda"))

load("model_fits/Fit_s_Full_crit_slopes_symmetrical.Rda")
load("model_fits/Fit_m_Full_crit_slopes_symmetrical.Rda")
load("model_fits/Fit_l_Full_crit_slopes_symmetrical.Rda")

# bfs
# model_s
summary(m_s_full)
h <- hypothesis(m_s_full, "syn = 0")
SD_bf10.m_s_syn <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_s_full, "sem = 0")
SD_bf10.m_s_sem <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_s_full, "syn:sem = 0")
SD_bf10.m_s_int <- 1 / h$hypothesis$Evid.Ratio

# model_m
h <- hypothesis(m_m_full, "syn = 0")
SD_bf10.m_m_syn <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_m_full, "sem = 0")
SD_bf10.m_m_sem <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_m_full, "syn:sem = 0")
SD_bf10.m_m_int <- 1 / h$hypothesis$Evid.Ratio

# model_l
h <- hypothesis(m_l_full, "syn = 0")
SD_bf10.m_l_syn <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_l_full, "sem = 0")
SD_bf10.m_l_sem <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_l_full, "syn:sem = 0")
SD_bf10.m_l_int <- 1 / h$hypothesis$Evid.Ratio

# save BFs in df
load("BFs_schoknecht_precrit.Rda")
df.bf_schoknecht[nrow(df.bf_schoknecht)+1,] <- c("S-D", "German", "critical", "syntactic","Normal(0, 0.01)","no", round(SD_bf10.m_s_syn,2))
df.bf_schoknecht[nrow(df.bf_schoknecht)+1,] <- c("S-D", "German", "critical", "syntactic","Normal(0, 0.05)","no", round(SD_bf10.m_m_syn,2))
df.bf_schoknecht[nrow(df.bf_schoknecht)+1,] <- c("S-D", "German", "critical", "syntactic","Normal(0, 0.1)","no", round(SD_bf10.m_l_syn,2))

df.bf_schoknecht[nrow(df.bf_schoknecht)+1,] <- c("S-D", "German", "critical", "semantic","Normal(0, 0.01)","no", round(SD_bf10.m_s_sem,2))
df.bf_schoknecht[nrow(df.bf_schoknecht)+1,] <- c("S-D", "German", "critical", "semantic","Normal(0, 0.05)","no", round(SD_bf10.m_m_sem,2))
df.bf_schoknecht[nrow(df.bf_schoknecht)+1,] <- c("S-D", "German", "critical", "semantic","Normal(0, 0.1)","no", round(SD_bf10.m_l_sem,2))

df.bf_schoknecht[nrow(df.bf_schoknecht)+1,] <- c("S-D", "German", "critical", "interaction","Normal(0, 0.01)","no", round(SD_bf10.m_s_int,2))
df.bf_schoknecht[nrow(df.bf_schoknecht)+1,] <- c("S-D", "German", "critical", "interaction","Normal(0, 0.05)","no", round(SD_bf10.m_m_int,2))
df.bf_schoknecht[nrow(df.bf_schoknecht)+1,] <- c("S-D", "German", "critical", "interaction","Normal(0, 0.1)","no", round(SD_bf10.m_l_int,2))
save(df.bf_schoknecht, file = paste("BFs_schoknecht_crit.Rda"))


load("model_fits/Fit_s_Full_spill_slopes_symmetrical.Rda")
load("model_fits/Fit_m_Full_spill_slopes_symmetrical.Rda")
load("model_fits/Fit_l_Full_spill_slopes_symmetrical.Rda")

# bfs
# model_s
summary(m_s_full)
h <- hypothesis(m_s_full, "syn = 0")
SD_bf10.m_s_syn <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_s_full, "sem = 0")
SD_bf10.m_s_sem <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_s_full, "syn:sem = 0")
SD_bf10.m_s_int <- 1 / h$hypothesis$Evid.Ratio

# model_m
h <- hypothesis(m_m_full, "syn = 0")
SD_bf10.m_m_syn <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_m_full, "sem = 0")
SD_bf10.m_m_sem <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_m_full, "syn:sem = 0")
SD_bf10.m_m_int <- 1 / h$hypothesis$Evid.Ratio

# model_l
h <- hypothesis(m_l_full, "syn = 0")
SD_bf10.m_l_syn <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_l_full, "sem = 0")
SD_bf10.m_l_sem <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_l_full, "syn:sem = 0")
SD_bf10.m_l_int <- 1 / h$hypothesis$Evid.Ratio

# save BFs in df
load("BFs_schoknecht_crit.Rda")
df.bf_schoknecht[nrow(df.bf_schoknecht)+1,] <- c("S-D", "German", "spill-over", "syntactic","Normal(0, 0.01)","no", round(SD_bf10.m_s_syn,2))
df.bf_schoknecht[nrow(df.bf_schoknecht)+1,] <- c("S-D", "German", "spill-over", "syntactic","Normal(0, 0.05)","no", round(SD_bf10.m_m_syn,2))
df.bf_schoknecht[nrow(df.bf_schoknecht)+1,] <- c("S-D", "German", "spill-over", "syntactic","Normal(0, 0.1)","no", round(SD_bf10.m_l_syn,2))

df.bf_schoknecht[nrow(df.bf_schoknecht)+1,] <- c("S-D", "German", "spill-over", "semantic","Normal(0, 0.01)","no", round(SD_bf10.m_s_sem,2))
df.bf_schoknecht[nrow(df.bf_schoknecht)+1,] <- c("S-D", "German", "spill-over", "semantic","Normal(0, 0.05)","no", round(SD_bf10.m_m_sem,2))
df.bf_schoknecht[nrow(df.bf_schoknecht)+1,] <- c("S-D", "German", "spill-over", "semantic","Normal(0, 0.1)","no", round(SD_bf10.m_l_sem,2))

df.bf_schoknecht[nrow(df.bf_schoknecht)+1,] <- c("S-D", "German", "spill-over", "interaction","Normal(0, 0.01)","no", round(SD_bf10.m_s_int,2))
df.bf_schoknecht[nrow(df.bf_schoknecht)+1,] <- c("S-D", "German", "spill-over", "interaction","Normal(0, 0.05)","no", round(SD_bf10.m_m_int,2))
df.bf_schoknecht[nrow(df.bf_schoknecht)+1,] <- c("S-D", "German", "spill-over", "interaction","Normal(0, 0.1)","no", round(SD_bf10.m_l_int,2))

save(df.bf_schoknecht, file = paste("BFs_schoknecht.Rda"))
