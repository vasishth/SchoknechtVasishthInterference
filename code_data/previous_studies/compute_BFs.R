# author: Pia Schoknecht
# date: 09.07.2024

# script to compute Bayes Factors with the Savage-Dickey method for Mertzen et al. (2023)
library(tidyverse)
library(brms)
library(ggplot2)
library(bayesplot)

# load English model fits
load("model_fits_E/Fit_s_Full_precrit.Rda")
load("model_fits_E/Fit_m_Full_precrit.Rda")
load("model_fits_E/Fit_l_Full_precrit.Rda")

load("model_fits_E/Fit_s_Full_crit.Rda")
load("model_fits_E/Fit_m_Full_crit.Rda")
load("model_fits_E/Fit_l_Full_crit.Rda")

load("model_fits_E/Fit_s_Full_postcrit.Rda")
load("model_fits_E/Fit_m_Full_postcrit.Rda")
load("model_fits_E/Fit_l_Full_postcrit.Rda")


#bfs 

compute_bfs <- function(model){
  h <- hypothesis(model, "syn = 0")
  bf10.syn <- 1 / h$hypothesis$Evid.Ratio
  
  h <- hypothesis(model, "sem = 0")
  bf10.sem <- 1 / h$hypothesis$Evid.Ratio
  
  h <- hypothesis(model, "syn:sem = 0")
  bf10.int <- 1 / h$hypothesis$Evid.Ratio
  
  return(c(bf10.syn, bf10.sem, bf10.int))
}

# save BFs in df
df.bf_m23 <- data.frame(matrix(ncol=8,nrow=0))
colnames(df.bf_m23) <- c("Experiment", "BF_method", "Language", "Region", "Effect","Prior","truncated", "BF10")

df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "English", "pre-critical", "syntactic","Normal(0, 0.01)","no", round(compute_bfs(m_s_full_precrit)[1],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "English", "pre-critical", "semantic","Normal(0, 0.01)","no", round(compute_bfs(m_s_full_precrit)[2],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "English", "pre-critical", "interaction","Normal(0, 0.01)","no", round(compute_bfs(m_s_full_precrit)[3],2))

df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "English", "pre-critical", "syntactic","Normal(0, 0.05)","no", round(compute_bfs(m_m_full_precrit)[1],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "English", "pre-critical", "semantic","Normal(0, 0.05)","no", round(compute_bfs(m_m_full_precrit)[2],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "English", "pre-critical", "interaction","Normal(0, 0.05)","no", round(compute_bfs(m_m_full_precrit)[3],2))

df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "English", "pre-critical", "syntactic","Normal(0, 0.1)","no", round(compute_bfs(m_l_full_precrit)[1],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "English", "pre-critical", "semantic","Normal(0, 0.1)","no", round(compute_bfs(m_l_full_precrit)[2],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "English", "pre-critical", "interaction","Normal(0, 0.1)","no", round(compute_bfs(m_l_full_precrit)[3],2))

df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "English", "critical", "syntactic","Normal(0, 0.01)","no", round(compute_bfs(m_s_full_crit)[1],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "English", "critical", "semantic","Normal(0, 0.01)","no", round(compute_bfs(m_s_full_crit)[2],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "English", "critical", "interaction","Normal(0, 0.01)","no", round(compute_bfs(m_s_full_crit)[3],2))

df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "English", "critical", "syntactic","Normal(0, 0.05)","no", round(compute_bfs(m_m_full_crit)[1],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "English", "critical", "semantic","Normal(0, 0.05)","no", round(compute_bfs(m_m_full_crit)[2],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "English", "critical", "interaction","Normal(0, 0.05)","no", round(compute_bfs(m_m_full_crit)[3],2))

df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "English", "critical", "syntactic","Normal(0, 0.1)","no", round(compute_bfs(m_l_full_crit)[1],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "English", "critical", "semantic","Normal(0, 0.1)","no", round(compute_bfs(m_l_full_crit)[2],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "English", "critical", "interaction","Normal(0, 0.1)","no", round(compute_bfs(m_l_full_crit)[3],2))

df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "English", "spill-over", "syntactic","Normal(0, 0.01)","no", round(compute_bfs(m_s_full_postcrit)[1],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "English", "spill-over", "semantic","Normal(0, 0.01)","no", round(compute_bfs(m_s_full_postcrit)[2],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "English", "spill-over", "interaction","Normal(0, 0.01)","no", round(compute_bfs(m_s_full_postcrit)[3],2))

df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "English", "spill-over", "syntactic","Normal(0, 0.05)","no", round(compute_bfs(m_m_full_postcrit)[1],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "English", "spill-over", "semantic","Normal(0, 0.05)","no", round(compute_bfs(m_m_full_postcrit)[2],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "English", "spill-over", "interaction","Normal(0, 0.05)","no", round(compute_bfs(m_m_full_postcrit)[3],2))

df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "English", "spill-over", "syntactic","Normal(0, 0.1)","no", round(compute_bfs(m_l_full_postcrit)[1],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "English", "spill-over", "semantic","Normal(0, 0.1)","no", round(compute_bfs(m_l_full_postcrit)[2],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "English", "spill-over", "interaction","Normal(0, 0.1)","no", round(compute_bfs(m_l_full_postcrit)[3],2))


# load German model fits
load("model_fits_G/Fit_s_Full_precrit.Rda")
load("model_fits_G/Fit_m_Full_precrit.Rda")
load("model_fits_G/Fit_l_Full_precrit.Rda")

load("model_fits_G/Fit_s_Full_crit.Rda")
load("model_fits_G/Fit_m_Full_crit.Rda")
load("model_fits_G/Fit_l_Full_crit.Rda")

load("model_fits_G/Fit_s_Full_postcrit.Rda")
load("model_fits_G/Fit_m_Full_postcrit.Rda")
load("model_fits_G/Fit_l_Full_postcrit.Rda")

df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "German", "pre-critical", "syntactic","Normal(0, 0.01)","no", round(compute_bfs(m_s_full_precrit)[1],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "German", "pre-critical", "semantic","Normal(0, 0.01)","no", round(compute_bfs(m_s_full_precrit)[2],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "German", "pre-critical", "interaction","Normal(0, 0.01)","no", round(compute_bfs(m_s_full_precrit)[3],2))

df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "German", "pre-critical", "syntactic","Normal(0, 0.05)","no", round(compute_bfs(m_m_full_precrit)[1],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "German", "pre-critical", "semantic","Normal(0, 0.05)","no", round(compute_bfs(m_m_full_precrit)[2],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "German", "pre-critical", "interaction","Normal(0, 0.05)","no", round(compute_bfs(m_m_full_precrit)[3],2))

df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "German", "pre-critical", "syntactic","Normal(0, 0.1)","no", round(compute_bfs(m_l_full_precrit)[1],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "German", "pre-critical", "semantic","Normal(0, 0.1)","no", round(compute_bfs(m_l_full_precrit)[2],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "German", "pre-critical", "interaction","Normal(0, 0.1)","no", round(compute_bfs(m_l_full_precrit)[3],2))

df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "German", "critical", "syntactic","Normal(0, 0.01)","no", round(compute_bfs(m_s_full_crit)[1],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "German", "critical", "semantic","Normal(0, 0.01)","no", round(compute_bfs(m_s_full_crit)[2],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "German", "critical", "interaction","Normal(0, 0.01)","no", round(compute_bfs(m_s_full_crit)[3],2))

df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "German", "critical", "syntactic","Normal(0, 0.05)","no", round(compute_bfs(m_m_full_crit)[1],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "German", "critical", "semantic","Normal(0, 0.05)","no", round(compute_bfs(m_m_full_crit)[2],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "German", "critical", "interaction","Normal(0, 0.05)","no", round(compute_bfs(m_m_full_crit)[3],2))

df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "German", "critical", "syntactic","Normal(0, 0.1)","no", round(compute_bfs(m_l_full_crit)[1],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "German", "critical", "semantic","Normal(0, 0.1)","no", round(compute_bfs(m_l_full_crit)[2],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "German", "critical", "interaction","Normal(0, 0.1)","no", round(compute_bfs(m_l_full_crit)[3],2))

df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "German", "spill-over", "syntactic","Normal(0, 0.01)","no", round(compute_bfs(m_s_full_postcrit)[1],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "German", "spill-over", "semantic","Normal(0, 0.01)","no", round(compute_bfs(m_s_full_postcrit)[2],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "German", "spill-over", "interaction","Normal(0, 0.01)","no", round(compute_bfs(m_s_full_postcrit)[3],2))

df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "German", "spill-over", "syntactic","Normal(0, 0.05)","no", round(compute_bfs(m_m_full_postcrit)[1],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "German", "spill-over", "semantic","Normal(0, 0.05)","no", round(compute_bfs(m_m_full_postcrit)[2],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "German", "spill-over", "interaction","Normal(0, 0.05)","no", round(compute_bfs(m_m_full_postcrit)[3],2))

df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "German", "spill-over", "syntactic","Normal(0, 0.1)","no", round(compute_bfs(m_l_full_postcrit)[1],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "German", "spill-over", "semantic","Normal(0, 0.1)","no", round(compute_bfs(m_l_full_postcrit)[2],2))
df.bf_m23[nrow(df.bf_m23)+1,] <- c("M23", "S-D", "German", "spill-over", "interaction","Normal(0, 0.1)","no", round(compute_bfs(m_l_full_postcrit)[3],2))


save(df.bf_m23, file = paste("BFs_mertzen23.Rda"))
