library(tidyverse)
library(brms)

# load data
eeg <- read.csv("../eeg/data_eeg.csv")

# create 10 folds
# repeat the numbers 1:0 and add it as set_id to data frame
id <- rep(c(1:10), times=1197)
id2 <- id[1:11967]
eeg$set_id <- id2

# checks
xtabs(~  set_id + subject, data=eeg)
xtabs(~  set_id + item, data=eeg)


eeg %>% group_by(set_id) %>%
  distinct(subject) %>%
  count()
# --> each set includes all subjects

eeg %>% group_by(set_id) %>%
  distinct(item) %>%
  count()
# --> each set includes all items

eeg %>% group_by(set_id) %>%
  distinct(cond) %>%
  count()
# --> each set includes all conditions


# run model for each fold

# symmetrical priors 
priors_m <- c(
  prior(normal(0, 5), class = Intercept),
  prior(normal(0, 2), class = b, coef="amplitude_prestim"),
  prior(normal(0, 0.5), class = b, coef="syn"),
  prior(normal(0, 0.5), class = b, coef="sem"),
  prior(normal(0, 0.5), class = b, coef="syn:sem"),
  prior(normal(10, 5), class = sigma),
  prior(normal(0, 2), class = sd)
)  

m_wo1 <- brm(amplitude_n400 ~ 1 + amplitude_prestim + syn*sem + (1+syn*sem||subject) + (1+syn*sem||item),
                 prior = priors_m,
                 warmup = 2000,
                 iter = 10000,
                 cores = 4,
                 control = list(adapt_delta = 0.9),
                 save_pars = save_pars(all = TRUE),
                 data = subset(eeg, set_id != 1),
                 sample_prior="yes")
save(m_wo1, file = paste("kfold_model_fits/Fit_wo1.Rda"))

m_wo2 <- brm(amplitude_n400 ~ 1 + amplitude_prestim + syn*sem + (1+syn*sem||subject) + (1+syn*sem||item),
                 prior = priors_m,
                 warmup = 2000,
                 iter = 10000,
                 cores = 4,
                 control = list(adapt_delta = 0.9),
                 save_pars = save_pars(all = TRUE),
                 data = subset(eeg, set_id != 2),
                 sample_prior="yes")
save(m_wo2, file = paste("kfold_model_fits/Fit_wo2.Rda"))

m_wo3 <- brm(amplitude_n400 ~ 1 + amplitude_prestim + syn*sem + (1+syn*sem||subject) + (1+syn*sem||item),
             prior = priors_m,
             warmup = 2000,
             iter = 10000,
             cores = 4,
             control = list(adapt_delta = 0.9),
             save_pars = save_pars(all = TRUE),
             data = subset(eeg, set_id != 3),
             sample_prior="yes")
save(m_wo3, file = paste("kfold_model_fits/Fit_wo3.Rda"))

m_wo4 <- brm(amplitude_n400 ~ 1 + amplitude_prestim + syn*sem + (1+syn*sem||subject) + (1+syn*sem||item),
             prior = priors_m,
             warmup = 2000,
             iter = 10000,
             cores = 4,
             control = list(adapt_delta = 0.9),
             save_pars = save_pars(all = TRUE),
             data = subset(eeg, set_id != 4),
             sample_prior="yes")
save(m_wo4, file = paste("kfold_model_fits/Fit_wo4.Rda"))

m_wo5 <- brm(amplitude_n400 ~ 1 + amplitude_prestim + syn*sem + (1+syn*sem||subject) + (1+syn*sem||item),
             prior = priors_m,
             warmup = 2000,
             iter = 10000,
             cores = 4,
             control = list(adapt_delta = 0.9),
             save_pars = save_pars(all = TRUE),
             data = subset(eeg, set_id != 5),
             sample_prior="yes")
save(m_wo5, file = paste("kfold_model_fits/Fit_wo5.Rda"))

m_wo6 <- brm(amplitude_n400 ~ 1 + amplitude_prestim + syn*sem + (1+syn*sem||subject) + (1+syn*sem||item),
             prior = priors_m,
             warmup = 2000,
             iter = 10000,
             cores = 4,
             control = list(adapt_delta = 0.9),
             save_pars = save_pars(all = TRUE),
             data = subset(eeg, set_id != 6),
             sample_prior="yes")
save(m_wo6, file = paste("kfold_model_fits/Fit_wo6.Rda"))

m_wo7 <- brm(amplitude_n400 ~ 1 + amplitude_prestim + syn*sem + (1+syn*sem||subject) + (1+syn*sem||item),
             prior = priors_m,
             warmup = 2000,
             iter = 10000,
             cores = 4,
             control = list(adapt_delta = 0.9),
             save_pars = save_pars(all = TRUE),
             data = subset(eeg, set_id != 7),
             sample_prior="yes")
save(m_wo7, file = paste("kfold_model_fits/Fit_wo7.Rda"))

m_wo8 <- brm(amplitude_n400 ~ 1 + amplitude_prestim + syn*sem + (1+syn*sem||subject) + (1+syn*sem||item),
             prior = priors_m,
             warmup = 2000,
             iter = 10000,
             cores = 4,
             control = list(adapt_delta = 0.9),
             save_pars = save_pars(all = TRUE),
             data = subset(eeg, set_id != 8),
             sample_prior="yes")
save(m_wo8, file = paste("kfold_model_fits/Fit_wo8.Rda"))

m_wo9 <- brm(amplitude_n400 ~ 1 + amplitude_prestim + syn*sem + (1+syn*sem||subject) + (1+syn*sem||item),
             prior = priors_m,
             warmup = 2000,
             iter = 10000,
             cores = 4,
             control = list(adapt_delta = 0.9),
             save_pars = save_pars(all = TRUE),
             data = subset(eeg, set_id != 9),
             sample_prior="yes")
save(m_wo9, file = paste("kfold_model_fits/Fit_wo9.Rda"))

m_wo10 <- brm(amplitude_n400 ~ 1 + amplitude_prestim + syn*sem + (1+syn*sem||subject) + (1+syn*sem||item),
             prior = priors_m,
             warmup = 2000,
             iter = 10000,
             cores = 4,
             control = list(adapt_delta = 0.9),
             save_pars = save_pars(all = TRUE),
             data = subset(eeg, set_id != 10),
             sample_prior="yes")
save(m_wo10, file = paste("kfold_model_fits/Fit_wo10.Rda"))

# load k-fold model fits
load("kfold_model_fits/Fit_wo1.Rda")
load("kfold_model_fits/Fit_wo2.Rda")
load("kfold_model_fits/Fit_wo3.Rda")
load("kfold_model_fits/Fit_wo4.Rda")
load("kfold_model_fits/Fit_wo5.Rda")
load("kfold_model_fits/Fit_wo6.Rda")
load("kfold_model_fits/Fit_wo7.Rda")
load("kfold_model_fits/Fit_wo8.Rda")
load("kfold_model_fits/Fit_wo9.Rda")
load("kfold_model_fits/Fit_wo10.Rda")

# extract N400 estimates per model

# Model without fold 1
round(posterior_summary(m_wo1, variable = "b_syn"),2) # mean: -0.08, CrI [-0.27, 0.12] --> SE: 0.1
round(posterior_summary(m_wo1, variable = "b_sem"),2) # mean: -0.29, CrI [-0.49, -0.08] --> SE: 0.1
round(posterior_summary(m_wo1, variable = "b_syn:sem"),2) # mean: 0.02, CrI [-0.35, 0.39] --> SE: 0.19

# I'm changing the sign of the effect here (sem: -0.3 --> 0.3)
# to match the sign of the latency factor
# --> a positive effect represents are more negative N400 
synlower <- 0.08-(2*0.1) # mean effect - 2*SE
synupper <- 0.08+(2*0.1) # mean effect + 2*SE

semlower <- 0.29-(2*0.1) # mean effect - 2*SE
semupper <- 0.29+(2*0.1) # mean effect + 2*SE

synsemlower <- -0.02-(2*0.19) # mean effect - 2*SE
synsemupper <- -0.02+(2*0.19) # mean effect + 2*SE

# save N400 estimates in df
df.kfold.estimates <- data.frame(matrix(ncol=7,nrow=0))
colnames(df.kfold.estimates) <- c("held_out_set", "synlower", "synupper", "semlower","semupper","synsemlower", "synsemupper")
df.kfold.estimates[nrow(df.kfold.estimates)+1,] <- c("1", synlower, synupper, semlower, semupper, synsemlower, synsemupper)

# Model without fold 2
round(posterior_summary(m_wo2, variable = "b_syn"),2) # mean: -0.19, CrI [-0.39, 0.01] --> SE: 0.1
round(posterior_summary(m_wo2, variable = "b_sem"),2) # mean: -0.33, CrI [-0.54, -0.13] --> SE: 0.1
round(posterior_summary(m_wo2, variable = "b_syn:sem"),2) # mean: 0.13, CrI [-0.24, 0.51] --> SE: 0.19

# I'm changing the sign of the effect here (sem: -0.3 --> 0.3)
# to match the sign of the latency factor
# --> a positive effect represents are more negative N400 
synlower <- 0.19-(2*0.1) # mean effect - 2*SE
synupper <- 0.19+(2*0.1) # mean effect + 2*SE

semlower <- 0.33-(2*0.1) # mean effect - 2*SE
semupper <- 0.33+(2*0.1) # mean effect + 2*SE

synsemlower <- -0.13-(2*0.19) # mean effect - 2*SE
synsemupper <- -0.13+(2*0.19) # mean effect + 2*SE

# save N400 estimates in df
df.kfold.estimates[nrow(df.kfold.estimates)+1,] <- c("2", synlower, synupper, semlower, semupper, synsemlower, synsemupper)

# Model without fold 3
round(posterior_summary(m_wo3, variable = "b_syn"),2) # mean: -0.18, CrI [-0.38, 0.02] --> SE: 0.1
round(posterior_summary(m_wo3, variable = "b_sem"),2) # mean: -0.3, CrI [-0.51, -0.09] --> SE: 0.1
round(posterior_summary(m_wo3, variable = "b_syn:sem"),2) # mean: 0.12, CrI [-0.25, 0.49] --> SE: 0.19

# I'm changing the sign of the effect here (sem: -0.3 --> 0.3)
# to match the sign of the latency factor
# --> a positive effect represents are more negative N400 
synlower <- 0.18-(2*0.1) # mean effect - 2*SE
synupper <- 0.18+(2*0.1) # mean effect + 2*SE

semlower <- 0.3-(2*0.1) # mean effect - 2*SE
semupper <- 0.3+(2*0.1) # mean effect + 2*SE

synsemlower <- -0.12-(2*0.19) # mean effect - 2*SE
synsemupper <- -0.12+(2*0.19) # mean effect + 2*SE

# save N400 estimates in df
df.kfold.estimates[nrow(df.kfold.estimates)+1,] <- c("3", synlower, synupper, semlower, semupper, synsemlower, synsemupper)

# Model without fold 4
round(posterior_summary(m_wo4, variable = "b_syn"),2) # mean: -0.15, CrI [-0.35, 0.04] --> SE: 0.1
round(posterior_summary(m_wo4, variable = "b_sem"),2) # mean: -0.29, CrI [-0.51, -0.08] --> SE: 0.11
round(posterior_summary(m_wo4, variable = "b_syn:sem"),2) # mean: 0.03, CrI [-0.4, 0.33] --> SE: 0.18

# I'm changing the sign of the effect here (sem: -0.3 --> 0.3)
# to match the sign of the latency factor
# --> a positive effect represents are more negative N400 
synlower <- 0.15-(2*0.1) # mean effect - 2*SE
synupper <- 0.15+(2*0.1) # mean effect + 2*SE

semlower <- 0.29-(2*0.11) # mean effect - 2*SE
semupper <- 0.29+(2*0.11) # mean effect + 2*SE

synsemlower <- -0.03-(2*0.18) # mean effect - 2*SE
synsemupper <- -0.03+(2*0.18) # mean effect + 2*SE

# save N400 estimates in df
df.kfold.estimates[nrow(df.kfold.estimates)+1,] <- c("4", synlower, synupper, semlower, semupper, synsemlower, synsemupper)

# Model without fold 5
round(posterior_summary(m_wo5, variable = "b_syn"),2) # mean: -0.14, CrI [-0.34, 0.06] --> SE: 0.1
round(posterior_summary(m_wo5, variable = "b_sem"),2) # mean: -0.31, CrI [-0.51, -0.1] --> SE: 0.13
round(posterior_summary(m_wo5, variable = "b_syn:sem"),2) # mean: -0.07, CrI [-0.44, 0.29] --> SE: 0.18

# I'm changing the sign of the effect here (sem: -0.3 --> 0.3)
# to match the sign of the latency factor
# --> a positive effect represents are more negative N400 
synlower <- 0.14-(2*0.1) # mean effect - 2*SE
synupper <- 0.14+(2*0.1) # mean effect + 2*SE

semlower <- 0.31-(2*0.13) # mean effect - 2*SE
semupper <- 0.31+(2*0.13) # mean effect + 2*SE

synsemlower <- 0.07-(2*0.18) # mean effect - 2*SE
synsemupper <- 0.07+(2*0.18) # mean effect + 2*SE

# save N400 estimates in df
df.kfold.estimates[nrow(df.kfold.estimates)+1,] <- c("5", synlower, synupper, semlower, semupper, synsemlower, synsemupper)


# Model without fold 6
round(posterior_summary(m_wo6, variable = "b_syn"),2) # mean: -0.19, CrI [-0.39, 0.01] --> SE: 0.1
round(posterior_summary(m_wo6, variable = "b_sem"),2) # mean: -0.29, CrI [-0.51, -0.06] --> SE: 0.11
round(posterior_summary(m_wo6, variable = "b_syn:sem"),2) # mean: 0.13, CrI [-0.24, 0.51] --> SE: 0.19

# I'm changing the sign of the effect here (sem: -0.3 --> 0.3)
# to match the sign of the latency factor
# --> a positive effect represents are more negative N400 
synlower <- 0.19-(2*0.1) # mean effect - 2*SE
synupper <- 0.19+(2*0.1) # mean effect + 2*SE

semlower <- 0.29-(2*0.11) # mean effect - 2*SE
semupper <- 0.29+(2*0.11) # mean effect + 2*SE

synsemlower <- 0.13-(2*0.19) # mean effect - 2*SE
synsemupper <- 0.13+(2*0.19) # mean effect + 2*SE

# save N400 estimates in df
df.kfold.estimates[nrow(df.kfold.estimates)+1,] <- c("6", synlower, synupper, semlower, semupper, synsemlower, synsemupper)


# Model without fold 7
round(posterior_summary(m_wo7, variable = "b_syn"),2) # mean: -0.11, CrI [-0.31, 0.09] --> SE: 0.1
round(posterior_summary(m_wo7, variable = "b_sem"),2) # mean: -0.29, CrI [-0.49, -0.08] --> SE: 0.1
round(posterior_summary(m_wo7, variable = "b_syn:sem"),2) # mean: -0.06, CrI [-0.42, 0.32] --> SE: 0.19

# I'm changing the sign of the effect here (sem: -0.3 --> 0.3)
# to match the sign of the latency factor
# --> a positive effect represents are more negative N400 
synlower <- 0.11-(2*0.1) # mean effect - 2*SE
synupper <- 0.11+(2*0.1) # mean effect + 2*SE

semlower <- 0.29-(2*0.1) # mean effect - 2*SE
semupper <- 0.29+(2*0.1) # mean effect + 2*SE

synsemlower <- 0.06-(2*0.19) # mean effect - 2*SE
synsemupper <- 0.06+(2*0.19) # mean effect + 2*SE

# save N400 estimates in df
df.kfold.estimates[nrow(df.kfold.estimates)+1,] <- c("7", synlower, synupper, semlower, semupper, synsemlower, synsemupper)


# Model without fold 8
round(posterior_summary(m_wo8, variable = "b_syn"),2) # mean: -0.09, CrI [-0.29, 0.11] --> SE: 0.1
round(posterior_summary(m_wo8, variable = "b_sem"),2) # mean: -0.28, CrI [-0.5, -0.06] --> SE: 0.11
round(posterior_summary(m_wo8, variable = "b_syn:sem"),2) # mean: 0.01, CrI [-0.37, 0.38] --> SE: 0.19

# I'm changing the sign of the effect here (sem: -0.3 --> 0.3)
# to match the sign of the latency factor
# --> a positive effect represents are more negative N400 
synlower <- 0.09-(2*0.1) # mean effect - 2*SE
synupper <- 0.09+(2*0.1) # mean effect + 2*SE

semlower <- 0.28-(2*0.11) # mean effect - 2*SE
semupper <- 0.28+(2*0.11) # mean effect + 2*SE

synsemlower <- -0.01-(2*0.19) # mean effect - 2*SE
synsemupper <- -0.01+(2*0.19) # mean effect + 2*SE

# save N400 estimates in df
df.kfold.estimates[nrow(df.kfold.estimates)+1,] <- c("8", synlower, synupper, semlower, semupper, synsemlower, synsemupper)


# Model without fold 9
round(posterior_summary(m_wo9, variable = "b_syn"),2) # mean: -0.13, CrI [-0.33, 0.07] --> SE: 0.1
round(posterior_summary(m_wo9, variable = "b_sem"),2) # mean: -0.31, CrI [-0.52, -0.1] --> SE: 0.13
round(posterior_summary(m_wo9, variable = "b_syn:sem"),2) # mean: 0.03, CrI [-0.34, 0.4] --> SE: 0.1

# I'm changing the sign of the effect here (sem: -0.3 --> 0.3)
# to match the sign of the latency factor
# --> a positive effect represents are more negative N400 
synlower <- 0.13-(2*0.1) # mean effect - 2*SE
synupper <- 0.13+(2*0.1) # mean effect + 2*SE

semlower <- 0.31-(2*0.13) # mean effect - 2*SE
semupper <- 0.31+(2*0.13) # mean effect + 2*SE

synsemlower <- -0.03-(2*0.1) # mean effect - 2*SE
synsemupper <- -0.03+(2*0.1) # mean effect + 2*SE

# save N400 estimates in df
df.kfold.estimates[nrow(df.kfold.estimates)+1,] <- c("9", synlower, synupper, semlower, semupper, synsemlower, synsemupper)


# Model without fold 10
round(posterior_summary(m_wo10, variable = "b_syn"),2) # mean: -0.1, CrI [-0.3, 0.1] --> SE: 0.1
round(posterior_summary(m_wo10, variable = "b_sem"),2) # mean: -0.34, CrI [-0.55, -0.14] --> SE: 0.1
round(posterior_summary(m_wo10, variable = "b_syn:sem"),2) # mean: -0.01, CrI [-0.39, 0.35] --> SE: 0.19

# I'm changing the sign of the effect here (sem: -0.3 --> 0.3)
# to match the sign of the latency factor
# --> a positive effect represents are more negative N400 
synlower <- 0.1-(2*0.1) # mean effect - 2*SE
synupper <- 0.1+(2*0.1) # mean effect + 2*SE

semlower <- 0.34-(2*0.1) # mean effect - 2*SE
semupper <- 0.34+(2*0.1) # mean effect + 2*SE

synsemlower <- 0.01-(2*0.19) # mean effect - 2*SE
synsemupper <- 0.01+(2*0.19) # mean effect + 2*SE

# save N400 estimates in df
df.kfold.estimates[nrow(df.kfold.estimates)+1,] <- c("10", synlower, synupper, semlower, semupper, synsemlower, synsemupper)

write.csv(df.kfold.estimates, file="kfold_N400_estimates.csv")
