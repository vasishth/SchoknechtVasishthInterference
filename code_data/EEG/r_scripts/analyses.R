# author: Pia Schoknecht
# date: June 21, 2024
# R version: 4.3.3

#load packages
library(tidyverse)
library(brms)
library(bayesplot)
library(ggplot2)
library(cowplot)
library(extraDistr)

#### prepare ####
#load data
prestim <- read_csv("../data/Pandora_mean_200_0.csv")
n400 <- read_csv("../data/Pandora_mean_300_500.csv")
p600 <- read_csv("../data/Pandora_mean_600_900.csv")
 
# remove subjects who have only one good session
xtabs(~ session + subject, n400)
n400.2 <- n400 %>% filter(subject != "0001" & subject != "0005" & subject != "0006" & subject != "0011" & subject != "0021"
                           & subject != "0040" & subject != "0041" & subject != "0055" & subject != "0062" & subject != "0067"
                           & subject != "0069" & subject != "0079" & subject != "0085" & subject != "0089" & subject != "0094"
                           & subject != "0095" & subject != "0106" & subject != "0122" & subject != "0123" & subject != "0129"
                           & subject != "0150")
n400.2$subject <- factor(n400.2$subject)
xtabs(~ session + subject, n400.2)
 
p600.2 <- p600 %>% filter(subject != "0001" & subject != "0005" & subject != "0006" & subject != "0011" & subject != "0021"
                       & subject != "0040" & subject != "0041" & subject != "0055" & subject != "0062" & subject != "0067"
                       & subject != "0069" & subject != "0079" & subject != "0085" & subject != "0089" & subject != "0094"
                       & subject != "0095" & subject != "0106" & subject != "0122" & subject != "0123" & subject != "0129"
                       & subject != "0150")
p600.2$subject <- factor(p600.2$subject)
 
prestim.2 <- prestim %>% filter(subject != "0001" & subject != "0005" & subject != "0006" & subject != "0011" & subject != "0021"
                                 & subject != "0040" & subject != "0041" & subject != "0055" & subject != "0062" & subject != "0067"
                                 & subject != "0069" & subject != "0079" & subject != "0085" & subject != "0089" & subject != "0094"
                                 & subject != "0095" & subject != "0106" & subject != "0122" & subject != "0123" & subject != "0129"
                                 & subject != "0150")
prestim.2$subject <- factor(prestim.2$subject)
 
# how many trials were artefact-free?
data_new <- prestim.2[!duplicated(prestim.2[, c("subject", "cond", "item")]), ]
keptseg <- as.data.frame(table(data_new$subject, data_new$cond))
names(keptseg)[names(keptseg) == "Var1"] <- "subject"
names(keptseg)[names(keptseg) == "Var2"] <- "cond"
names(keptseg)[names(keptseg) == "Freq"] <- "num_seg"
plot(keptseg$subj, keptseg$num_seg, keptseg$cond)
 
# see how many subjects have less than 20 trials in any condition
low10 <- subset(keptseg, keptseg$num_seg < 20)
lows <- low10[!duplicated(low10[1]),]
 
write.csv(lows, file="TooManyArtefacts.csv", row.names = FALSE)
 
# exclude subjects with less than 20 trials per condition
prestim.3 <- anti_join(prestim.2, lows, by="subject")
n400.3 <- anti_join(n400.2, lows, by="subject")
p600.3 <- anti_join(p600.2, lows, by="subject")
 
# rename "mean" column for each time window
prestim.4 <- prestim.3 %>% dplyr::rename(amplitude_prestim = mean)
n400.4 <- n400.3 %>% dplyr::rename(amplitude_n400 = amplitude)
p600.4 <- p600.3 %>% dplyr::rename(amplitude_p600 = amplitude)
 
# join time windows (get rid of row names first)
prestim.4$...1 <- NULL
n400.4$...1 <- NULL
p600.4$...1 <- NULL
 
eeg <- left_join(prestim.4, n400.4,  by=c("subject", "session", "item", "channel", "cond")) %>%
                 inner_join(p600.4,  by=c("subject", "session", "item", "channel", "cond")) 
                 # inner_join because we want only trials were we have n400 and p600 time windows
 
# Number of subjects
eeg %>%
   distinct(subject) %>%
   count()
# 103 good subjects (July 2023)
# good =
# completed both sessions
# accuracy at least 70% in both sessions
# at least 20 trials per condition
 
# select region-of-interest
eeg_cp <- eeg  %>% filter(channel != "F3" & channel != "F4" & channel != "FC1" &
                                channel != "FC2" & channel != "FC5"  & channel != "FC6"  &
                                channel != "FCz"  & channel != "Fz" & channel != "O1" &
                                channel != "O2" & channel != "P7" & channel != "P8") %>%
 droplevels()
 
levels(factor(eeg_cp$channel))


# aggregate over channels
# (--> one mean value for the whole roi per subject, session, item and condition)
eeg_cp_agg <- eeg_cp %>% dplyr::group_by(subject, session, item, cond) %>%
                      dplyr::summarise(across(c(amplitude_prestim,amplitude_n400,amplitude_p600), mean))

eeg_cp_agg$roi <- "centro-parietal"

# contrast coding

# Trigger 225 is condition low syntactic / low semantic interference
# Trigger 235 is condition low syntactic / high semantic interference
# Trigger 245 is condition high syntactic / low semantic interference
# Trigger 255 is condition high syntactic / high semantic interference

eeg_cp_agg$sem <- ifelse(eeg_cp_agg$cond %in% c("235", "255"), 0.5, -0.5)
eeg_cp_agg$syn <- ifelse(eeg_cp_agg$cond %in% c("245", "255"), 0.5, -0.5)

write.csv(eeg_cp_agg, "data_eeg.csv", row.names = FALSE)
eeg_cp_agg <- read.csv("data_eeg.csv")

# varying symmetrical priors 
priors_s <- c(
  prior(normal(0, 5), class = Intercept),
  prior(normal(0, 2), class = b, coef="amplitude_prestim"),
  prior(normal(0, 0.1), class = b, coef="syn"),
  prior(normal(0, 0.1), class = b, coef="sem"),
  prior(normal(0, 0.1), class = b, coef="syn:sem"),
  prior(normal(10, 5), class = sigma),
  prior(normal(0, 2), class = sd)
)  

priors_m <- c(
  prior(normal(0, 5), class = Intercept),
  prior(normal(0, 2), class = b, coef="amplitude_prestim"),
  prior(normal(0, 0.5), class = b, coef="syn"),
  prior(normal(0, 0.5), class = b, coef="sem"),
  prior(normal(0, 0.5), class = b, coef="syn:sem"),
  prior(normal(10, 5), class = sigma),
  prior(normal(0, 2), class = sd)
  )  

priors_l <- c(
  prior(normal(0, 5), class = Intercept),
  prior(normal(0, 2), class = b, coef="amplitude_prestim"),
  prior(normal(0, 1), class = b, coef="syn"),
  prior(normal(0, 1), class = b, coef="sem"),
  prior(normal(0, 1), class = b, coef="syn:sem"),
  prior(normal(10, 5), class = sigma),
  prior(normal(0, 2), class = sd)
)  

priors_xl <- c(
  prior(normal(0, 5), class = Intercept),
  prior(normal(0, 2), class = b, coef="amplitude_prestim"),
  prior(normal(0, 2), class = b, coef="syn"),
  prior(normal(0, 2), class = b, coef="sem"),
  prior(normal(0, 2), class = b, coef="syn:sem"),
  prior(normal(10, 5), class = sigma),
  prior(normal(0, 2), class = sd)
)  


# check priors
extraDistr::qtnorm(0.025,mean=0, sd=5); extraDistr::qtnorm(0.975,mean=0, sd=5)# intercept normal(0, 5) upper and lower
extraDistr::qtnorm(0.025,mean=0, sd=.1); extraDistr::qtnorm(0.975,mean=0, sd=.1) # normal(0, 0.1) upper and lower
extraDistr::qtnorm(0.025,mean=0, sd=.5); extraDistr::qtnorm(0.975,mean=0, sd=.5) # normal(0, 0.5) upper and lower
extraDistr::qtnorm(0.025,mean=0, sd=1); extraDistr::qtnorm(0.975,mean=0, sd=1) # normal(0, 1) upper and lower
extraDistr::qtnorm(0.025,mean=0, sd=2); extraDistr::qtnorm(0.975,mean=0, sd=2) # normal(0, 2) upper and lower

#### N400 models ####
m_s_n4 <- brm(amplitude_n400 ~ 1 + amplitude_prestim + syn*sem + (1+syn*sem||subject) + (1+syn*sem||item),
                  prior = priors_s,
                  warmup = 2000,
                  iter = 20000,
                  cores = 4,
                  control = list(adapt_delta = 0.9),
                  save_pars = save_pars(all = TRUE),
                  data = eeg_cp_agg, 
                  sample_prior="yes")
save(m_s_n4, file = paste("model_fits/Fit_s_full_n4.Rda"))

m_m_n4 <- brm(amplitude_n400 ~ 1 + amplitude_prestim + syn*sem + (1+syn*sem||subject) + (1+syn*sem||item),
               prior = priors_m,
               warmup = 2000,
               iter = 20000,
               cores = 4,
               control = list(adapt_delta = 0.9),
               save_pars = save_pars(all = TRUE),
               data = eeg_cp_agg, 
               sample_prior="yes")
save(m_m_n4, file = paste("model_fits/Fit_m_full_n4.Rda"))

m_l_n4 <- brm(amplitude_n400 ~ 1 + amplitude_prestim + syn*sem + (1+syn*sem||subject) + (1+syn*sem||item),
              prior = priors_l,
              warmup = 2000,
              iter = 20000,
              cores = 4,
              control = list(adapt_delta = 0.9),
              save_pars = save_pars(all = TRUE),
              data = eeg_cp_agg, 
              sample_prior="yes")
save(m_l_n4, file = paste("model_fits/Fit_l_full_n4.Rda"))

m_xl_n4 <- brm(amplitude_n400 ~ 1 + amplitude_prestim + syn*sem + (1+syn*sem||subject) + (1+syn*sem||item),
               prior = priors_xl,
               warmup = 2000,
               iter = 20000,
               cores = 4,
               control = list(adapt_delta = 0.9),
               save_pars = save_pars(all = TRUE),
               data = eeg_cp_agg, 
               sample_prior="yes")
save(m_xl_n4, file = paste("model_fits/Fit_xl_full_n4.Rda"))

#### P600 models ####
m_s_p6 <- brm(amplitude_p600 ~ 1 + amplitude_prestim + syn*sem + (1+syn*sem||subject) + (1+syn*sem||item),
              prior = priors_s,
              warmup = 2000,
              iter = 20000,
              cores = 4,
              control = list(adapt_delta = 0.9),
              save_pars = save_pars(all = TRUE),
              data = eeg_cp_agg, 
              sample_prior="yes")
save(m_s_p6, file = paste("model_fits/Fit_s_full_p6.Rda"))

m_m_p6 <- brm(amplitude_p600 ~ 1 + amplitude_prestim + syn*sem + (1+syn*sem||subject) + (1+syn*sem||item),
              prior = priors_m,
              warmup = 2000,
              iter = 20000,
              cores = 4,
              control = list(adapt_delta = 0.9),
              save_pars = save_pars(all = TRUE),
              data = eeg_cp_agg, 
              sample_prior="yes")
save(m_m_p6, file = paste("model_fits/Fit_m_full_p6.Rda"))

m_l_p6 <- brm(amplitude_p600 ~ 1 + amplitude_prestim + syn*sem + (1+syn*sem||subject) + (1+syn*sem||item),
              prior = priors_l,
              warmup = 2000,
              iter = 20000,
              cores = 4,
              control = list(adapt_delta = 0.9),
              save_pars = save_pars(all = TRUE),
              data = eeg_cp_agg, 
              sample_prior="yes")
save(m_l_p6, file = paste("model_fits/Fit_l_full_p6.Rda"))

m_xl_p6 <- brm(amplitude_p600 ~ 1 + amplitude_prestim + syn*sem + (1+syn*sem||subject) + (1+syn*sem||item),
               prior = priors_xl,
               warmup = 2000,
               iter = 20000,
               cores = 4,
               control = list(adapt_delta = 0.9),
               save_pars = save_pars(all = TRUE),
               data = eeg_cp_agg, 
               sample_prior="yes")
save(m_xl_p6, file = paste("model_fits/Fit_xl_full_p6.Rda"))

#### load N400 models ####
load("model_fits/Fit_s_full_n4.Rda")
load("model_fits/Fit_m_full_n4.Rda")
load("model_fits/Fit_l_full_n4.Rda")
load("model_fits/Fit_xl_full_n4.Rda")

## Posterior predictive check
# for our favorite model
pp_check(m_m_n4, ndraws = 100, type = "dens_overlay")
## Plot posterior predictive distribution of statistical summaries:
pp_check(m_m_n4, ndraws = 100, type = "stat", stat = "mean")
pp_check(m_m_n4, ndraws = 100, type = "stat", stat = "min")
pp_check(m_m_n4, ndraws = 100, type = "stat", stat = "max")

# look at model
plot(m_m_n4)
summary(m_m_n4)

# posteriors
round(posterior_summary(m_m_n4, variable = "b_syn"),2)
round(posterior_summary(m_m_n4, variable = "b_sem"),2)
round(posterior_summary(m_m_n4, variable = "b_syn:sem"),2)

samples <- as_draws_df(m_m_n4)

posteriors_n400 <- mcmc_areas(
                      samples,
                      pars = c("b_syn", "b_sem", "b_syn:sem"),
                      prob = 0.8, # 80% intervals
                      #prob_outer = 0.99, # 99%
                      point_est = "mean")+
                      scale_y_discrete(labels = c("b_syn" = "syntactic",
                                   "b_sem" = "semantic", 
                                   "b_syn:sem" = "interaction"), 
                                   limits = c("b_syn:sem", "b_sem", "b_syn"))+
                      labs(title= "N400", x = "effect in microvolts")+
                      vline_0(linetype="dashed")+
                      scale_x_continuous(breaks=(seq(-1, 1, 0.2)), limits = c(-1, 1)) +
                      theme_bw(base_size=11)+
                      annotate("text", x = 0.55, y = 3.75, label = "-0.33 — -0.14 — 0.05", size=3) +
                      annotate("text", x = 0.55, y = 2.65, label = "-0.5 — -0.3 — -0.1", size=3)+
                      annotate("text", x = 0.55, y = 1.7, label = "-0.33 — 0.03 — 0.38", size=3)
ggsave("plots/posteriors_eeg_n400.jpg", width = 8, height = 6, units = "cm", dpi=300)

# bfs
# model_s
h <- hypothesis(m_s_n4, "syn = 0")
N4_bf10.m_s_syn <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_s_n4, "sem = 0")
N4_bf10.m_s_sem <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_s_n4, "syn:sem = 0")
N4_bf10.m_s_int <- 1 / h$hypothesis$Evid.Ratio

# model_m
h <- hypothesis(m_m_n4, "syn = 0")
N4_bf10.m_m_syn <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_m_n4, "sem = 0")
N4_bf10.m_m_sem <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_m_n4, "syn:sem = 0")
N4_bf10.m_m_int <- 1 / h$hypothesis$Evid.Ratio

# model_l
h <- hypothesis(m_l_n4, "syn = 0")
N4_bf10.m_l_syn <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_l_n4, "sem = 0")
N4_bf10.m_l_sem <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_l_n4, "syn:sem = 0")
N4_bf10.m_l_int <- 1 / h$hypothesis$Evid.Ratio

# model_xl
h <- hypothesis(m_xl_n4, "syn = 0")
N4_bf10.m_xl_syn <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_xl_n4, "sem = 0")
N4_bf10.m_xl_sem <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_xl_n4, "syn:sem = 0")
N4_bf10.m_xl_int <- 1 / h$hypothesis$Evid.Ratio

##### load P600 models ####
load("model_fits/Fit_s_full_p6.Rda")
load("model_fits/Fit_m_full_p6.Rda")
load("model_fits/Fit_l_full_p6.Rda")
load("model_fits/Fit_xl_full_p6.Rda")
 
## Posterior predictive check
# for our favorite models
pp_check(m_m_p6, ndraws = 100, type = "dens_overlay")
## Plot posterior predictive distribution of statistical summaries:
pp_check(m_m_p6, ndraws = 100, type = "stat", stat = "mean")
pp_check(m_m_p6, ndraws = 100, type = "stat", stat = "min")
pp_check(m_m_p6, ndraws = 100, type = "stat", stat = "max")

# look at model
plot(m_m_p6)
summary(m_m_p6)

# posteriors
round(posterior_summary(m_m_p6, variable = "b_syn"),2)
round(posterior_summary(m_m_p6, variable = "b_sem"),2)
round(posterior_summary(m_m_p6, variable = "b_syn:sem"),2)

samples <- as_draws_df(m_m_p6)

posteriors_p600 <- mcmc_areas(
                        samples,
                        pars = c("b_syn", "b_sem", "b_syn:sem"),
                        prob = 0.8, # 80% intervals
                        #prob_outer = 0.99, # 99%
                        point_est = "mean")+
                        scale_y_discrete(labels = c("b_syn" = "syntactic",
                                                    "b_sem" = "semantic", 
                                                    "b_syn:sem" = "interaction"), 
                                         limits = c("b_syn:sem", "b_sem", "b_syn"))+
                        labs(title= "P600", x = "effect in microvolts")+
                        vline_0(linetype="dashed")+
                        scale_x_continuous(breaks=(seq(-1, 1, 0.2)), limits = c(-1, 1)) +
                        theme_bw(base_size=11)+
                        annotate("text", x = 0.55, y = 3.75, label = "-0.22 — -0.08 — 0.06", size=3) +
                        annotate("text", x = 0.55, y = 2.65, label = "-0.37 — -0.21 — -0.05", size=3) +
                        annotate("text", x = 0.55, y = 1.7, label = "-0.48 — -0.2 — 0.08", size=3)
ggsave("plots/posteriors_eeg_p600.jpg", width = 8, height = 6, units = "cm", dpi=300)

# bfs
# model_s
h <- hypothesis(m_s_p6, "syn = 0")
P6_bf10.m_s_syn <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_s_p6, "sem = 0")
P6_bf10.m_s_sem <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_s_p6, "syn:sem = 0")
P6_bf10.m_s_int <- 1 / h$hypothesis$Evid.Ratio

# model_m
h <- hypothesis(m_m_p6, "syn = 0")
P6_bf10.m_m_syn <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_m_p6, "sem = 0")
P6_bf10.m_m_sem <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_m_p6, "syn:sem = 0")
P6_bf10.m_m_int <- 1 / h$hypothesis$Evid.Ratio

# model_l
h <- hypothesis(m_l_p6, "syn = 0")
P6_bf10.m_l_syn <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_l_p6, "sem = 0")
P6_bf10.m_l_sem <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_l_p6, "syn:sem = 0")
P6_bf10.m_l_int <- 1 / h$hypothesis$Evid.Ratio

# model_xl
h <- hypothesis(m_xl_p6, "syn = 0")
P6_bf10.m_xl_syn <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_xl_p6, "sem = 0")
P6_bf10.m_xl_sem <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_xl_p6, "syn:sem = 0")
P6_bf10.m_xl_int <- 1 / h$hypothesis$Evid.Ratio


# save BFs in df
df.bf_eeg <- data.frame(matrix(ncol=7,nrow=0))
colnames(df.bf_eeg) <- c("BF_method", "language", "region", "effect","prior","truncated", "BF10")
 
df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "N400", "syntactic","Normal(0, 0.1)","no", round(N4_bf10.m_s_syn,2))
df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "N400", "syntactic","Normal(0, 0.5)","no", round(N4_bf10.m_m_syn,2))
df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "N400", "syntactic","Normal(0, 1)","no", round(N4_bf10.m_l_syn,2))
df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "N400", "syntactic","Normal(0, 2)","no", round(N4_bf10.m_xl_syn,2))

df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "N400", "semantic","Normal(0, 0.1)","no", round(N4_bf10.m_s_sem,2))
df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "N400", "semantic","Normal(0, 0.5)","no", round(N4_bf10.m_m_sem,2))
df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "N400", "semantic","Normal(0, 1)","no", round(N4_bf10.m_l_sem,2))
df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "N400", "semantic","Normal(0, 2)","no", round(N4_bf10.m_xl_sem,2))

df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "N400", "interaction","Normal(0, 0.1)","no", round(N4_bf10.m_s_int,2))
df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "N400", "interaction","Normal(0, 0.5)","no", round(N4_bf10.m_m_int,2))
df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "N400", "interaction","Normal(0, 1)","no", round(N4_bf10.m_l_int,2))
df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "N400", "interaction","Normal(0, 2)","no", round(N4_bf10.m_xl_int,2))


df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "P600", "syntactic","Normal(0, 0.1)","no", round(P6_bf10.m_s_syn,2))
df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "P600", "syntactic","Normal(0, 0.5)","no", round(P6_bf10.m_m_syn,2))
df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "P600", "syntactic","Normal(0, 1)","no", round(P6_bf10.m_l_syn,2))
df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "P600", "syntactic","Normal(0, 2)","no", round(P6_bf10.m_xl_syn,2))
 
df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "P600", "semantic","Normal(0, 0.1)","no", round(P6_bf10.m_s_sem,2))
df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "P600", "semantic","Normal(0, 0.5)","no", round(P6_bf10.m_m_sem,2))
df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "P600", "semantic","Normal(0, 1)","no", round(P6_bf10.m_l_sem,2))
df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "P600", "semantic","Normal(0, 2)","no", round(P6_bf10.m_xl_sem,2))
 
df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "P600", "interaction","Normal(0, 0.1)","no", round(P6_bf10.m_s_int,2))
df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "P600", "interaction","Normal(0, 0.5)","no", round(P6_bf10.m_m_int,2))
df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "P600", "interaction","Normal(0, 1)","no", round(P6_bf10.m_l_int,2))
df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "P600", "interaction","Normal(0, 2)","no", round(P6_bf10.m_xl_int,2))
 
save(df.bf_eeg, file = paste("BFs_eeg_crit_N400_P600.Rda"))
load("BFs_eeg_crit_N400_P600.Rda")
 
df.bf_eeg$BF10 <- as.numeric(df.bf_eeg$BF10)
df.bf_eeg$effect <- factor(df.bf_eeg$effect, levels=c("semantic", "syntactic", "interaction"))
 
#### Plot posteriors for N400 and P600 together ####
plot_grid(posteriors_n400, posteriors_p600, ncol=2, labels="AUTO")
ggsave("plots/posteriors_eeg.jpg", width = 16, height = 6, units = "cm", dpi=300)


#### Plot BFs ####
ggplot(df.bf_eeg, aes(x = prior, y = BF10, group = effect)) +
   geom_hline(yintercept = 1, linetype="dashed") +
   geom_point(aes(color=effect)) +
   geom_line(aes(color=effect)) +
   facet_grid(. ~ region)+
   theme_bw(base_size = 12)+
   theme(legend.position = "top")+
   scale_y_log10("Bayes factor (BF10)",
                 breaks =  c(0.1, 0.25, 0.5, 1, 2, 3, 5, 10, 20),
                 labels = c(0.1, 0.25, 0.5, 1, 2, 3, 5, 10, 20)) +
   xlab("prior")
 
ggsave("plots/BF_plot_eeg.jpg", width = 21, height = 9, units = "cm", dpi=300)
 
