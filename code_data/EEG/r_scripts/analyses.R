# author: Pia Schoknecht
# date: June 14, 2024
# R version: 4.3.3

#load packages
library(tidyverse)
library(brms)
library(bayesplot)
library(ggplot2)

#load data
prestim <- read_csv("data/Pandora_mean_200_0.csv")
n400 <- read_csv("data/Pandora_mean_300_500.csv")

# remove subjects who have only one good session
xtabs(~ session + subject, n400)
n400.2 <- n400 %>% filter(subject != "0001" & subject != "0005" & subject != "0006" & subject != "0011" & subject != "0021"
                      & subject != "0040" & subject != "0041" & subject != "0055" & subject != "0062" & subject != "0067"
                      & subject != "0069" & subject != "0079" & subject != "0085" & subject != "0089" & subject != "0094"
                      & subject != "0095" & subject != "0106" & subject != "0122" & subject != "0123" & subject != "0129"
                      & subject != "0150")
n400.2$subject <- factor(n400.2$subject)

prestim.2 <- prestim %>% filter(subject != "0001" & subject != "0005" & subject != "0006" & subject != "0011" & subject != "0021"
                                & subject != "0040" & subject != "0041" & subject != "0055" & subject != "0062" & subject != "0067"
                                & subject != "0069" & subject != "0079" & subject != "0085" & subject != "0089" & subject != "0094"
                                & subject != "0095" & subject != "0106" & subject != "0122" & subject != "0123" & subject != "0129"
                                & subject != "0150")
prestim.2$subject <- factor(prestim.2$subject)

# how many trials were artefact-free?
data_new <- n400.2[!duplicated(n400.2[, c("subject", "cond", "item")]), ]
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
n400.3 <- anti_join(n400.2, lows, by="subject")
prestim.3 <- anti_join(prestim.2, lows, by="subject")


# Number of subjects
n400.3 %>%
  distinct(subject) %>%
  count()
# 103 good subjects (July 2023)
# good =
# completed both sessions
# accuracy at least 70% in both sessions
# at least 20 trials per condition

# rename "mean" column
prestim.4 <- prestim.3 %>% dplyr::rename(prestim_amp = mean)

# join n400 and prestim time window
eeg <- left_join(n400.3, prestim.4, by=c("subject", "session", "item", "channel", "cond"))


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
                      dplyr::summarise(across(c(amplitude, prestim_amp), mean))

# contrast coding

## Trigger 225 is condition low syntactic / low semantic interference
## Trigger 235 is condition low syntactic / high semantic interference
## Trigger 245 is condition high syntactic / low semantic interference
## Trigger 255 is condition high syntactic / high semantic interference

eeg_cp_agg$sem <- ifelse(eeg_cp_agg$cond %in% c("235", "255"), 0.5, -0.5)
eeg_cp_agg$syn <- ifelse(eeg_cp_agg$cond %in% c("245", "255"), 0.5, -0.5)

#write.csv(eeg_cp_agg, "data_eeg.csv")
#eeg_cp_agg <- read.csv("data_eeg.csv")

# varying priors 
# truncated (only negative effects)
priors_s_tr <- c(
  prior(normal(2, 5), class = Intercept),
  prior(normal(0, 0.1), class = b, ub=0),
  prior(normal(10, 5), class = sigma),
  prior(normal(0, 2), class = sd)
)  

priors_m_tr <- c(
  prior(normal(2, 5), class = Intercept),
  prior(normal(0, 0.5), class = b, ub=0),
  prior(normal(10, 5), class = sigma),
  prior(normal(0, 2), class = sd)
  )  

priors_l_tr <- c(
  prior(normal(2, 5), class = Intercept),
  prior(normal(0, 1), class = b, ub=0),
  prior(normal(10, 5), class = sigma),
  prior(normal(0, 2), class = sd)
)  

priors_xl_tr <- c(
  prior(normal(2, 5), class = Intercept),
  prior(normal(0, 5), class = b, ub=0),
  prior(normal(10, 5), class = sigma),
  prior(normal(0, 2), class = sd)
)  

# check priors
extraDistr::qtnorm(0.025,mean=2, sd=5) # intercept normal(2, 5) lower bound
extraDistr::qtnorm(0.975,mean=2, sd=5) # intercept normal(2, 5) upper bound
extraDistr::qtnorm(0.025,mean=0, sd=.1,b=0) # normal-(0, 0.1)
extraDistr::qtnorm(0.025,mean=0, sd=.5,b=0) # normal-(0, 0.5)
extraDistr::qtnorm(0.025,mean=0, sd=1,b=0)  # normal-(0, 1)
extraDistr::qtnorm(0.025,mean=0, sd=5,b=0)  # normal-(0, 5)

# full models with truncated priors
m_s_tr <- brm(amplitude ~ 1 + prestim_amp + syn*sem + (1+syn*sem|subject) + (1+syn*sem|item),
                 prior = priors_s_tr,
                 warmup = 2000,
                 iter = 20000,
                 cores = 4,
                 control = list(adapt_delta = 0.9),
                 save_pars = save_pars(all = TRUE),
                 data = eeg_cp_agg, 
                 sample_prior="yes")
save(m_s_tr, file = paste("model_fits/Fit_s_tr.Rda"))

m_m_tr <- brm(amplitude ~ 1 + prestim_amp + syn*sem + (1+syn*sem|subject) + (1+syn*sem|item),
              prior = priors_m_tr,
              warmup = 2000,
              iter = 20000,
              cores = 4,
              control = list(adapt_delta = 0.9),
              save_pars = save_pars(all = TRUE),
              data = eeg_cp_agg, 
              sample_prior="yes")
save(m_m_tr, file = paste("model_fits/Fit_m_tr.Rda"))

m_l_tr <- brm(amplitude ~ 1 + prestim_amp + syn*sem + (1+syn*sem|subject) + (1+syn*sem|item),
              prior = priors_l_tr,
              warmup = 2000,
              iter = 20000,
              cores = 4,
              control = list(adapt_delta = 0.9),
              save_pars = save_pars(all = TRUE),
              data = eeg_cp_agg, 
              sample_prior="yes")
save(m_l_tr, file = paste("model_fits/Fit_l_tr.Rda"))

m_xl_tr <- brm(amplitude ~ 1 + prestim_amp + syn*sem + (1+syn*sem|subject) + (1+syn*sem|item),
              prior = priors_xl_tr,
              warmup = 2000,
              iter = 20000,
              cores = 4,
              control = list(adapt_delta = 0.9),
              save_pars = save_pars(all = TRUE),
              data = eeg_cp_agg, 
              sample_prior="yes")
save(m_xl_tr, file = paste("model_fits/Fit_xl_tr.Rda"))


load("model_fits/Fit_s_tr.Rda")
load("model_fits/Fit_m_tr.Rda")
load("model_fits/Fit_l_tr.Rda")
load("model_fits/Fit_xl_tr.Rda")

## Posterior predictive check
# for our favorite model
pp_check(m_m_tr, ndraws = 100, type = "dens_overlay")
## Plot posterior predictive distribution of statistical summaries:
pp_check(m_m_tr, ndraws = 100, type = "stat", stat = "mean")
pp_check(m_m_tr, ndraws = 100, type = "stat", stat = "min")
pp_check(m_m_tr, ndraws = 100, type = "stat", stat = "max")

plot(m_m_tr)
summary(m_m_tr)

samples <- as_draws_df(m_m_tr)
syn_eff <- exp(samples$b_Intercept + samples$b_syn) -
  exp(samples$b_Intercept- samples$b_syn)

sem_eff <- exp(samples$b_Intercept + samples$b_sem) -
  exp(samples$b_Intercept- samples$b_sem)

int_eff <- exp(samples$b_Intercept + samples$`b_syn:sem`) - 
  exp(samples$b_Intercept- samples$`b_syn:sem`)

round(c(mean = mean(syn_eff), quantile(syn_eff, probs = c(.025, .975))),2)
round(c(mean = mean(sem_eff), quantile(sem_eff, probs = c(.025, .975))),2)
round(c(mean = mean(int_eff), quantile(int_eff, probs = c(.025, .975))),2)

samples$syntactic <- syn_eff
samples$semantic <- sem_eff
samples$interaction <- int_eff

mcmc_areas(
  samples,
  pars = c("syntactic", "semantic", "interaction"),
  prob = 0.8, # 80% intervals
  #prob_outer = 0.99, # 99%
  point_est = "mean")+
  labs( x = "effect in microvolts")+
  vline_0(linetype="dashed")+
  scale_x_continuous(n.breaks = 5)+
  theme_bw(base_size=9)+
  annotate("text", x = -0.4, y = 3.75, label = "-0.37 — -0.16 — -0.02", size=2) +
  annotate("text", x = -0.57, y = 2.65, label = "-0.59 — -0.32 — -0.10", size=2) +
  annotate("text", x = -0.35, y = 1.7, label = "-0.44 — -0.14 — -0.01", size=2)
ggsave("plots/posteriors_eeg.jpg", width = 8, height = 6, units = "cm", dpi=300)

# bfs
# model_s
h <- hypothesis(m_s_tr, "syn = 0")
SD_bf10.m_s_syn <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_s_tr, "sem = 0")
SD_bf10.m_s_sem <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_s_tr, "syn:sem = 0")
SD_bf10.m_s_int <- 1 / h$hypothesis$Evid.Ratio

# model_m
h <- hypothesis(m_m_tr, "syn = 0")
SD_bf10.m_m_syn <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_m_tr, "sem = 0")
SD_bf10.m_m_sem <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_m_tr, "syn:sem = 0")
SD_bf10.m_m_int <- 1 / h$hypothesis$Evid.Ratio

# model_l
h <- hypothesis(m_l_tr, "syn = 0")
SD_bf10.m_l_syn <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_l_tr, "sem = 0")
SD_bf10.m_l_sem <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_l_tr, "syn:sem = 0")
SD_bf10.m_l_int <- 1 / h$hypothesis$Evid.Ratio

# model_xl
h <- hypothesis(m_xl_tr, "syn = 0")
SD_bf10.m_xl_syn <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_xl_tr, "sem = 0")
SD_bf10.m_xl_sem <- 1 / h$hypothesis$Evid.Ratio

h <- hypothesis(m_xl_tr, "syn:sem = 0")
SD_bf10.m_xl_int <- 1 / h$hypothesis$Evid.Ratio

# save BFs in df
df.bf_eeg <- data.frame(matrix(ncol=7,nrow=0))
colnames(df.bf_eeg) <- c("BF_method", "language", "region", "effect","prior","truncated", "BF10")

df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "N400", "syntactic","Normal(0, 0.1)","yes", round(SD_bf10.m_s_syn,2))
df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "N400", "syntactic","Normal(0, 0.5)","yes", round(SD_bf10.m_m_syn,2))
df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "N400", "syntactic","Normal(0, 1)","yes", round(SD_bf10.m_l_syn,2))
df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "N400", "syntactic","Normal(0, 5)","yes", round(SD_bf10.m_xl_syn,2))

df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "N400", "semantic","Normal(0, 0.1)","yes", round(SD_bf10.m_s_sem,2))
df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "N400", "semantic","Normal(0, 0.5)","yes", round(SD_bf10.m_m_sem,2))
df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "N400", "semantic","Normal(0, 1)","yes", round(SD_bf10.m_l_sem,2))
df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "N400", "semantic","Normal(0, 5)","yes", round(SD_bf10.m_xl_sem,2))

df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "N400", "interaction","Normal(0, 0.1)","yes", round(SD_bf10.m_s_int,2))
df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "N400", "interaction","Normal(0, 0.5)","yes", round(SD_bf10.m_m_int,2))
df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "N400", "interaction","Normal(0, 1)","yes", round(SD_bf10.m_l_int,2))
df.bf_eeg[nrow(df.bf_eeg)+1,] <- c("S-D", "German", "N400", "interaction","Normal(0, 5)","yes", round(SD_bf10.m_xl_int,2))

save(df.bf_eeg, file = paste("BFs_eeg_critN400.Rda"))

df.bf_eeg$BF10 <- as.numeric(df.bf_eeg$BF10)
df.bf_eeg$effect <- factor(df.bf_eeg$effect, levels=c("semantic", "syntactic", "interaction"))


df.bf_eeg$prior2 <- ifelse(df.bf_eeg$prior == "Normal(0, 0.1)", "Normal_(0, 0.1)", (
                          ifelse(df.bf_eeg$prior == "Normal(0, 0.5)", "Normal_(0, 0.5)",(
                               ifelse(df.bf_eeg$prior == "Normal(0, 1)", "Normal_(0, 1)",
                                  "Normal_(0, 5)")))))

# Plot different BFs
ggplot(df.bf_eeg, aes(x = prior2, y = BF10, group = interaction(effect, truncated))) +
  geom_hline(yintercept = 1, linetype="dashed") +
  geom_point(aes(color=effect)) +
  geom_line(aes(color=effect)) +
  theme_bw(base_size = 12)+
  theme(legend.position = c(0.85, 0.85))+
  scale_y_log10("Bayes factor (BF10)",
                breaks =  c(0.05, 0.1, 0.2, 0.35, 0.5, 0.8, 1, 2, 5, 10, 80),
                labels = c(0.05, 0.1, 0.2, 0.35, 0.5, 0.8, 1, 2, 5, 10, 80)) +
  xlab("Truncated Prior")

ggsave("plots/BF_plot_N103_cp_300_500.jpg", width = 16, height = 12, units = "cm", dpi=300)

