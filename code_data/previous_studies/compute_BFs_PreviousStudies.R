# author: Pia Schoknecht
# date: 21.05.2024

# script to compute Bayes Factors for the previous studies

# Mertzen et al., (2024) English and German eye-tracking data
# Van Dyke (2007) English eye-tracking and self-paced reading data
# for comparability with self-paced reading times, we use the total fixation times (TFT) eye-tracking measure


library(tidyverse)
library(lme4)
library(brms)
library(ggplot2)
library(bayesplot)
library(truncnorm)
library(ggbreak)
library(ggimage) 
library(cowplot)


# load Mertzen et al. GERMAN data
df <- read.table("dataRetroDE_processed.txt")

df_crit <- df %>% filter(region == "crit") %>%
                  select(subject, trial, list, item, condition, roi, region, TFT) %>%
                  filter(TFT != 0)

# contrast coding
df_crit$syn <- ifelse(df_crit$condition%in%c("a","b"),-0.5,0.5)
df_crit$sem <- ifelse(df_crit$condition%in%c("b","d"),0.5,-0.5)

# trim all rts below 150 and above 3000 ms
range(df_crit$TFT)
crit_trim <- subset(df_crit, df_crit$TFT > 80)
range(crit_trim$TFT)


# Inferential statistics
# truncated varying priors 
# (only positive effects -- longer reading times for high interference)

priors_s_tr <- c(
  prior(normal(6, 0.6), class = Intercept),
  prior(normal(0, 0.01), class = b, lb=0),
  prior(normal(0, 0.5), class = sigma),
  prior(normal(0, 0.1), class = sd),
  prior(lkj(2), class = cor)
)  

priors_m_tr <- c(
  prior(normal(6, 0.6), class = Intercept),
  prior(normal(0, 0.05), class = b, lb=0),
  prior(normal(0, 0.5), class = sigma),
  prior(normal(0, 0.1), class = sd),
  prior(lkj(2), class = cor)
) 

priors_l_tr <- c(
  prior(normal(6, 0.6), class = Intercept),
  prior(normal(0, 0.1), class = b, lb=0),
  prior(normal(0, 0.5), class = sigma),
  prior(normal(0, 0.1), class = sd),
  prior(lkj(2), class = cor)
) 


# models
#directional priors
m_s_full <- brm(TFT ~ 1 + syn * sem +
                  (1 + syn * sem| subject) +
                  (1 + syn * sem| item),
               data = crit_trim,
               family = lognormal(),
               prior = priors_s_tr,
               warmup = 2000,
               iter = 20000,
               cores = 4,
               save_pars = save_pars(all = TRUE),
               sample_prior="yes"
               )
save(m_s_full, file = paste("model_fits_G/Fit_s_Full_crit.Rda"))

m_m_full <- brm(TFT ~ 1 + syn * sem +
                  (1 + syn * sem| subject) +
                  (1 + syn * sem| item),
                data = crit_trim,
                family = lognormal(),
                prior = priors_m_tr,
                warmup = 2000,
                iter = 20000,
                cores = 4,
                save_pars = save_pars(all = TRUE),
                sample_prior="yes"
)
save(m_m_full, file = paste("model_fits_G/Fit_m_Full_crit.Rda"))

m_l_full <- brm(TFT ~ 1 + syn * sem +
                  (1 + syn * sem| subject) +
                  (1 + syn * sem| item),
                data = crit_trim,
                family = lognormal(),
                prior = priors_l_tr,
                warmup = 2000,
                iter = 20000,
                cores = 4,
                save_pars = save_pars(all = TRUE),
                sample_prior="yes"
)
save(m_l_full, file = paste("model_fits_G/Fit_l_Full_crit.Rda"))

load("model_fits_G/Fit_s_full_crit.Rda")
load("model_fits_G/Fit_m_full_crit.Rda")
load("model_fits_G/Fit_l_full_crit.Rda")

# bfs
# model_s
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
df.bf_mertzen <- data.frame(matrix(ncol=7,nrow=0))
colnames(df.bf_mertzen) <- c("BF_method", "Language", "Region", "Effect","Prior","truncated", "BF10")

df.bf_mertzen[nrow(df.bf_mertzen)+1,] <- c("S-D", "German", "critical", "syntactic","Normal(0, 0.01)","yes", round(SD_bf10.m_s_syn,2))
df.bf_mertzen[nrow(df.bf_mertzen)+1,] <- c("S-D", "German", "critical", "syntactic","Normal(0, 0.05)","yes", round(SD_bf10.m_m_syn,2))
df.bf_mertzen[nrow(df.bf_mertzen)+1,] <- c("S-D", "German", "critical", "syntactic","Normal(0, 0.1)","yes", round(SD_bf10.m_l_syn,2))

df.bf_mertzen[nrow(df.bf_mertzen)+1,] <- c("S-D", "German", "critical", "semantic","Normal(0, 0.01)","yes", round(SD_bf10.m_s_sem,2))
df.bf_mertzen[nrow(df.bf_mertzen)+1,] <- c("S-D", "German", "critical", "semantic","Normal(0, 0.05)","yes", round(SD_bf10.m_m_sem,2))
df.bf_mertzen[nrow(df.bf_mertzen)+1,] <- c("S-D", "German", "critical", "semantic","Normal(0, 0.1)","yes", round(SD_bf10.m_l_sem,2))

df.bf_mertzen[nrow(df.bf_mertzen)+1,] <- c("S-D", "German", "critical", "interaction","Normal(0, 0.01)","yes", round(SD_bf10.m_s_int,2))
df.bf_mertzen[nrow(df.bf_mertzen)+1,] <- c("S-D", "German", "critical", "interaction","Normal(0, 0.05)","yes", round(SD_bf10.m_m_int,2))
df.bf_mertzen[nrow(df.bf_mertzen)+1,] <- c("S-D", "German", "critical", "interaction","Normal(0, 0.1)","yes", round(SD_bf10.m_l_int,2))

save(df.bf_mertzen, file = paste("BFs_mertzen_german_crit.Rda"))

###########################################################################################


# load Mertzen et al. ENGLISH data
df <- read.table("dataRetroEN_processed.txt")

df_crit <- df %>% filter(region == "crit") %>%
  select(subject, trial, list, item, condition, roi, region, TFT) %>%
  filter(TFT != 0)

# contrast coding
df_crit$syn <- ifelse(df_crit$condition%in%c("a","b"),-0.5,0.5)
df_crit$sem <- ifelse(df_crit$condition%in%c("b","d"),0.5,-0.5)

# trim all rts below 150 and above 3000 ms
range(df_crit$TFT)
crit_trim <- subset(df_crit, df_crit$TFT > 80)
range(crit_trim$TFT)


# models
#directional priors
m_s_full <- brm(TFT ~ 1 + syn * sem +
                  (1 + syn * sem| subject) +
                  (1 + syn * sem| item),
                data = crit_trim,
                family = lognormal(),
                prior = priors_s_tr,
                warmup = 2000,
                iter = 20000,
                cores = 4,
                save_pars = save_pars(all = TRUE),
                sample_prior="yes"
)
save(m_s_full, file = paste("model_fits_E/Fit_s_Full_crit.Rda"))


m_m_full <- brm(TFT ~ 1 + syn * sem +
                  (1 + syn * sem| subject) +
                  (1 + syn * sem| item),
                data = crit_trim,
                family = lognormal(),
                prior = priors_m_tr,
                warmup = 2000,
                iter = 20000,
                cores = 4,
                save_pars = save_pars(all = TRUE),
                sample_prior="yes"
)
save(m_m_full, file = paste("model_fits_E/Fit_m_Full_crit.Rda"))


m_l_full <- brm(TFT ~ 1 + syn * sem +
                  (1 + syn * sem| subject) +
                  (1 + syn * sem| item),
                data = crit_trim,
                family = lognormal(),
                prior = priors_l_tr,
                warmup = 2000,
                iter = 20000,
                cores = 4,
                save_pars = save_pars(all = TRUE),
                sample_prior="yes"
)
save(m_l_full, file = paste("model_fits_E/Fit_l_Full_crit.Rda"))

load("model_fits_E/Fit_s_full_crit.Rda")
load("model_fits_E/Fit_m_full_crit.Rda")
load("model_fits_E/Fit_l_full_crit.Rda")


# bfs
# model_s
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
df.bf_mertzen <- data.frame(matrix(ncol=7,nrow=0))
colnames(df.bf_mertzen) <- c("BF_method", "Language", "Region", "Effect","Prior","truncated", "BF10")

df.bf_mertzen[nrow(df.bf_mertzen)+1,] <- c("S-D", "English", "critical", "syntactic","Normal(0, 0.01)","yes", round(SD_bf10.m_s_syn,2))
df.bf_mertzen[nrow(df.bf_mertzen)+1,] <- c("S-D", "English", "critical", "syntactic","Normal(0, 0.05)","yes", round(SD_bf10.m_m_syn,2))
df.bf_mertzen[nrow(df.bf_mertzen)+1,] <- c("S-D", "English", "critical", "syntactic","Normal(0, 0.1)","yes", round(SD_bf10.m_l_syn,2))

df.bf_mertzen[nrow(df.bf_mertzen)+1,] <- c("S-D", "English", "critical", "semantic","Normal(0, 0.01)","yes", round(SD_bf10.m_s_sem,2))
df.bf_mertzen[nrow(df.bf_mertzen)+1,] <- c("S-D", "English", "critical", "semantic","Normal(0, 0.05)","yes", round(SD_bf10.m_m_sem,2))
df.bf_mertzen[nrow(df.bf_mertzen)+1,] <- c("S-D", "English", "critical", "semantic","Normal(0, 0.1)","yes", round(SD_bf10.m_l_sem,2))

df.bf_mertzen[nrow(df.bf_mertzen)+1,] <- c("S-D", "English", "critical", "interaction","Normal(0, 0.01)","yes", round(SD_bf10.m_s_int,2))
df.bf_mertzen[nrow(df.bf_mertzen)+1,] <- c("S-D", "English", "critical", "interaction","Normal(0, 0.05)","yes", round(SD_bf10.m_m_int,2))
df.bf_mertzen[nrow(df.bf_mertzen)+1,] <- c("S-D", "English", "critical", "interaction","Normal(0, 0.1)","yes", round(SD_bf10.m_l_int,2))

save(df.bf_mertzen, file = paste("BFs_mertzen_english_crit.Rda"))

#####################################################################################

load("BFs_mertzen_english_crit.Rda")
en <- df.bf_mertzen
load("BFs_mertzen_german_crit.Rda")
ge <- df.bf_mertzen

df.bf_mertzen <- rbind(en, ge)

# plot
df.bf_mertzen$Prior2 <- ifelse(df.bf_mertzen$Prior == "Normal(0, 0.01)", "N+(0, 0.01)", 
                       ifelse(df.bf_mertzen$Prior == "Normal(0, 0.05)", "N+(0, 0.05)", 
                              ("N+(0, 0.1)")))

df.bf_mertzen$BF10 <- as.numeric(df.bf_mertzen$BF10)
df.bf_mertzen$effect <- factor(df.bf_mertzen$Effect, levels=c("syntactic", "semantic", "interaction"))

ggplot(df.bf_mertzen, aes(x = Prior, y = BF10, group = effect)) +
  geom_point(aes(color=effect)) +
  geom_line(aes(color=effect)) +
  facet_grid(.~Language)+
  geom_hline(yintercept = 1, linetype="dashed") +
  theme_bw(base_size = 12)+
  xlab("Truncated Prior")+
  theme(legend.position = "top")

ggsave("BF_plot_mertzen.png", width = 14, height = 8, units = "cm", dpi=300)


#####################################################################################

# calculate BFs with the Savage-Dickey method for Van Dyke (2007) (without having the data, based on CIs)
# we will work with the SPR and TFT data of the critical region

# load estimates 
load("VD07_M21_estimates.rda")

# as a test case, we first try this approach with the Mertzen data, for which we properly computed the BFs above
# note: This is done on the ms scale!
(MG_tft <- previous %>% filter(experiment == "M21.G (N=121)") %>% filter(region == "critical") %>% filter(measure == "TFT"))

# simulate data for a reasonable prior (effect between 0 and 40 ms) and the estimates of Mertzen
set.seed(2)
prior_samples <- data.frame(b = round(rtruncnorm(n = 100000, a=0, mean = 15, sd = 5.5),1))
data_samples_syn <- data.frame(b = round(rnorm(n = 100000, mean = 13.7, sd = 3.02),1))
data_samples_sem <- data.frame(b = round(rnorm(n = 100000, mean = 5, sd = 2.7),1))
data_samples_int <- data.frame(b = round(rnorm(n = 100000, mean = -2.85, sd = 2.65),1))

# the range of the simulated data should approximate Mertzen's CrIs (see MG_tft)
range(prior_samples)
range(data_samples_syn)
range(data_samples_sem)
range(data_samples_int)

# visualize
combined_samples <- rbind(
  data.frame(Value = prior_samples$b, Distribution = "Prior"),
  data.frame(Value = data_samples_syn$b, Distribution = "syntactic"),
  data.frame(Value = data_samples_sem$b, Distribution = "semantic"),
  data.frame(Value = data_samples_int$b, Distribution = "interaction")
)
ggplot(combined_samples, aes(x = Value, fill = Distribution)) +
  geom_density(alpha = 0.5) +
  geom_abline(intercept=0, linewidth=1, linetype="dashed")+
  labs(x = "b",
       y = "Density") +
  scale_fill_manual(values = c("green", "red", "yellow", "blue")) +
  theme_minimal()


prior_samples <- data.frame(b = round(rtruncnorm(n = 100000, a=0, mean = 15, sd = 5.5),1))
data_samples_syn <- data.frame(b = round(rnorm(n = 100000, mean = 13.7, sd = 3.02),1))
data_samples_sem <- data.frame(b = round(rnorm(n = 100000, mean = 5, sd = 2.7),1))
data_samples_int <- data.frame(b = round(rnorm(n = 100000, mean = -2.85, sd = 2.65),1))


# Compute BF10
# find points where the distributions cross the x=0 line
value_prior <- dtruncnorm(0, a=0, mean = 15, sd = 5.5)
value_syn <- dnorm(0, mean=13.7,sd=3.02)
value_sem <- dnorm(0, mean=5,sd=2.7)
value_int <- dnorm(0, mean=-2.85,sd=2.65)

1 / (value_syn/value_prior) 
1 / (value_sem/value_prior)
1 / (value_int/value_prior)

# compare these approximated BFs with the properly computed BFs from the models above
load("BFs_mertzen_german_crit.Rda")
df.bf_mertzen %>% filter(Prior == "Normal(0, 0.05)")

# --> Evidence for/against an effect is the same (BFs above or below 1), 
# but the magnitude of the BF for the syntactic effect is very off