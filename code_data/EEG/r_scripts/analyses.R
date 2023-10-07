# author: Pia Schoknecht
# date: July 20, 2023
# R version: 4.1.3

#load packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(brms)
library(bayesplot)
library(truncnorm)
library(plyr)
library(cowplot)
library(lattice)
library(graphics)
library("gridExtra")     ## for grid.arrange()
library(stats)
options(scipen = 999)

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
round(2-2*5);round(2+2*5) # intercept
(2+(-0.1))-(2-(-0.1)) # beta normal-(0, 0.1)
(2+(-0.5))-(2-(-0.5)) # beta normal-(0, 0.5)
(2+(-1))-(2-(-1))     # beta normal-(0, 1)
(2+(-5))-(2-(-5))     # beta normal-(0, 5)

# full models with truncated priors
m_s_tr <- brm(amplitude ~ 1 + prestim_amp + syn * sem  + (1 + sem ||subject) + (1|item),
                 prior = priors_s_tr,
                 warmup = 2000,
                 iter = 20000,
                 cores = 4,
                 control = list(adapt_delta = 0.9),
                 save_pars = save_pars(all = TRUE),
                 data = eeg_cp_agg)
save(m_s_tr, file = paste("model_fits/Fit_s_tr.Rda"))

m_m_tr <- brm(amplitude ~ 1 + prestim_amp + syn * sem  + (1 + sem ||subject) + (1|item),
              prior = priors_m_tr,
              warmup = 2000,
              iter = 20000,
              cores = 4,
              control = list(adapt_delta = 0.9),
              save_pars = save_pars(all = TRUE),
              data = eeg_cp_agg)
save(m_m_tr, file = paste("model_fits/Fit_m_tr.Rda"))

m_l_tr <- brm(amplitude ~ 1 + prestim_amp + syn * sem  + (1 + sem ||subject) + (1|item),
              prior = priors_l_tr,
              warmup = 2000,
              iter = 20000,
              cores = 4,
              control = list(adapt_delta = 0.9),
              save_pars = save_pars(all = TRUE),
              data = eeg_cp_agg)
save(m_l_tr, file = paste("model_fits/Fit_l_tr.Rda"))

m_xl_tr <- brm(amplitude ~ 1 + prestim_amp + syn * sem  + (1 + sem ||subject) + (1|item),
              prior = priors_xl_tr,
              warmup = 2000,
              iter = 20000,
              cores = 4,
              control = list(adapt_delta = 0.9),
              save_pars = save_pars(all = TRUE),
              data = eeg_cp_agg)
save(m_xl_tr, file = paste("model_fits/Fit_xl_tr.Rda"))

# models without semantic effect
m_s_nullsem <- brm(amplitude ~ 1 + prestim_amp + syn + syn:sem  + (1 + sem ||subject) + (1|item),
               prior = priors_s_tr,
               warmup = 2000,
               iter = 20000,
               cores = 4,
               control = list(adapt_delta = 0.9),
               save_pars = save_pars(all = TRUE),
               data = eeg_cp_agg)
save(m_s_nullsem, file = paste("model_fits/Fit_s_nullsem.Rda"))

m_m_nullsem <- brm(amplitude ~ 1 + prestim_amp + syn + syn:sem  + (1 + sem ||subject) + (1|item),
                   prior = priors_m_tr,
                   warmup = 2000,
                   iter = 20000,
                   cores = 4,
                   control = list(adapt_delta = 0.9),
                   save_pars = save_pars(all = TRUE),
                   data = eeg_cp_agg)
save(m_m_nullsem, file = paste("model_fits/Fit_m_nullsem.Rda"))

m_l_nullsem <- brm(amplitude ~ 1 + prestim_amp + syn + syn:sem  + (1 + sem ||subject) + (1|item),
                   prior = priors_l_tr,
                   warmup = 2000,
                   iter = 20000,
                   cores = 4,
                   control = list(adapt_delta = 0.9),
                   save_pars = save_pars(all = TRUE),
                   data = eeg_cp_agg)
save(m_l_nullsem, file = paste("model_fits/Fit_l_nullsem.Rda"))

m_xl_nullsem <- brm(amplitude ~ 1 + prestim_amp + syn + syn:sem  + (1 + sem ||subject) + (1|item),
                   prior = priors_xl_tr,
                   warmup = 2000,
                   iter = 20000,
                   cores = 4,
                   control = list(adapt_delta = 0.9),
                   save_pars = save_pars(all = TRUE),
                   data = eeg_cp_agg)
save(m_xl_nullsem, file = paste("model_fits/Fit_xl_nullsem.Rda"))

# models without syntactic effect
m_s_nullsyn <- brm(amplitude ~ 1 + prestim_amp + sem + syn:sem  + (1 + sem ||subject) + (1|item),
                   prior = priors_s_tr,
                   warmup = 2000,
                   iter = 20000,
                   cores = 4,
                   control = list(adapt_delta = 0.9),
                   save_pars = save_pars(all = TRUE),
                   data = eeg_cp_agg)
save(m_s_nullsyn, file = paste("model_fits/Fit_s_nullsyn.Rda"))

m_m_nullsyn <- brm(amplitude ~ 1 + prestim_amp + sem + syn:sem  + (1 + sem ||subject) + (1|item),
                   prior = priors_m_tr,
                   warmup = 2000,
                   iter = 20000,
                   cores = 4,
                   control = list(adapt_delta = 0.9),
                   save_pars = save_pars(all = TRUE),
                   data = eeg_cp_agg)
save(m_m_nullsyn, file = paste("model_fits/Fit_m_nullsyn.Rda"))

m_l_nullsyn <- brm(amplitude ~ 1 + prestim_amp + sem + syn:sem  + (1 + sem ||subject) + (1|item),
                   prior = priors_l_tr,
                   warmup = 2000,
                   iter = 20000,
                   cores = 4,
                   control = list(adapt_delta = 0.9),
                   save_pars = save_pars(all = TRUE),
                   data = eeg_cp_agg)
save(m_l_nullsyn, file = paste("model_fits/Fit_l_nullsyn.Rda"))

m_xl_nullsyn <- brm(amplitude ~ 1 + prestim_amp + sem + syn:sem  + (1 + sem ||subject) + (1|item),
                    prior = priors_xl_tr,
                    warmup = 2000,
                    iter = 20000,
                    cores = 4,
                    control = list(adapt_delta = 0.9),
                    save_pars = save_pars(all = TRUE),
                    data = eeg_cp_agg)
save(m_xl_nullsyn, file = paste("model_fits/Fit_xl_nullsyn.Rda"))

# models without interaction
m_s_nullint <- brm(amplitude ~ 1 + prestim_amp + sem + syn + (1 + sem ||subject) + (1|item),
                   prior = priors_s_tr,
                   warmup = 2000,
                   iter = 20000,
                   cores = 4,
                   control = list(adapt_delta = 0.9),
                   save_pars = save_pars(all = TRUE),
                   data = eeg_cp_agg)
save(m_s_nullint, file = paste("model_fits/Fit_s_nullint.Rda"))

m_m_nullint <- brm(amplitude ~ 1 + prestim_amp + sem + syn  + (1 + sem ||subject) + (1|item),
                   prior = priors_m_tr,
                   warmup = 2000,
                   iter = 20000,
                   cores = 4,
                   control = list(adapt_delta = 0.9),
                   save_pars = save_pars(all = TRUE),
                   data = eeg_cp_agg)
save(m_m_nullint, file = paste("model_fits/Fit_m_nullint.Rda"))

m_l_nullint <- brm(amplitude ~ 1 + prestim_amp + sem + syn  + (1 + sem ||subject) + (1|item),
                   prior = priors_l_tr,
                   warmup = 2000,
                   iter = 20000,
                   cores = 4,
                   control = list(adapt_delta = 0.9),
                   save_pars = save_pars(all = TRUE),
                   data = eeg_cp_agg)
save(m_l_nullint, file = paste("model_fits/Fit_l_nullint.Rda"))

m_xl_nullint <- brm(amplitude ~ 1 + prestim_amp + sem + syn  + (1 + sem ||subject) + (1|item),
                    prior = priors_xl_tr,
                    warmup = 2000,
                    iter = 20000,
                    cores = 4,
                    control = list(adapt_delta = 0.9),
                    save_pars = save_pars(all = TRUE),
                    data = eeg_cp_agg)
save(m_xl_nullint, file = paste("model_fits/Fit_xl_nullint.Rda"))

## Posterior predictive check
# for our favorite model
pp_check(m_m_tr, ndraws = 100, type = "dens_overlay")
## Plot posterior predictive distribution of statistical summaries:
pp_check(m_m_tr, ndraws = 100, type = "stat", stat = "mean")
pp_check(m_m_tr, ndraws = 100, type = "stat", stat = "min")
pp_check(m_m_tr, ndraws = 100, type = "stat", stat = "max")


samples <- as_draws_df(m_m_tr)
syn_eff <- exp(samples$b_Intercept + samples$b_syn) -
  exp(samples$b_Intercept- samples$b_syn)

sem_eff <- exp(samples$b_Intercept + samples$b_sem) -
  exp(samples$b_Intercept- samples$b_sem)

colnames(samples)
int_eff <- exp(samples$b_Intercept + samples[,5]) -
  exp(samples$b_Intercept- samples[,5])

round(c(mean = mean(syn_eff), quantile(syn_eff, probs = c(.025, .975))),1)
round(c(mean = mean(sem_eff), quantile(sem_eff, probs = c(.025, .975))),1)
round(c(mean = mean(int_eff[-1,]), quantile(int_eff[,1], probs = c(.025, .975))),1)

samples$syntactic <- syn_eff
samples$semantic <- sem_eff
samples$interaction <- int_eff

mcmc_areas(
  samples,
  pars = c("syntactic", "semantic", "interaction"),
  prob = 0.8, # 80% intervals
  #prob_outer = 0.99, # 99%
  point_est = "mean")+
  labs( x = "effect in ms")+
  vline_0(linetype="dashed")+
  #scale_y_discrete(labels=c(
  #  "int.syn.sem_eff" = "interaction")
  #)+
  #scale_x_continuous(n.breaks = 3)+
  theme_bw(base_size=10)
ggsave("plots/posteriors_eeg.jpg", width = 10, height = 5, units = "cm", dpi=300)


# BFs
# SEM effect
sem_bf_s_tr1 <- bayes_factor(m_s_tr, m_s_nullsem)$bf
sem_bf_m_tr1 <- bayes_factor(m_m_tr, m_m_nullsem)$bf
sem_bf_l_tr1 <- bayes_factor(m_l_tr, m_l_nullsem)$bf
sem_bf_xl_tr1 <- bayes_factor(m_xl_tr, m_xl_nullsem)$bf

sem_bf_s_tr2 <- bayes_factor(m_s_tr, m_s_nullsem)$bf
sem_bf_m_tr2 <- bayes_factor(m_m_tr, m_m_nullsem)$bf
sem_bf_l_tr2 <- bayes_factor(m_l_tr, m_l_nullsem)$bf
sem_bf_xl_tr2 <- bayes_factor(m_xl_tr, m_xl_nullsem)$bf

# SYN effect
syn_bf_s_tr1 <- bayes_factor(m_s_tr, m_s_nullsyn)$bf
syn_bf_m_tr1 <- bayes_factor(m_m_tr, m_m_nullsyn)$bf
syn_bf_l_tr1 <- bayes_factor(m_l_tr, m_l_nullsyn)$bf
syn_bf_xl_tr1 <- bayes_factor(m_xl_tr, m_xl_nullsyn)$bf

syn_bf_s_tr2 <- bayes_factor(m_s_tr, m_s_nullsyn)$bf
syn_bf_m_tr2 <- bayes_factor(m_m_tr, m_m_nullsyn)$bf
syn_bf_l_tr2 <- bayes_factor(m_l_tr, m_l_nullsyn)$bf
syn_bf_xl_tr2 <- bayes_factor(m_xl_tr, m_xl_nullsyn)$bf

# INTERACTION
int_bf_s_tr1 <- bayes_factor(m_s_tr, m_s_nullint)$bf
int_bf_m_tr1 <- bayes_factor(m_m_tr, m_m_nullint)$bf
int_bf_l_tr1 <- bayes_factor(m_l_tr, m_l_nullint)$bf
int_bf_xl_tr1 <- bayes_factor(m_xl_tr, m_xl_nullint)$bf

int_bf_s_tr2 <- bayes_factor(m_s_tr, m_s_nullint)$bf
int_bf_m_tr2 <- bayes_factor(m_m_tr, m_m_nullint)$bf
int_bf_l_tr2 <- bayes_factor(m_l_tr, m_l_nullint)$bf
int_bf_xl_tr2 <- bayes_factor(m_xl_tr, m_xl_nullint)$bf

# save BFs in df
df.bf <- data.frame(matrix(ncol=8,nrow=0))
colnames(df.bf) <- c("N", "time.window","roi", "effect","prior","truncated", "BF10.1", "BF10.2")
df.bf[nrow(df.bf)+1,] <- c("103","300-500","centro-parietal", "semantic","Normal(0, 0.1)", "yes", round(sem_bf_s_tr1,2), round(sem_bf_s_tr2,2))
df.bf[nrow(df.bf)+1,] <- c("103","300-500","centro-parietal","semantic","Normal(0, 0.5)","yes", round(sem_bf_m_tr1,2), round(sem_bf_m_tr2,2))
df.bf[nrow(df.bf)+1,] <- c("103","300-500","centro-parietal","semantic","Normal(0, 1)","yes", round(sem_bf_l_tr1,2), round(sem_bf_l_tr2,2))
df.bf[nrow(df.bf)+1,] <- c("103","300-500","centro-parietal","semantic","Normal(0, 5)","yes", round(sem_bf_xl_tr1,2), round(sem_bf_xl_tr2,2))

df.bf[nrow(df.bf)+1,] <- c("103","300-500","centro-parietal","syntactic","Normal(0, 0.1)", "yes", round(syn_bf_s_tr1,2), round(syn_bf_s_tr2,2))
df.bf[nrow(df.bf)+1,] <- c("103","300-500","centro-parietal","syntactic","Normal(0, 0.5)","yes", round(syn_bf_m_tr1,2), round(syn_bf_m_tr2,2))
df.bf[nrow(df.bf)+1,] <- c("103","300-500","centro-parietal","syntactic","Normal(0, 1)","yes", round(syn_bf_l_tr1,2), round(syn_bf_l_tr2,2))
df.bf[nrow(df.bf)+1,] <- c("103","300-500","centro-parietal","syntactic","Normal(0, 5)","yes", round(syn_bf_xl_tr1,2), round(syn_bf_xl_tr2,2))

df.bf[nrow(df.bf)+1,] <- c("103","300-500","centro-parietal","interaction","Normal(0, 0.1)", "yes", round(int_bf_s_tr1,2), round(int_bf_s_tr2,2))
df.bf[nrow(df.bf)+1,] <- c("103","300-500","centro-parietal","interaction","Normal(0, 0.5)","yes", round(int_bf_m_tr1,2), round(int_bf_m_tr2,2))
df.bf[nrow(df.bf)+1,] <- c("103","300-500","centro-parietal","interaction","Normal(0, 1)","yes", round(int_bf_l_tr1,2), round(int_bf_l_tr2,2))
df.bf[nrow(df.bf)+1,] <- c("103","300-500","centro-parietal","interaction","Normal(0, 5)","yes", round(int_bf_xl_tr1,2), round(int_bf_xl_tr2,2))

save(df.bf, file = paste("BFs.Rda"))

load("BFs.Rda")

df.bf$BF10 <- as.numeric(df.bf$BF10.1)
df.bf$effect <- factor(df.bf$effect, levels=c("semantic", "syntactic", "interaction"))



# Plot different BFs
library(ggbreak)
library(ggimage) 

## original plot
plot_BF<- ggplot(df.bf, aes(x = prior, y = BF10, group = interaction(effect, truncated))) +
  geom_point(aes(color=effect)) +
  geom_line(aes(color=effect)) +
  geom_hline(yintercept = 1, linetype="dashed") +
  #coord_cartesian(ylim = c(0, 50)) +
  #annotate("text", x =3,y= 1.5, label = "Evidence in favor of H1", size =3)+
  #annotate("text", x =3,y= 2/3, label = "Evidence in favor of H0", size =3) +
  theme_bw(base_size = 8)+
  theme(legend.position = c(0.8, 0.8), legend.text=element_text(size=6))+
  #scale_y_break(c(12, 21), scales=2) + #scale_y_continuous(limits = c(0, 90), breaks =  c(1, 3, 10, 30, 50, 80, 90))+
  #scale_y_break(c(12, 21), scales=2) +
  xlab("Truncated Prior")

## ggbreak plot without legend
p2 <- plot_BF + scale_y_break(c(3, 10), scales=1.5) + theme(legend.position="none") 

## extract legend from original plot
leg = cowplot::get_legend(plot_BF)

## redraw the figure
p3 <- ggplotify::as.ggplot(print(p2))

## place the legend 
p3 + ggimage::geom_subview(x=.85, y=.78, subview=leg)
ggsave("plots/BF_plot_N103_cp_300_500.jpg", width = 10, height = 8, units = "cm", dpi=300)
