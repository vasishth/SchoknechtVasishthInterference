# author: Pia Schoknecht
# date: 17.07.24

# load packages
library(ggplot2)
library(tidyverse)
library(ordinal)
library(brms)
library(bayesplot)

# load data
df <- read.csv("norming_data_HighvsLowSem.csv")

# number of participants
length(unique(df$subject))

# number of items
length(unique(df$item))

# demographics
uniq <- df[!duplicated(df[2]),]
table(uniq$Sex)
range(uniq$Age)
round(mean(uniq$Age))
 
# did the latin square design work? -yes
xtabs(~ condition + item, data=filter(df, latin_square_list == 0))
xtabs(~ condition + item, data=filter(df, latin_square_list == 1))
 
# factorize
df$condition <- factor(df$condition, levels = c("c", "d", "f"))
#df$value <- factor(df$value, levels = c("1", "2", "3", "4", "5", "6", "7"))
 
# overview table
n <- df %>% group_by(condition, value) %>% count()
pivot_wider(n, values_from = "n", names_from = "value")

# filler ratings 
rating_byfilleritem <- aggregate(value ~ item, FUN=mean, data= filter(df, condition == "f"))
round(range(rating_byfilleritem$value),1)
 
# exclude fillers 
df_crit<-subset(df,condition!="f")
 
# contrast coding 
# keep the default dummy coding. We compare conditions c and d
# sum contrast d is high semantic interference, c is low semantic interference
df_crit$sem <- ifelse(df_crit$condition == "d", 0.5, -0.5)
df_crit$sem <- as.factor(df_crit$sem)
 
# priors
priors <- c(prior(normal(0, 3), class = Intercept),
             prior(normal(0, 1), class = b),
             prior(normal(0, 2), class = sd))
 
# check priors
extraDistr::qtnorm(0.025,mean=0, sd=3); extraDistr::qtnorm(0.975,mean=0, sd=3)# intercept normal(0, 3) upper and lower
extraDistr::qtnorm(0.025,mean=0, sd=1); extraDistr::qtnorm(0.975,mean=0, sd=1) # normal(0, 1) upper and lower
extraDistr::qtnorm(0.025,mean=0, sd=2); extraDistr::qtnorm(0.975,mean=0, sd=2) # normal(0, 2) upper and lower
 
# cumulative model
m <- brm(value ~ 1 + sem +
                    (1 + sem || subject) +
                    (1 + sem || item),
                  data = df_crit,
                  prior = priors,
                  family = cumulative,
                  cores = 4,
                  warmup = 2000,
                  iter = 8000,
                  sample_prior="yes")


save(m, file = paste("Fit_cumulative.Rda"))
 
load("Fit_cumulative.Rda")
 
# checks
pp_check(m, ndraws = 100, type = "dens_overlay")
pp_check(m, ndraws = 100, type = "stat", stat = "mean")
pp_check(m, ndraws = 100, type = "stat", stat = "min")
pp_check(m, ndraws = 100, type = "stat", stat = "max")
 
plot(m)
 
summary(m)
 

# effects
conditional_effects(m, "sem", categorical = TRUE)
c_eff <- conditional_effects(m, "sem", categorical = TRUE)

# plot
p <- plot(c_eff, plot = FALSE)[[1]]
p+ theme_bw() +
  scale_x_discrete(name = "distractor", breaks=c(-0.5, 0.5), labels=c("inanimate", "animate"))+
   scale_color_discrete(name="rating") +
   scale_fill_discrete(name="rating")+
  labs(title = "Plausibility ratings",
       subtitle = "7 = absolutely plausible; 1 = absolutely implausible")+
   guides(color = guide_legend(reverse=TRUE), fill = guide_legend(reverse=TRUE))
ggsave("plausibility_anim_inan.jpg", dpi=600, unit="cm", height = 8, width=12)
 
# BF
h <- hypothesis(m, "sem0.5 = 0")
plot(h)
1 / h$hypothesis$Evid.Ratio
# BF10: 267411811368578842624 # 2.7e+20
 
# ########################################
# 
# # rating as co-predictor in the reading times model analyzing the pre-critical region
# 
# aggregate rating data by item
rating_byitem <- aggregate(value ~ condition + item, FUN=mean, data= df_crit)
range(rating_byitem$value)

rating_byitem$rating <- round((rating_byitem$value/10),3)
range(rating_byitem$rating)

# reading times data
precrit_trim <- read.csv("pandora_spr_774_precrit.csv")
precrit_trim$c_trial <- precrit_trim$trial/100
 
# combine
precrit_plausib <- inner_join(precrit_trim, rating_byitem, by=c("item", "condition"))

# center plausibility rating
precrit_plausib$c_rating <- precrit_plausib$value - mean(precrit_plausib$value)


# priors
priors_m <- c(
  prior(normal(6, 0.6), class = Intercept),
  prior(normal(0, 0.05), class = b, coef="sem"),
  prior(normal(0, 0.1),  class = b, coef="c_trial"),
  prior(normal(0, 0.1),  class = b, coef="c_rating"),
  prior(normal(0, 0.5), class = sigma),
  prior(normal(0, 0.1), class = sd)
) 


# model
m_m_full <- brm(rt ~ 1 +  sem * c_trial * c_rating +
                  (1 + sem * c_trial * c_rating || participant) +
                  (1 + sem * c_trial || item),
                data = precrit_plausib,
                family = lognormal(),
                prior = priors_m,
                warmup = 2000,
                iter = 20000,
                cores = 4,
                save_pars = save_pars(all = TRUE),
                sample_prior="yes",
                control = list(adapt_delta = 0.9) 
)
save(m_m_full, file = paste("Fit_m_Full_precrit_plausibility_c_rating.Rda"))
load("Fit_m_Full_precrit_plausibility_c_rating.Rda")
 
## Posterior predictive check
pp_check(m_m_full, ndraws = 100, type = "dens_overlay")
## Plot posterior predictive distribution of statistical summaries:
pp_check(m_m_full, ndraws = 100, type = "stat", stat = "mean")
pp_check(m_m_full, ndraws = 100, type = "stat", stat = "min")
pp_check(m_m_full, ndraws = 100, type = "stat", stat = "max")
 
summary(m_m_full)
plot(m_m_full)

# plot posteriors

# extract samples
samples_precrit <- as_draws_df(m_m_full)

# backtransform estimates to ms scale
sem_eff_precrit <- exp(samples_precrit$b_Intercept + samples_precrit$b_sem) - exp(samples_precrit$b_Intercept - samples_precrit$b_sem)
rating_eff_precrit <- exp(samples_precrit$b_Intercept + samples_precrit$b_c_rating) - exp(samples_precrit$b_Intercept - samples_precrit$b_c_rating)
int_eff_precrit <- exp(samples_precrit$b_Intercept + samples_precrit$`b_sem:c_rating`) - exp(samples_precrit$b_Intercept - samples_precrit$`b_sem:c_rating`)

# credible intervals
round(c(mean = mean(sem_eff_precrit), quantile(sem_eff_precrit, probs = c(.025, .975))))
round(c(mean = mean(rating_eff_precrit), quantile(rating_eff_precrit, probs = c(.025, .975))))
round(c(mean = mean(int_eff_precrit), quantile(int_eff_precrit, probs = c(.025, .975))))

# add back-transformed values to samples df
samples_precrit$semantic <- sem_eff_precrit
samples_precrit$plausibility <- rating_eff_precrit
samples_precrit$interaction <- int_eff_precrit

# Plot
posteriors_precrit <- mcmc_areas(
  samples_precrit,
  pars = c("semantic", "plausibility", "interaction"),
  prob = 0.8, # 80% intervals
  #prob_outer = 0.99, # 99%
  point_est = "mean")+
  labs(title= "Pre-critical region", x = "effect in ms")+
  vline_0(linetype="dashed")+
  scale_x_continuous(breaks=(seq(-50, 40, 10)), limits = c(-50, 40)) +
  theme_bw(base_size=12) +
  annotate("text", x = 35, y = 3.8, label = "2 — 13 — 24", size=3) +
  annotate("text", x = -20, y = 2.9, label = "-30 — -18 — -6", size=3) +
  annotate("text", x = -20, y = 1.7, label = "-22 — -4 — 14", size=3) 
ggsave("posteriors_spr_pooled_774_plausibility.png", width = 12, height = 10, units = "cm", dpi=300)

                
# bayes factors
# semantic
h <- hypothesis(m_m_full, "sem = 0")
1 / h$hypothesis$Evid.Ratio

# plausibility rating
h <- hypothesis(m_m_full, "c_rating = 0")
1 / h$hypothesis$Evid.Ratio

# sem x plausibility rating interaction
h <- hypothesis(m_m_full, "sem:c_rating = 0")
1 / h$hypothesis$Evid.Ratio
 
