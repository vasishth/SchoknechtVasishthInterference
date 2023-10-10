# Plausibility Norming: PANDORA
# author: Pia Schoknecht
# date: 17 06 2022

library(plyr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lme4)
library(ordinal)
library(brms)


# load data
dfp <- read.csv("pandora_plausibility_rating_corrected.csv")

# number of participants
(N_subj <- length(unique(dfp$subj)))

uniq <- dfp[!duplicated(dfp[1]),]

#age
(age <- table(uniq$age))
(mean(uniq$age))

#sex
(sex <- table(uniq$sex))



# Pandora Ratings

# number of items
uniq <- dfp[!duplicated(dfp[1]),]
(N_item <- length(unique(dfp$item)))

(dfp2 <- dfp %>%
    group_by(cond) %>%
    dplyr::summarize(mean_rating = mean(rating, na.rm=TRUE), sd_rating = sd(rating, na.rm=TRUE)))

dfp$cond <- factor(dfp$cond, levels = c("intro", "subject", "distr_anim", "distr_inan"))

ggplot(dfp, aes(x=cond, y=rating))+
  geom_jitter(color="lightblue", alpha=0.2)+
  geom_boxplot(fill="grey", alpha=0.7)+
  stat_summary(fun=mean, geom="point", shape=18, size=4) + #adding the mean
  expand_limits(y=c(0.5,7.5))+
  scale_y_continuous(name="plausibility rating", breaks=seq(1, 7, 1))+
  scale_x_discrete(name="", labels=c("distr_anim" = "animate \ndistractor", "distr_inan" = "inanimate \ndistractor",
                                     "intro" = "introduction", "subject" = "subject"))+
  theme_bw()+
  labs(title = "Plausibility ratings",
       subtitle = "1 = absolute implausible; 7 = absolute plausible")

ggsave("plots/pandora_plausibility_ratings.jpg", dpi=600, width=4, height=4)

