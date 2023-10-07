# author: Pia Schoknecht
# date: 30.07.2023

# load packages
library(tidyverse)
library(brms)

# load data
df1 <- read.csv("../pandora_web/data/pandora_web_data_allsessions_short.csv", sep=";")
df2 <- read.csv("../pandora_repS1/data/pandora_repS1_data_cleaned_short.csv", sep=";")
df3 <- read.csv("../pandora_repS2/data/pandora_repS2_data_cleaned_short.csv", sep=";")

# add session info to df2 and df3 (both experiments had only one session)
df2$session <- 1
df3$session <- 1

# delete some columns in df1 to have identical columns in all data sets
df1$delay <- NULL
df1$dist_len <- NULL

# combine data sets
df <- rbind(df1, df2, df3)


# Over-all Accuracy 
# subset with only one row per subject and item with question
acc <- df %>% 
  drop_na(question_rt) %>% 
  filter(wordno == 01)# %>%
# filter(question_rt < 20000)

#calculate accuracy per participant
acc_subj <- table(acc$correct, acc$fullname, acc$session) %>%
  as.data.frame() %>%
  rename(answ = "Var1", subj = "Var2", session = "Var3")

#answers in percent (number of questions per subject: 32)
acc_subj$percent <- acc_subj$Freq/32*100

#how many questions were answered correctly?
# (exclude Freq=0 because that's only the case when a subject did not participate in a session)
acc_cor <- subset(acc_subj, acc_subj$answ == "1" & acc_subj$Freq != 0)
round(mean(acc_cor$percent),2)
round(range(acc_cor$percent),1)

ggplot(acc_cor, aes(x=percent)) +
  geom_density(aes()) +
  ggtitle("accuracy") + 
  theme_bw()

# response time
aggregate(question_rt ~ correct, data= acc, FUN=mean)

# per condition
aggregate(question_rt ~ correct + condition, data= acc, FUN=mean)

# Exclude fillers
df2 <- droplevels(subset(df, label != "filler"))

# Contrast coding
# high = 0.5, low = -0.5
df2$syn <- ifelse(df2$condition%in%c("a","b"),-0.5,0.5)
df2$sem <- ifelse(df2$condition%in%c("b","d"),0.5,-0.5)

# Accuracy (only critical)
# subset with only one row per subject and items with question
acc <- df2 %>% 
  drop_na(question_rt) %>% 
  filter(wordno == 01)

#calculate accuracy per participant
acc_subj <- table(acc$correct, acc$fullname, acc$condition, acc$session) %>%
  as.data.frame() %>%
  rename(answ = "Var1", subj = "Var2", cond = "Var3", session ="Var4")


#answers in percent (
#number of questions per subject per condition with 2 sessions= 10, with 1 session = 5)
acc_subj$percent <- acc_subj$Freq/5*100

#how many questions were answered correctly?
# (exclude Freq=0 because that's only the case when a subject did not participate in a session)
acc_cor <- subset(acc_subj, acc_subj$answ == "1" & acc_subj$Freq != 0)
round(mean(acc_cor$percent),1)
round(range(acc_cor$percent),1)

# calculate accuracy per condition
acc_per_cond <- acc_cor %>%
  group_by(cond) %>%
  summarise(percent = round(mean(percent),1))

ggplot(data=acc_per_cond, aes(x=cond, y=percent)) +
  geom_bar(stat="identity")

acc_per_cond


# Inferential statistics
# priors (and code in general) from the chapter about logistic regression in Nicenboim et al., 2023 
# fullname is the unique id for each participant

fit_acc <- brm(correct ~ 1 + syn*sem + (1|fullname) + (1|item),
               data = acc,
               family = bernoulli(link = logit),
               prior = c(
                 prior(normal(0, 1.5), class = Intercept),
                 prior(normal(0, .1), class = b)),
              warmup = 2000,
              iter = 8000,
              cores = 4
              )

plot(fit_acc)

round(posterior_summary(fit_acc,
                  variable = c("b_Intercept", "b_syn", "b_sem", "b_syn:sem")),2)


