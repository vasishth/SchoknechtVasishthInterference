# author: Pia Schoknecht
# date: 30.07.2023

# load packages
library(tidyverse)
library(brms)


# load all .csv files from logs folder
tbl <- list.files(path = "../data/logs/", pattern = "*.csv", full.names = TRUE) %>% 
                  map_df(~read_csv(., col_types = cols(.default = "c"))) 

tbl$subject_nr <- ifelse(tbl$subject_nr == "136" & tbl$datetime == "Mon May  8 10:21:50 2023", "135", tbl$subject_nr)


# select relevant columns, delete practice items
dat <- tbl %>% select(subject_nr, session, lst, item, cond, cond_name, correct, correct_response,  response, response_time, exp_sentence, exp_question, is_question, question_type, np_in_question, live_row,experiment_file, datetime,  opensesame_version) %>%
              drop_na(exp_sentence)


dat2 <- dat %>% mutate(subject_nr = fct_recode(subject_nr, "1" = "200",  "3" = "7", "125" = "155"))


# number of participants
(N_subj <- length(unique(dat2$subject_nr)))

# exclude participants who were excluded due to low accuracy or to many artefacts
ex_s1 <- read.table("../data/exclude/exclude_s1.txt")
ex_s1$subj <- as.factor(ex_s1$subj)
ex_s2 <- read.table("../data/exclude/exclude_s2.txt")
ex_s2$subj <- as.factor(ex_s2$subj)

ex_arte <- read.csv("../data/exclude/TooManyArtefacts.csv")
ex_arte$subject <- as.factor(ex_arte$subject)


dat3 <- dat2 %>% anti_join(ex_s1, by=c("subject_nr"="subj")) %>%
                 anti_join(ex_s2, by=c("subject_nr"="subj")) %>%
                 anti_join(ex_arte, by=c("subject_nr"="subject"))%>%
                 droplevels()

(N_subj <- length(unique(dat3$subject_nr)))
nrows_all <- dat3 %>% count(subject_nr)


#### overall accuracy including fillers ####
acc <- filter(dat3, is_question == "yes")

# by-participant accuracy (per session)
acc_s1 <- acc %>% filter(session == "1")
acc_s2 <- acc %>% filter(session == "2")

#calculate accuracy per participant
acc_subj_s1 <- table(acc_s1$correct, acc_s1$subject_nr) %>%
  as.data.frame() %>%
  rename(answ = "Var1", subj = "Var2")

acc_subj_s2 <- table(acc_s2$correct, acc_s2$subject_nr) %>%
  as.data.frame() %>%
  rename(answ = "Var1", subj = "Var2")

#answers in percent (number of questions per subject per session: 32)
acc_subj_s1$percent <- acc_subj_s1$Freq/32*100
acc_subj_s2$percent <- acc_subj_s2$Freq/32*100

#how many questions were answered correctly?
s1_acc_cor <- subset(acc_subj_s1, acc_subj_s1$answ == "1")
s1_acc_cor$session <- 1
round(mean(s1_acc_cor$percent),1)
round(range(s1_acc_cor$percent),1)

s2_acc_cor <- subset(acc_subj_s2, acc_subj_s2$answ == "1" & Freq != 0)
s2_acc_cor$session <- 2
round(mean(s2_acc_cor$percent),1)
round(range(s2_acc_cor$percent),1)

# combine
acc_cor <- rbind(s1_acc_cor, s2_acc_cor)
acc_cor$session <- as.factor(acc_cor$session)
round(mean(acc_cor$percent),1)
round(range(acc_cor$percent),1)

#### only critical trials ####
acc_crit <- filter(dat3, is_question == "yes" & cond != "f")
round(prop.table(table(acc_crit$correct)),2)

nrows_crit <- acc_crit %>% count(subject_nr)

acc_crit_s1 <- filter(dat3, is_question == "yes" & cond != "f" & session == 1)
round(prop.table(table(acc_crit_s1$correct)),2)

acc_crit_s2 <- filter(dat3, is_question == "yes" & cond != "f" & session == 2)
round(prop.table(table(acc_crit_s2$correct)),2)

# by condition accuracy
table(acc_crit$correct, acc_crit$cond)

means <- acc_crit %>%
            filter(correct==1) %>%
            count(correct, cond) %>%
            mutate(percent_correct = n/1025*100)
means

# effect sizes
syn <- round(( (((means[3,4] + means[4,4])/2)) - (((means[1,4] + means[2,4])/2)) ) ,1)
sem <- round(( (((means[2,4] + means[4,4])/2)) - (((means[1,4] + means[3,4])/2)) ) ,1)
interaction <- round((means[1,4] - means[2,4]) - (means[3,4] - means[4,4]), 1)
syn;sem;interaction

# by-participant accuracy (per session)
acc_s1 <- acc_crit %>% filter(session == "1")
acc_s2 <- acc_crit %>% filter(session == "2")

#calculate accuracy per participant
acc_subj_s1 <- table(acc_s1$correct, acc_s1$subject_nr) %>%
  as.data.frame() %>%
  rename(answ = "Var1", subj = "Var2")

acc_subj_s2 <- table(acc_s2$correct, acc_s2$subject_nr) %>%
  as.data.frame() %>%
  rename(answ = "Var1", subj = "Var2")

#answers in percent (number of questions after critical trials
#per subject per session: 20)
acc_subj_s1$percent <- acc_subj_s1$Freq/20*100
acc_subj_s2$percent <- acc_subj_s2$Freq/20*100

#how many questions were answered correctly?
s1_acc_cor <- subset(acc_subj_s1, acc_subj_s1$answ == "1")
s1_acc_cor$session <- 1
round(mean(s1_acc_cor$percent),1)
round(range(s1_acc_cor$percent),1)

s2_acc_cor <- subset(acc_subj_s2, acc_subj_s2$answ == "1" & Freq != 0)
s2_acc_cor$session <- 2
round(mean(s2_acc_cor$percent),1)
round(range(s2_acc_cor$percent),1)

# combine
acc_cor <- rbind(s1_acc_cor, s2_acc_cor)
acc_cor$session <- as.factor(acc_cor$session)
round(mean(acc_cor$percent),1)
round(range(acc_cor$percent),1)

ggplot(acc_cor, aes(x=percent)) +
  geom_histogram(binwidth = 1, color="black", fill="lightgrey")+
  facet_grid(. ~session, labeller = label_both)+
  ggtitle("accuracy in critical trials") + 
  scale_y_continuous(breaks=seq(0, 20, 2))+
  geom_vline(xintercept=70, linetype="dashed")+
  ylab("number of participants") + 
  xlab("percentage correct")+
  theme_bw(base_size=14)
ggsave("accuracy_crit.png", height=4, width=8)


pd <- position_dodge(0.2)

ggplot(acc_cor, aes(x = session, y=percent, group=subj)) +
  geom_point(position = pd, size = 1.5)+
  geom_line(position = pd, color="grey")+
  geom_hline(yintercept=70, linetype="dashed")+
  ylab("percentage correct")+
  ggtitle("by-participant accuracy in critical trials")+
  theme_bw(base_size=14)
ggsave('accuracy_by_part_crit.png', height=4, width=5)


# response time
aggregate(as.numeric(response_time) ~ correct, data= acc_crit, FUN=mean)

# per condition
aggregate(as.numeric(response_time) ~ correct + cond, data= acc_crit, FUN=mean)



# Inferential statistics
# priors (and code in general) from the chapter about logistic regression in Nicenboim et al., 2023 

# contrast coding
acc_crit$sem <- ifelse(acc_crit$cond %in% c("b", "d"), 0.5, -0.5)
acc_crit$syn <- ifelse(acc_crit$cond %in% c("c", "d"), 0.5, -0.5)


fit_acc <- brm(correct ~ 1 + syn*sem + (1|subject_nr) + (1|item),
               data = acc_crit,
               family = bernoulli(link = logit),
               prior = c(
                 prior(normal(0, 1.5), class = Intercept),
                 prior(normal(0, .1), class = b)),
               warmup = 2000,
               iter = 8000,
               cores = 4
)

plot(fit_acc)

posterior_summary(fit_acc,
                  variable = c("b_Intercept", "b_syn", "b_sem", "b_syn:sem"))


