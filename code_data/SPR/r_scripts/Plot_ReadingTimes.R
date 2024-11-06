#author: "Pia Schoknecht"
#date: "05 11 2024"

# load packages
library(plyr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(cowplot)
library(ggh4x)

# load data
df1 <- read.csv("../data/pandora_web_data_allsessions_short.csv", sep=";")
df2 <- read.csv("../data/pandora_repS1_data_cleaned_short.csv", sep=";")
df3 <- read.csv("../data/pandora_repS2_data_cleaned_short.csv", sep=";")

# delete some columns in df1 to have identical columns in all data sets
df1$delay <- NULL
df1$dist_len <- NULL

# create exp column
df1$exp <- ifelse(df1$session%in%c("1"),"E1a, items 1-60, N=204","E1a, items 61-120, N=160")
df1$session <- NULL
df2$exp <- "E1b, items 1-60, N=350"
df3$exp <- "E1b, items 61-120, N=220"

# combine data sets
df <- rbind(df1, df2, df3)
df$participant <- df$fullname

# exclude fillers
df_nofill <- droplevels(subset(df, label != "filler"))

# trim all rts below 150 and above 3000 ms
df_trim<- subset(df_nofill, df_nofill$rt > 150 & df_nofill$rt < 3000)

# how much data was affected by the trimming?
round(100 - (nrow(df_trim) / nrow(df_nofill)) *100,1)

# subsets for hisyn and losyn (because they have different lengths)
df_hisyn <- df_trim %>% filter(condition == "c" | condition == "d")
df_losyn <- df_trim %>% filter(condition == "a" | condition == "b")

#### Plot whole sentence ####

#calculate mean reading time per subject per word per condition
hisyn_by_participants <- summarize(
  group_by(
    filter(df_hisyn,wordno %in% c("0", "1", "2", "3", "4", "5", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "21")),
    wordno,condition,subj, exp),
  logrt=mean(log(rt)))

losyn_by_participants <- summarize(
  group_by(
    filter(df_losyn,wordno %in% c("0", "1", "2", "3", "4", "5", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "20")),
    wordno,condition,subj, exp),
  logrt=mean(log(rt)))

# calculate mean per word per condition (averaged over subjects)
hisyn_summ_by_participants <- summarize(
  group_by(hisyn_by_participants,wordno,condition),
  meanlogrt=mean(logrt), 
  SE = sd(logrt)/sqrt(nrow(hisyn_by_participants)), 
  low2SE= meanlogrt - 2*SE, high2SE= meanlogrt + 2*SE)

losyn_summ_by_participants <- summarize(
  group_by(losyn_by_participants,wordno,condition),
  meanlogrt=mean(logrt), 
  SE = sd(logrt)/sqrt(nrow(losyn_by_participants)), 
  low2SE= meanlogrt - 2*SE, high2SE= meanlogrt + 2*SE)

# add nice condition labels
hisyn_summ_by_participants$syntactic.interference <- ifelse(hisyn_summ_by_participants$condition %in% c("a","b"), "low", "high")
hisyn_summ_by_participants$semantic.interference <- ifelse(hisyn_summ_by_participants$condition %in% c("a","c"), "low", "high")
losyn_summ_by_participants$syntactic.interference <- ifelse(losyn_summ_by_participants$condition %in% c("a","b"), "low", "high")
losyn_summ_by_participants$semantic.interference <- ifelse(losyn_summ_by_participants$condition %in% c("a","c"), "low", "high")

# add nice region labels
losyn_summ_by_participants$region<- ifelse(losyn_summ_by_participants$wordno == 16, "critical",
                                           ifelse(losyn_summ_by_participants$wordno == 15, "pre-critical", 
                                                  ifelse(losyn_summ_by_participants$wordno == 14, "pre-pre-critical",        
                                                         ifelse(losyn_summ_by_participants$wordno == 17, "spill-over",        
                                                                "other"))))

hisyn_summ_by_participants$region<- ifelse(hisyn_summ_by_participants$wordno == 17, "critical",
                                           ifelse(hisyn_summ_by_participants$wordno == 16, "pre-critical", 
                                                  ifelse(hisyn_summ_by_participants$wordno == 15, "pre-pre-critical",        
                                                         ifelse(hisyn_summ_by_participants$wordno == 18, "spill-over",        
                                                                "other"))))

hisyn_words <- c("Die", "Nachbarin", "glaubte,", "dass", "der", "Witwer,", "[line\nbreak]", "der", "erzählt", "hatte,", "dass",           "der",                  "Verlust/\nEinbrecher", "schrecklich", "war,", "regelmäßig", "abends", "trank,", "um", "zu", "[line\nbreak]", "ver-\ngessen.")
losyn_words <- c("Die", "Nachbarin", "glaubte,", "dass", "der", "Witwer,", "[line\nbreak]", "der", "von",    "dem",     "schrecklichen", "Verlust/\nEinbrecher", "erzählt",                "hatte",              "regelmäßig", "abends", "trank,", "um", "zu", "[line\nbreak]", "ver-\ngessen.")


# plot reading times of the whole sentence for high syntactic interference 
p_hisyn <- ggplot(data=hisyn_summ_by_participants,aes(x=wordno, 
                                                      y=meanlogrt,
                                                      ymin=low2SE, 
                                                      ymax=high2SE)) +
  geom_point(aes(), size=1.2, position= position_dodge(0.1)) + 
  geom_line(aes(linetype=semantic.interference)) +
  geom_errorbar(aes(), position= position_dodge(0.1),size=.3, width = 0.2)+
  theme(axis.text.x = element_text(size=7)) +
  theme_bw(base_size = 10)+
  ggtitle("High syntactic interference")+
  scale_y_continuous(name="observed reading\ntime in (ms)",breaks=log(seq(320,475,20)),labels= seq(320,475,20))+ 
  scale_x_continuous(breaks = c(0:21), labels= hisyn_words, expand = c(0.01, 0.01)) +
  xlab("")+
  geom_text(aes(x = 12, y = 6.01, label = "distractor"), color="black") +
  geom_text(aes(x = 5.1, y = 5.83, label = "subject"), color="black") +
  geom_segment(aes(x = 5.05, y = 5.88, xend = 5.05, yend = 5.99), color="black",
                arrow = arrow(length = unit(0.03, "npc")), inherit.aes = FALSE)+
  geom_segment(aes(x = 12, y = 5.98, xend = 12, yend = 5.91), color="black",
                arrow = arrow(length = unit(0.03, "npc")), inherit.aes = FALSE)+
  geom_text(aes(x = 17, y =6.09, label = "critical verb"), color="black") +
  geom_segment(aes(x = 17, y = 6.06, xend = 17, yend = 5.99), color="black",
                arrow = arrow(length = unit(0.03, "npc")), inherit.aes = FALSE)+
  theme(legend.position="none")


# plot reading times of the whole sentence for low syntactic interference 
p_losyn <- ggplot(data=losyn_summ_by_participants,aes(x=wordno, 
                                                      y=meanlogrt,
                                                      ymin=low2SE, 
                                                      ymax=high2SE)) +
  geom_point(aes(),shape=17,size=1.2, position= position_dodge(0.1)) + 
  geom_line(aes(linetype=semantic.interference)) +
  geom_errorbar(aes(), position= position_dodge(0.1),size=.3, width = 0.2)+
  theme(axis.text.x = element_text(size=7)) +
  theme_bw(base_size = 10)+
  xlab("")+
  ggtitle("Low syntactic interference")+
  scale_y_continuous(name="observed reading\ntime in (ms)",breaks=log(seq(320,475,20)),labels= seq(320,475,20))+ 
  scale_x_continuous(breaks = c(0:20), labels= losyn_words, expand = c(0.01, 0.01)) +
  geom_text(aes(x = 5.1, y = 5.83, label = "subject"), color="black") +
  geom_segment(aes(x = 5.05, y = 5.88, xend = 5.05, yend = 5.99), color="black",
               arrow = arrow(length = unit(0.03, "npc")), inherit.aes = FALSE)+
  geom_text(aes(x = 11, y = 6.1, label = "distractor"), color="black") +
  geom_segment(aes(x = 11, y = 6.077, xend = 11, yend = 6.01), color="black",
               arrow = arrow(length = unit(0.03, "npc")), inherit.aes = FALSE)+
  geom_text(aes(x = 16, y =6.08, label = "critical verb"), color="black") +
  geom_segment(aes(x = 16, y = 6.05, xend = 16, yend = 5.98), color="black",
               arrow = arrow(length = unit(0.03, "npc")), inherit.aes = FALSE)+
  scale_shape(guide="none")+
  theme(legend.position=c(0.435,0.73))  +
  theme(legend.key.size = unit(0.25, "cm"))+
  scale_linetype_discrete(name="Semantic Interference")

#### Plot reading times of the whole sentence per experiment ####
# start with by-experiment averages
#calculate mean reading time per subject per word per condition
hisyn_by_participants <- summarize(
  group_by(
    filter(df_hisyn,wordno %in% c("0", "1", "2", "3", "4", "5", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19")),
    wordno,condition,subj, exp),
  logrt=mean(log(rt)))

losyn_by_participants <- summarize(
  group_by(
    filter(df_losyn,wordno %in% c("0", "1", "2", "3", "4", "5", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18")),
    wordno,condition,subj, exp),
  logrt=mean(log(rt)))

#calculate mean per word per condition (averaged over subjects)
hisyn_summ_by_participants <- summarize(
  group_by(hisyn_by_participants,wordno,condition, exp),
  meanlogrt=mean(logrt), 
  SE = sd(logrt)/sqrt(nrow(hisyn_by_participants)), 
  low2SE= meanlogrt - 2*SE, high2SE= meanlogrt + 2*SE)

losyn_summ_by_participants <- summarize(
  group_by(losyn_by_participants,wordno,condition, exp),
  meanlogrt=mean(logrt), 
  SE = sd(logrt)/sqrt(nrow(losyn_by_participants)), 
  low2SE= meanlogrt - 2*SE, high2SE= meanlogrt + 2*SE)

# add nice condition labels
hisyn_summ_by_participants$syntactic.interference <- ifelse(hisyn_summ_by_participants$condition %in% c("a","b"), "low", "high")
hisyn_summ_by_participants$semantic.interference <- ifelse(hisyn_summ_by_participants$condition %in% c("a","c"), "low", "high")
losyn_summ_by_participants$syntactic.interference <- ifelse(losyn_summ_by_participants$condition %in% c("a","b"), "low", "high")
losyn_summ_by_participants$semantic.interference <- ifelse(losyn_summ_by_participants$condition %in% c("a","c"), "low", "high")


# split exp info
hisyn_summ_by_participants2 <- hisyn_summ_by_participants %>% separate(exp, c("exp", "more_info"), sep = ",", extra="merge")
losyn_summ_by_participants2 <- losyn_summ_by_participants %>% separate(exp, c("exp", "more_info"), sep = ",", extra="merge")


# create minimal x-axis labels
hisyn_words_minimal <- c("subject","distractor",  "critical\nverb")
losyn_words_minimal <- c("subject","distractor","critical\nverb")

# hisyn plot per exp
p_hisyn_exp <- ggplot(data=filter(hisyn_summ_by_participants2, wordno>=5 & wordno<=19),aes(x=wordno, 
                                                      y=meanlogrt,
                                                      ymin=low2SE, 
                                                      ymax=high2SE)) +
  geom_point(aes(), size=1, position= position_dodge(0.1)) + 
  geom_line(aes(linetype=semantic.interference)) +
  geom_errorbar(aes(), position= position_dodge(0.1),size=.3, width = 0.2)+
  theme(axis.text.x = element_text(size=7)) +
  theme_bw(base_size = 10)+
  facet_nested(. ~ exp + more_info)+
  ggtitle("High syntactic interference in each experimental session")+
  scale_y_continuous(name="observed reading time in (ms)",breaks=log(seq(320,480,40)),labels= seq(320,480,40))+ 
  scale_x_continuous(breaks = c(5, 12, 17), labels= hisyn_words_minimal, expand = c(0.01, 0.01)) +
  xlab("")+
  theme(legend.position="none")+
  theme(panel.spacing = unit(1.025, "lines"))

p_hisyn_exp 

# losyn plot per exp
p_losyn_exp <- ggplot(data=filter(losyn_summ_by_participants2, wordno>=5 & wordno<=18),aes(x=wordno, 
                                                          y=meanlogrt,
                                                          ymin=low2SE, 
                                                          ymax=high2SE)) +
  geom_point(aes(), shape=17, size=1, position= position_dodge(0.1)) + 
  geom_line(aes(linetype=semantic.interference)) +
  geom_errorbar(aes(), position= position_dodge(0.1),size=.3, width = 0.2)+
  theme(axis.text.x = element_text(size=7)) +
  theme_bw(base_size = 10)+
  facet_nested(. ~ exp + more_info)+
  ggtitle("Low syntactic interference in each experimental session")+
  scale_y_continuous(name="observed reading time in (ms)",breaks=log(seq(320,480,40)),labels= seq(320,480,40))+ 
  scale_x_continuous(breaks = c(5,11,16), labels= losyn_words_minimal, expand = c(0.01, 0.01)) +
  xlab("")+
  theme(legend.position="none")+
  theme(panel.spacing = unit(1.025, "lines"))

p_losyn_exp 

# Put everything together
p_sentence <- plot_grid(p_hisyn, p_losyn, nrow=2, labels=c("A", "B"))
ggsave("Pandora_all_wholesentence_pooled.jpg", width=12, height=4.5, dpi=600)

p_exp <- plot_grid(p_hisyn_exp, p_losyn_exp, ncol=2, rel_widths = c(1.01,1), labels=c("C", "D"))

plot_grid(p_sentence, p_exp, ncol=1, rel_heights = c(1.5,1))
ggsave("Pandora_all_wholesentence_pooled_exp.jpg", width=12, height=7, dpi=600)

