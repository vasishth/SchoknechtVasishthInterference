#author: "Pia Schoknecht"
#date: "16 06 2023"

# load packages
library(plyr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(cowplot)

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

#extract words from example item for x-axis labels
allwords_losyn<-unique(df_trim[df_trim$item=="008" & df_trim$condition=="a" ,c("wordno")])
allwords_hisyn<-unique(df_trim[df_trim$item=="008" & df_trim$condition=="c" ,c("wordno")])

# subsets for hisyn and losyn (because they have different lengths)
df_hisyn <- df_trim %>% filter(condition == "c" | condition == "d")
df_losyn <- df_trim %>% filter(condition == "a" | condition == "b")

#### Plot whole sentence ####

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

hisyn_summ_by_participants$word <- hisyn_summ_by_participants$wordno
losyn_summ_by_participants$word <- losyn_summ_by_participants$wordno


# plot reading times of the whole sentence for high syntactic interference 
p_hisyn <- ggplot(data=hisyn_summ_by_participants,aes(x=word, 
                                                      y=meanlogrt,
                                                      linetype=semantic.interference,
                                                      shape=syntactic.interference,
                                                      ymin=low2SE, 
                                                      ymax=high2SE)) +
  geom_point(aes(), size=1.2, position= position_dodge(0.1)) + 
  geom_line(aes()) +
  geom_errorbar(aes(), position= position_dodge(0.1),size=.3, width = 0.2)+
  theme(axis.text.x = element_text(size=10)) +
  theme_bw(base_size = 10)+
  ggtitle("High syntactic interference")+
  scale_y_continuous(name="observed reading\ntime in (ms)",breaks=log(seq(320,475,20)),labels= seq(320,475,20))+ 
  theme(axis.text.x = element_blank())+
  xlab("")+
  geom_text(aes(x = 12, y = 6.0, label = "distractor"), color="black") +
  geom_text(aes(x = 5.1, y = 5.81, label = "subject"), color="black") +
  geom_segment(aes(x = 5.05, y = 5.84, xend = 5.05, yend = 5.9), color="black",
               arrow = arrow(length = unit(0.03, "npc")), inherit.aes = FALSE)+
  geom_segment(aes(x = 12, y = 5.98, xend = 12, yend = 5.91), color="black",
               arrow = arrow(length = unit(0.03, "npc")), inherit.aes = FALSE)+
  geom_text(aes(x = 17, y =6.05, label = "critical verb"), color="black") +
  geom_segment(aes(x = 17, y = 6.03, xend = 17, yend = 5.97), color="black",
               arrow = arrow(length = unit(0.03, "npc")), inherit.aes = FALSE)+
  scale_shape(guide="none")+
  theme(legend.position=c(0.483,0.74))  +
  theme(legend.key.size = unit(0.3, "cm"))+
  scale_linetype_discrete(name="Semantic Interference")


# plot reading times of the whole sentence for low syntactic interference 
p_losyn <- ggplot(data=losyn_summ_by_participants,aes(x=word, 
                                                      y=meanlogrt,
                                                      linetype=semantic.interference,
                                                      shape=syntactic.interference, 
                                                      ymin=low2SE, 
                                                      ymax=high2SE)) +
  geom_point(aes(),shape=17,size=1.2, position= position_dodge(0.1)) + 
  geom_line(aes()) +
  geom_errorbar(aes(), position= position_dodge(0.1),size=.3, width = 0.2)+
  theme(axis.text.x = element_text(size=10)) +
  theme_bw(base_size = 10)+
  theme(legend.position="none")  +
  #theme(legend.position=c(0.483,0.75), legend.text=element_text(size=1))  +
  #scale_linetype_discrete(name="Semantic Interference")+
  xlab("")+
  ggtitle("Low syntactic interference")+
  scale_y_continuous(name="observed reading\ntime in (ms)",breaks=log(seq(320,475,20)),labels= seq(320,475,20))+ 
  theme(axis.text.x = element_blank())+
  geom_text(aes(x = 11, y = 6.1, label = "distractor"), color="black") +
  geom_text(aes(x = 5.1, y = 5.8, label = "subject"), color="black") +
  geom_segment(aes(x = 5.05, y = 5.84, xend = 5.05, yend = 5.9), color="black",
               arrow = arrow(length = unit(0.03, "npc")), inherit.aes = FALSE)+
  geom_segment(aes(x = 11, y = 6.08, xend = 11, yend = 6.0), color="black",
               arrow = arrow(length = unit(0.03, "npc")), inherit.aes = FALSE)+ 
  geom_text(aes(x = 16, y =6.08, label = "critical verb"), color="black") +
  geom_segment(aes(x = 16, y = 6.06, xend = 16, yend = 5.98), color="black",
               arrow = arrow(length = unit(0.03, "npc")), inherit.aes = FALSE)+
  scale_shape(guide="none")

#### Plot critical region (and surrounding regions) of all data ####
lo <- filter(losyn_summ_by_participants,wordno %in% c(14:17))
hi <- filter(hisyn_summ_by_participants,wordno %in% c(15:18))

zoom <- rbind(lo,hi)

zoom$region <- factor(zoom$region, levels = c("pre-pre-critical", "pre-critical", "critical", "spill-over"))
zoom$word <- ifelse(zoom$region %in% c("pre-pre-critical"), "n-2",
                    ifelse(zoom$region %in% c("pre-critical"), "n-1",
                           ifelse(zoom$region %in% c("spill-over"), "n+1",
                                  "critical")))

zoom$word <- factor(zoom$word, levels = c("n-2", "n-1", "critical", "n+1"))

zoom$highlight <- ifelse(zoom$word %in% c("critical"), "yes", "no")

p_zoom <- ggplot(data=zoom, aes(x=word, y=meanlogrt,
                                shape=syntactic.interference,
                                linetype=semantic.interference,
                                ymin=low2SE,ymax=high2SE)) +
  geom_point(aes(group = interaction(syntactic.interference, semantic.interference),color=highlight), size=3, position= position_dodge(0.4)) + 
  geom_line(aes(group = interaction(syntactic.interference, semantic.interference)), position= position_dodge(0.4), size=0.4) +
  geom_errorbar(aes(group = interaction(syntactic.interference, semantic.interference),color=highlight), position= position_dodge(0.4), width = 0.2)+
  scale_color_manual(values = c("black", "red"))+
  theme_bw(base_size = 8)+
  theme(legend.position="none")+
  #theme(legend.position=c(0.8,0.3), legend.direction = "vertical")  +
  scale_linetype_discrete(name="Semantic Interference")+
  scale_shape_discrete(name="Syntactic Interference")+
  ggtitle("E1a and E1b combined")+
  theme(legend.text = element_text(size = 12))+
  scale_y_continuous(name="observed reading time in (ms)", breaks=log(seq(340,390, 10)),labels= seq(340,390, 10))+
  guides(color = "none")

#### Plot critical region (and surrounding regions) per experiment ####
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



hisyn_summ_by_participants$syntactic.interference <- ifelse(hisyn_summ_by_participants$condition %in% c("a","b"), "low", "high")
hisyn_summ_by_participants$semantic.interference <- ifelse(hisyn_summ_by_participants$condition %in% c("a","c"), "low", "high")
losyn_summ_by_participants$syntactic.interference <- ifelse(losyn_summ_by_participants$condition %in% c("a","b"), "low", "high")
losyn_summ_by_participants$semantic.interference <- ifelse(losyn_summ_by_participants$condition %in% c("a","c"), "low", "high")

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
lo <- filter(losyn_summ_by_participants,wordno %in% c(14:17))
hi <- filter(hisyn_summ_by_participants,wordno %in% c(15:18))

zoom <- rbind(lo,hi)

zoom$word <- ifelse(zoom$region %in% c("pre-pre-critical"), "n-2",
                    ifelse(zoom$region %in% c("pre-critical"), "n-1",
                           ifelse(zoom$region %in% c("spill-over"), "n+1",
                                  zoom$region)))

zoom$word <- factor(zoom$word, levels = c("n-2", "n-1", "critical", "n+1"))

zoom$highlight <- ifelse(zoom$word %in% c("critical"), "yes", "no")

# split into sub-exps
e1a_zoom <- zoom %>% filter(exp %in% c("E1a, items 1-60, N=204","E1a, items 61-120, N=160"))
e1b_zoom <- zoom %>% filter(exp %in% c("E1b, items 1-60, N=350","E1b, items 61-120, N=220"))

e1a_zoom2 <- e1a_zoom %>% separate(exp, c("exp", "more_info"), sep = ",", extra="merge")
e1b_zoom2 <- e1b_zoom %>% separate(exp, c("exp", "more_info"), sep = ",", extra="merge")

# plot
p_e1a_zoom  <- ggplot(data=e1a_zoom2, aes(x=word, y=meanlogrt,
                                          shape=syntactic.interference,
                                          linetype=semantic.interference,
                                          ymin=low2SE,ymax=high2SE)) +
  geom_point(aes(group = interaction(syntactic.interference, semantic.interference),color=highlight), size=3, position= position_dodge(0.6)) + 
  geom_line(aes(group = interaction(syntactic.interference, semantic.interference)), position= position_dodge(0.6), size=0.4) +
  geom_errorbar(aes(group = interaction(syntactic.interference, semantic.interference),color=highlight), position= position_dodge(0.6), width = 0.2)+
  scale_color_manual(values = c("black", "red"))+
  theme_bw(base_size = 8)+
  facet_grid(.~more_info)+
  theme(legend.position=c(0.75,0.7), legend.direction = "vertical")  +
  ggtitle("E1a")+
  scale_linetype_discrete(name="Semantic Interference")+
  scale_shape_discrete(name="Syntactic Interference")+
  theme(legend.text = element_text(size = 10))+
  scale_y_continuous(name="observed reading time in (ms)", breaks=log(seq(300,410, 10)),labels= seq(300,410, 10))+
  guides(color = "none")


p_e1b_zoom <- ggplot(data=e1b_zoom2, aes(x=word, y=meanlogrt,
                                         shape=syntactic.interference,
                                         linetype=semantic.interference,
                                         ymin=low2SE,ymax=high2SE)) +
  geom_point(aes(group = interaction(syntactic.interference, semantic.interference),color=highlight), size=3, position= position_dodge(0.6)) + 
  geom_line(aes(group = interaction(syntactic.interference, semantic.interference)), position= position_dodge(0.6), size=0.4) +
  geom_errorbar(aes(group = interaction(syntactic.interference, semantic.interference),color=highlight), position= position_dodge(0.6), width = 0.2)+
  scale_color_manual(values = c("black", "red"))+
  theme_bw(base_size = 8)+
  facet_grid(.~more_info)+
  ggtitle("E1b")+
  theme(legend.position="none")+
  scale_linetype_discrete(name="Semantic Interference")+
  scale_shape_discrete(name="Syntactic Interference")+
  theme(legend.text = element_text(size = 10))+
  scale_y_continuous(name="observed reading time in (ms)", breaks=log(seq(300,410, 10)),labels= seq(300,410, 10))+
  guides(color = "none")

# Put everything together
p_sentence <- plot_grid(p_hisyn, p_losyn, nrow=2, labels=c("A", "B"))
ggsave("Pandora_all_wholesentence_pooled.jpg", width=10, height=4.5, dpi=600)

p_zooms <- plot_grid(p_zoom, p_e1a_zoom, p_e1b_zoom, ncol=3, rel_widths = c(1,1.45,1.45), labels=c("C", "D", "E"))


plot_grid(p_sentence, p_zooms, ncol=1)
ggsave("Pandora_all_wholesentence_pooled_zoom_exp.jpg", width=10, height=7, dpi=600)

