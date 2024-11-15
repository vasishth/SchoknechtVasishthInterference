#author: "Pia Schoknecht"
#date: "15 11 2024"

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
lo_wordno <- c(10:18) 
hi_wordno <- c(11:19) 
region <- c("distractor-1", "distractor", "distractor+1", "distractor+2",
            "pre-pre-critical", "pre-critical",
            "critical", "critical+1", "critical+2")
fake_wordno <- c(1:9)
lo_dict.df<- data.frame(lo_wordno, fake_wordno, region) 
hi_dict.df<- data.frame(hi_wordno, fake_wordno, region) 

lo <- left_join(losyn_summ_by_participants, lo_dict.df, by=c("wordno" = "lo_wordno"))
hi <- left_join(hisyn_summ_by_participants, hi_dict.df, by=c("wordno" = "hi_wordno"))


# words from example item for x axis labels
hisyn_words <- c("Die", "Nachbarin", "glaubte,", "dass", "der", "Witwer,", "[line\nbreak]", "der", "erzählt", "hatte,", "dass",           "der",                  "Verlust/\nEinbrecher", "schrecklich", "war,", "regelmäßig", "abends", "trank,", "um", "zu", "[line\nbreak]", "ver-\ngessen.")
losyn_words <- c("Die", "Nachbarin", "glaubte,", "dass", "der", "Witwer,", "[line\nbreak]", "der", "von",    "dem",     "schrecklichen", "Verlust/\nEinbrecher", "erzählt",                "hatte",              "regelmäßig", "abends", "trank,", "um", "zu", "[line\nbreak]", "ver-\ngessen.")


# plot reading times of the whole sentence for high syntactic interference 
p_hisyn <- ggplot(data=hi,aes(x=wordno, y=meanlogrt, ymin=low2SE, ymax=high2SE)) +
                geom_point(aes(), size=1.2, position= position_dodge(0.1)) + 
                geom_line(aes(linetype=semantic.interference)) +
                geom_errorbar(aes(), position= position_dodge(0.1),size=.3, width = 0.2)+
                theme_bw(base_size = 10)+
                ggtitle("High syntactic interference")+
                scale_y_continuous(name="observed reading\ntime in (ms)",breaks=log(seq(320,475,20)),labels= seq(320,475,20))+ 
                scale_x_continuous(breaks = c(0:21), labels= hisyn_words, expand = c(0.01, 0.01)) +
                xlab("")+
                geom_text(aes(x = 12, y = 6.0, label = "distractor"), color="black") +
                geom_text(aes(x = 5.1, y = 5.81, label = "subject"), color="black") +
                geom_segment(aes(x = 5, y = 5.84, xend = 5, yend = 5.9), color="black",
                              arrow = arrow(length = unit(0.03, "npc")), inherit.aes = FALSE)+
                geom_segment(aes(x = 12, y = 5.98, xend = 12, yend = 5.91), color="black",
                              arrow = arrow(length = unit(0.03, "npc")), inherit.aes = FALSE)+
                geom_text(aes(x = 17, y =6.05, label = "critical verb"), color="black") +
                geom_segment(aes(x = 17, y = 6.03, xend = 17, yend = 5.97), color="black",
                              arrow = arrow(length = unit(0.03, "npc")), inherit.aes = FALSE)+
                scale_shape(guide="none")+
                theme(legend.position=c(0.69,0.79))  +
                theme(legend.key.size = unit(0.25, "cm"))+
                scale_linetype_discrete(name="Semantic Interference")


# plot reading times of the whole sentence for low syntactic interference 
p_losyn <- ggplot(data=lo,aes(x=wordno, y=meanlogrt, ymin=low2SE, ymax=high2SE)) +
                geom_point(aes(),shape=17,size=1.2, position= position_dodge(0.1)) + 
                geom_line(aes(linetype=semantic.interference)) +
                geom_errorbar(aes(), position= position_dodge(0.1),size=.3, width = 0.2)+
                theme_bw(base_size = 10)+
                xlab("")+
                ggtitle("Low syntactic interference")+
                scale_y_continuous(name="observed reading\ntime in (ms)",breaks=log(seq(320,475,20)),labels= seq(320,475,20))+ 
                scale_x_continuous(breaks = c(0:20), labels= losyn_words, expand = c(0.01, 0.01)) +
                geom_text(aes(x = 11, y = 6.1, label = "distractor"), color="black") +
                geom_text(aes(x = 5.1, y = 5.8, label = "subject"), color="black") +
                geom_segment(aes(x = 5, y = 5.84, xend = 5, yend = 5.9), color="black",
                             arrow = arrow(length = unit(0.03, "npc")), inherit.aes = FALSE)+
                geom_segment(aes(x = 11, y = 6.08, xend = 11, yend = 6.0), color="black",
                             arrow = arrow(length = unit(0.03, "npc")), inherit.aes = FALSE)+ 
                geom_text(aes(x = 16, y =6.08, label = "critical verb"), color="black") +
                geom_segment(aes(x = 16, y = 6.06, xend = 16, yend = 5.98), color="black",
                             arrow = arrow(length = unit(0.03, "npc")), inherit.aes = FALSE)+
                theme(legend.position="none")  

#### Create plot with only the interesting regions for all data combined ####
lo2 <- na.omit(lo)
hi2 <- na.omit(hi)

zoom <- rbind(lo2,hi2)

p_zoom <- ggplot(data=zoom, aes(x=fake_wordno, y=meanlogrt,
                                shape=syntactic.interference,
                                linetype=semantic.interference,
                                ymin=low2SE,ymax=high2SE)) +
                geom_point(aes(group = interaction(syntactic.interference, semantic.interference)), size=3, position= position_dodge(0.4)) + 
                geom_line(aes(group = interaction(syntactic.interference, semantic.interference)), position= position_dodge(0.4), size=0.4) +
                geom_errorbar(aes(group = interaction(syntactic.interference, semantic.interference)), position= position_dodge(0.4), width = 0.2)+
                scale_x_continuous(name="", breaks = c(1:9), labels= region, expand = c(0.01, 0.01)) +
                theme_bw(base_size = 10)+
               # theme(legend.position="none")+
                theme(legend.position=c(0.8,0.87), legend.direction = "horizontal", legend.box = "horizontal")  +
                scale_linetype_discrete(name="Semantic Interf.")+
                scale_shape_discrete(name="Syntactic Interf.")+
                ggtitle("All conditions")+
                theme(legend.text = element_text(size = 11), legend.key.size = unit(0.25, "cm"))+
                scale_y_continuous(name="observed reading time in (ms)", breaks=log(seq(320,420, 20)),labels= seq(320,420, 20))

# Put everything together
plot_grid(p_hisyn, p_losyn, p_zoom, ncol=1, rel_heights = c(0.9,0.9,1), labels="AUTO")
ggsave("Pandora_all_wholesentence_pooled_zoom_exp_test.jpg", width=12, height=7, dpi=600)

