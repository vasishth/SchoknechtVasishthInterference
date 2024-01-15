# author: Pia Schoknecht
# date: 30.07.2023

# script to create data sets for the pre-critical, critical and spill-over region

# load packages
library(tidyverse)


# load data
df1 <- read.csv("../pandora_web/data/pandora_web_data_allsessions_short.csv", sep=";")
df2 <- read.csv("../pandora_repS1/data/pandora_repS1_data_cleaned_short.csv", sep=";")
df3 <- read.csv("../pandora_repS2/data/pandora_repS2_data_cleaned_short.csv", sep=";")

# delete some columns in df1 to have identical columns in all data sets
df1$delay <- NULL
df1$dist_len <- NULL

# create exp column
df1$exp <- paste0("twosessions_s",df1$session)
df1$session <- NULL
df2$exp <- "rep_s1"
df3$exp <- "rep_s2"

# combine data sets
df <- rbind(df1, df2, df3)
df$participant <- df$fullname

# exclude fillers
df2 <- droplevels(subset(df, label != "filler"))

# contrast coding
df2$syn <- ifelse(df2$condition%in%c("a","b"),-0.5,0.5)
df2$sem <- ifelse(df2$condition%in%c("b","d"),0.5,-0.5)

# trim all rts below 150 and above 3000 ms
df3 <- subset(df2, df2$rt > 150 & df2$rt < 3000)


## Pre-critical word ## 
# subset for pre-critical word
precrit <- df3 %>% filter(region == "pre-crit")

# save
write.csv(precrit, file = "pandora_spr_774_precrit.csv", row.names = FALSE)



## Critical word ## 
# subset for critical word
crit <- df3 %>% filter(region == "critical")

# save
write.csv(crit, file = "pandora_spr_774_crit.csv", row.names = FALSE)




## Spill-over word ## 
# subset for spill-over word
spill <- df3 %>% filter(region == "spill1")

# save
write.csv(spill, file = "pandora_spr_774_spill.csv", row.names = FALSE)
