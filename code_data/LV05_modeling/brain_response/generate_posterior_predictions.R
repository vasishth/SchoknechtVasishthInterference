# author: Pia Schoknecht
# date: 23.07.2024

# load packages
library(truncnorm)
library(ggplot2)
library(ggbreak)
library(cowplot)
library(stringr)
library(tidyverse)

# save Schoknecht & Vasishth mean, lower and upper separately to add them to plots later
# add data from Schoknecht & Vasishth ERP experiment
SVdat <- data.frame(pred   = rep("data",3),
                  effect    = c("syntactic", "semantic", "interaction"), 
                  mean  = c(-0.14,-0.3,-0.03),
                  lower = c(0.05,-0.1,0.38),
                  upper = c(-0.33,-0.5,-0.33))
SVdat$effect <- factor(SVdat$effect, levels=c("syntactic", "semantic", "interaction"))


#### TWO CUE VERSION OF THE ORIGINAL INTERACT MODEL WITH COMPLEX SYNTACTIC CUE {+subject within same clause} #####

# Load model 
source("interACT.R")

reset_params()

# Parameters to be passed to run() function for the simulations, including cue weights
model_4cond <- list(
  target_match = list(c(1,1), c(1,1), c(1,1), c(1,1)),
  distractor_match = list(c(0,0), c(0,1), c(0,0), c(0,1)),
  Target =     c("LoSyn","LoSyn","HiSyn","HiSyn"),
  Distractor = c("LoSem","HiSem","LoSem","HiSem"),
  weights = list(c(strWeight(),semWeight()))
)

mas <<- c(1.5) # Maximum associative strength
mp <<- 0.5 # Mismatch penalty
psc <<- 0 # Prominence scaling
qcf <<- 1 # Match quality correction factor
cuesim <<- -1 # Cue-feature similarity [-1..0]
cueweighting <<- 1 # Cue weighting
VERBOSE <<- FALSE

load("simdat_2cues_sameclause.Rda")

# multiply by -1 for negative effects
simdat_2cues_sameclause$MESyn <- simdat_2cues_sameclause$MESyn * -1
simdat_2cues_sameclause$MESem <- simdat_2cues_sameclause$MESem * -1
simdat_2cues_sameclause$InteractionSynSem <- simdat_2cues_sameclause$InteractionSynSem * -1

# effect sizes in simulated data
round(c(mean = mean(simdat_2cues_sameclause$MESyn), quantile(simdat_2cues_sameclause$MESyn, probs = c(.025, .975))),2)
round(c(mean = mean(simdat_2cues_sameclause$MESem), quantile(simdat_2cues_sameclause$MESem, probs = c(.025, .975))),2)
round(c(mean = mean(simdat_2cues_sameclause$InteractionSynSem), quantile(simdat_2cues_sameclause$InteractionSynSem, probs = c(.025, .975))),2)

# data wrangling
simdat_2cues_sameclause2 <- simdat_2cues_sameclause %>% rename("syntactic" =MESyn, "semantic"=MESem, "interaction"=InteractionSynSem)
simdat_2cues_sameclause_long <- tidyr::pivot_longer(simdat_2cues_sameclause2, cols=c("syntactic","semantic","interaction"),
                                       names_to="effect", values_to="effsize")
simdat_2cues_sameclause_long$effsize <- as.numeric(simdat_2cues_sameclause_long$effsize)
simdat_2cues_sameclause_long$effect <- factor(simdat_2cues_sameclause_long$effect, levels=c("syntactic", "semantic", "interaction"))


#### Generate posterior predictions ####

# Run ABC with rejection sampling

# posterior latency factor 
# (we base this only on the semantic interference result)
SEM<- SVdat$upper[2]<=simdat_2cues_sameclause$MESem & simdat_2cues_sameclause$MESem<=SVdat$lower[2]
table(SEM)
posterior_lf_sem <-simdat_2cues_sameclause[SEM,]$lfs
post_lfs <- data.frame(lfs = posterior_lf_sem)
  
# generate posterior predicted data
len_lf_sem <- length(posterior_lf_sem)
maineffectSyn <- rep(NA,len_lf_sem)
maineffectSem <- rep(NA,len_lf_sem)
interactionSynSem <- rep(NA,len_lf_sem)
  
for(i in 1:len_lf_sem){
    # Run model
    print(paste("iteration: ",i,sep=""))  
    lf<<- posterior_lf_sem[i]  
    # ---------------------------
    sims <- create_param_matrix(model_4cond, iterations=5000)
    results <- run(sims)
    condMeans <- compute_cond_means(results)
    meSyn<-mean(c(condMeans$Latency[3],condMeans$Latency[4]))-
      mean(c(condMeans$Latency[1],condMeans$Latency[2]))
    maineffectSyn[i]<-meSyn
    meSem<-mean(c(condMeans$Latency[2],condMeans$Latency[4]))-
      mean(c(condMeans$Latency[1],condMeans$Latency[3]))
    maineffectSem[i]<-meSem
    synsem<- (condMeans$Latency[4] - condMeans$Latency[3])-
      (condMeans$Latency[2] - condMeans$Latency[1])
    interactionSynSem[i]<-synsem
}
postpreds_2cues <- data.frame(lfs=post_lfs,syntactic=(maineffectSyn*-1),semantic=(maineffectSem*-1), interaction=(interactionSynSem*-1))

save(postpreds_2cues,file="posterior_predictions_2cues.Rda")
load("posterior_predictions_2cues.Rda")

###############################################

##### Plot prior and posterior predictions together #####
prior_predictions <- simdat_2cues_sameclause_long
prior_predictions$pred <- "prior predictions"

postpreds_2cues$pred <- "posterior predictions"

# prepare for plotting
postpreds_long <- tidyr::pivot_longer(postpreds_2cues, cols=c("syntactic","semantic","interaction"),
                                                    names_to="effect", values_to="effsize")

preds <- rbind(postpreds_long, prior_predictions)
preds$effect <- factor(preds$effect, levels=c("syntactic", "semantic", "interaction"))


library(ggbreak)
p.2cues <- ggplot(data=preds, x=effsize) +
  geom_density(aes(x=effsize,fill=pred), color="black", alpha=0.4) +
  geom_point(data=SVdat, aes(x=mean, y=-0.2), color="red", size=3)+
  geom_errorbar(data=SVdat, aes(y=-0.2, xmin=lower, xmax=upper), color="red", width=0.001, linewidth=1) +
  scale_fill_grey(start = 0, end = .9) +
  facet_wrap(~ effect)+
  theme_bw(base_size = 12)+
  geom_vline(xintercept=0, linetype="dashed")+
  xlab("amplitude difference (microV)")+
  ylab("density")+
  guides(fill = guide_legend(reverse=TRUE))+
  theme(legend.title=element_blank(), legend.position ="none")+
  scale_y_break(c(3, 10), scales=0.5)

ggsave("ERP_priorpred_vs_postpred_vs_data_2cues.png", width=7, height=4, dpi=600)


#### THREE CUE MODEL FROM MERTZEN ET AL. (2023) https://osf.io/a7cg2/?view_only= ####

# Load model 
source("interACT-threecues.R")
reset_params()

# Parameters to be passed to run() function for the simulations, including cue weights
model_4cond <- list(
  target_match = list(c(1,1,1), c(1,1,1), c(1,1,1), c(1,1,1)),
  distractor_match = list(c(0,0,0), c(0,0,1), c(0,1,0), c(0,1,1)),
  Target =     c("LoSyn","LoSyn","HiSyn","HiSyn"),
  Distractor = c("LoSem","HiSem","LoSem","HiSem"),
  weights = list(c(0.33,0.33,0.33))
)

mas <<- c(1.5) # Maximum associative strength
mp <<- 0.5 # Mismatch penalty
psc <<- 0 # Prominence scaling
qcf <<- 1 # Match quality correction factor
cuesim <<- -1 # Cue-feature similarity [-1..0]
cueweighting <<- 1 # Cue weighting
VERBOSE <<- FALSE

load("simdat_3cues.Rda")
simdat_3cues <- simdat_3cues_complete %>% select(lfs, MESyn, MESem, InteractionSynSem)

# multiply by -1 for negative effects
simdat_3cues$MESyn <- simdat_3cues$MESyn * -1
simdat_3cues$MESem <- simdat_3cues$MESem * -1
simdat_3cues$InteractionSynSem <- simdat_3cues$InteractionSynSem * -1

# effect sizes
round(c(mean = mean(simdat_3cues$MESyn), quantile(simdat_3cues$MESyn, probs = c(.025, .975))),2)
round(c(mean = mean(simdat_3cues$MESem), quantile(simdat_3cues$MESem, probs = c(.025, .975))),2)
round(c(mean = mean(simdat_3cues$InteractionSynSem), quantile(simdat_3cues$InteractionSynSem, probs = c(.025, .975))),2)

simdat_3cues2 <- simdat_3cues %>% rename("syntactic" =MESyn, "semantic"=MESem, "interaction"=InteractionSynSem)

# data wrangling
simdat_3cues_long <- tidyr::pivot_longer(simdat_3cues2, cols=c("syntactic", "semantic","interaction"),
                                                    names_to="effect", values_to="effsize")
simdat_3cues_long$effsize <- as.numeric(simdat_3cues_long$effsize)
simdat_3cues_long$effect <- factor(simdat_3cues_long$effect, levels=c("syntactic", "semantic", "interaction"))

#### Generate posterior predictions ####

# Run ABC with rejection sampling
  # posterior latency factor (we base this only on the semantic interference result)
SEM<- SVdat$upper[2]<=simdat_3cues$MESem & simdat_3cues$MESem<=SVdat$lower[2]
table(SEM)
posterior_lf_sem <-simdat_3cues[SEM,]$lfs
post_lfs <- data.frame(lfs = posterior_lf_sem)
  
# generate posterior predicted data
len_lf_sem <- length(posterior_lf_sem)
maineffectSyn <- rep(NA,len_lf_sem)
maineffectSem <- rep(NA,len_lf_sem)
interactionSynSem <- rep(NA,len_lf_sem)
  
for(i in 1:len_lf_sem){
 # Run model
    print(paste("iteration: ",i,sep=""))  
    lf<<- posterior_lf_sem[i]  
    # ---------------------------
    sims <- create_param_matrix(model_4cond, iterations=5000)
    results <- run(sims)
    condMeans <- compute_cond_means(results)
    meSyn<-mean(c(condMeans$Latency[3],condMeans$Latency[4]))-
      mean(c(condMeans$Latency[1],condMeans$Latency[2]))
    maineffectSyn[i]<-meSyn
    meSem<-mean(c(condMeans$Latency[2],condMeans$Latency[4]))-
      mean(c(condMeans$Latency[1],condMeans$Latency[3]))
    maineffectSem[i]<-meSem
    synsem<- (condMeans$Latency[4] - condMeans$Latency[3])-
      (condMeans$Latency[2] - condMeans$Latency[1])
    interactionSynSem[i]<-synsem
  }
postpreds_3cues <-data.frame(lfs=post_lfs,syntactic=(maineffectSyn*-1),semantic=(maineffectSem*-1), interaction=(interactionSynSem*-1))

save(postpreds_3cues,file="posterior_predictions_3cues.Rda")
load("posterior_predictions_3cues.Rda")

###############################################

##### Plot prior and posterior predictions together #####
prior_predictions <- simdat_3cues_long
prior_predictions$pred <- "prior predictions"

postpreds_3cues$pred <- "posterior predictions"

# prepare for plotting
postpreds_3cues_long <- tidyr::pivot_longer(postpreds_3cues, cols=c("syntactic","semantic","interaction"),
                                      names_to="effect", values_to="effsize")
postpreds_3cues_long$effsize <- as.numeric(postpreds_3cues_long$effsize)
postpreds_3cues_long$effect <- factor(postpreds_3cues_long$effect, levels=c("syntactic", "semantic", "interaction"))

preds <- rbind(postpreds_3cues_long, prior_predictions)


p.3cues <- ggplot(data=preds, x=effsize) +
  geom_density(aes(x=effsize,fill=pred), color="black", alpha=0.4) +
  geom_point(data=SVdat, aes(x=mean, y=-0.2), color="red", size=3)+
  geom_errorbar(data=SVdat, aes(y=-0.2, xmin=lower, xmax=upper), color="red", width=0.001, linewidth=1) +
  scale_fill_grey(start = 0, end = .9) +
  facet_wrap(~ effect)+
  theme_bw(base_size = 12)+
  geom_vline(xintercept=0, linetype="dashed")+
  xlab("amplitude difference (microV)")+
  ylab("density")+
  guides(fill = guide_legend(reverse=TRUE))+
  theme(legend.title=element_blank(), legend.position ="top")+
  scale_y_break(c(3, 4), scales=0.5)

ggsave("ERP_priorpred_vs_postpred_vs_data_3cues.png", width=7, height=4, dpi=600)

#### ONE CUE VERSION OF THE ORIGINAL INTERACT MODEL WITH COMPLEX SYNSEM CUE {+animate subject within same clause} #####

# Load model 
source("interACT-onecue.R")

reset_params()

# Parameters to be passed to run() function for the simulations, including cue weights
model_4cond <- list(
  target_match = list(c(1), c(1), c(1), c(1)),
  distractor_match = list(c(0), c(0), c(0), c(0)),
  Target =     c("LoSyn","LoSyn","HiSyn","HiSyn"),
  Distractor = c("LoSem","HiSem","LoSem","HiSem"),
    weights = list(c(1))
)

mas <<- c(1.5) # Maximum associative strength
mp <<- 0.5 # Mismatch penalty
psc <<- 0 # Prominence scaling
qcf <<- 1 # Match quality correction factor
cuesim <<- -1 # Cue-feature similarity [-1..0]
cueweighting <<- 1 # Cue weighting
VERBOSE <<- FALSE

load("simdat_1cue.Rda")

# multiply by -1 for negative effects 
# (it doesn't really matter because the effects in this model are centered around zero, but we do it for consistency)
simdat_1cue$MESyn <- simdat_1cue$MESyn * -1
simdat_1cue$MESem <- simdat_1cue$MESem * -1
simdat_1cue$InteractionSynSem <- simdat_1cue$InteractionSynSem * -1


# effect sizes
round(c(mean = mean(simdat_1cue$MESyn), quantile(simdat_1cue$MESyn, probs = c(.025, .975))),2)
round(c(mean = mean(simdat_1cue$MESem), quantile(simdat_1cue$MESem, probs = c(.025, .975))),2)
round(c(mean = mean(simdat_1cue$InteractionSynSem), quantile(simdat_1cue$InteractionSynSem, probs = c(.025, .975))),2)

simdat_1cue2 <- simdat_1cue %>% rename("syntactic" =MESyn, "semantic"=MESem, "interaction"=InteractionSynSem)

# data wrangeling
simdat_1cue_long <- tidyr::pivot_longer(simdat_1cue2, cols=c("syntactic","semantic","interaction"),
                                                    names_to="effect", values_to="effsize")
simdat_1cue_long$effsize <- as.numeric(simdat_1cue_long$effsize)
simdat_1cue_long$effect <- factor(simdat_1cue_long$effect, levels=c("syntactic", "semantic", "interaction"))


#### Generate posterior predictions ####

# Run ABC with rejection sampling

# posterior latency factor (we base this only on the semantic interference result)
SEM<- SVdat$upper[2]<=simdat_1cue$MESem & simdat_1cue$MESem<=SVdat$lower[2]

table(SEM)
posterior_lf_sem <-simdat_1cue[SEM,]$lfs
post_lfs <- data.frame(lfs = posterior_lf_sem)
  
# generate posterior predicted data
len_lf_sem <- length(posterior_lf_sem)
maineffectSyn <- rep(NA,len_lf_sem)
maineffectSem <- rep(NA,len_lf_sem)
interactionSynSem <- rep(NA,len_lf_sem)
  
for(i in 1:len_lf_sem){
  # Run model
    print(paste("iteration: ",i,sep=""))  
    lf<<- posterior_lf_sem[i]  
    # ---------------------------
    sims <- create_param_matrix(model_4cond, iterations=5000)
    results <- run(sims)
    condMeans <- compute_cond_means(results)
    meSyn<-mean(c(condMeans$Latency[3],condMeans$Latency[4]))-
      mean(c(condMeans$Latency[1],condMeans$Latency[2]))
    maineffectSyn[i]<-meSyn
    meSem<-mean(c(condMeans$Latency[2],condMeans$Latency[4]))-
      mean(c(condMeans$Latency[1],condMeans$Latency[3]))
    maineffectSem[i]<-meSem
    synsem<- (condMeans$Latency[4] - condMeans$Latency[3])-
      (condMeans$Latency[2] - condMeans$Latency[1])
    interactionSynSem[i]<-synsem
  }
postpreds_1cue <-data.frame(lfs=post_lfs,syntactic=(maineffectSyn*-1),semantic=(maineffectSem*-1), interaction=(interactionSynSem*-1))

save(postpreds_1cue,file="posterior_predictions_1cue.Rda")
load("posterior_predictions_1cue.Rda")

###############################################

##### Plot prior and posterior predictions together #####
prior_predictions <- simdat_1cue_long
prior_predictions$pred <- "prior predictions"
postpreds_1cue $pred <- "posterior predictions"

# prepare for plotting
postpreds_1cue_long <- tidyr::pivot_longer(postpreds_1cue , cols=c("syntactic","semantic","interaction"),
                                      names_to="effect", values_to="effsize")
postpreds_1cue_long$effsize <- as.numeric(postpreds_1cue_long$effsize)

preds <- rbind(postpreds_1cue_long, prior_predictions)
preds$effect <- factor(preds$effect, levels=c("syntactic", "semantic", "interaction"))


p.1cues <-ggplot(data=preds, x=effsize) +
  geom_density(aes(x=effsize,fill=pred), color="black", alpha=0.4) +
  geom_point(data=SVdat, aes(x=mean, y=-0.2), color="red", size=3)+
  geom_errorbar(data=SVdat, aes(y=-0.2, xmin=lower, xmax=upper), color="red", width=0.001, linewidth=1) +
  scale_fill_grey(start = 0, end = .9) +
  facet_wrap(~ effect)+
  theme_bw(base_size = 12)+
  geom_vline(xintercept=0, linetype="dashed")+
  xlab("amplitude difference (microV)")+
  ylab("density")+
  guides(fill = guide_legend(reverse=TRUE))+
  theme(legend.title=element_blank(), legend.position ="none")+
  scale_y_break(c(3, 4), scales=0.5)

ggsave("ERP_priorpred_vs_postpred_vs_data_1cue.png", width=7, height=4, dpi=600)



### all plots
plot_grid(print(p.3cues), NULL, print(p.2cues), NULL, print(p.1cues), nrow=5, label_x=-0.05, label_y = c(1,1,1.1,1,1.1),   
          rel_heights = c(1.1, 0.1, 1, 0.1, 1),
          labels=c("A. Model using three cues", "", "B. Model using two cues","",  "C. Model using one cue"))
ggsave("ERP_priorpred_vs_postpred_vs_data.png", width=16, height=8, dpi=600)

