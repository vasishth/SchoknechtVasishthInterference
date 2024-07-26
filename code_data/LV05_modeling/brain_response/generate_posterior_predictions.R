# author: Pia Schoknecht
# date: 23.07.2024

# load packages
library(truncnorm)
library(ggplot2)
library(ggbreak)
library(cowplot)
library(stringr)
library(tidyverse)

# load k-fold model fits
kfold.estimates <- read.csv("kfold_N400_estimates.csv")

# We add the estimates of the full data set to the kfold estimates to loop through the estimates later
# results from Schoknecht & Vasishth (N400 elicited by critical verb): 
# syntactic mean effect -0.14 muV, CrI [-0.33,0.05] --> SE: 0.1
# semantic mean effect -0.3 muV, CrI [-0.5,-0.1] --> SE: 0.1
# interaction mean effect 0.03 muV, CrI [-0.33, 0.38] --> SE: 0.18

# I'm changing the sign of the effect here (sem: -0.3 --> 0.3)
# to match the sign of the latency factor
# --> a positive effect represents are more negative N400 

synlower <- 0.14-(2*0.1) # mean effect - 2*SE
synupper <- 0.14+(2*0.1) # mean effect + 2*SE

semlower <- 0.3-(2*0.1) # mean effect - 2*SE
semupper <- 0.3+(2*0.1) # mean effect + 2*SE

synsemlower <- -0.03-(2*0.18) # mean effect - 2*SE
synsemupper <- -0.03+(2*0.18) # mean effect + 2*SE

all <- c(11, 0, synlower, synupper, semlower, semupper, synsemlower, synsemupper)

estimates <- rbind(kfold.estimates, all)

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
simdat_2cues_sameclause_long$effsize_neg <- -1 * simdat_2cues_sameclause_long$effsize


#### Generate posterior predictions ####

# Run ABC with rejection sampling

cues2_postpreds_wo <- list()

for (k in 1:11){
# posterior latency factor (we base this only on the semantic interference result)
SEM<- estimates$semlower[k]<=simdat_2cues_sameclause$MESem & simdat_2cues_sameclause$MESem<=estimates$semupper[k]
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
cues2_postpreds_wo[[k]] <-data.frame(lfs=post_lfs,syntactic=maineffectSyn,semantic=maineffectSem, interaction=interactionSynSem)
}
save(cues2_postpreds_wo,file="posterior_predictions_2cues_kfold.Rda")
load("posterior_predictions_2cues_kfold.Rda")

###############################################

##### Plot prior and posterior predictions together #####
prior_predictions <- simdat_2cues_sameclause_long
prior_predictions$pred <- "prior predictions"
prior_predictions$heldout <- 0

cues2_postpreds_wo[[1]]$heldout <- 1
cues2_postpreds_wo[[2]]$heldout <- 2
cues2_postpreds_wo[[3]]$heldout <- 3
cues2_postpreds_wo[[4]]$heldout <- 4
cues2_postpreds_wo[[5]]$heldout <- 5
cues2_postpreds_wo[[6]]$heldout <- 6
cues2_postpreds_wo[[7]]$heldout <- 7
cues2_postpreds_wo[[8]]$heldout <- 8
cues2_postpreds_wo[[9]]$heldout <- 9
cues2_postpreds_wo[[10]]$heldout <- 10
cues2_postpreds_wo[[11]]$heldout <- 0

postpreds <- rbind(cues2_postpreds_wo[[1]],cues2_postpreds_wo[[2]], cues2_postpreds_wo[[3]], cues2_postpreds_wo[[4]], cues2_postpreds_wo[[5]],
                   cues2_postpreds_wo[[6]], cues2_postpreds_wo[[7]], cues2_postpreds_wo[[8]], cues2_postpreds_wo[[9]], cues2_postpreds_wo[[10]],
                   cues2_postpreds_wo[[11]])
postpreds$pred <- "posterior predictions"

# prepare for plotting
postpreds_long <- tidyr::pivot_longer(postpreds, cols=c("syntactic","semantic","interaction"),
                                                    names_to="effect", values_to="effsize")
postpreds_long$effsize <- as.numeric(postpreds_long$effsize)
postpreds_long$effect <- factor(postpreds_long$effect, levels=c("syntactic", "semantic", "interaction"))
postpreds_long$effsize_neg <- -1 * postpreds_long$effsize

preds <- rbind(postpreds_long, prior_predictions)

ggplot(data=filter(preds, pred=="posterior predictions"), x=effsize_neg) +
  geom_density(aes(x=lfs), color="black", alpha=0.4)+
  facet_wrap(~heldout)

library(ggbreak)
p.2cues <- ggplot(data=filter(preds, heldout==0), x=effsize_neg) +
  geom_density(aes(x=effsize_neg,fill=pred), color="black", alpha=0.4) +
  geom_point(data=SVdat, aes(x=mean, y=-0.1), color="red", size=3)+
  geom_errorbar(data=SVdat, aes(y=-0.1, xmin=lower, xmax=upper), color="red", width=0.001, linewidth=1) +
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
simdat_3cues_long$effsize_neg <- -1 * simdat_3cues_long$effsize

#### Generate posterior predictions ####

# Run ABC with rejection sampling

cues3_postpreds_wo <- list()

for (k in 1:11){
  # posterior latency factor (we base this only on the semantic interference result)
  SEM<- estimates$semlower[k]<=simdat_3cues$MESem & simdat_3cues$MESem<=estimates$semupper[k]
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
  cues3_postpreds_wo[[k]] <-data.frame(lfs=post_lfs,syntactic=maineffectSyn,semantic=maineffectSem, interaction=interactionSynSem)
}
save(cues3_postpreds_wo,file="posterior_predictions_3cues_kfold.Rda")
load("posterior_predictions_3cues_kfold.Rda")

###############################################

##### Plot prior and posterior predictions together #####
prior_predictions <- simdat_3cues_long
prior_predictions$pred <- "prior predictions"
prior_predictions$heldout <- 0

cues3_postpreds_wo[[1]]$heldout <- 1
cues3_postpreds_wo[[2]]$heldout <- 2
cues3_postpreds_wo[[3]]$heldout <- 3
cues3_postpreds_wo[[4]]$heldout <- 4
cues3_postpreds_wo[[5]]$heldout <- 5
cues3_postpreds_wo[[6]]$heldout <- 6
cues3_postpreds_wo[[7]]$heldout <- 7
cues3_postpreds_wo[[8]]$heldout <- 8
cues3_postpreds_wo[[9]]$heldout <- 9
cues3_postpreds_wo[[10]]$heldout <- 10
cues3_postpreds_wo[[11]]$heldout <- 0

postpreds <- rbind(cues3_postpreds_wo[[1]],cues3_postpreds_wo[[2]], cues3_postpreds_wo[[3]], cues3_postpreds_wo[[4]], cues3_postpreds_wo[[5]],
                   cues3_postpreds_wo[[6]], cues3_postpreds_wo[[7]], cues3_postpreds_wo[[8]], cues3_postpreds_wo[[9]], cues3_postpreds_wo[[10]],
                   cues3_postpreds_wo[[11]])
postpreds$pred <- "posterior predictions"

# prepare for plotting
postpreds_long <- tidyr::pivot_longer(postpreds, cols=c("syntactic","semantic","interaction"),
                                      names_to="effect", values_to="effsize")
postpreds_long$effsize <- as.numeric(postpreds_long$effsize)
postpreds_long$effect <- factor(postpreds_long$effect, levels=c("syntactic", "semantic", "interaction"))
postpreds_long$effsize_neg <- -1 * postpreds_long$effsize

preds <- rbind(postpreds_long, prior_predictions)


p.3cues <- ggplot(data=filter(preds, heldout==0), x=effsize_neg) +
  geom_density(aes(x=effsize_neg,fill=pred), color="black", alpha=0.4) +
  geom_point(data=SVdat, aes(x=mean, y=-0.1), color="red", size=3)+
  geom_errorbar(data=SVdat, aes(y=-0.1, xmin=lower, xmax=upper), color="red", width=0.001, linewidth=1) +
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

# Generate prior predictions #####

# Generate 20,000 simulations from model:
nsims<-20000

## uniform prior (effect between 0.5 and 4 muV):
lfs<-runif(nsims,min=0.005,max=0.04)

## check:
hist(lfs,freq=FALSE)


maineffectSyn<-rep(NA,nsims)
maineffectSem<-rep(NA,nsims)
interactionSynSem<-rep(NA,nsims)


for(i in 1:nsims){
  # Run model
  print(paste("run: ",i,sep=""))  
  lf<<- lfs[i]  
  sims <- create_param_matrix(model_4cond, iterations=5000)
  results <- run(sims)
  condMeans <- compute_cond_means(results)
  ## compute main effect of Syntactic manipulation
  ## HiSyn-LoSyn:
  meSyn<-mean(c(condMeans$Latency[3],condMeans$Latency[4]))-
    mean(c(condMeans$Latency[1],condMeans$Latency[2]))
  ## save the estimate:
  maineffectSyn[i]<-meSyn
  ## compute main effect of Semantic manipulation
  ## HiSem-LoSem:
  meSem<-mean(c(condMeans$Latency[2],condMeans$Latency[4]))-
    mean(c(condMeans$Latency[1],condMeans$Latency[3]))
  ## save estimate:
  maineffectSem[i]<-meSem
  ## Compute Interaction
  ## HiSynHiSem-HiSynLoSem - LoSynHiSem-LoSynLoSem:
  intSynSem<-(condMeans$Latency[4] - condMeans$Latency[3])-
    (condMeans$Latency[2] - condMeans$Latency[1])
  ## save estimate:
  interactionSynSem[i]<-intSynSem
}

## Create data frame with lf's, main effects and interaction simulated from model:
simdat_1cue <-data.frame(lfs=lfs, MESyn=maineffectSyn,MESem=maineffectSem, InteractionSynSem=interactionSynSem)

## save to avoid recomputing each time:
save(simdat_1cue,file="simdat_1cue.Rda")

plot(simdat_1cue$lfs, simdat_1cue$MESem)

load("simdat_1cue.Rda")

# effect sizes
round(c(mean = mean(simdat_1cue$MESyn), quantile(simdat_1cue$MESyn, probs = c(.025, .975))),2)
round(c(mean = mean(simdat_1cue$MESem), quantile(simdat_1cue$MESem, probs = c(.025, .975))),2)
round(c(mean = mean(simdat_1cue$InteractionSynSem), quantile(simdat_1cue$InteractionSynSem, probs = c(.025, .975))),2)

simdat_1cue2 <- simdat_1cue %>% rename("syntactic" =MESyn, "semantic"=MESem, "interaction"=InteractionSynSem)

# plot effects
simdat_1cue_long <- tidyr::pivot_longer(simdat_1cue2, cols=c("syntactic","semantic","interaction"),
                                                    names_to="effect", values_to="effsize")
simdat_1cue_long$effsize <- as.numeric(simdat_1cue_long$effsize)
simdat_1cue_long$effect <- factor(simdat_1cue_long$effect, levels=c("syntactic", "semantic", "interaction"))
simdat_1cue_long$effsize_neg <- -1 * simdat_1cue_long$effsize


ggplot(data=simdat_1cue_long, x=effsize_neg) +
  geom_density(aes(x=effsize_neg), fill="lightgrey") +
  ggh4x::facet_grid2(. ~ effect, scale="free_y", independent = "y")+
  theme_bw(base_size = 10)+
  geom_vline(xintercept=0, linetype="dashed")+
  xlab("amplitude difference (microV)")

ggsave("PriorPredicted_1cue.png", width=6, height=4, dpi=600)

#### Generate posterior predictions ####

# Run ABC with rejection sampling

cue1_postpreds_wo <- list()

for (k in 1:11){
  # posterior latency factor (we base this only on the semantic interference result)
  SEM<- estimates$semlower[k]<=simdat_1cue$MESem & simdat_1cue$MESem<=estimates$semupper[k]
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
  cue1_postpreds_wo[[k]] <-data.frame(lfs=post_lfs,syntactic=maineffectSyn,semantic=maineffectSem, interaction=interactionSynSem)
}
save(cue1_postpreds_wo,file="posterior_predictions_1cue_kfold.Rda")
load("posterior_predictions_1cue_kfold.Rda")

###############################################

##### Plot prior and posterior predictions together #####
prior_predictions <- simdat_1cue_long
prior_predictions$pred <- "prior predictions"
prior_predictions$heldout <- 0

cue1_postpreds_wo[[1]]$heldout <- 1
cue1_postpreds_wo[[2]]$heldout <- 2
cue1_postpreds_wo[[3]]$heldout <- 3
cue1_postpreds_wo[[4]]$heldout <- 4
cue1_postpreds_wo[[5]]$heldout <- 5
cue1_postpreds_wo[[6]]$heldout <- 6
cue1_postpreds_wo[[7]]$heldout <- 7
cue1_postpreds_wo[[8]]$heldout <- 8
cue1_postpreds_wo[[9]]$heldout <- 9
cue1_postpreds_wo[[10]]$heldout <- 10
cue1_postpreds_wo[[11]]$heldout <- 0

postpreds <- rbind(cue1_postpreds_wo[[1]],cue1_postpreds_wo[[2]], cue1_postpreds_wo[[3]], cue1_postpreds_wo[[4]], cue1_postpreds_wo[[5]],
                   cue1_postpreds_wo[[6]], cue1_postpreds_wo[[7]], cue1_postpreds_wo[[8]], cue1_postpreds_wo[[9]], cue1_postpreds_wo[[10]],
                   cue1_postpreds_wo[[11]])
postpreds$pred <- "posterior predictions"

# prepare for plotting
postpreds_long <- tidyr::pivot_longer(postpreds, cols=c("syntactic","semantic","interaction"),
                                      names_to="effect", values_to="effsize")
postpreds_long$effsize <- as.numeric(postpreds_long$effsize)
postpreds_long$effect <- factor(postpreds_long$effect, levels=c("syntactic", "semantic", "interaction"))
postpreds_long$effsize_neg <- -1 * postpreds_long$effsize

preds <- rbind(postpreds_long, prior_predictions)


p.1cues <-ggplot(data=filter(preds, heldout==0), x=effsize_neg) +
  geom_density(aes(x=effsize_neg,fill=pred), color="black", alpha=0.4) +
  geom_point(data=SVdat, aes(x=mean, y=-0.1), color="red", size=3)+
  geom_errorbar(data=SVdat, aes(y=-0.1, xmin=lower, xmax=upper), color="red", width=0.001, linewidth=1) +
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
          rel_heights = c(1, 0.1, 1, 0.1, 1),
          labels=c("A. Model using three cues", "", "B. Model using two cues","",  "C. Model using one cue"))
ggsave("ERP_priorpred_vs_postpred_vs_data.png", width=10, height=13, dpi=600)

