# load packages
library(truncnorm)
library(ggplot2)
library(cowplot)
library(stringr)
library(tidyverse)


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

#### Generate prior predictions #####

# Generate 100,000 simulations from model:
nsims<-100000

## Initial hyperparameters for latency factor
## Visualization of uniform prior
latency_factor_prior <-data.frame(x_temp=runif(1000000,min=0.005,max=0.04))
hist(latency_factor_prior$x_temp, freq=FALSE)

prior_lf <-ggplot(latency_factor_prior,aes(x=x_temp))+
  geom_histogram(aes(y=..density..),
                 position="identity",fill="gray", color="black",
                 binwidth=0.003)+
  theme_bw()+
  theme(strip.text.x = element_text(size = 16, colour = "black", angle = 0))+
  xlab("latency factor")+
  ggtitle("Prior on scaling factor")
prior_lf
ggsave("LF_Prior.png", width=5, height=3, dpi=600)

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
simdat_2cues_sameclause <-data.frame(lfs=lfs,
                    MESyn=maineffectSyn,MESem=maineffectSem, InteractionSynSem=interactionSynSem)
## save to avoid recomputing each time:
save(simdat_2cues_sameclause,file="simdat_2cues_sameclause.Rda")

plot(simdat_2cues_sameclause$lfs, simdat_2cues_sameclause$MESem)

load("simdat_2cues_sameclause.Rda")

# effect sizes
round(c(mean = mean(simdat_2cues_sameclause$MESyn), quantile(simdat_2cues_sameclause$MESyn, probs = c(.025, .975))),2)
round(c(mean = mean(simdat_2cues_sameclause$MESem), quantile(simdat_2cues_sameclause$MESem, probs = c(.025, .975))),2)
round(c(mean = mean(simdat_2cues_sameclause$InteractionSynSem), quantile(simdat_2cues_sameclause$InteractionSynSem, probs = c(.025, .975))),2)

simdat_2cues_sameclause2 <- simdat_2cues_sameclause %>% rename("syntactic" =MESyn, "semantic"=MESem, "interaction"=InteractionSynSem)

# plot effects
simdat_2cues_sameclause_long <- tidyr::pivot_longer(simdat_2cues_sameclause2, cols=c("syntactic","semantic","interaction"),
                                       names_to="effect", values_to="effsize")
simdat_2cues_sameclause_long$effsize <- as.numeric(simdat_2cues_sameclause_long$effsize)
simdat_2cues_sameclause_long$effect <- factor(simdat_2cues_sameclause_long$effect, levels=c("syntactic", "semantic", "interaction"))
simdat_2cues_sameclause_long$effsize_neg <- -1 * simdat_2cues_sameclause_long$effsize


ggplot(data=simdat_2cues_sameclause_long, x=effsize_neg) +
  geom_density(aes(x=effsize_neg), fill="lightgrey") +
  ggh4x::facet_grid2(. ~ effect, scale="free_y", independent = "y")+
  theme_bw(base_size = 10)+
  geom_vline(xintercept=0, linetype="dashed")+
  xlab("amplitude difference (microV)")

ggsave("PriorPredicted_2cues.png", width=6, height=4, dpi=600)



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

# Generate prior predictions #####
# Generate 100,000 simulations from model:
nsims<-100000

## uniform prior (effect between 0.5 and 4 muV):
lfs<-runif(nsims,min=0.005,max=0.04)

maineffectSyn<-rep(NA,nsims)
maineffectSem<-rep(NA,nsims)
interactionSynSem<-rep(NA,nsims)

# empty df for saving means
generated_means <- data.frame(matrix(ncol=9,nrow=nsims))
colnames(generated_means) <- c("sample.id","RT_LoSynLoSem","RT_LoSynHiSem","RT_HiSynLoSem","RT_HiSynHiSem", "Acc_LoSynLoSem","Acc_LoSynHiSem","Acc_HiSynLoSem","Acc_HiSynHiSem")


for(i in 1:nsims){
  # Run model
  print(paste("run: ",i,sep=""))  
  lf<<- lfs[i]  
  sims <- create_param_matrix(model_4cond, iterations=5000)
  results <- run(sims)
  condMeans <- compute_cond_means(results)
  # save means and acc
  generated_means[i,] <- c(i, round(condMeans$Latency), round(condMeans$Acc,2))
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
simdat_3cues<-data.frame(lfs=lfs,
                         MESyn=maineffectSyn,MESem=maineffectSem, InteractionSynSem=interactionSynSem)
simdat_3cues_complete <- cbind(simdat_3cues, generated_means)

## save to avoid recomputing each time:
save(simdat_3cues_complete,file="simdat_3cues.Rda")
load("simdat_3cues.Rda")
simdat_3cues <- simdat_3cues_complete[,1:4]

# effect sizes
round(c(mean = mean(simdat_3cues$MESyn), quantile(simdat_3cues$MESyn, probs = c(.025, .975))),2)
round(c(mean = mean(simdat_3cues$MESem), quantile(simdat_3cues$MESem, probs = c(.025, .975))),2)
round(c(mean = mean(simdat_3cues$InteractionSynSem), quantile(simdat_3cues$InteractionSynSem, probs = c(.025, .975))),2)

simdat_3cues2 <- simdat_3cues %>% rename("syntactic" =MESyn, "semantic"=MESem, "interaction"=InteractionSynSem)

# plot effects
simdat_3cues_long <- tidyr::pivot_longer(simdat_3cues2, cols=c("syntactic", "semantic","interaction"),
                                                    names_to="effect", values_to="effsize")
simdat_3cues_long$effsize <- as.numeric(simdat_3cues_long$effsize)
simdat_3cues_long$effect <- factor(simdat_3cues_long$effect, levels=c("syntactic", "semantic", "interaction"))
simdat_3cues_long$effsize_neg <- -1 * simdat_3cues_long$effsize

ggplot(data=simdat_3cues_long, x=effsize_neg) +
  geom_density(aes(x=effsize_neg), fill="lightgrey") +
  ggh4x::facet_grid2(. ~ effect, scale="free_y", independent = "y")+
  theme_bw(base_size = 10)+
  geom_vline(xintercept=0, linetype="dashed")+
  xlab("amplitude difference (microV)")

ggsave("PriorPredicted_3cues.png", width=6, height=4, dpi=600)


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

# Generate 100,000 simulations from model:
nsims<-100000

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
