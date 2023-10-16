# load packages
library(truncnorm)
library(ggplot2)
library(cowplot)
library(stringr)

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

# Generate 20,000 simulations from model:
nsims<-20000

## Initial hyperparameters for latency factor
## from Methodsx paper:
a<-4
b<-6
lfs<-rbeta(nsims,a,b)
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
simdat_3cues<-data.frame(lfs=lfs,
                    MESyn=maineffectSyn,MESem=maineffectSem, InteractionSynSem=interactionSynSem)
## save to avoid recomputing each time:
save(simdat_3cues,file="simdat_3cues.Rda")


#### TWO CUE VERSION OF THE ORIGINAL INTERACT MODEL WITH SIMPLE SYNTACTIC CUE {+subject} #####

# Load model 
source("interACT.R")

reset_params()

# Parameters to be passed to run() function for the simulations, including cue weights
model_4cond <- list(
  target_match = list(c(1,1), c(1,1), c(1,1), c(1,1)),
  distractor_match = list(c(0,0), c(0,1), c(1,0), c(1,1)),
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

# Generate 20,000 simulations from model:
nsims<-20000

## Initial hyperparameters for latency factor
## from Methodsx paper:
a<-4
b<-6
lfs<-rbeta(nsims,a,b)
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
simdat_2cues_subj <-data.frame(lfs=lfs,
                          MESyn=maineffectSyn,MESem=maineffectSem, InteractionSynSem=interactionSynSem)
## save to avoid recomputing each time:
save(simdat_2cues_subj,file="simdat_2cues_subject.Rda")


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

# Generate 20,000 simulations from model:
nsims<-20000

## Initial hyperparameters for latency factor
## from Methodsx paper:
a<-4
b<-6
lfs<-rbeta(nsims,a,b)
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


#### COMPARE MODELS #####
load("simdat_3cues.Rda")
load("simdat_2cues_subject.Rda")
load("simdat_2cues_sameclause.Rda")


# effect sizes
# three cues model
round(c(mean = mean(simdat_3cues$MESyn), quantile(simdat_3cues$MESyn, probs = c(.025, .975))))
round(c(mean = mean(simdat_3cues$MESem), quantile(simdat_3cues$MESem, probs = c(.025, .975))))
round(c(mean = mean(simdat_3cues$InteractionSynSem), quantile(simdat_3cues$InteractionSynSem, probs = c(.025, .975))))

# two cues subject model
round(c(mean = mean(simdat_2cues_subj$MESyn), quantile(simdat_2cues_subj$MESyn, probs = c(.025, .975))))
round(c(mean = mean(simdat_2cues_subj$MESem), quantile(simdat_2cues_subj$MESem, probs = c(.025, .975))))
round(c(mean = mean(simdat_2cues_subj$InteractionSynSem), quantile(simdat_2cues_subj$InteractionSynSem, probs = c(.025, .975))))

# two cues same clause model
round(c(mean = mean(simdat_2cues_sameclause$MESyn), quantile(simdat_2cues_sameclause$MESyn, probs = c(.025, .975))))
round(c(mean = mean(simdat_2cues_sameclause$MESem), quantile(simdat_2cues_sameclause$MESem, probs = c(.025, .975))))
round(c(mean = mean(simdat_2cues_sameclause$InteractionSynSem), quantile(simdat_2cues_sameclause$InteractionSynSem, probs = c(.025, .975))))

# combine sim data
simdat_3cues$model.version <- "three cues \n ({±grammatical subject}, \n {±same.clause} and {±animate})"
simdat_2cues_sameclause$model.version <- "two cues \n ({±subject within same clause} \n and {±animate})"
simdat_2cues_subj$model.version <- "two cues \n ({±subject} and {±animate})"

generated_effects <- rbind(simdat_3cues, simdat_2cues_sameclause, simdat_2cues_subj)
colnames(generated_effects) <- c("lfs", "syntactic", "semantic", "interaction", "model.version")


# plot effects
generated_effects_long <- pivot_longer(generated_effects, cols=c("syntactic","semantic","interaction"),
                                       names_to="int_type", values_to="effsize")
generated_effects_long$effsize <- as.numeric(generated_effects_long$effsize)
generated_effects_long$int_type <- factor(generated_effects_long$int_type, levels=c("syntactic", "semantic", "interaction"))
generated_effects_long$model.version <- factor(generated_effects_long$model.version, levels=c("two cues \n ({±subject} and {±animate})", "three cues \n ({±grammatical subject}, \n {±same.clause} and {±animate})", "two cues \n ({±subject within same clause} \n and {±animate})"))


ggplot(data=generated_effects_long, x=effsize) +
  geom_density(aes(x=effsize), fill="lightgrey") +
  facet_grid(model.version ~ int_type, scale="free_y")+
  theme_bw(base_size = 10)+
  geom_abline(intercept=0, linetype="dashed")+
  xlab("retrieval time difference (ms)")

ggsave("PriorPredicted_interACTversions.png", width=6, height=6, dpi=600)
