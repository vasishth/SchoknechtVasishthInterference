# load packages
library(truncnorm)
library(ggplot2)
library(cowplot)
library(stringr)
library(tidyverse)

#### TWO CUE VERSION OF THE ORIGINAL INTERACT MODEL WITH COMPLEX SYNTACTIC CUE {+subject within same clause} #####

# Load model 
source("Models/interACT.R")

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
ans <<- 0.2
VERBOSE <<- FALSE


#### Generate prior predictions #####

nsims<-2000


lfs <- rtruncnorm(nsims,a=0,b=0.05,mean=0.01,sd=0.01)

maineffectSyn<-rep(NA,nsims)
maineffectSem<-rep(NA,nsims)
interactionSynSem<-rep(NA,nsims)

for(i in 1:nsims){
  # Run model
  print(paste("run: ",i,sep=""))  
  lf<<- lfs[i]  
  sims <- create_param_matrix(model_4cond, iterations=2000)
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
df.predictions <-data.frame(lfs=lfs,
                                     MESyn=maineffectSyn,MESem=maineffectSem, InteractionSynSem=interactionSynSem)
df.predictions$prior = "Normal(0.01.0.01)"
save(df.predictions,file="Prior-predictions-two-cues-model-prior-normal(0.01,0.01).Rda")


######################################


lfs <- rtruncnorm(nsims,a=0,b=0.05,mean=0.01,sd=0.02)

maineffectSyn<-rep(NA,nsims)
maineffectSem<-rep(NA,nsims)
interactionSynSem<-rep(NA,nsims)

for(i in 1:nsims){
  # Run model
  print(paste("run: ",i,sep=""))  
  lf<<- lfs[i]  
  sims <- create_param_matrix(model_4cond, iterations=2000)
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
df.predictions <-data.frame(lfs=lfs,
                            MESyn=maineffectSyn,MESem=maineffectSem, InteractionSynSem=interactionSynSem)
df.predictions$prior = "Normal(0.01.0.02)"
save(df.predictions,file="Prior-predictions-two-cues-model-prior-normal(0.01,0.02).Rda")

#### THREE CUE VERSION OF THE ORIGINAL INTERACT MODEL #####

# Load model 
source("Models/interACT-threecues.R")
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


#### Generate prior predictions #####

nsims<-2000


lfs <- rtruncnorm(nsims,a=0,b=0.05,mean=0.01,sd=0.01)

maineffectSyn<-rep(NA,nsims)
maineffectSem<-rep(NA,nsims)
interactionSynSem<-rep(NA,nsims)

for(i in 1:nsims){
  # Run model
  print(paste("run: ",i,sep=""))  
  lf<<- lfs[i]  
  sims <- create_param_matrix(model_4cond, iterations=2000)
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
df.predictions <-data.frame(lfs=lfs,
                            MESyn=maineffectSyn,MESem=maineffectSem, InteractionSynSem=interactionSynSem)
df.predictions$prior = "Normal(0.01.0.01)"
save(df.predictions,file="Prior-predictions-three-cues-model-prior-normal(0.01,0.01).Rda")


######################################


lfs <- rtruncnorm(nsims,a=0,b=0.05,mean=0.01,sd=0.02)

maineffectSyn<-rep(NA,nsims)
maineffectSem<-rep(NA,nsims)
interactionSynSem<-rep(NA,nsims)

for(i in 1:nsims){
  # Run model
  print(paste("run: ",i,sep=""))  
  lf<<- lfs[i]  
  sims <- create_param_matrix(model_4cond, iterations=2000)
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
df.predictions <-data.frame(lfs=lfs,
                            MESyn=maineffectSyn,MESem=maineffectSem, InteractionSynSem=interactionSynSem)
df.predictions$prior = "Normal(0.01.0.02)"
save(df.predictions,file="Prior-predictions-three-cues-model-prior-normal(0.01,0.02).Rda")


#### One CUE VERSION OF THE ORIGINAL INTERACT MODEL WITH COMPLEX SYNTACTIC CUE {+subject within same clause} #####


# Load model 
source("Models/interACT-onecue.R")

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

#### Generate prior predictions #####

nsims<-2000


lfs <- rtruncnorm(nsims,a=0,b=0.05,mean=0.01,sd=0.01)

maineffectSyn<-rep(NA,nsims)
maineffectSem<-rep(NA,nsims)
interactionSynSem<-rep(NA,nsims)

for(i in 1:nsims){
  # Run model
  print(paste("run: ",i,sep=""))  
  lf<<- lfs[i]  
  sims <- create_param_matrix(model_4cond, iterations=2000)
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
df.predictions <-data.frame(lfs=lfs,
                            MESyn=maineffectSyn,MESem=maineffectSem, InteractionSynSem=interactionSynSem)
df.predictions$prior = "Normal(0.01.0.01)"
save(df.predictions,file="Prior-predictions-one-cue-model-prior-normal(0.01,0.01).Rda")


######################################


lfs <- rtruncnorm(nsims,a=0,b=0.05,mean=0.01,sd=0.02)

maineffectSyn<-rep(NA,nsims)
maineffectSem<-rep(NA,nsims)
interactionSynSem<-rep(NA,nsims)

for(i in 1:nsims){
  # Run model
  print(paste("run: ",i,sep=""))  
  lf<<- lfs[i]  
  sims <- create_param_matrix(model_4cond, iterations=2000)
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
df.predictions <-data.frame(lfs=lfs,
                            MESyn=maineffectSyn,MESem=maineffectSem, InteractionSynSem=interactionSynSem)
df.predictions$prior = "Normal(0.01.0.02)"
save(df.predictions,file="Prior-predictions-one-cue-model-prior-normal(0.01,0.02).Rda")

####

# Plot all predictions
load("Prior-predictions-one-cue-model-prior-normal(0.01,0.01).Rda")
df.prediction1 <- df.predictions
df.prediction1$model <- "one-cue model"
load("Prior-predictions-two-cues-model-prior-normal(0.01,0.01).Rda")
df.prediction2 <- df.predictions
df.prediction2$model <- "two-cues model"
load("Prior-predictions-three-cues-model-prior-normal(0.01,0.01).Rda")
df.prediction3 <- df.predictions
df.prediction3$model <- "three-cues model"


df.predictions <- rbind(df.prediction1, df.prediction2, df.prediction3)
df.predictions <- df.predictions %>% rename("syntactic" =MESyn, "semantic"=MESem, "interaction"=InteractionSynSem)
df.predictions <- pivot_longer(df.predictions, cols = c("syntactic", "semantic", "interaction"),
                            names_to = "effect", values_to = "effsize")
df.predictions$effect <- factor(df.predictions$effect, levels = c("syntactic", "semantic", "interaction"))
df.predictions$model <- factor(df.predictions$model, levels = c("one-cue model", "two-cues model", "three-cues model"))

#change sign to represent negative ERPs
df.predictions$effsize_neg <- -1 * df.predictions$effsize

# observed data
#############
SVdat <- data.frame(pred   = rep("data",3),
                    effect    = c("syntactic", "semantic", "interaction"), 
                    mean  = c(-0.14,-0.3,-0.03),
                    upper = c(0.05,-0.1,0.38),
                    lower = c(-0.33,-0.5,-0.33))
SVdat$effect <- factor(SVdat$effect, levels=c("syntactic", "semantic", "interaction"))

# plot
ggplot(data=df.predictions, x=effsize_neg) +
  geom_density(aes(x=effsize_neg),fill="lightgrey", color="black", alpha=0.5, bw = 0.25) +
  geom_point(data=SVdat, aes(x=mean, y=0), color="darkred", size=2)+
  geom_errorbar(data=SVdat, aes(y=0, xmin=lower, xmax=upper), color="darkred", width=0.001, linewidth=1) +
  ggh4x::facet_grid2(model ~ effect, scales = "free_y", independent = "y")+
  theme_bw(base_size = 12)+
  geom_vline(xintercept=0, linetype="dashed")+
  xlab("amplitude difference (microV)")+
  ylab("density")+
  guides(fill = guide_legend(reverse=TRUE))+
  theme(legend.title=element_blank(), legend.position ="top")

ggsave("compmod_predictions.png",height=4.5,width=8)
