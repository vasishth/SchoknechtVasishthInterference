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
simdat_2cues_sameclause <-data.frame(lfs=lfs,
                    MESyn=maineffectSyn,MESem=maineffectSem, InteractionSynSem=interactionSynSem)
simdat_2cues_sameclause_complete <- cbind(simdat_2cues_sameclause, generated_means)
## save to avoid recomputing each time:
save(simdat_2cues_sameclause_complete,file="simdat_2cues_sameclause.Rda")


#### COMPARE MODELS #####
load("simdat_3cues.Rda")
load("simdat_2cues_sameclause.Rda")


# effect sizes
# three cues model
round(c(mean = mean(simdat_3cues_complete$MESyn), quantile(simdat_3cues_complete$MESyn, probs = c(.025, .975))))
round(c(mean = mean(simdat_3cues_complete$MESem), quantile(simdat_3cues_complete$MESem, probs = c(.025, .975))))
round(c(mean = mean(simdat_3cues_complete$InteractionSynSem), quantile(simdat_3cues_complete$InteractionSynSem, probs = c(.025, .975))))

# two cues same clause model
round(c(mean = mean(simdat_2cues_sameclause_complete$MESyn), quantile(simdat_2cues_sameclause_complete$MESyn, probs = c(.025, .975))))
round(c(mean = mean(simdat_2cues_sameclause_complete$MESem), quantile(simdat_2cues_sameclause_complete$MESem, probs = c(.025, .975))))
round(c(mean = mean(simdat_2cues_sameclause_complete$InteractionSynSem), quantile(simdat_2cues_sameclause_complete$InteractionSynSem, probs = c(.025, .975))))

# combine sim data
simdat_3cues_complete$model.version <- "three cues \n ({±grammatical subject}, \n {±same.clause} \n and {±animate})"
simdat_2cues_sameclause_complete$model.version <- "two cues \n ({±subject within \n same clause} \n and {±animate})"

generated_effects <- rbind(simdat_3cues_complete, simdat_2cues_sameclause_complete)
colnames(generated_effects) <- c("lfs", "syntactic", "semantic", "interaction", "sample.id", "RT_LoSynLoSem", "RT_LoSynHiSem", "RT_HiSynLoSem", "RT_HiSynHiSem", "Acc_LoSynLoSem", "Acc_LoSynHiSem", "Acc_HiSynLoSem", "Acc_HiSynHiSem", "model.version")


# plot effects
generated_effects_long <- tidyr::pivot_longer(generated_effects, cols=c("syntactic","semantic","interaction"),
                                       names_to="int_effect", values_to="effsize")
generated_effects_long$effsize <- as.numeric(generated_effects_long$effsize)
generated_effects_long$int_effect <- factor(generated_effects_long$int_effect, levels=c("syntactic", "semantic", "interaction"))
generated_effects_long$model.version <- factor(generated_effects_long$model.version, 
                                               levels=c("three cues \n ({±grammatical subject}, \n {±same.clause} \n and {±animate})",
                                                        "two cues \n ({±subject within \n same clause} \n and {±animate})"))

ggplot(data=generated_effects_long, x=effsize) +
  geom_density(aes(x=effsize), fill="lightgrey") +
  #facet_grid(model.version ~ int_effect, scales="free")+
  ggh4x::facet_grid2(model.version ~ int_effect, scales = "free_y", independent = "y")+
  theme_bw(base_size = 9)+
  geom_abline(intercept=0, linetype="dashed")+
  xlab("retrieval time difference (ms)")

ggsave("PriorPredicted_interACTversions.png", width=6, height=3, dpi=600)

# plot means
generated_means_long <- tidyr::pivot_longer(generated_effects, cols=c("RT_LoSynLoSem", "RT_LoSynHiSem", "RT_HiSynLoSem", "RT_HiSynHiSem"),
                                              names_to="condition", values_to="mean_rt")
generated_means_long$mean_rt <- as.numeric(generated_means_long$mean_rt)
generated_means_long$condition <- factor(generated_means_long$condition, levels=c("RT_LoSynLoSem", "RT_LoSynHiSem", "RT_HiSynLoSem", "RT_HiSynHiSem"))
generated_means_long$model.version <- factor(generated_means_long$model.version, 
                                             levels=c("three cues \n ({±grammatical subject}, \n {±same.clause} \n and {±animate})",
                                                      "two cues \n ({±subject within \n same clause} \n and {±animate})"))


ggplot(generated_means_long, aes(x=condition, y=mean_rt))+
  geom_jitter(color="lightblue", alpha=0.2)+
  geom_boxplot(fill="grey", alpha=0.7)+
  stat_summary(fun=mean, geom="point", shape=18, size=4) + #adding the mean
  stat_summary(aes(label=round(..y..)), fun=mean, geom="text", size=2,
               vjust = -0.25, hjust= -1)+
  facet_grid(model.version ~ ., scales="free")+
  theme_bw(base_size = 9) +
  scale_y_continuous(name="retrieval time (ms)")+
  scale_x_discrete(name="condition", labels=c("RT_LoSynLoSem" = "low syntactic interference \nlow semantic interference", "RT_LoSynHiSem" = "low syntactic interference \nhigh semantic interference",
                                     "RT_HiSynLoSem" = "high syntactic interference \nlow semantic interference", "RT_HiSynHiSem" = "high syntactic interference \nhigh semantic interference"))

ggsave("PriorPredictedMeans_interACTversions.png", width=7, height=3, dpi=600)

# Accuracy 
generated_acc_long <- tidyr::pivot_longer(generated_effects, cols=c("Acc_LoSynLoSem", "Acc_LoSynHiSem", "Acc_HiSynLoSem", "Acc_HiSynHiSem"),
                                            names_to="condition", values_to="mean_acc")
aggregate(mean_acc ~ condition + model.version, data=generated_acc_long, FUN=mean)
