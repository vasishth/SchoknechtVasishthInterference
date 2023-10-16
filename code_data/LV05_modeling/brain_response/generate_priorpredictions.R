# load packages
library(truncnorm)
library(ggplot2)
library(cowplot)
library(stringr)


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
## Visualization of priors

#Truncated Normal
latency_factor_prior <-data.frame(x_temp=rtruncnorm(1000000, 
                                                    a=0.003, b=0.04, mean=0.01, sd=0.005))

prior_lf <-ggplot(latency_factor_prior,aes(x=x_temp))+
  geom_histogram(aes(y=..density..),
                 position="identity",fill="gray",
                 binwidth=0.001, center = 0.0005)+
  theme_bw()+
  theme(strip.text.x = element_text(size = 16, colour = "black", angle = 0))+
  xlab("latency factor")+
  ggtitle("Prior on scaling factor")
prior_lf
ggsave("LF_Prior.png", width=5, height=3, dpi=600)

lfs<-rtruncnorm(nsims, a=0.003, b=0.04, mean=0.01, sd=0.005)
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
# two cues same clause model
round(c(mean = mean(simdat_2cues_sameclause$MESyn), quantile(simdat_2cues_sameclause$MESyn, probs = c(.025, .975))),2)
round(c(mean = mean(simdat_2cues_sameclause$MESem), quantile(simdat_2cues_sameclause$MESem, probs = c(.025, .975))),2)
round(c(mean = mean(simdat_2cues_sameclause$InteractionSynSem), quantile(simdat_2cues_sameclause$InteractionSynSem, probs = c(.025, .975))),2)

simdat_2cues_sameclause$model.version <- "two cues \n ({±subject within same clause} \n and {±animate})"
colnames(simdat_2cues_sameclause) <- c("lfs", "syntactic", "semantic", "interaction", "model.version")


# plot effects
simdat_2cues_sameclause_long <- pivot_longer(simdat_2cues_sameclause, cols=c("syntactic","semantic","interaction"),
                                       names_to="int_type", values_to="effsize")
simdat_2cues_sameclause_long$effsize <- as.numeric(simdat_2cues_sameclause_long$effsize)
simdat_2cues_sameclause_long$int_type <- factor(simdat_2cues_sameclause_long$int_type, levels=c("syntactic", "semantic", "interaction"))
simdat_2cues_sameclause_long$model.version <- factor(simdat_2cues_sameclause_long$model.version, levels=c("two cues \n ({±subject} and {±animate})", "three cues \n ({±grammatical subject}, \n {±same.clause} and {±animate})", "two cues \n ({±subject within same clause} \n and {±animate})"))


ggplot(data=simdat_2cues_sameclause_long, x=effsize) +
  geom_density(aes(x=effsize), fill="lightgrey") +
  ggh4x::facet_grid2(model.version ~ int_type, scale="free_y", independent = "y")+
  theme_bw(base_size = 10)+
  geom_vline(xintercept=0, linetype="dashed")+
  xlab("N400 amplitude difference (microV)")

ggsave("PriorPredicted.png", width=6, height=4, dpi=600)
