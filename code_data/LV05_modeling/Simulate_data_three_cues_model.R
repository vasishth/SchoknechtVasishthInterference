# author: Himanshu Yadav
# date: 11.08.2024

# load packages
library(truncnorm)
library(ggplot2)
library(cowplot)
library(stringr)


################


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

# Generate prior predictions #####
# Generate 100,000 simulations from model:
nsims<-200000


lfs <- rtruncnorm(nsims,a=0,b=0.05,mean=0,sd=0.025)

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
simdat_3cues<-data.frame(lfs=lfs,
                         MESyn=maineffectSyn,MESem=maineffectSem, InteractionSynSem=interactionSynSem)

save(simdat_3cues,file="Simulated_data_three_cues_model.Rda")




##############################
# Load data
#############
SVdat <- data.frame(pred   = rep("data",3),
                    effect    = c("syntactic", "semantic", "interaction"), 
                    mean  = c(-0.14,-0.3,-0.03),
                    upper = c(0.05,-0.1,0.38),
                    lower = c(-0.33,-0.5,-0.33))
SVdat$effect <- factor(SVdat$effect, levels=c("syntactic", "semantic", "interaction"))
################

tol_scaler <- 9
delta <- (abs(SVdat[2,]$upper-SVdat[2,]$lower)/(4*tol_scaler))
df.lkl <- simdat_3cues
df.lkl$prior <- "normal(0.01,0.01)"
df.lkl$ans <- 0.2
df.lkl$iter <- 2000
df.lkl$lkl <- dnorm(df.lkl$MESem-(-SVdat[2,]$mean),0,delta)
df.lkl$prior.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0.01,sd=0.01)
df.lkl$proposal.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0,sd=0.025)
ML <- mean((df.lkl$lkl)*(df.lkl$prior.dens)/(df.lkl$proposal.dens))
df.lkl$ML <- ML

save(df.lkl,file="Marginal_likelihood_three_cues_model.Rda")

#########################

