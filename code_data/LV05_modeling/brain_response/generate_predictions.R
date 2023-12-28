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

#### Generate prior predictions #####

# Generate 20,000 simulations from model:
nsims<-20000

## Initial hyperparameters for latency factor
## Visualization of priors

#Truncated Normal
## the first two are not reasonable, they cause bimodality.
#latency_factor_prior <-data.frame(x_temp=rtruncnorm(1000000, 
                                                    a=0.003, b=0.04, mean=0.01, sd=0.005))

#latency_factor_prior <-data.frame(x_temp=rtruncnorm(1000000, 
                                                    a=0, mean=0.01, sd=0.005))

latency_factor_prior <-data.frame(x_temp=rtruncnorm(1000000, 
                                                    a=0.005, b=0.04, mean=0.0001, sd=0.0005))


hist(latency_factor_prior$x_temp)

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



## not reasonable, cause bimodality:
#lfs<-rtruncnorm(nsims, a=0.003, b=0.04, mean=0.01, sd=0.005)
#lfs<-rtruncnorm(nsims, a=0, mean=0.01, sd=0.005)

lfs<-rtruncnorm(nsims, a=0.005, b=0.04, mean=0.0001, sd=0.0005)

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

colnames(simdat_2cues_sameclause) <- c("lfs", "syntactic", "semantic", "interaction")

# plot effects
simdat_2cues_sameclause_long <- tidyr::pivot_longer(simdat_2cues_sameclause, cols=c("syntactic","semantic","interaction"),
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

ggsave("PriorPredicted.png", width=6, height=4, dpi=600)

#### Generate posterior predictions ####

# Load simulated data
load("simdat_2cues_sameclause.Rda")

# Run ABC with rejection sampling
# results from Schoknecht & Vasishth (N400 elicited by critical verb): 
# syntactic mean effect -0.2 muV, CrI [-0.4,0.0] --> Normal(-0.2,0.1) --> SE: 0.1
# semantic mean effect -0.3 muV, CrI [-0.6,-0.1] --> Normal(-0.3,0.15) --> SE should be 0.125 but set slightly wider at SE: 0.15
# interaction mean effect -0.1 muV, CrI [-0.4, 0.0] --> Normal(-0.1,0.1) --> SE: 0.1

# I'm changing the sign of the effect here (sem: -0.3 --> 0.3)
# to match the sign of the latency factor
# --> a positive effect represents are more negative N400 

synlower <- 0.2-(2*0.1) # mean effect - 2*SE
synupper <- 0.2+(2*0.1) # mean effect + 2*SE

semlower <- 0.3-(2*0.15) # mean effect - 2*SE
semupper <- 0.3+(2*0.15) # mean effect + 2*SE

synsemlower <- 0.1-(2*0.1) # mean effect - 2*SE
synsemupper <- 0.1+(2*0.1) # mean effect + 2*SE

# posterior latency factor (we base this only on the semantic interference result)
## Pia, this line does not work for me:
#SEM<-semlower<=simdat_2cues_sameclause$MESem & simdat_2cues_sameclause$MESem<=semupper
SEM<-semlower<=simdat_2cues_sameclause$semantic & simdat_2cues_sameclause$semantic<=semupper
table(SEM)
posterior_lf_sem <-simdat_2cues_sameclause[SEM,]$lfs

length(posterior_lf_sem)
quantile(posterior_lf_sem,probs=c(0.025,0.975))
mean(posterior_lf_sem)
hist(posterior_lf_sem, xlab="Latency factor")

post_lfs <- data.frame(lfs = posterior_lf_sem)

# plot
ggplot(post_lfs, aes(x=lfs)) +
  geom_density(aes(),fill="darkgrey")+
  xlim(c(0,0.2))+
  xlab("Posterior distribution of the latency factor")+
  theme_bw(base_size = 12)
ggsave("posterior_lf.png", width=6, height=3, dpi=600)

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

# data range:
synlower;synupper
semlower;semupper
synsemlower;synsemupper

## post pred:
hist(maineffectSyn)
hist(maineffectSem)
hist(interactionSynSem)

postpreds <-data.frame(lfs=lf,syntactic=maineffectSyn,semantic=maineffectSem, interaction=interactionSynSem)
postpreds_longer <- pivot_longer(postpreds, cols = c("syntactic", "semantic", "interaction"),
                                 names_to = "effect", values_to = "effsize")
postpreds_longer$effect <- factor(postpreds_longer$effect, levels = c("syntactic", "semantic", "interaction"))

#change sign to represent negative ERPs
postpreds_longer$effsize_neg <- -1 * postpreds_longer$effsize

# plot
ggplot(postpreds_longer, aes(x=effsize_neg)) +
  geom_density(aes(),fill="darkgrey")+
  ggh4x::facet_grid2(. ~ effect, scale="free_y", independent = "y")+
  #xlim(c(-0.2,2))+
  xlab("Predicted effect (ms)")+
  geom_vline(xintercept=0, linetype="dashed")+
  theme_bw(base_size = 12)
ggsave("posteriors.png", width=6, height=3, dpi=600)


##### Plot prior and posterior predictions together #####
prior_predictions <- simdat_2cues_sameclause_long
prior_predictions$pred <- "prior predictions"
postpreds_longer$pred <- "posterior predictions"
preds <- rbind(postpreds_longer, prior_predictions)

# add data from Schoknecht & Vasishth SPR experiment
dat <- data.frame(pred   = rep("data",3),
                  effect    = c("syntactic", "semantic", "interaction"), 
                  mean  = c(-0.2,-0.3,-0.1),
                  lower = c(0,-0.1,0),
                  upper = c(-0.4,-0.6,-0.4))

dat$effect <- factor(dat$effect, levels=c("syntactic", "semantic", "interaction"))

ggplot(data=preds, x=effsize_neg) +
  geom_point(data=dat, aes(x=mean, y=0), color="darkred", size=3)+
  geom_errorbar(data=dat, aes(y=0, xmin=lower, xmax=upper), color="darkred", width=0.001, size=1) +
  geom_density(aes(x=effsize_neg,fill=pred), color="black", alpha=0.3) +
  scale_fill_grey(start = 0, end = .9) +
  ggh4x::facet_grid2(. ~ effect, scales = "free_y", independent = "y")+
  theme_bw(base_size = 12)+
  #geom_abline(xintercept=0, linetype="dashed")+
  xlab("amplitude difference (microV)")+
  guides(fill = guide_legend(reverse=TRUE))+
  theme(legend.title=element_blank(), legend.position = c(0.83, 0.8))

ggsave("ERP_priorpred_vs_postpred_vs_data.png", width=7, height=3, dpi=600)

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

#### Generate prior predictions #####

# Generate 20,000 simulations from model:
nsims<-20000

## Initial hyperparameters for latency factor
## Visualization of priors

#Truncated Normal
latency_factor_prior <-data.frame(x_temp=rtruncnorm(1000000, 
                                                    a=0.005, b=0.04, mean=0.02, sd=0.005))

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

## not reasonable, cause bimodality:
#lfs<-rtruncnorm(nsims, a=0.003, b=0.04, mean=0.01, sd=0.005)
#lfs<-rtruncnorm(nsims, a=0, mean=0.01, sd=0.005)

lfs<-rtruncnorm(nsims, a=0.005, b=0.04, mean=0.02, sd=0.005)

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

colnames(simdat_2cues_sameclause) <- c("lfs", "syntactic", "semantic", "interaction")

# plot effects
simdat_2cues_sameclause_long <- tidyr::pivot_longer(simdat_2cues_sameclause, cols=c("syntactic","semantic","interaction"),
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

ggsave("PriorPredicted.png", width=6, height=4, dpi=600)

#### Generate posterior predictions ####

# Load simulated data
load("simdat_2cues_sameclause.Rda")

# Run ABC with rejection sampling
# results from Schoknecht & Vasishth (N400 elicited by critical verb): 
# syntactic mean effect -0.2 muV, CrI [-0.4,0.0] --> Normal(-0.2,0.1) --> SE: 0.1
# semantic mean effect -0.3 muV, CrI [-0.6,-0.1] --> Normal(-0.3,0.15) --> SE: 0.15
# interaction mean effect -0.1 muV, CrI [-0.4, 0.0] --> Normal(-0.1,0.1) --> SE: 0.1

# I'm changing the sign of the effect here (sem: -0.3 --> 0.3)
# to match the sign of the latency factor
# --> a positive effect represents are more negative N400 

synlower <- 0.2-(2*0.1) # mean effect - 2*SE
synupper <- 0.2+(2*0.1) # mean effect + 2*SE

semlower <- 0.3-(2*0.15) # mean effect - 2*SE
semupper <- 0.3+(2*0.15) # mean effect + 2*SE

synsemlower <- 0.1-(2*0.1) # mean effect - 2*SE
synsemupper <- 0.1+(2*0.1) # mean effect + 2*SE

# posterior latency factor (we base this only on the semantic interference result)
SEM<-semlower<=simdat_2cues_sameclause$MESem & simdat_2cues_sameclause$MESem<=semupper
table(SEM)
posterior_lf_sem <-simdat_2cues_sameclause[SEM,]$lfs

length(posterior_lf_sem)
quantile(posterior_lf_sem,probs=c(0.025,0.975))
mean(posterior_lf_sem)
hist(posterior_lf_sem, xlab="Latency factor")

post_lfs <- data.frame(lfs = posterior_lf_sem)

# plot
ggplot(post_lfs, aes(x=lfs)) +
  geom_density(aes(),fill="darkgrey")+
  xlim(c(0,0.2))+
  xlab("Posterior distribution of the latency factor")+
  theme_bw(base_size = 12)
ggsave("posterior_lf.png", width=6, height=3, dpi=600)

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

# data range:
synlower;synupper
semlower;semupper
synsemlower;synsemupper

## post pred:
hist(maineffectSyn)
hist(maineffectSem)
hist(interactionSynSem)

postpreds <-data.frame(lfs=lf,syntactic=maineffectSyn,semantic=maineffectSem, interaction=interactionSynSem)
postpreds_longer <- pivot_longer(postpreds, cols = c("syntactic", "semantic", "interaction"),
                                 names_to = "effect", values_to = "effsize")
postpreds_longer$effect <- factor(postpreds_longer$effect, levels = c("syntactic", "semantic", "interaction"))

#change sign to represent negative ERPs
postpreds_longer$effsize_neg <- -1 * postpreds_longer$effsize

# plot
ggplot(postpreds_longer, aes(x=effsize_neg)) +
  geom_density(aes(),fill="darkgrey")+
  ggh4x::facet_grid2(. ~ effect, scale="free_y", independent = "y")+
  xlim(c(-0.7,0.1))+
  xlab("Predicted effect (ms)")+
  geom_vline(xintercept=0, linetype="dashed")+
  theme_bw(base_size = 12)
ggsave("posteriors.png", width=6, height=3, dpi=600)


##### Plot prior and posterior predictions together #####
prior_predictions <- simdat_2cues_sameclause_long
prior_predictions$pred <- "prior predictions"
postpreds_longer$pred <- "posterior predictions"
preds <- rbind(postpreds_longer, prior_predictions)

# add data from Schoknecht & Vasishth ERP experiment
dat <- data.frame(pred   = rep("data",3),
                  effect    = c("syntactic", "semantic", "interaction"), 
                  mean  = c(-0.2,-0.3,-0.1),
                  lower = c(0,-0.1,0),
                  upper = c(-0.4,-0.6,-0.4))

dat$effect <- factor(dat$effect, levels=c("syntactic", "semantic", "interaction"))

ggplot(data=preds, x=effsize_neg) +
  geom_point(data=dat, aes(x=mean, y=0), color="darkred", size=3)+
  geom_errorbar(data=dat, aes(y=0, xmin=lower, xmax=upper), color="darkred", width=0.001, size=1) +
  geom_density(aes(x=effsize_neg,fill=pred), color="black", alpha=0.3) +
  scale_fill_grey(start = 0, end = .9) +
  ggh4x::facet_grid2(. ~ effect, scales = "free_y", independent = "y")+
  theme_bw(base_size = 12)+
  #geom_abline(xintercept=0, linetype="dashed")+
  xlab("amplitude difference (microV)")+
  guides(fill = guide_legend(reverse=TRUE))+
  theme(legend.title=element_blank(), legend.position = c(0.83, 0.8))

ggsave("ERP_priorpred_vs_postpred_vs_data.png", width=7.5, height=3, dpi=600)

