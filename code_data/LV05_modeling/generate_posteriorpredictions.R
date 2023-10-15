library(truncnorm)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(stringr)

# Load model
source("interACT.R")
reset_params()

# TWO CUE VERSION OF THE ORIGINAL INTERACT MODEL WITH COMPLEX SYNTACTIC CUE {+subject within same clause} #
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

# Load simulated data
load("simdat_2cues_sameclause.Rda")
# values used to generate prior predictions: 
# nsims<-5000
# lfs<-rbeta(nsims,a,b)

# Run ABC with rejection sampling
# results from Schoknecht & Vasishth (critical region): 
# syntactic mean effect 2ms CrI [0,4] --> Normal(2,1) --> SE: 1
# semantic mean effect 8ms CrI [4,12] --> Normal(8,2) --> SE: 2
# interaction mean effect 14ms, CrI [7, 22] --> Normal(14,4) --> SE: 4

synlower <- 2-1 # mean effect - 1*SE
synupper <- 2+1 # mean effect + 1*SE

semlower <- 8-2 # mean effect - 1*SE
semupper <- 8+2 # mean effect + 1*SE

synsemlower <- 14-4 # mean effect - 1*SE
synsemupper <- 14+4 # mean effect + 1*SE

# posterior latency factor (we base this only on the semantic interference result)
SEM<-semlower<=simdat_2cues_sameclause$MESem & simdat_2cues_sameclause$MESem<=semupper
table(SEM)
posterior_lf_sem <-simdat_2cues_sameclause[SEM,]$lfs

length(posterior_lf_sem)/nsims
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
                                  names_to = "effect", values_to = "rts")
postpreds_longer$effect <- factor(postpreds_longer$effect, levels = c("syntactic", "semantic", "interaction"))

# plot
ggplot(postpreds_longer, aes(x=rts)) +
  geom_density(aes(),fill="darkgrey")+
  facet_wrap(.~effect)+
  xlim(c(-2,12))+
  xlab("Predicted effect (ms)")+
  geom_vline(xintercept=0, linetype="dashed")+
  theme_bw(base_size = 12)
ggsave("posteriors.png", width=6, height=3, dpi=600)


#### compare posteriors and data ####
## compute mean and 95% CI of each posterior
## combine all posteriors in a list
vectors <- list(maineffectSyn, maineffectSem, interactionSynSem)

## length of list vectors
vec_len <- length(vectors)
## initialize empty data.frame to store values
names <- c("mean", "lower", "upper")
posteriors <- as.data.frame(matrix(numeric(),nrow = vec_len, ncol = 3))
colnames(posteriors) <- names

for (i in 1:vec_len){
  posteriors[i,1] <- mean(vectors[[i]])
  posteriors[i,2] <- unname(quantile(vectors[[i]], probs=0.025))
  posteriors[i,3] <- unname(quantile(vectors[[i]], probs=0.975))
}

## add info regarding experiment, measure and cue (interference) to posteriors dataframe
real_model <- rep(c("model 2cues.sameclause"), times=3)
interference <- c("syntactic", "semantic", "interaction")
posteriors2 <- cbind(real_model, interference, posteriors)

# add data from Schoknecht & Vasishth SPR experiment
dat_syn <- c("data S&V", "syntactic", 2, 0, 4) 
dat_sem <- c("data S&V","semantic",  8, 4, 12) 
dat_synsem <- c("data S&V", "interaction", 14, 7, 22) 

posteriors3 <- rbind(posteriors2, dat_syn, dat_sem, dat_synsem)

## change ordering of factors to make plot look nicer
posteriors3$interference <- factor(posteriors3$interference, levels = c("syntactic", "semantic", "interaction"))
posteriors3$real_model <- factor(posteriors3$real_model, levels = c("data S&V", "model 2cues.sameclause"))
posteriors3$mean <- as.numeric(posteriors3$mean)
posteriors3$upper <- as.numeric(posteriors3$upper)
posteriors3$lower <- as.numeric(posteriors3$lower)

## plot
ggplot(posteriors3,aes(x=interference,y=mean,ymin=lower,ymax=upper)) +
  geom_point(aes(color=real_model), size=3, 
             position =  position_dodge2(.25))+
  geom_errorbar(aes(ymin=lower, ymax=upper, color=real_model),
                width=.25, position = position_dodge2(.25)) +
  geom_hline(yintercept=0, linetype="dashed")+
  theme_bw(base_size = 12)+
  scale_y_continuous(name="retrieval/reading time difference (ms)") + 
  scale_color_manual(values = c("darkgray", "black"),
                     name="data source",
                     breaks=c("data S&V", "model 2cues.sameclause"),
                     labels=c("data S&V", "model 2cues.sameclause")
  )

ggsave("posteriors_vs_data.png", width=6, height=4, dpi=600)

