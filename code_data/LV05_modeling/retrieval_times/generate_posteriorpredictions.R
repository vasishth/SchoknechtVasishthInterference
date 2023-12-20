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
# nsims<-20000
# lfs<-rbeta(nsims,a,b)

# Run ABC with rejection sampling
# results from Schoknecht & Vasishth (critical region): 
# syntactic mean effect 2ms CrI [0,4] --> Normal(2,1) --> SE: 1
# semantic mean effect 8ms CrI [4,12] --> Normal(8,2) --> SE: 2
# interaction mean effect 14ms, CrI [7, 22] --> Normal(14,4) --> SE: 4

synlower <- 2-(2*1) # mean effect - 2*SE
synupper <- 2+(2*1) # mean effect + 2*SE

semlower <- 8-(2*2) # mean effect - 2*SE
semupper <- 8+(2*2) # mean effect + 2*SE

synsemlower <- 14-(2*4) # mean effect - 2*SE
synsemupper <- 14+(2*4) # mean effect + 2*SE

# posterior latency factor (we base this only on the semantic interference result)
SEM<-semlower<=simdat_2cues_sameclause_complete$MESem & simdat_2cues_sameclause_complete$MESem<=semupper
table(SEM)
posterior_lf_sem <-simdat_2cues_sameclause_complete[SEM,]$lfs

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
                                  names_to = "effect", values_to = "rts")
postpreds_longer$effect <- factor(postpreds_longer$effect, levels = c("syntactic", "semantic", "interaction"))
postpreds_longer$pred <- "posterior predictive"

# plot
ggplot(postpreds_longer, aes(x=rts)) +
  geom_density(aes(),fill="darkgrey")+
  facet_wrap(.~effect)+
  xlim(c(-2,20))+
  xlab("Predicted effect (ms)")+
  geom_vline(xintercept=0, linetype="dashed")+
  theme_bw(base_size = 12)
ggsave("posteriors.png", width=6, height=3, dpi=600)


# plot together with prior predictives
prior_predictions <- simdat_2cues_sameclause_complete
colnames(prior_predictions) <- c("lfs", "syntactic", "semantic", "interaction", "sample.id", "RT_LoSynLoSem", "RT_LoSynHiSem", "RT_HiSynLoSem", "RT_HiSynHiSem", "Acc_LoSynLoSem", "Acc_LoSynHiSem", "Acc_HiSynLoSem", "Acc_HiSynHiSem")
prior_predictions <- select(prior_predictions, "syntactic", "semantic", "interaction",)
prior_predictions_long <- tidyr::pivot_longer(prior_predictions, cols=c("syntactic","semantic","interaction"),
                                              names_to="effect", values_to="rts")
prior_predictions_long$rts <- as.numeric(prior_predictions_long$rts)
prior_predictions_long$effect <- factor(prior_predictions_long$effect, levels=c("syntactic", "semantic", "interaction"))
prior_predictions_long$pred <- "prior predictive"
postpreds_longer$lfs <- NULL
preds <- rbind(postpreds_longer, prior_predictions_long)

# add data from Schoknecht & Vasishth SPR experiment
dat <- data.frame(pred   = rep("data",3),
                  effect    = c("syntactic", "semantic", "interaction"), 
                  mean  = c(2,8,14),
                  lower=c(0,4,7),
                  upper=c(4,12,22))

dat$effect <- factor(dat$effect, levels=c("syntactic", "semantic", "interaction"))

ggplot(data=preds, x=rts) +
  geom_point(data=dat, aes(x=mean, y=0), color="darkred", size=3)+
  geom_errorbar(data=dat, aes(y=0, xmin=lower, xmax=upper), color="darkred", width=0.001, size=1) +
  geom_density(aes(x=rts,fill=pred), color="black", alpha=0.3) +
  scale_fill_grey(start = 0, end = .9) +
  ggh4x::facet_grid2(. ~ effect, scales = "free_y", independent = "y")+
  theme_bw(base_size = 12)+
  #geom_abline(intercept=0, linetype="dashed")+
  xlab("retrieval time difference (ms)")+
  guides(fill = guide_legend(reverse=TRUE))+
  theme(legend.title=element_blank(), legend.position = c(0.87, 0.8))

ggsave("priorpred_vs_postpred_vs_data.png", width=7, height=3, dpi=600)
