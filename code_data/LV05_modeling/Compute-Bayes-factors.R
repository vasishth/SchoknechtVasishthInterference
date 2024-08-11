# author: Himanshu Yadav
# date: 11.08.2024

# load packages
library(truncnorm)
library(ggplot2)
library(cowplot)
library(stringr)
library(dplyr)

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

## Prior 1: normal_{lb=0,ub=0.05}(0.01,0.01)

load("Simulated-data/Simulated_data_two_cues_model.Rda")
df.lkl <- simdat_2cues_sameclause
head(df.lkl)
df.lkl$lkl <- dnorm(df.lkl$MESem-(-SVdat[2,]$mean),0,delta)
df.lkl$prior.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0.01,sd=0.01)
df.lkl$proposal.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0,sd=0.025)
ML <- mean((df.lkl$lkl)*(df.lkl$prior.dens)/(df.lkl$proposal.dens))
df.lkl$ML <- ML
df.lkl$prior <- "normal(0.01,0.01)"
df.bf <- as.data.frame(df.lkl %>% group_by(prior) %>% 
                         summarize(MargLik=mean(ML)))
df.bf
df.bf$Model <- "Two cues model"

load("Simulated-data/Simulated_data_three_cues_model.Rda")
df.lkl <- simdat_3cues
df.lkl$Model <- "Three cues model"
df.lkl$lkl <- dnorm(df.lkl$MESem-(-SVdat[2,]$mean),0,delta)
df.lkl$prior.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0.01,sd=0.01)
df.lkl$proposal.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0,sd=0.025)
ML <- mean((df.lkl$lkl)*(df.lkl$prior.dens)/(df.lkl$proposal.dens))
df.lkl$ML <- ML
df.lkl$prior <- "normal(0.01,0.01)"
df.bf <- rbind(df.bf,as.data.frame(df.lkl %>% group_by(Model,prior) %>% 
                         summarize(MargLik=mean(ML))))
df.bf

load("Simulated-data/Simulated_data_one_cue_model.Rda")
df.lkl <- simdat_1cue
df.lkl$Model <- "One cue model"
df.lkl$lkl <- dnorm(df.lkl$MESem-(-SVdat[2,]$mean),0,delta)
df.lkl$prior.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0.01,sd=0.01)
df.lkl$proposal.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0,sd=0.025)
ML <- mean((df.lkl$lkl)*(df.lkl$prior.dens)/(df.lkl$proposal.dens))
df.lkl$ML <- ML
df.lkl$prior <- "normal(0.01,0.01)"
df.bf <- rbind(df.bf,as.data.frame(df.lkl %>% group_by(Model,prior) %>% 
                                     summarize(MargLik=mean(ML))))
df.bf

#############################
## Prior 2: normal(0.01,0.02)

load("Simulated-data/Simulated_data_two_cues_model.Rda")
df.lkl <- simdat_2cues_sameclause
df.lkl$Model <- "Two cues model"
df.lkl$lkl <- dnorm(df.lkl$MESem-(-SVdat[2,]$mean),0,delta)
df.lkl$prior.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0.01,sd=0.02)
df.lkl$proposal.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0,sd=0.025)
ML <- mean((df.lkl$lkl)*(df.lkl$prior.dens)/(df.lkl$proposal.dens))
df.lkl$ML <- ML
df.lkl$prior <- "normal(0.01,0.02)"
df.bf <- rbind(df.bf,as.data.frame(df.lkl %>% group_by(Model,prior) %>% 
                         summarize(MargLik=mean(ML))))
df.bf

load("Simulated-data/Simulated_data_three_cues_model.Rda")
df.lkl <- simdat_3cues
df.lkl$Model <- "Three cues model"
df.lkl$lkl <- dnorm(df.lkl$MESem-(-SVdat[2,]$mean),0,delta)
df.lkl$prior.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0.01,sd=0.02)
df.lkl$proposal.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0,sd=0.025)
ML <- mean((df.lkl$lkl)*(df.lkl$prior.dens)/(df.lkl$proposal.dens))
df.lkl$ML <- ML
df.lkl$prior <- "normal(0.01,0.02)"
df.bf <- rbind(df.bf,as.data.frame(df.lkl %>% group_by(Model,prior) %>% 
                                     summarize(MargLik=mean(ML))))
df.bf

load("Simulated-data/Simulated_data_one_cue_model.Rda")
df.lkl <- simdat_1cue
df.lkl$Model <- "One cue model"
df.lkl$lkl <- dnorm(df.lkl$MESem-(-SVdat[2,]$mean),0,delta)
df.lkl$prior.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0.01,sd=0.02)
df.lkl$proposal.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0,sd=0.025)
ML <- mean((df.lkl$lkl)*(df.lkl$prior.dens)/(df.lkl$proposal.dens))
df.lkl$ML <- ML
df.lkl$prior <- "normal(0.01,0.02)"
df.bf <- rbind(df.bf,as.data.frame(df.lkl %>% group_by(Model,prior) %>% 
                                     summarize(MargLik=mean(ML))))
df.bf

########################
## Prior 3: normal(0.01,0.005)

load("Simulated-data/Simulated_data_two_cues_model.Rda")
df.lkl <- simdat_2cues_sameclause
df.lkl$Model <- "Two cues model"
df.lkl$lkl <- dnorm(df.lkl$MESem-(-SVdat[2,]$mean),0,delta)
df.lkl$prior.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0.01,sd=0.005)
df.lkl$proposal.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0,sd=0.025)
ML <- mean((df.lkl$lkl)*(df.lkl$prior.dens)/(df.lkl$proposal.dens))
df.lkl$ML <- ML
df.lkl$prior <- "normal(0.01,0.005)"
df.bf <- rbind(df.bf,as.data.frame(df.lkl %>% group_by(Model,prior) %>% 
                                     summarize(MargLik=mean(ML))))
df.bf

load("Simulated-data/Simulated_data_three_cues_model.Rda")
df.lkl <- simdat_3cues
df.lkl$Model <- "Three cues model"
df.lkl$lkl <- dnorm(df.lkl$MESem-(-SVdat[2,]$mean),0,delta)
df.lkl$prior.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0.01,sd=0.005)
df.lkl$proposal.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0,sd=0.025)
ML <- mean((df.lkl$lkl)*(df.lkl$prior.dens)/(df.lkl$proposal.dens))
df.lkl$ML <- ML
df.lkl$prior <- "normal(0.01,0.005)"
df.bf <- rbind(df.bf,as.data.frame(df.lkl %>% group_by(Model,prior) %>% 
                                     summarize(MargLik=mean(ML))))
df.bf

load("Simulated-data/Simulated_data_one_cue_model.Rda")
df.lkl <- simdat_1cue
df.lkl$Model <- "One cue model"
df.lkl$lkl <- dnorm(df.lkl$MESem-(-SVdat[2,]$mean),0,delta)
df.lkl$prior.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0.01,sd=0.005)
df.lkl$proposal.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0,sd=0.025)
ML <- mean((df.lkl$lkl)*(df.lkl$prior.dens)/(df.lkl$proposal.dens))
df.lkl$ML <- ML
df.lkl$prior <- "normal(0.01,0.005)"
df.bf <- rbind(df.bf,as.data.frame(df.lkl %>% group_by(Model,prior) %>% 
                                     summarize(MargLik=mean(ML))))
df.bf

#######################

## Prior 4: normal(0.02,0.01)

load("Simulated-data/Simulated_data_two_cues_model.Rda")
df.lkl <- simdat_2cues_sameclause
df.lkl$Model <- "Two cues model"
df.lkl$lkl <- dnorm(df.lkl$MESem-(-SVdat[2,]$mean),0,delta)
df.lkl$prior.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0.02,sd=0.01)
df.lkl$proposal.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0,sd=0.025)
ML <- mean((df.lkl$lkl)*(df.lkl$prior.dens)/(df.lkl$proposal.dens))
df.lkl$ML <- ML
df.lkl$prior <- "normal(0.02,0.01)"
df.bf <- rbind(df.bf,as.data.frame(df.lkl %>% group_by(Model,prior) %>% 
                                     summarize(MargLik=mean(ML))))
df.bf

load("Simulated-data/Simulated_data_three_cues_model.Rda")
df.lkl <- simdat_3cues
df.lkl$Model <- "Three cues model"
df.lkl$lkl <- dnorm(df.lkl$MESem-(-SVdat[2,]$mean),0,delta)
df.lkl$prior.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0.02,sd=0.01)
df.lkl$proposal.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0,sd=0.025)
ML <- mean((df.lkl$lkl)*(df.lkl$prior.dens)/(df.lkl$proposal.dens))
df.lkl$ML <- ML
df.lkl$prior <- "normal(0.02,0.01)"
df.bf <- rbind(df.bf,as.data.frame(df.lkl %>% group_by(Model,prior) %>% 
                                     summarize(MargLik=mean(ML))))
df.bf

load("Simulated-data/Simulated_data_one_cue_model.Rda")
df.lkl <- simdat_1cue
df.lkl$Model <- "One cue model"
df.lkl$lkl <- dnorm(df.lkl$MESem-(-SVdat[2,]$mean),0,delta)
df.lkl$prior.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0.02,sd=0.01)
df.lkl$proposal.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0,sd=0.025)
ML <- mean((df.lkl$lkl)*(df.lkl$prior.dens)/(df.lkl$proposal.dens))
df.lkl$ML <- ML
df.lkl$prior <- "normal(0.02,0.01)"
df.bf <- rbind(df.bf,as.data.frame(df.lkl %>% group_by(Model,prior) %>% 
                                     summarize(MargLik=mean(ML))))
df.bf

#################
## Prior 5: normal(0.02,0.02)

load("Simulated-data/Simulated_data_two_cues_model.Rda")
df.lkl <- simdat_2cues_sameclause
df.lkl$Model <- "Two cues model"
df.lkl$lkl <- dnorm(df.lkl$MESem-(-SVdat[2,]$mean),0,delta)
df.lkl$prior.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0.02,sd=0.02)
df.lkl$proposal.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0,sd=0.025)
ML <- mean((df.lkl$lkl)*(df.lkl$prior.dens)/(df.lkl$proposal.dens))
df.lkl$ML <- ML
df.lkl$prior <- "normal(0.02,0.02)"
df.bf <- rbind(df.bf,as.data.frame(df.lkl %>% group_by(Model,prior) %>% 
                                     summarize(MargLik=mean(ML))))
df.bf

load("Simulated-data/Simulated_data_three_cues_model.Rda")
df.lkl <- simdat_3cues
df.lkl$Model <- "Three cues model"
df.lkl$lkl <- dnorm(df.lkl$MESem-(-SVdat[2,]$mean),0,delta)
df.lkl$prior.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0.02,sd=0.02)
df.lkl$proposal.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0,sd=0.025)
ML <- mean((df.lkl$lkl)*(df.lkl$prior.dens)/(df.lkl$proposal.dens))
df.lkl$ML <- ML
df.lkl$prior <- "normal(0.02,0.02)"
df.bf <- rbind(df.bf,as.data.frame(df.lkl %>% group_by(Model,prior) %>% 
                                     summarize(MargLik=mean(ML))))
df.bf

load("Simulated-data/Simulated_data_one_cue_model.Rda")
df.lkl <- simdat_1cue
df.lkl$Model <- "One cue model"
df.lkl$lkl <- dnorm(df.lkl$MESem-(-SVdat[2,]$mean),0,delta)
df.lkl$prior.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0.02,sd=0.02)
df.lkl$proposal.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0,sd=0.025)
ML <- mean((df.lkl$lkl)*(df.lkl$prior.dens)/(df.lkl$proposal.dens))
df.lkl$ML <- ML
df.lkl$prior <- "normal(0.02,0.02)"
df.bf <- rbind(df.bf,as.data.frame(df.lkl %>% group_by(Model,prior) %>% 
                                     summarize(MargLik=mean(ML))))
df.bf



#########################
## Prior 6: normal(0.02,0.005)

load("Simulated-data/Simulated_data_two_cues_model.Rda")
df.lkl <- simdat_2cues_sameclause
df.lkl$Model <- "Two cues model"
df.lkl$lkl <- dnorm(df.lkl$MESem-(-SVdat[2,]$mean),0,delta)
df.lkl$prior.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0.02,sd=0.005)
df.lkl$proposal.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0,sd=0.025)
ML <- mean((df.lkl$lkl)*(df.lkl$prior.dens)/(df.lkl$proposal.dens))
df.lkl$ML <- ML
df.lkl$prior <- "normal(0.02,0.005)"
df.bf <- rbind(df.bf,as.data.frame(df.lkl %>% group_by(Model,prior) %>% 
                                     summarize(MargLik=mean(ML))))
df.bf

load("Simulated-data/Simulated_data_three_cues_model.Rda")
df.lkl <- simdat_3cues
df.lkl$Model <- "Three cues model"
df.lkl$lkl <- dnorm(df.lkl$MESem-(-SVdat[2,]$mean),0,delta)
df.lkl$prior.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0.02,sd=0.005)
df.lkl$proposal.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0,sd=0.025)
ML <- mean((df.lkl$lkl)*(df.lkl$prior.dens)/(df.lkl$proposal.dens))
df.lkl$ML <- ML
df.lkl$prior <- "normal(0.02,0.005)"
df.bf <- rbind(df.bf,as.data.frame(df.lkl %>% group_by(Model,prior) %>% 
                                     summarize(MargLik=mean(ML))))
df.bf

load("Simulated-data/Simulated_data_one_cue_model.Rda")
df.lkl <- simdat_1cue
df.lkl$Model <- "One cue model"
df.lkl$lkl <- dnorm(df.lkl$MESem-(-SVdat[2,]$mean),0,delta)
df.lkl$prior.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0.02,sd=0.005)
df.lkl$proposal.dens <- dtruncnorm(df.lkl$lfs,a=0,b=0.05,mean=0,sd=0.025)
ML <- mean((df.lkl$lkl)*(df.lkl$prior.dens)/(df.lkl$proposal.dens))
df.lkl$ML <- ML
df.lkl$prior <- "normal(0.02,0.005)"
df.bf <- rbind(df.bf,as.data.frame(df.lkl %>% group_by(Model,prior) %>% 
                                     summarize(MargLik=mean(ML))))
df.bf

write.csv(df.bf,file="Marginal-likelihood-results.csv")

df.bf %>% group_by(Model) %>% summarise(count=n())
df.bf %>% group_by(prior,Model) %>% summarise(count=n())

df.bf <- as.data.frame(df.bf %>% group_by(prior) %>% mutate(BF=MargLik[which(Model=="Two cues model")]/MargLik))
write.csv(df.bf,file="Bayes-factor-in-favor-of-two-cues-model.csv")

evidence_for_two_cues <- subset(df.bf,Model!="Two cues model")

evidence_for_two_cues$prior <- factor(evidence_for_two_cues$prior,
                                      levels = evidence_for_two_cues$prior[c(11,5,7,1,9,3)])
evidence_for_two_cues$Model <- ifelse(evidence_for_two_cues$Model=="One cue model","compared to the one cue model","compared to the three cues model")

evidence_for_two_cues3 <- subset(evidence_for_two_cues, Model=="compared to the three cues model")
evidence_for_two_cues1 <- subset(evidence_for_two_cues, Model=="compared to the one cue model")


p1 <- ggplot(evidence_for_two_cues3,aes(x=prior,y=BF,group=Model))+
  geom_point(size=1.5)+
  geom_line()+
  theme_bw(base_size = 12)+
  ylab("Evidence for the two cues model \n (Bayes factor)")+
  geom_hline(yintercept = 1, linetype="dashed") +
  facet_wrap(~Model,scales = "free")+
  theme(axis.text.x = element_text(angle=60, hjust=0.9, vjust = 0.9))

p2 <- ggplot(evidence_for_two_cues1,aes(x=prior,y=BF,group=Model))+
  geom_point(size=1.5)+
  geom_line()+
  theme_bw(base_size = 12)+
  ylab("Evidence for the two cues model \n (Bayes factor)")+
  geom_hline(yintercept = 1, linetype="dashed") +
  facet_wrap(~Model,scales = "free")+
  scale_y_log10("",
                breaks =  c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000),
                labels =  c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000)) +
  theme(axis.text.x = element_text(angle=60, hjust=0.9, vjust = 0.9))

plot_grid(p1, p2)

ggsave("BF_plot_compmodels.png",height=4.5,width=7)
