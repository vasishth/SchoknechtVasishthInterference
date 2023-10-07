library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(rstan)
library(brms)
library(cowplot)


load("model_fits/Fit_m_Full_RSLOPES.Rda")

#variables(m_m_full)
samples <- rstan::extract(m_m_full$fit)

nsubj <- 774
nsamples <- nrow(samples$b_Intercept)

SynInt_subj <- matrix(rep(NA,nsamples*nsubj),ncol=nsubj)
SemInt_subj <- matrix(rep(NA,nsamples*nsubj),ncol=nsubj)
SynSemInt_subj <- matrix(rep(NA,nsamples*nsubj),ncol=nsubj)


for(i in 1:nsubj){
  SynInt_subj[,i] <-
    exp((samples$b_Intercept + samples$r_2_1[,i]) + (0.5*(samples$b[,1] + samples$r_2_2[,i])))-
    exp((samples$b_Intercept + samples$r_2_1[,i]) - (0.5*(samples$b[,1] + samples$r_2_2[,i])))
  SemInt_subj[,i] <-
    exp((samples$b_Intercept + samples$r_2_1[,i]) + (0.5*(samples$b[,2] + samples$r_2_3[,i])))-
    exp((samples$b_Intercept + samples$r_2_1[,i]) - (0.5*(samples$b[,2] + samples$r_2_3[,i])))
 SynSemInt_subj[,i] <-
    exp((samples$b_Intercept + samples$r_2_1[,i]) + (0.5*(samples$b[,3] + samples$r_2_4[,i])))-
    exp((samples$b_Intercept + samples$r_2_1[,i]) - (0.5*(samples$b[,3] + samples$r_2_4[,i])))
}

subj_estimates <- NULL

effect_subj.m <- melt(SynInt_subj)
colnames(effect_subj.m) <- c("sample_id","subject","effect")
as.data.frame(effect_subj.m %>% group_by(subject) %>% summarise(Effect=mean(effect)))
effect_subj.m$condition <- "SynInt"
subj_estimates <- rbind(subj_estimates,effect_subj.m)

effect_subj.m <- melt(SemInt_subj)
colnames(effect_subj.m) <- c("sample_id","subject","effect")
as.data.frame(effect_subj.m %>% group_by(subject) %>% summarise(Effect=mean(effect)))
effect_subj.m$condition <- "SemInt"
subj_estimates <- rbind(subj_estimates,effect_subj.m)

effect_subj.m <- melt(SynSemInt_subj)
colnames(effect_subj.m) <- c("sample_id","subject","effect")
as.data.frame(effect_subj.m %>% group_by(subject) %>% summarise(Effect=mean(effect)))
effect_subj.m$condition <- "SynSemInt"
subj_estimates <- rbind(subj_estimates,effect_subj.m)

save(subj_estimates,file="individual_estimates/critical_subjwise_774.Rda")


#load("Individual-level-estimates/critical_subjwise.Rda")
colnames(subj_estimates)[2] <- "subj_id"

# add experiment info
exp <- c(rep("multiple sessions", 204*3), 
         rep("single session, items 1-60", 350*3),
         rep("single session, items 61-120", 220*3)
         )

dat.m <- subset(subj_estimates) %>% group_by(subj_id,condition) %>%
  summarise(Effect=mean(effect),
            lower.CI=unname(quantile(effect,probs = c(.025,.975)))[1],
            upper.CI=unname(quantile(effect,probs = c(.025,.975)))[2]) %>%
            cbind(exp)
  
dat.m <- as.data.frame(dat.m)
colnames(dat.m)[6] <- "experiment"


## Individual-level distribution of syntactic and semantic interference
subj_levels=subset(dat.m,condition=="SynInt")$subj_id[
  order(subset(dat.m,condition=="SynInt")$Effect)]
dat.m$subj_id <- factor(dat.m$subj_id,levels = subj_levels)

p_syn <- ggplot(subset(dat.m,condition=="SynInt"),
                aes(x=Effect,y=subj_id,))+
  geom_point(size=2,shape=1)+
  geom_errorbar(aes(xmin=lower.CI,xmax=upper.CI),
                width=0,alpha=0.2,size=1.2)+
  coord_cartesian(xlim=c(-60, 100))+
  scale_x_continuous(breaks=seq(-60, 100, 20))+
  ylab("Participant")+
  xlab("Effect (ms)")+
  ggtitle("Syntactic")+
  theme(axis.text.y = element_blank())+
  geom_vline(xintercept = 0,linetype="dashed", colour= "red", size=0.8)+
  geom_vline(xintercept = 10,linetype="solid", colour= "red", size=0.2)+
  geom_vline(xintercept = 20,linetype="solid", colour= "red", size=0.2)

ggsave("plots/individual_syn.jpg", dpi=600, height=6, width=4)

subj_levels=subset(dat.m,condition=="SemInt")$subj_id[
  order(subset(dat.m,condition=="SemInt")$Effect)]
dat.m$subj_id <- factor(dat.m$subj_id,levels = subj_levels)
p_sem <- ggplot(subset(dat.m,condition=="SemInt"),
                aes(x=Effect,y=subj_id))+
  geom_point(size=2,shape=1)+
  geom_errorbar(aes(xmin=lower.CI,xmax=upper.CI),
                width=0,alpha=0.2,size=1.2)+
  coord_cartesian(xlim=c(-60, 100))+
  scale_x_continuous(breaks=seq(-60, 100, 20))+
  ylab("Participant")+
  xlab("Effect (ms)")+
  ggtitle("Semantic")+
  theme(axis.text.y = element_blank())+
  geom_vline(xintercept = 0,linetype="dashed", colour= "red", size=0.8)+
  geom_vline(xintercept = 10,linetype="solid", colour= "red", size=0.2)+
  geom_vline(xintercept = 20,linetype="solid", colour= "red", size=0.2)

ggsave("plots/individual_sem.jpg", dpi=600, height=6, width=4)

subj_levels=subset(dat.m,condition=="SynSemInt")$subj_id[
  order(subset(dat.m,condition=="SynSemInt")$Effect)]
dat.m$subj_id <- factor(dat.m$subj_id,levels = subj_levels)
p_synsem <- ggplot(subset(dat.m,condition=="SynSemInt"),
                aes(x=Effect,y=subj_id))+
  geom_point(size=2,shape=1)+
  geom_errorbar(aes(xmin=lower.CI,xmax=upper.CI),
                width=0,alpha=0.2,size=1.2)+
  coord_cartesian(xlim=c(-60, 100))+
  scale_x_continuous(breaks=seq(-60, 100, 20))+
  ylab("Participant")+
  xlab("Effect (ms)")+
  ggtitle("Interaction")+
  theme(axis.text.y = element_blank())+
  geom_vline(xintercept = 0,linetype="dashed", colour= "red", size=0.8)+
  geom_vline(xintercept = 10,linetype="solid", colour= "red", size=0.2)+
  geom_vline(xintercept = 20,linetype="solid", colour= "red", size=0.2)



ggsave("plots/individual_synsem.jpg", dpi=600, height=6, width=4)

plot_row <- plot_grid(p_syn, p_sem, p_synsem, ncol=3)

ggsave("plots/individual_diffs.jpg", dpi=600, height=6, width=10)


