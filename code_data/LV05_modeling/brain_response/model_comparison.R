# author: Pia Schoknecht
# date: 30.07.2024

# load packages
library(truncnorm)
library(ggplot2)
library(cowplot)
library(stringr)
library(tidyverse)

# load posterior predictions from all 3 models

load("posterior_predictions_3cues_kfold.Rda")
load("posterior_predictions_2cues_kfold.Rda")
load("posterior_predictions_1cue_kfold.Rda")

####################

#### 3 cues model ####

# prepare data
# combine all kfold sets
cues3_postpreds_wo[[1]]$set_id <- 1
cues3_postpreds_wo[[2]]$set_id <- 2
cues3_postpreds_wo[[3]]$set_id <- 3
cues3_postpreds_wo[[4]]$set_id <- 4
cues3_postpreds_wo[[5]]$set_id <- 5
cues3_postpreds_wo[[6]]$set_id <- 6
cues3_postpreds_wo[[7]]$set_id <- 7
cues3_postpreds_wo[[8]]$set_id <- 8
cues3_postpreds_wo[[9]]$set_id <- 9
cues3_postpreds_wo[[10]]$set_id <- 10

postpreds_cues3 <- rbind(cues3_postpreds_wo[[1]], cues3_postpreds_wo[[2]], cues3_postpreds_wo[[3]], cues3_postpreds_wo[[4]], cues3_postpreds_wo[[5]],
                         cues3_postpreds_wo[[6]], cues3_postpreds_wo[[7]], cues3_postpreds_wo[[8]], cues3_postpreds_wo[[9]], cues3_postpreds_wo[[10]])

#rename columns
postpreds_cues3 <- postpreds_cues3 %>% rename(ypred.syntactic = "syntactic", ypred.semantic = "semantic", ypred.interaction = "interaction")

# multiply by -1 for negative effects
postpreds_cues3$ypred.syntactic <- postpreds_cues3$ypred.syntactic * -1
postpreds_cues3$ypred.semantic <- postpreds_cues3$ypred.semantic * -1
postpreds_cues3$ypred.interaction <- postpreds_cues3$ypred.interaction * -1

# load test data
kfold.estimates <- read.csv("kfold_N400_estimates.csv")
#test_data <- kfold.estimates[,c(1,8:13)]
test_data <- kfold.estimates %>% rename(test.syntactic = "syn.test", test.semantic = "sem.test", test.interaction = "synsem.test",
                                        SE.syn="syn.test.SE",SE.sem="sem.test.SE",SE.synsem="synsem.test.SE")

#### code adapted from https://osf.io/f4tkd/?view_only=, authored by Himanshu Yadav ####

#Empty summary table to save elpds
elpd.df <- data.frame(matrix(ncol = 3,nrow=0))
colnames(elpd.df) <- c("Model","set_id","ELPD")

#Compute log predictive densitiy of seeing the test data given samples from the posterior
source("predictive-density-functions.R")

# The function 'abc_elpd' computes the log predictive density using ABC method
# The function compares the model-generated data and test data
# for each posterior sample to calculate predictive density
# Specify the prior for which we are comparing the models
# You do not need this, if you have only one prior for each model
px <- 0.1

# Define an internal parameter of ABC algorithm, the tolerance scaler
# it scales the delta parameter of the kernel density which is used to
# estimate the predictive density based on difference between test data and
# posterior predictive data, See the formula before this code chunk.
# delta = sd(testData$effect)/tolc
# We have used a similar delta parameter for training the model.
# Keep tolc large enough to get better approximation.
# However, after a point, the large value of tolc does not improve approximation.

###################

tol_scaler <- 9
ytest <- test_data

ytest <- test_data %>% mutate(tol_syn = SE.syn/tol_scaler,
                              tol_sem = SE.sem/tol_scaler,
                              tol_int = SE.synsem/tol_scaler)

df.lpd <- data.frame(matrix(ncol=2,nrow=nrow(ytest)))
colnames(df.lpd) <- c("set_id","ELPD")

for(k in 1:nrow(ytest)){
  ypred.syntactic   <- subset(postpreds_cues3,set_id==ytest[k,]$set_id)$ypred.syntactic
  ypred.semantic    <- subset(postpreds_cues3,set_id==ytest$set_id)$ypred.syntactic
  ypred.interaction <- subset(postpreds_cues3,set_id==ytest$set_id)$ypred.interaction
  
  lpd_k <- log(mean(dnorm((ytest[k,]$test.syntactic-ypred.syntactic),0,ytest[k,]$tol_syn)*
                      dnorm((ytest[k,]$test.semantic-ypred.semantic),0,ytest[k,]$tol_sem)*
                      dnorm((ytest[k,]$test.interaction-ypred.interaction),0,ytest[k,]$tol_int)))
  
  df.lpd[k,] <- c(ytest[k,]$set_id,lpd_k)
}
df.lpd$Model <- "3 cues"

elpd.df <- rbind(elpd.df,df.lpd)


####################

##### 2 cues model ####

# prepare data
# combine all kfold sets
cues2_postpreds_wo[[1]]$set_id <- 1
cues2_postpreds_wo[[2]]$set_id <- 2
cues2_postpreds_wo[[3]]$set_id <- 3
cues2_postpreds_wo[[4]]$set_id <- 4
cues2_postpreds_wo[[5]]$set_id <- 5
cues2_postpreds_wo[[6]]$set_id <- 6
cues2_postpreds_wo[[7]]$set_id <- 7
cues2_postpreds_wo[[8]]$set_id <- 8
cues2_postpreds_wo[[9]]$set_id <- 9
cues2_postpreds_wo[[10]]$set_id <- 10

postpreds_cues2 <- rbind(cues2_postpreds_wo[[1]], cues2_postpreds_wo[[2]], cues2_postpreds_wo[[3]], cues2_postpreds_wo[[4]], cues2_postpreds_wo[[5]],
                         cues2_postpreds_wo[[6]], cues2_postpreds_wo[[7]], cues2_postpreds_wo[[8]],cues2_postpreds_wo[[9]], cues2_postpreds_wo[[10]])

#rename columns
postpreds_cues2 <- postpreds_cues2 %>% rename(ypred.syntactic = "syntactic", ypred.semantic = "semantic", ypred.interaction = "interaction")

# multiply by -1 for negative effects
postpreds_cues2$ypred.syntactic <- postpreds_cues2$ypred.syntactic * -1
postpreds_cues2$ypred.semantic  <- postpreds_cues2$ypred.semantic * -1
postpreds_cues2$ypred.interaction <- postpreds_cues2$ypred.interaction * -1

# load test data
kfold.estimates <- read.csv("kfold_N400_estimates.csv")
test_data <- kfold.estimates %>% rename(test.syntactic = "syn.test", test.semantic = "sem.test", test.interaction = "synsem.test",
                                        SE.syn="syn.test.SE",SE.sem="sem.test.SE",SE.synsem="synsem.test.SE")

#### code adapted from https://osf.io/f4tkd/?view_only=, authored by Himanshu Yadav ####

#Compute log predictive densitiy of seeing the test data given samples from the posterior
source("predictive-density-functions.R")

# The function 'abc_elpd' computes the log predictive density using ABC method
# The function compares the model-generated data and test data
# for each posterior sample to calculate predictive density
# Specify the prior for which we are comparing the models
# You do not need this, if you have only one prior for each model
px <- 0.1

# Define an internal parameter of ABC algorithm, the tolerance scaler
# it scales the delta parameter of the kernel density which is used to
# estimate the predictive density based on difference between test data and
# posterior predictive data, See the formula before this code chunk.
# delta = sd(testData$effect)/tolc
# We have used a similar delta parameter for training the model.
# Keep tolc large enough to get better approximation.
# However, after a point, the large value of tolc does not improve approximation.

###################

tol_scaler <- 9
ytest <- test_data

ytest <- test_data %>% mutate(tol_syn = SE.syn/tol_scaler,
                              tol_sem = SE.sem/tol_scaler,
                              tol_int = SE.synsem/tol_scaler)

df.lpd <- data.frame(matrix(ncol=2,nrow=nrow(ytest)))
colnames(df.lpd) <- c("set_id","ELPD")

for(k in 1:nrow(ytest)){
  ypred.syntactic   <- subset(postpreds_cues2,set_id==ytest[k,]$set_id)$ypred.syntactic
  ypred.semantic    <- subset(postpreds_cues2,set_id==ytest$set_id)$ypred.syntactic
  ypred.interaction <- subset(postpreds_cues2,set_id==ytest$set_id)$ypred.interaction
  
  lpd_k <- log(mean(dnorm((ytest[k,]$test.syntactic-ypred.syntactic),0,ytest[k,]$tol_syn)*
                    dnorm((ytest[k,]$test.semantic-ypred.semantic),0,ytest[k,]$tol_sem)*
                    dnorm((ytest[k,]$test.interaction-ypred.interaction),0,ytest[k,]$tol_int)))
  
  df.lpd[k,] <- c(ytest[k,]$set_id,lpd_k)
}
df.lpd$Model <- "2 cues"

elpd.df <- rbind(elpd.df,df.lpd)

####################

##### 1 cue model ####

# prepare data
# combine all kfold sets
cue1_postpreds_wo[[1]]$set_id <- 1
cue1_postpreds_wo[[2]]$set_id <- 2
cue1_postpreds_wo[[3]]$set_id <- 3
cue1_postpreds_wo[[4]]$set_id <- 4
cue1_postpreds_wo[[5]]$set_id <- 5
cue1_postpreds_wo[[6]]$set_id <- 6
cue1_postpreds_wo[[7]]$set_id <- 7
cue1_postpreds_wo[[8]]$set_id <- 8
cue1_postpreds_wo[[9]]$set_id <- 9
cue1_postpreds_wo[[10]]$set_id <- 10

postpreds_cue1 <- rbind(cue1_postpreds_wo[[1]], cue1_postpreds_wo[[2]], cue1_postpreds_wo[[3]], cue1_postpreds_wo[[4]], cue1_postpreds_wo[[5]],
                        cue1_postpreds_wo[[6]], cue1_postpreds_wo[[7]], cue1_postpreds_wo[[8]], cue1_postpreds_wo[[9]], cue1_postpreds_wo[[10]])

#rename columns
postpreds_cue1 <- postpreds_cue1 %>% rename(ypred.syntactic = "syntactic", ypred.semantic = "semantic", ypred.interaction = "interaction")

# multiply by -1 for negative effects
postpreds_cue1$ypred.syntactic <- postpreds_cue1$ypred.syntactic * -1
postpreds_cue1$ypred.semantic <- postpreds_cue1$ypred.semantic * -1
postpreds_cue1$ypred.interaction <- postpreds_cue1$ypred.interaction * -1

# load test data
kfold.estimates <- read.csv("kfold_N400_estimates.csv")
test_data <- kfold.estimates %>% rename(test.syntactic = "syn.test", test.semantic = "sem.test", test.interaction = "synsem.test",
                                        SE.syn="syn.test.SE",SE.sem="sem.test.SE",SE.synsem="synsem.test.SE")

#### code adapted from https://osf.io/f4tkd/?view_only=, authored by Himanshu Yadav ####

#Compute log predictive densitiy of seeing the test data given samples from the posterior
source("predictive-density-functions.R")

# The function 'abc_elpd' computes the log predictive density using ABC method
# The function compares the model-generated data and test data
# for each posterior sample to calculate predictive density
# Specify the prior for which we are comparing the models
# You do not need this, if you have only one prior for each model
px <- 0.1

# Define an internal parameter of ABC algorithm, the tolerance scaler
# it scales the delta parameter of the kernel density which is used to
# estimate the predictive density based on difference between test data and
# posterior predictive data, See the formula before this code chunk.
# delta = sd(testData$effect)/tolc
# We have used a similar delta parameter for training the model.
# Keep tolc large enough to get better approximation.
# However, after a point, the large value of tolc does not improve approximation.

###################

tol_scaler <- 9
ytest <- test_data

ytest <- test_data %>% mutate(tol_syn = SE.syn/tol_scaler,
                              tol_sem = SE.sem/tol_scaler,
                              tol_int = SE.synsem/tol_scaler)

df.lpd <- data.frame(matrix(ncol=2,nrow=nrow(ytest)))
colnames(df.lpd) <- c("set_id","ELPD")

for(k in 1:nrow(ytest)){
  ypred.syntactic   <- subset(postpreds_cue1,set_id==ytest[k,]$set_id)$ypred.syntactic
  ypred.semantic    <- subset(postpreds_cue1,set_id==ytest$set_id)$ypred.syntactic
  ypred.interaction <- subset(postpreds_cue1,set_id==ytest$set_id)$ypred.interaction
  
  lpd_k <- log(mean(dnorm((ytest[k,]$test.syntactic-ypred.syntactic),0,ytest[k,]$tol_syn)*
                      dnorm((ytest[k,]$test.semantic-ypred.semantic),0,ytest[k,]$tol_sem)*
                      dnorm((ytest[k,]$test.interaction-ypred.interaction),0,ytest[k,]$tol_int)))
  
  df.lpd[k,] <- c(ytest[k,]$set_id,lpd_k)
}
df.lpd$Model <- "1 cue"

elpd.df <- rbind(elpd.df,df.lpd)

# elpd.df contains 10 log predictive density values for each model
# we will now calculate overall elpd for each model along with standard error
elpd.df$ELPD <- as.numeric(elpd.df$ELPD)
elpd.m <- elpd.df %>% group_by(Model) %>% summarise(ELPDsum=sum(ELPD),SE=sqrt(10*var(ELPD)))
elpd.m <- as.data.frame(elpd.m)

elpd.m

save(elpd.df,file="ELPDs_table.Rda")

# compare
ls_models <- c("3 cues","2 cues","1 cue")

#  function
generate.compare.df <- function(elpd.df.m,ns=10,ls_models){
compare.df <- data.frame(matrix(ncol = 4,nrow = 0))
colnames(compare.df) <- c("Model 1","Model 2","ELPDdiff","SE")

for(m1 in ls_models){
  for(m2 in ls_models){
    compare.df[nrow(compare.df)+1,] <- 
      c(m1,m2,
        (sum(subset(elpd.df.m,Model==m1)$ELPD)-
           sum(subset(elpd.df.m,Model==m2)$ELPD)), # ELPDdiff
        sqrt(ns*var(subset(elpd.df.m,Model==m1)$ELPD-
                      subset(elpd.df.m,Model==m2)$ELPD))) # SE
  }
}
compare.df$ELPDdiff <- round(as.numeric(compare.df$ELPDdiff))
compare.df$SE <- round(as.numeric(compare.df$SE))
return(compare.df)
}
#

compare.df <- generate.compare.df(elpd.df,ns=10,ls_models = ls_models)
compare.df


compare.df$`Model 1` <- 
  factor(compare.df$`Model 1`,
         levels = rev(c("","3 cues","2 cues","1 cue")))
         
ggplot(subset(compare.df,ELPDdiff!=0),aes(x=`Model 2`,y=ELPDdiff))+
           geom_point(size=2.5)+
           theme_bw(base_size = 12)+
           facet_grid(~`Model 1`, scales = "free_x")+
           geom_errorbar(aes(ymin=ELPDdiff-(2*SE),
                             ymax=ELPDdiff+(2*SE)),width=0.3)+
           #scale_shape_manual(values=c(0,1,2,4,5,6,8))+
           geom_hline(yintercept = 0,linetype="dashed")+
           ylab(expression(paste("Difference in ",widehat(elpd)," values" )))+
           xlab("compared to a model with...")+
           theme(legend.position = "none")
           #theme(axis.text.x = element_blank(),legend.position = "top",
          #                legend.direction = "vertical",legend.title=element_blank())+
           #theme(panel.spacing.y=unit (1.5, "lines"))
         
ggsave("ELPD_difference.jpg",width=7,height = 4)

         