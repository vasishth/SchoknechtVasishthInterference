# author: Pia Schoknecht
# date: 23.07.2024

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

unique(cues3_postpreds_wo[[1]]$lfs)
unique(cues3_postpreds_wo[[2]]$lfs)
unique(cue1_postpreds_wo[[2]]$lfs)
unique(cue1_postpreds_wo[[1]]$lfs)

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

cues2_postpreds_wo[[9]]$heldout <- 9
cues2_postpreds_wo[[10]]$heldout <- 10

postpreds2 <- rbind(cues2_postpreds_wo[[1]], cues2_postpreds_wo[[2]], cues2_postpreds_wo[[3]], cues2_postpreds_wo[[4]], cues2_postpreds_wo[[5]],
                    cues2_postpreds_wo[[6]], cues2_postpreds_wo[[7]], cues2_postpreds_wo[[8]],cues2_postpreds_wo[[9]], cues2_postpreds_wo[[10]])


#rename columns and add dataset column
postpreds2 <- postpreds2 %>% rename(test.syntactic = "syntactic", test.semantic = "semantic", test.interaction = "interaction")
postpreds2$dataset <- 1

# save complete data (no heldout data) as test_data and create the same columns as in the postpreds df
test_data <- cues2_postpreds_wo[[11]]
test_data <- test_data %>% rename(test.syntactic = "syntactic", test.semantic = "semantic", test.interaction = "interaction")
test_data$dataset <- 1
test_data$set_id <- "11"

#### code adapted from https://osf.io/f4tkd/?view_only=, authored by Himanshu Yadav ####

#Empty summary table to save elpds
elpd.df <- data.frame(matrix(ncol = 4,nrow=0))
colnames(elpd.df) <- c("Model","set_id","testset","ELPD")

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

posterior_samples <- postpreds2
kfold_data <- test_data
tol_scaler <- 9

  ytest <- as.data.frame(test_data %>% group_by(set_id,dataset) %>% 
                           summarise(mean_test_syntactic=mean(test.syntactic),
                                     mean_test_semantic=mean(test.semantic),
                                     mean_test_interaction=mean(test.interaction),
                                     tol_syn=sd(test.syntactic)/9,
                                     tol_sem=sd(test.semantic)/9,
                                     tol_int=sd(test.interaction)/9))
  
  
  
  df.lpd <- data.frame(matrix(ncol=3,nrow=nrow(ytest)))
  colnames(df.lpd) <- c("set_id","testset","ELPD")
  
  
  
  for(k in 1:nrow(ytest)){
    ypred_mk.syntactic <- subset(postpreds2,set_id==ytest$set_id)$xsim.syntactic
    ypred_mk.semantic <- subset(postpreds2,set_id==ytest$set_id)$xsim.semantic
    ypred_mk.interaction <- subset(postpreds2,set_id==ytest$set_id)$xsim.interaction
    
    lpd_k <-   log(mean(dnorm((ytest[k,]$mean_test_syntactic-ypred_mk.syntactic),0,ytest[k,]$tol_syn)*
                          dnorm((ytest[k,]$mean_test_semantic-ypred_mk.semantic),0,ytest[k,]$tol_sem))*
                     dnorm((ytest[k,]$mean_test_interaction-ypred_mk.interaction),0,ytest[k,]$tol_int))
    
    df.lpd[k,] <- c(ytest[k,]$set_id,ytest[k,]$dataset,lpd_k)
  }
  df.lpd
}

####################
tolc <- 9
elpd_values <- abc_elpd(postpreds2,test_data,tolc)

elpd_values$Model <- "3cues"
elpd.df <- rbind(elpd.df,elpd_values)


cues2_postpreds_wo[[1]]$heldout <- 1
cues2_postpreds_wo[[2]]$heldout <- 2
cues2_postpreds_wo[[3]]$heldout <- 3
cues2_postpreds_wo[[4]]$heldout <- 4
cues2_postpreds_wo[[5]]$heldout <- 5
cues2_postpreds_wo[[6]]$heldout <- 6
cues2_postpreds_wo[[7]]$heldout <- 7
cues2_postpreds_wo[[8]]$heldout <- 8
cues2_postpreds_wo[[9]]$heldout <- 9
cues2_postpreds_wo[[10]]$heldout <- 10
cues2_postpreds_wo[[11]]$heldout <- 0

postpreds <- rbind(cues2_postpreds_wo[[1]],cues2_postpreds_wo[[2]], cues2_postpreds_wo[[3]], cues2_postpreds_wo[[4]], cues2_postpreds_wo[[5]],
                   cues2_postpreds_wo[[6]], cues2_postpreds_wo[[7]], cues2_postpreds_wo[[8]], cues2_postpreds_wo[[9]], cues2_postpreds_wo[[10]],
                   cues2_postpreds_wo[[11]])
postpreds$pred <- "posterior predictions"
