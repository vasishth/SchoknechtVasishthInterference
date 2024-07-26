# adapted script from Himanshu Yadav
# downloaded from https://osf.io/f4tkd/?view_only=

#####################################
# Using only mean of test data

abc_elpd <- function(posterior_samples,kfold_data,tol_scaler){
  ytest <- as.data.frame(kfold_data %>% group_by(set_id,dataset) %>% 
                           summarise(mean_test_syntactic=mean(test.syntactic),
                                     mean_test_semantic=mean(test.semantic),
                                     mean_test_interaction=mean(test.interaction),
                                     tol_syn=sd(test.syntactic)/tol_scaler,
                                     tol_sem=sd(test.semantic)/tol_scaler,
                                     tol_int=sd(test.interaction)/tol_scaler))
  
  df.lpd <- data.frame(matrix(ncol=3,nrow=nrow(ytest)))
  colnames(df.lpd) <- c("set_id","testset","ELPD")
  
  
  
  for(k in 1:nrow(ytest)){
    ypred_mk.syntactic <- subset(posterior_samples,set_id==ytest[k,]$set_id)$xsim.syntactic
    ypred_mk.semantic <- subset(posterior_samples,set_id==ytest[k,]$set_id)$xsim.semantic
    ypred_mk.interaction <- subset(posterior_samples,set_id==ytest[k,]$set_id)$xsim.interaction
    
    lpd_k <-   log(mean(dnorm((ytest[k,]$mean_test_syntactic-ypred_mk.syntactic),0,ytest[k,]$tol_syn)*
                        dnorm((ytest[k,]$mean_test_semantic-ypred_mk.semantic),0,ytest[k,]$tol_sem))*
                        dnorm((ytest[k,]$mean_test_interaction-ypred_mk.interaction),0,ytest[k,]$tol_int))
    
    df.lpd[k,] <- c(ytest[k,]$set_id,ytest[k,]$dataset,lpd_k)
  }
  df.lpd
}



abc_elpd2 <- function(posterior_samples,kfold_data,tol){
  ytest <- as.data.frame(kfold_data %>% group_by(set_id,dataset) %>% 
                           summarise(mean_test_gram=mean(test.gramInt),
                                     mean_test_ungram=mean(test.ungramInt),
                                     tol_gram=sd(test.gramInt)/4,
                                     tol_ungram=sd(test.ungramInt)/4))
  
  df.lpd <- data.frame(matrix(ncol=3,nrow=nrow(ytest)))
  colnames(df.lpd) <- c("set_id","testset","ELPD")
  for(k in 1:nrow(ytest)){
    ypred_mk.gram <- subset(posterior_samples,set_id==ytest[k,]$set_id)$xsim.gram
    ypred_mk.ungram <- subset(posterior_samples,set_id==ytest[k,]$set_id)$xsim.ungram
    lpd_k <- log(mean(dnorm((ytest[k,]$mean_test_gram-ypred_mk.gram),0,tol)*dnorm((ytest[k,]$mean_test_ungram-ypred_mk.ungram),0,tol)))
    df.lpd[k,] <- c(ytest[k,]$set_id,ytest[k,]$dataset,lpd_k)
  }
  df.lpd
}


#####################################
# Using complete test data

abc_elppd <- function(posterior_samples,kfold_data,tol){
  sum_lpd <- 0
  for(k in 1:6){
    ytest.gram <- subset(kfold_data,fold_id==k)$test.gramInt
    ytest.ungram <- subset(kfold_data,fold_id==k)$test.gramInt
    ypred.gram <- subset(posterior_samples,fold_id==k)$xsim.gram
    ypred.ungram <- subset(posterior_samples,fold_id==k)$xsim.ungram
    exp_lppd <- 0
    for(s in length(ypred.gram)){
      lppd_s <- prod(dnorm((ytest.gram-ypred.gram[s]),0,tol)*dnorm((ytest.ungram-ypred.ungram[s]),0,tol))
      exp_lppd <- exp_lppd+lppd_s
    }
    lpd_k <- log((exp_lppd/length(ypred.gram)))
    sum_lpd <- sum_lpd+lpd_k
  }
  sum_lpd
}

#####################
## KL Divergence
##########

KLdivergence <- function(posterior_samples,kfold_data){
  distance <- 0
  for(k in 1:6){
    ytest.gram <- subset(kfold_data,fold_id==k)$test.gramInt
    ytest.ungram <- subset(kfold_data,fold_id==k)$test.gramInt
    ypred.gram <- subset(posterior_samples,fold_id==k)$xsim.gram
    ypred.ungram <- subset(posterior_samples,fold_id==k)$xsim.ungram
    g <- function(x)
    {
      (dnorm(x, mean=mean(ytest.gram), sd=sd(ytest.gram), log=TRUE) -
         dnorm(x, mean=mean(ypred.gram), sd=sd(ypred.gram), log=TRUE)) *
        dnorm(x, mean=mean(ytest.gram), sd=sd(ytest.gram))
    }
    dist.gram <- integrate(g, -Inf, Inf)$value
    h <- function(x)
    {
      (dnorm(x, mean=mean(ytest.ungram), sd=sd(ytest.ungram), log=TRUE) -
         dnorm(x, mean=mean(ypred.ungram), sd=sd(ypred.ungram), log=TRUE)) *
        dnorm(x, mean=mean(ytest.ungram), sd=sd(ytest.ungram))
    }
    dist.ungram <- integrate(h, -Inf, Inf)$value
    distance <- distance+mean(c(dist.gram,dist.ungram))
  }
  return(distance/6)
}

#######################
## Directly use KLdiv function
KLdiv_kfold <- function(posterior_samples,kfold_data){
  distance <- 0
  for(k in 1:6){
    ytest.gram <- subset(kfold_data,fold_id==k)$test.gramInt
    ytest.ungram <- subset(kfold_data,fold_id==k)$test.gramInt
    ypred.gram <- subset(posterior_samples,fold_id==k)$xsim.gram
    ypred.ungram <- subset(posterior_samples,fold_id==k)$xsim.ungram
    dist_k <- mean(c(KLdiv(cbind(ytest.gram,ypred.gram),symmetric=T,eps=1e-16)[1,2],KLdiv(cbind(ytest.ungram,ypred.ungram),symmetric=T,eps=1e-16)[1,2]))
    distance <- distance+dist_k
  }
  return(distance/6)
}

