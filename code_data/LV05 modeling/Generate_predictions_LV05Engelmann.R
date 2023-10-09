library(dplyr)
library(tidyr)
library(ggplot2)
library(parallel)
library(LaplacesDemon)
library(mvtnorm)
library(truncnorm)
library(reshape2)


## some helper functions:
rmsd <- function (obs, pred) {
  sqrt(mean((obs - pred)^2, na.rm = TRUE))
}

compute_int_means <- function(d){
  cond_means <- as.data.frame(d) %>% group_by(Set, Target, Distractor, lf, ans, mas, mp, rth, bll, psc, pic, qcf, qco, cuesim, tprom, dprom, lp, ldp, blc, dbl, ndistr, cueweighting) %>% summarise(meanRT=mean(latency), SE=sd(latency)/sqrt(length(latency)))
  int <- reshape(as.data.frame(d[,c(2,5,40)]),direction="wide",idvar = "Iteration",timevar = "Distractor")
  colnames(int)[2:5] <- gsub("latency.","",colnames(int)[2:5])
  int <- int %>% mutate(SynInt=(SynMatch+FullMatch)-(NoMatch+SemMatch),SemInt=(SemMatch+FullMatch)-(NoMatch+SynMatch),SynSemInt=FullMatch-SynMatch,NoSynSemInt=SemMatch-NoMatch)
  Effect <- unname(colMeans(int))[-1]
  return(Effect)
}

convert2log <- function(x){
  ifelse(x>=1, log(x), ifelse(x<=-1, -log(abs(x)), 0))
}

convert2log10 <- function(x){
  x <- ifelse(x>-1 & x<1, 0, x)
  x <- ifelse(x<=-1, -log10(abs(x)), x) 
  x <- ifelse(x>=1, log10(abs(x)), x)
}


# Introduction 

## Load interact subject VERSION (syntactic cue = +-subject position)

#The following piece of code, written by Felix Engelmann and available on github, provides the main computation code for ACT-R calculations.

source("interACT_subject.R")

## Basic engine for generating predictions

printcounts<-FALSE

iterate_lf <- function(values,iterations=1000){
  ## values is a dataframe containing either 1 row of parameters 
  # (a combination of latency factor and cue-weighting parameter, or multiple rows i.e., multiple combinations of two parameters)
  ## iterations is the number of iterations for that given value.
  ## We need multiple iterations as noise is non-zero and there will be some 
  ## variability due to noise. 
  for(v in 1:nrow(values)){
    lf <<- values[v,]$latency_f
    cueweighting <<- values[v,]$cue_w
    pmatr <- create_param_matrix(model_4cond, iterations) 
    results <- run(pmatr)
    int <- reshape(as.data.frame(results[,c(2,5,40)]),direction="wide",idvar = "Iteration",timevar = "Distractor")
    colnames(int)[2:5] <- gsub("latency.","",colnames(int)[2:5])
    int2 <- int %>% mutate(MESyn=((SynMatch+FullMatch)/2)-((NoMatch+SemMatch)/2),MESem=((SemMatch+FullMatch)/2)-((NoMatch+SynMatch)/2),Int=(FullMatch-SynMatch)-(SemMatch-NoMatch))
    Effect <- unname(colMeans(int2))[-1]
  }
  return(Effect)
}


reset_params()
psc <<- 0
qcf <<- 0
cuesim <<- -1
bll <<- 0.5
## default in Engelmann et al 2019 Cog Sci paper
mp <<- 0.15
## default in Engelmann et al 2019 Cog Sci paper
mas <<- 1.5 ## could change this to a random starting value: 
## mas <- runif(1,min=1,max=2)
## mas<<-sort(rnorm(50,mean=1.5,sd=0.25))
# default in Engelmann et al 2019 Cog Sci paper
ans <<- 0.2
# default in Engelmann et al 2019 Cog Sci paper
rth <<-  -1.5
dbl <<- 0
#cueweighting <<- 1 



#### Estimate posterior using RMCMC with tuning 0.1, 0.14, tolerance SD and summary automatic:
nsims <- 500
a<-4
b<-6
lfs <-rbeta(nsims,a,b)

# empty dfs for saving
generated_means_l_ss <- data.frame(matrix(ncol=5,nrow=nsims))
colnames(generated_means_l_ss) <- c("sample.id", "LoSynLoSem","LoSynHiSem","HiSynLoSem","HiSynHiSem")
generated_effects_l_ss <- data.frame(matrix(ncol=4,nrow=nsims))
colnames(generated_effects_l_ss) <- c("sample.id","syntactic", "semantic", "interaction")

for(i in 1:nsims){
  # Run model
  print(paste("run: ",i,sep=""))  
  latency_f <- lfs[i]
  
  cue_w <<- 1
  proposal <- data.frame(latency_f,cue_w)
  ## get generated cond means and effects:   
  generated <- iterate_lf(proposal)
  generated_means_l_ss[i,] <- c(i, generated[1:4]) #NoMatch=LoSynLoSem, SemMatch=LoSynHiSem,
  #SynMatch=HiSynLoSem, FullMatch=HiSynHiSem
  generated_effects_l_ss[i,] <- c(i, generated[5:7]) # MeSyn, MESem, Int
}

save(generated_means_l_ss, file="sim_dat_LV05Engelmann_conds_subject.Rda")
save(generated_effects_l_ss, file="sim_dat_LV05Engelmann_effects_subject.Rda")



## Load interact SAME CLAUSE VERSION (syntactic cue = +-same clause, distractor is always -same clause)

#The following piece of code, written by Felix Engelmann and available on github, provides the main computation code for ACT-R calculations.

source("interACT_sameclause.R")

## Basic engine for generating predictions

printcounts<-FALSE

iterate_lf <- function(values,iterations=1000){
  ## values is a dataframe containing either 1 row of parameters 
  # (a combination of latency factor and cue-weighting parameter, or multiple rows i.e., multiple combinations of two parameters)
  ## iterations is the number of iterations for that given value.
  ## We need multiple iterations as noise is non-zero and there will be some 
  ## variability due to noise. 
  for(v in 1:nrow(values)){
    lf <<- values[v,]$latency_f
    cueweighting <<- values[v,]$cue_w
    pmatr <- create_param_matrix(model_4cond, iterations) 
    results <- run(pmatr)
    int <- reshape(as.data.frame(results[,c(2,5,40)]),direction="wide",idvar = "Iteration",timevar = "Distractor")
    colnames(int)[2:5] <- gsub("latency.","",colnames(int)[2:5])
    int2 <- int %>% mutate(MESyn=((SynMatch+FullMatch)/2)-((NoMatch+SemMatch)/2),MESem=((SemMatch+FullMatch)/2)-((NoMatch+SynMatch)/2),Int=(FullMatch-SynMatch)-(SemMatch-NoMatch))
    Effect <- unname(colMeans(int2))[-1]
  }
  return(Effect)
}


reset_params()
psc <<- 0
qcf <<- 0
cuesim <<- -1
bll <<- 0.5
## default in Engelmann et al 2019 Cog Sci paper
mp <<- 0.15
## default in Engelmann et al 2019 Cog Sci paper
mas <<- 1.5 ## could change this to a random starting value: 
## mas <- runif(1,min=1,max=2)
## mas<<-sort(rnorm(50,mean=1.5,sd=0.25))
# default in Engelmann et al 2019 Cog Sci paper
ans <<- 0.2
# default in Engelmann et al 2019 Cog Sci paper
rth <<-  -1.5
dbl <<- 0
#cueweighting <<- 1 



#### Estimate posterior using RMCMC with tuning 0.1, 0.14, tolerance SD and summary automatic:
nsims <- 500
a<-4
b<-6
lfs <-rbeta(nsims,a,b)

# empty dfs for saving
generated_means_l_sc <- data.frame(matrix(ncol=5,nrow=nsims))
colnames(generated_means_l_sc) <- c("sample.id", "LoSynLoSem","LoSynHiSem","HiSynLoSem","HiSynHiSem")
generated_effects_l_sc <- data.frame(matrix(ncol=4,nrow=nsims))
colnames(generated_effects_l_sc) <- c("sample.id","syntactic", "semantic", "interaction")

for(i in 1:nsims){
  # Run model
  print(paste("run: ",i,sep=""))  
  latency_f <- lfs[i]

  cue_w <<- 1
  proposal <- data.frame(latency_f,cue_w)
  ## get generated cond means and effects:   
  generated <- iterate_lf(proposal)
  generated_means_l_sc[i,] <- c(i, generated[1:4]) #NoMatch=LoSynLoSem, SemMatch=LoSynHiSem,
                                                       #SynMatch=HiSynLoSem, FullMatch=HiSynHiSem
  generated_effects_l_sc[i,] <- c(i, generated[5:7]) # MeSyn, MESem, Int
}

save(generated_means_l_sc, file="sim_dat_LV05Engelmann_conds_sameclause.Rda")
save(generated_effects_l_sc, file="sim_dat_LV05Engelmann_effects_sameclause.Rda")


# effect sizes
round(c(mean = mean(generated_effects_l_ss$semantic), quantile(generated_effects_l_ss$semantic, probs = c(.025, .975))))
round(c(mean = mean(generated_effects_l_ss$syntactic), quantile(generated_effects_l_ss$syntactic, probs = c(.025, .975))))

round(c(mean = mean(generated_effects_l_sc$semantic), quantile(generated_effects_l_sc$semantic, probs = c(.025, .975))))
round(c(mean = mean(generated_effects_l_sc$syntactic), quantile(generated_effects_l_sc$syntactic, probs = c(.025, .975))))

# combine sim data
generated_effects_l_ss$syncue <- "± grammatical subject"
generated_effects_l_sc$syncue <- "± same.clause"

generated_effects <- rbind(generated_effects_l_ss, generated_effects_l_sc)

# plot effects
generated_effects_long <- pivot_longer(generated_effects, cols=c("syntactic","semantic","interaction"),
                               names_to="int_type", values_to="effsize")
generated_effects_long$effsize <- as.numeric(generated_effects_long$effsize)
generated_effects_long$int_type <- factor(generated_effects_long$int_type, levels=c("syntactic", "semantic", "interaction"))
generated_effects_long$syncue <- factor(generated_effects_long$syncue, levels=c("± grammatical subject", "± same.clause"))


ggplot(data=generated_effects_long, x=effsize) +
  geom_density(aes(x=effsize), fill="lightgrey") +
  facet_grid(syncue ~ int_type, scale="free_y")+
  theme_bw(base_size = 10)+
  geom_abline(intercept=0, linetype="dashed")+
  xlab("reading time difference (ms)")

ggsave("PriorPredicted_LV05_engelmann_effects_syncue.png", width=6, height=4, dpi=600)
