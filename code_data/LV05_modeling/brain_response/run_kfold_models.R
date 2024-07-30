library(tidyverse)
library(brms)

# load data
eeg <- read.csv("../eeg/data_eeg.csv")

# create 10 folds
# repeat the numbers 1:0 and add it as set_id to data frame
id <- rep(c(1:10), times=1197)
id2 <- id[1:11967]
eeg$set_id <- id2

# checks
eeg %>% group_by(set_id) %>%
  distinct(subject) %>%
  count()
# --> each set includes all subjects

eeg %>% group_by(set_id) %>%
  distinct(item) %>%
  count()
# --> each set includes all items

eeg %>% group_by(set_id) %>%
  distinct(cond) %>%
  count()
# --> each set includes all conditions


# run model for each fold

# symmetrical priors 
priors_m <- c(
  prior(normal(0, 5), class = Intercept),
  prior(normal(0, 2), class = b, coef="amplitude_prestim"),
  prior(normal(0, 0.5), class = b, coef="syn"),
  prior(normal(0, 0.5), class = b, coef="sem"),
  prior(normal(0, 0.5), class = b, coef="syn:sem"),
  prior(normal(10, 5), class = sigma),
  prior(normal(0, 2), class = sd)
)  

for(k in 1:10){
  m_wo_train <- brm(amplitude_n400 ~ 1 + amplitude_prestim + syn*sem + (1+syn*sem||subject) + (1+syn*sem||item),
                     prior = priors_m,
                     warmup = 2000,
                     iter = 10000,
                     cores = 4,
                     control = list(adapt_delta = 0.9),
                     save_pars = save_pars(all = TRUE),
                     data = subset(eeg, set_id != k),
                     sample_prior="yes")
  save(m_wo_train, file = paste0("kfold_model_fits/Fit_wo",as.character(k),"_training.Rda"))
  
  m_wo_test <- brm(amplitude_n400 ~ 1 + amplitude_prestim + syn*sem + (1+syn*sem||subject) + (1+syn*sem||item),
                     prior = priors_m,
                     warmup = 2000,
                     iter = 10000,
                     cores = 4,
                     control = list(adapt_delta = 0.9),
                     save_pars = save_pars(all = TRUE),
                     data = subset(eeg, set_id == k),
                     sample_prior="yes")
  save(m_wo_test, file = paste0("kfold_model_fits/Fit_wo",as.character(k),"_test.Rda"))
}



df.kfold.estimates <- data.frame(matrix(ncol=13,nrow=0))
colnames(df.kfold.estimates) <- c("set_id", "syn.train", "syn.train.SE", "sem.train","sem.train.SE","synsem.train","synsem.train.SE",
                                  "syn.test", "syn.test.SE", "sem.test","sem.test.SE","synsem.test","synsem.test.SE")

# load k-fold model fits
for(k in 1:10){
  load(paste0("kfold_model_fits/Fit_wo",as.character(k),"_training.Rda"))
  load(paste0("kfold_model_fits/Fit_wo",as.character(k),"_test.Rda"))
  
  # extract N400 estimates for each fold of training and test data
  
  # training estimates 
  mean.syn.train <- round(posterior_summary(m_wo_train, variable = "b_syn"),2)[1]
  SE.syn.train <- round(posterior_summary(m_wo_train, variable = "b_syn"),2)[2]
  
  mean.sem.train <- round(posterior_summary(m_wo_train, variable = "b_sem"),2)[1]
  SE.sem.train <- round(posterior_summary(m_wo_train, variable = "b_sem"),2)[2]
  
  mean.interact.train <- round(posterior_summary(m_wo_train, variable = "b_syn:sem"),2)[1]
  SE.interact.train <- round(posterior_summary(m_wo_train, variable = "b_syn:sem"),2)[2]
  
  # test estimates 
  mean.syn.test <- round(posterior_summary(m_wo_test, variable = "b_syn"),2)[1]
  SE.syn.test <- round(posterior_summary(m_wo_test, variable = "b_syn"),2)[2]
  
  mean.sem.test <- round(posterior_summary(m_wo_test, variable = "b_sem"),2)[1]
  SE.sem.test <- round(posterior_summary(m_wo_test, variable = "b_sem"),2)[2]
  
  mean.interact.test <- round(posterior_summary(m_wo_test, variable = "b_syn:sem"),2)[1]
  SE.interact.test <- round(posterior_summary(m_wo_test, variable = "b_syn:sem"),2)[2]
  
  # Store them together
  df.kfold.estimates[nrow(df.kfold.estimates)+1,] <- c(k,mean.syn.train,SE.syn.train,mean.sem.train,SE.sem.train,mean.interact.train,SE.interact.train,
                          mean.syn.test,SE.syn.test,mean.sem.test,SE.sem.test,mean.interact.test,SE.interact.test)
}

write.csv(df.kfold.estimates, file="kfold_N400_estimates.csv")
