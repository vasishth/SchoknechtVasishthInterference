library(tidyr)
library(dplyr)

# Original code by Felix Engelmann (https://github.com/felixengelmann/inter-act)

###############################################################
## Global parameters
###############################################################

reset_params <- function(){
  lf        <<- 0.15       # latency factor
  le        <<- 1       # latency exponent
  rth 	    <<- -1.5    # retrieval threshold
  bll       <<- 0.5     # decay parameter
  ans       <<- 0.2    # activation noise
  mas       <<- 1       # maximum associative strength 
  mp        <<- 1       # mismatch penalty
  ga        <<- 1       # goal source activation
  rand_time <<- 3       # latency variability
  blc       <<- 0       # base-level activation
  ##
  lp        <<- 1     # default time since last presentation (msec)
  ldp       <<- 1       # last distractor presentation (msec)
  ## Distractor control:
  ndistr    <<- 1				# number of distractors
  dbl       <<- 0       # distractor base-level
  ##
  ## Cue weighting, cue confusion and prominence:
  cueweighting <<- 1     # Strength of structural cue as ratio str/sem
  normalizeWeights <<- TRUE 
  qcf 		  <<- 1 			# match quality correction factor
  qco 		  <<- -2*rth 			# match quality correction offset
  psc       <<- 1       # prominence scaling constant C1
  pic       <<- 0       # prominence scaling constant C2
  tprom     <<- 0       # target prominence 
  dprom     <<- 0       # distractor prominence
  # cl 				<<- 0 			# cue confusion level (0-100)
  cuesim    <<- -1    # cue-feature similarity [-1..0]
  #
  VERBOSE <<- TRUE
}
reset_params()

idnames <- c("Set","Iteration","Condition","Target","Distractor")
actrnames <- c("weights","bl1","bl2","times1","times2","noise1","noise2","blact1","blact2","act1","act2","activation","latency","retrieved","acc","miss","fail")
paramnames <- c("lf","le","rth","bll","ans","mas","mp","ga","rand_time","lp","blc","ldp","dbl","ndistr","cueweighting","psc","pic","qcf", "qco" ,"cuesim","tprom","dprom")

set_prameters <- function(){
	parameters <<- list(lf,le,rth,bll,ans,mas,mp,ga,rand_time,lp,blc,ldp,dbl,ndistr,cueweighting,psc,pic,qcf,qco,cuesim,tprom,dprom
		)
	names(parameters) <<- paramnames
}
set_prameters()

# This function takes the output of run() as input and outputs condition means
compute_cond_means <- function(results){
	params <- results %>% select(Set:weights, -Iteration, -latency) %>% distinct()
	condMeans <- results %>% group_by(Set, Condition, Target, Distractor) %>% summarise(Latency = mean(latency), SE=sd(latency, na.rm=TRUE)/sqrt(n()), Acc=mean(acc), Miss=mean(miss), Fail=mean(fail)) %>% ungroup()
	left_join(condMeans, params)
}

###############################################################
## RUN MODEL
###############################################################

##
## PARAMETER MATRIX
##

## This function sets up the data frame for the simulations using the list argument (model) passed from the Rmd file
create_param_matrix <- function(model, iterations=1000){
  if(VERBOSE) print("Creating parameter matrix...")
	#
	cl <<- (cuesim+1)*100
	set_prameters()
	#
	n_params <- length(parameters);
	#
	## The total number of combinations is the product of the number of values
	## for each parameter
	n_sets <- prod(unlist(lapply(parameters, length)))
	n_cond <- length(model$target_match)
	total <- iterations*n_sets*n_cond
	print(total)
	#
	if(VERBOSE) print(paste("Conditions: ",n_cond))
	if(VERBOSE) print(paste("Combinations: ",n_sets))
	if(VERBOSE) print(paste("Total runs: ",total))
	#
	## Set up matrix of parameter combinations.  Rows are model experiments,
	## columns are parameters.
	param_combs <- matrix(nrow=n_sets, ncol=n_params);
	#
	cumulative_num_combs <- 1;
	for (p in 1:n_params) {
		param_combs[,p] <- rep(parameters[p][[1]], each=cumulative_num_combs, length.out=n_sets);
		cumulative_num_combs <- cumulative_num_combs * length(parameters[p][[1]]);
	}
	#
	param_matrix <- matrix(data=t(param_combs), nrow=total,ncol=n_params, byrow=TRUE);
 # 
	condnames <- 1:length(model$target_match)
	header <- c(idnames,paramnames,actrnames)
#
	id_matrix <- matrix(nrow=total, ncol=length(idnames))
	actr_matrix <- matrix(nrow=total, ncol=length(actrnames))
	d <- data.frame(cbind(id_matrix,param_matrix,actr_matrix))
	colnames(d) <- header
	d$Set <- 1:n_sets
	d$Condition <- rep(condnames, each=n_sets)
	d$Target <- rep(model$Target, each=n_sets)
	d$Distractor <- rep(model$Distractor, each=n_sets)
	d$match1 <- rep(model$target_match, each=n_sets)
	d$match2 <- rep(model$distractor_match, each=n_sets)
	d$Iteration <- rep(1:iterations, each=n_sets*n_cond)
	d$weights <- model$weights
	return(d)
}

## The run() function takes a parameter matrix created by create_param_matrix() as input and outputs the simulation results
run <- function(d){
	if(VERBOSE) print("Computing base-levels...")
	total <- nrow(d) # Number of samples
	d$times1 <- d$lp # Time elapsed since target was last seen
	d$times2 <- d$ldp # Time elapsed since distractor was last seen
	d$bl1 <- d$blc # Base activation of target
	d$bl2 <- d$dbl # Base activation of distractor
	d$cl <- (cuesim+1)*100 # Cue similarity
	# Sample noise values based on ANS parameter
	d$noise1 <- act_r_noise_n(total, d$ans)
	d$noise2 <- act_r_noise_n(total, d$ans)
	# Compute activations for target and distractor
	d$blact1 <- base_act(prom=d$tprom, times=d$times1, C1=d$pic, C2=d$psc, b=d$bl1,  noise=d$noise1, dec=d$bll)# Target
	d$blact2 <- base_act(prom=d$dprom, times=d$times2, C1=d$pic, C2=d$psc, b=d$bl2,  noise=d$noise2, dec=d$bll) # Distractor
	## Cue match 
	match1 <- matrix(unlist(d$match1),nrow=total,ncol=length(d$match1[[1]]),byrow=TRUE)
	match2 <- matrix(unlist(d$match2),nrow=total,ncol=length(d$match2[[1]]),byrow=TRUE)
	weights <- matrix(unlist(d$weights),nrow=total,ncol=1,byrow=TRUE)
	## Match quality (including cue confusion and scaled by base-level activation)
	cueconf1 <- match1-((match1-1)*(1+d$cuesim))
	cueconf1[cueconf1==0] <- 1
	cueconf2 <- match2-((match2-1)*(1+d$cuesim))
	cueconf2[cueconf2==0] <- 1
	if(VERBOSE) print("Computing match quality...")
	Qj1 <- match_quality(match=match1, sim=d$cuesim, blact=d$blact1, tau=d$rth, s=d$ans, q=d$qcf, q2=d$qco) # Target match quality
	Qj2 <- match_quality(match=match2, sim=d$cuesim, blact=d$blact2, tau=d$rth, s=d$ans, q=d$qcf, q2=d$qco) # Distractor match quality
	# 
	## Probability of item given cue 
	if(VERBOSE) print("Computing probability P(i|j)...")
	P1j <- Qj1 / (Qj1+(Qj2*d$ndistr) +0.0001) # Proportion of activation going to target
	P2j <- Qj2 / (Qj1+(Qj2*d$ndistr) +0.0001) # Proportion of activation going to distractor
	# 
	## Spreading activation
	if(VERBOSE) print("Computing spreading activation...")
	Sj1 <- spreading_act(Pij=P1j, S=d$mas) # Amount of activation spreading to target
  Sj2 <- spreading_act(Pij=P2j, S=d$mas) # Amount of activation spreading to distractor
	d$Sji_neg <- rowMeans(ifelse(Sj1 < 0 | Sj2 < 0, T, F)) # Is the spreading activation negative? (It shouldn't be)
	# 
	## Activations
	if(VERBOSE) print("Computing activations...")
  d$act1 <- d$blact1 + weight_spreading_act(Sji=Sj1, weights=weights, W=d$ga) + mismatch_penalty(cuematch=match1, P=d$mp) + d$noise1 # Target activation
  d$act2 <- d$blact2 + weight_spreading_act(Sji=Sj2, weights=weights, W=d$ga) + mismatch_penalty(cuematch=match2, P=d$mp) + d$noise2 # Distractor activation
  
	##
	## FINAL VALUES
	##
  
	if(VERBOSE) print("Computing latencies...")
	d$activation <- ifelse(d$act1>d$act2, d$act1, d$act2) # Activation of retrieved chunk
	retrieved <- ifelse(d$act1>d$act2, 1, 2) # Which chunk was retrieved?
	d$retrieved <- ifelse(d$activation>d$rth, retrieved, 0) # Did chunk pass the retrieval threshold?
	d$latency <- latency(d$activation, F=d$lf, f=d$le, tau=d$rth) # Latency of the retrieval
	d$acc <- ifelse(d$retrieved==1, 1, 0) # Was the target correctly retrieved?
	d$miss <- ifelse(d$retrieved==2, 1, 0) # Was the distractor retrieved?
	d$fail <- ifelse(d$retrieved==0, 1, 0) # Did the retrieval fail completely?
	#
	if(VERBOSE) print("FINISHED")
	#
	return(tibble::as.tibble(d))
}

###############################################################
## ACT-R
###############################################################

# Computes spreading activation for given chunk based on MAS and probability P(i|j) passed by run()
spreading_act <- function(Pij=matrix(c(1,1),1,2,T), S=mas){
	Sji <- S+log(Pij)
  Sji <- ifelse(Sji==-Inf | is.na(Sji), 0, Sji)
  ifelse(Sji < 0, message("!!! WARNING: Sji < 0 !!!"), T)
  Sji <- ifelse(Sji < 0, 0, Sji)
  Sji
}

# Applies cue weights to spreading activation
weight_spreading_act <- function(Sji=matrix(c(0),1,3,T), weights=matrix(c(1),1,3,T), W=ga){
  Wkj <- matrix(unlist(c(1)),nrow=nrow(Sji),ncol=1,byrow=TRUE)
  rowSums(Wkj*Sji)
}

# Applies penalty if cue doesn't match
mismatch_penalty <- function(cuematch=matrix(c(1,1),1,2,T), P=mp){
	penalty <- cuematch-1
  Pi <- rowSums(P*penalty)
  Pi
}

# Base activation based on equation 1 in Lewis & Vasishth (2005)
base_act <- function(prom=0, times=lp, C1=pic, C2=psc, b=blc, noise=act_r_noise(ans), dec=bll){
  log(times^(-dec)) + b + (C2*(prom+C1))
}

# Computes match quality
match_quality <- function(match=matrix(c(1,1,1),nrow=1), sim=cuesim, blact=0, tau=rth, s=ans, q=qcf, q2=qco){
	qcorr <- 1/(1+q*exp(-(blact-tau-q2)))
	(match) * qcorr
}

# Computes latency based on equation 4 in Lewis & Vasishth (2005)
latency <- function(A, F=lf, f=le, tau=rth){
  t <- ifelse(A>=tau, F*exp(-f*A)*1000, F*exp(-f*tau)*1000)
  round(randomize_time(t))
}

# Generates n noise values
act_r_noise_n <- function(n, s=ans){
  var <- pi^2/3*s^2
  rnorm(n, 0, sqrt(var))
}

# Generates one noise value
act_r_noise <- function(s=ans){
  var <- pi^2/3*s^2
  rnorm(1, 0, sqrt(var))
}

# Random latency component
randomize_time <- function(time, n=rand_time){
  if(n>0) runif(length(time), time*(n-1)/n, time*(n+1)/n) else time
}