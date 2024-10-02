
#ALPHA FUNCTION:

#Error estimation, inattentiveness.
#Error is estimated differently by subgroup. lm (choice on wage diff)
#Wage gap in code seems to be defined as -deltaW from the paper... 

estimate_error <- function(df,estimation_type=0){
  #ESTIMATION 0 IS HOW THEY DO IT IN THE PAPER.
  if (estimation_type==0) {
    dataset <- df %>%
      group_by(wagegap) %>%
      summarize(
        chose_position1 = mean(chose_position1)
      )
    #This just collapses the data into means for every wage gap value.
    first_stage <- lm(chose_position1~wagegap,data=dataset[dataset$wagegap<=-2,])
    if (first_stage$coef[2]>0){
      data<-dataset[dataset$wagegap<=-2,]
      
      return(1-mean(data$chose_position1)) #We will be here for cases 4 and 5.
      #It doesn't make any sense that 4== combined flex is not viewed as an amenity.
      #It is viewed as an amenity but the curve gets kind of flat in -2 to -5 range,
      #that's why the coefficient is negative.
      # Why not do this for all? Or for the individual data???
    }
    else if (first_stage$coef[2]<=0){
      est_alpha<- 1 - (first_stage$coef[1]-5*first_stage$coef[2])
      return(est_alpha)
    }
  }
  #EXTENSIONS OF ALPHA:
  #PEOPLE WITH WAGE GAP -2 AND UNDER, so should pick the value of the amenity and I take the mean.
  #Now we use same method for all.  
  if (estimation_type==1){ 

    data<-df[df$wagegap<=-2,]
    return(1-mean(data$chose_position1)) 
    
  }
  if (estimation_type==2){ 
    return(0) 
    
  }
  #IDENTICAL PROCEDURE TO 1, for the rest of the code this means that alpha constructed
  # on different sample.
  if (estimation_type==3){ 
    
    data<-df[df$wagegap<=-2,]
    return(1-mean(data$chose_position1)) 
    
  }
}

#Likelihood functions:

invlogit <-function(beta,wagegap){
  invdensity<- exp(beta[1]+beta[2]*wagegap)/(1+exp(beta[1]+beta[2]*wagegap))
  return(invdensity)
}
#CDF with inattentiveness correction:
i_log <-function(beta,choice,wagegap,alpha){
  if (choice==1){
    return(log(invlogit(beta,wagegap)*(1-alpha)+(1-invlogit(beta,wagegap))*alpha))
    #ln(invlogit(`Xb')*(1-error) + (1-invlogit(`Xb'))*(error))
  }
  else if (choice==0){
    return(log(invlogit(beta,wagegap)*alpha+(1-invlogit(beta,wagegap))*(1-alpha)))
    #ln(invlogit(`Xb')*(error) + (1-invlogit(`Xb'))*(1-error))   if $ML_y1==0
    # (1-alpha)
  }
}

fullikelihood <- function(beta,data,alpha){
  likeli <-c()
  for (i in 1:nrow(data)){
    obs_likelihood <- i_log(beta,data$chose_position1[i],data$wagegap[i],alpha)
    likeli <- c(likeli, obs_likelihood)
    
  }
  #check that this works It doesn't it still collapses.
  #Just some issues with the minimization software... On some rare occasions it goes crazy.
  if (is.infinite(-sum(likeli))) {
    return(-100000)
  }
  else{
    return (-sum(likeli))
    
  }
}

#MAIN TABLE:

table5 <-function(all_data,B=500,alpha_mode=0,max_iter=50,sd_or_conf=0,...){
  results = c()
  for (i in 1:5){
    initial_parameters <- c(0,0)  # Initial guess for parameters
    if (alpha_mode==3){
      alpha_estimated<-estimate_error(...,alpha_mode)
      #The ... will be the full dataset which will be used to estimate the alphas
      # depending on the specification.
    }
    else{
      alpha_estimated<-estimate_error(all_data[[i]],alpha_mode)
    }
    result <- optim(par = initial_parameters, fn = fullikelihood,data=all_data[[i]],alpha=alpha_estimated ,method = "L-BFGS-B")
    mean <- -result$par[1]/result$par[2] 
    sd  <- -1/(result$par[2]*0.5516)
    q25 <- (log((0.75/0.25)) -result$par[1])/result$par[2]
    q50 <- (log((0.5/0.5)) -result$par[1])/result$par[2]
    q75 <- (log((0.25/0.75)) -result$par[1])/result$par[2]
    results_diff_modalities <-round(c(mean,sd,q25,q50,q75),digits = 2)
    results <- rbind(results, results_diff_modalities)
    sd<-boot_f(all_data[[i]],500,max_iter,alpha_mode,sd_or_conf,...)
    results <-rbind(results,sd)
  }
  return(results)
}

#BOOTSTRAP OF MAIN TABLE: 

boot_f<-function(data,n_boot=500,max_iter=100,alpha_mode,sd_or_conf=0,...){ 
  b_results = matrix(, nrow = n_boot, ncol = 5)
  fail <-0
  for (b_s in 1:n_boot){
    bflex <- data[sample(nrow(data),nrow(data),replace = TRUE), ]
    if (alpha_mode==3){#Again use all data for the alpha estimation.
      b_whole_data <- ...[sample(nrow(...),nrow(...),replace = TRUE), ]
      alpha <- estimate_error(b_whole_data,alpha_mode)
    }
    else{
      alpha <- estimate_error(bflex,alpha_mode)
    }
    if(alpha<0){
      alpha <-0
    }
    #Alpha can bring problems... My function?
    #Do a bs with estimated alpha then change...
    result <- optim(par = c(0,0), fn = fullikelihood,data=bflex,alpha=alpha ,method = "L-BFGS-B",control=list(maxit=max_iter))
    if (result$convergence == 0) {
      mean <- -result$par[1]/result$par[2] 
      sd  <- -1/(result$par[2]*0.5516)
      q25 <- (log((0.75/0.25)) -result$par[1])/result$par[2]
      q50 <- (log((0.5/0.5)) -result$par[1])/result$par[2]
      q75 <- (log((0.25/0.75)) -result$par[1])/result$par[2]
      b_results[b_s,1] <- mean
      b_results[b_s,2] <- sd
      b_results[b_s,3] <- q25
      b_results[b_s,4] <- q50
      b_results[b_s,5] <- q75
    } 
    else {
      fail <-fail+1
    }  
    
  }
  if (sd_or_conf==0){
    results <-c(sd(b_results[,1],na.rm = TRUE),
                sd(b_results[,2],na.rm = TRUE),
                sd(b_results[,3],na.rm = TRUE),
                sd(b_results[,4],na.rm = TRUE),
                sd(b_results[,5],na.rm = TRUE))
  }
  else if (sd_or_conf==1){#For 95 conf bootstrap intervals
    bresults <-data.frame((b_results[,1]),
                         (b_results[,2]),
                         (b_results[,3]),
                         (b_results[,4]),
                         (b_results[,5]))
    results <-c()
    for (column in 1:ncol(bresults)){
    up <- round(quantile(bresults[,column] ,0.975,na.rm = TRUE),digits = 2)
    down <-round(quantile(bresults[,column] ,0.025,na.rm = TRUE),digits = 2)
    conf_int <- paste(down, up, sep = "-")
    results <-c(results,conf_int)
    }
    
  }
  
  print(fail)
  return(results)
}
## DIFFERENT ALPHA FOR DIFFERENT WAGE GAP CODED SEPARATELY BECAUSE I DON'T HAVE 
# MUCH TIME AND DON'T WANT TO MAKE A MESS OF THE THINGS THAT WORK.
#Step by step
#Non stochastic for now, linear function
alpha_i <-function(max,min,deltawage){
  beta <-(min-max)/5
  return(max+beta*abs(deltawage)) #
}
fullikelihood_mod <- function(beta,data,max,min){
  likeli <-c()
  for (i in 1:nrow(data)){
    obs_likelihood <- i_log(beta,data$chose_position1[i],data$wagegap[i],alpha_i(max,min,data$wagegap[i]))
    likeli <- c(likeli, obs_likelihood)
    
  }
  #check that this works It doesn't it still collapses.
  #Just some issues with the minimization software... On some rare occasions it goes crazy.
  if (is.infinite(-sum(likeli))) {
    return(-100000)
  }
  else{
    return (-sum(likeli))
    
  }
}
table5_mod <-function(all_data,B=500,max_iter=50,...){
  extra_params <- list(...)
  results = c()
  for (i in 1:5){
    initial_parameters <- c(0,0)  # Initial guess for parameters
    result <- optim(par = initial_parameters, fn = fullikelihood_mod,data=all_data[[i]],max=extra_params[[1]],min=extra_params[[2]] ,method = "L-BFGS-B")
    mean <- -result$par[1]/result$par[2] 
    sd  <- -1/(result$par[2]*0.5516)
    q25 <- (log((0.75/0.25)) -result$par[1])/result$par[2]
    q50 <- (log((0.5/0.5)) -result$par[1])/result$par[2]
    q75 <- (log((0.25/0.75)) -result$par[1])/result$par[2]
    results_diff_modalities <-round(c(mean,sd,q25,q50,q75),digits = 2)
    results <- rbind(results, results_diff_modalities)
    sd<-boot_f_mod(all_data[[i]],500,max_iter,extra_params)#Will need to change this if diff alphas!
    results <-rbind(results,sd)
    #I can just put a if to check how the standard errors/estimates change.
  }
  return(results)
}
boot_f_mod<-function(data,n_boot=500,max_iter=100,extra){ #REMAKE THIS FUNCTION... ALPHA ESTIMATED INSIDE ONLY...
  
  b_results = matrix(, nrow = n_boot, ncol = 5)
  fail <-0
  for (b_s in 1:n_boot){
    bflex <- data[sample(nrow(data),nrow(data),replace = TRUE), ]
    result <- optim(par = c(0,0), fn = fullikelihood_mod,data=bflex,max=extra[[1]],min=extra[[2]] ,method = "L-BFGS-B",control=list(maxit=max_iter))
    if (result$convergence == 0) {
      mean <- -result$par[1]/result$par[2] 
      sd  <- -1/(result$par[2]*0.5516)
      q25 <- (log((0.75/0.25)) -result$par[1])/result$par[2]
      q50 <- (log((0.5/0.5)) -result$par[1])/result$par[2]
      q75 <- (log((0.25/0.75)) -result$par[1])/result$par[2]
      b_results[b_s,1] <- mean
      b_results[b_s,2] <- sd
      b_results[b_s,3] <- q25
      b_results[b_s,4] <- q50
      b_results[b_s,5] <- q75
    } 
    else {
      fail <-fail+1
    }  
    
  }
  results <-  c(sd(b_results[,1],na.rm = TRUE),
                sd(b_results[,2],na.rm = TRUE),
                sd(b_results[,3],na.rm = TRUE),
                sd(b_results[,4],na.rm = TRUE),
                sd(b_results[,5],na.rm = TRUE))
  print(fail)
  return(results)
}


#My weird bootstrap simulation: Not used not completed.
simulate <-function(data,alpha){
  binomial_draws <-rbinom(nrow(data), size = 1, prob = alpha)
  #alpha vs 2*alpha and then split shouldn't make a big difference
  for (i in 1:nrow(data)){
    if (binomial_draws[i]==1){
      #Half of the innatentive will have made the wrong choice.
      if (data$chose_position[i]==0){
        data$chose_position1[i]<-1
      }
      else{
        data$chose_position1[i]<-0
        
      }  
      
    }
  }
  return(data)
}

#BREAKPOINT FUNCTION: Not completed not used.
breakpoint <-function(df,error_type=0){
  alpha <-estimate_error(df,error_type)
  dataset <- df %>%
  group_by(wagegap) %>%
  summarize(
  chose_position1 = mean(chose_position1)
  )
  dataset["mod_shares"]=(dataset$chose_position1-alpha)/(1-2*alpha)
  return(dataset)
}



#GRAPHING FUNCTION: should return the plot, as I want to put them together.
#Also, modify the legend... And rename axis.
graph <-function(df,title){
  alfa <-estimate_error(df)
  #Collapse data
  collapsed <- df %>%
    group_by(wagegap) %>%
    summarize(
      chose_position1 = mean(chose_position1)
    )
  #Result from non collapsed data.
  result <- optim(par = c(0,0), fn = fullikelihood,data=df,alpha=alfa ,method = "L-BFGS-B")
  result2 <- optim(par = c(0,0), fn = fullikelihood,data=df,alpha=0 ,method = "L-BFGS-B")
  #Acabar de modificar esta función.
  my_vector <- seq(-5, 5, by = 0.01)
  test <-c()
  test2 <-c()
  #Obtain values of function to plot, I don't know broadcasting in R
  for (num in my_vector){
    test_givenbeta <- invlogit(c(result$par[1],result$par[2]),-num)
    test_givenbeta2 <- invlogit(c(result2$par[1],result2$par[2]),-num)
    test <-c(test,test_givenbeta)
    test2 <-c(test2,test_givenbeta2)
  }
  plot(my_vector,test,type = "l",ylim = c(-0.2, 1.2),
       xlab = "Wage differential in dollars", 
       ylab = "CDF of Willingness To Pay",
       main = title
       )
  
  #Add shares of people by wage gap, unmodified==RED and modified==ORANGE
  lines(my_vector, test2, col = "green", pch = 16)
  points(-collapsed$wagegap, collapsed$chose_position1, col = "red", pch = 16)
  points(-collapsed$wagegap, (collapsed$chose_position1-alfa)/(1-2*alfa), col = "orange", pch = 16)
  
  legend("bottomright",
         legend = c("MLE corrected for innatention","MLE without correction","Shares uncorrected","Shares corrected"),
         col = c("black","green","red","orange"),
         lwd = 2,
         pch = c(1, 16),
        )#Need to modify the legend
  
  #I can return the plot and rearrange.
}


#MISCELLANEOUS:
alpha_review <-function(df,alpha_type=0){
  n_boot <-500
  alphas <-c()
  for (b_s in 1:n_boot){
    bflex <- df[sample(nrow(df),nrow(df),replace = TRUE), ]
    alpha <- estimate_error(bflex,alpha_type)
    alphas <- c(alphas,alpha)
  }
  hist(alphas)
  #return(alphas)
}





