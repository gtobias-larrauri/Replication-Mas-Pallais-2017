#For the observational thing
library(data.table) 
library(ranger)
library(DoubleML)
library(mlr3learners)
library(mlr3)
############################### SET WORKING DIRECTORY TO THE REPLICATION FOLDER:

setwd("")

######################################################################
data <- read.csv("observational.csv")
#Drop people who don't have earnings and also weight variable.
data <- subset(data, select = -c(earnwt))
data <- data[complete.cases(data), ] #Keep complete cases

#Collapse if logs.
  #data$earnweek <-log(data$earnweek)

#Prep for doubleML
data<-setDT(data)
covariates<-names(data)
#Different data back ends for different treatments. 
data_ml <-function(treatment){
  names_to_exclude <- c(treatment,"earnweek")  # Replace ... with the names you want to exclude
  covariates <- make.names(covariates[!(covariates %in% names_to_exclude)])
  
  data_dml_base <- DoubleMLData$new(data,
                                    y_col = "earnweek",
                                    d_cols = treatment,
                                    x_cols = covariates)
  return(data_dml_base)
}
flexhrs <-data_ml("flexhrs")
workhome_dum <-data_ml("workhome_dum")
workhome_paid <-data_ml("workhome_paid")
irreg <-data_ml("irreg")
irreg_const <-data_ml("irreg_const")
irreg_inconst <-data_ml("irreg_inconst")
all <-c(flexhrs,workhome_dum,workhome_paid,irreg,irreg_const,irreg_inconst)

#Random forests because I didn't augment my dataset further, 
#guidelines from DoubleML examples
randomForest <- lrn("regr.ranger", max.depth = 7,
                   mtry = 3, min.node.size = 3)
randomForest_class <- lrn("classif.ranger", max.depth = 5,
                         mtry = 4, min.node.size = 7)

set.seed(123)
result <-c()
for (i in 1:length(all)){
  dml_plr_forest <- DoubleMLPLR$new(all[[i]],
                                    ml_l = randomForest,
                                    ml_m = randomForest_class,
                                    n_folds = 3)
  dml_plr_forest$fit()
  a<-dml_plr_forest$summary()
  result <-rbind(result,a)
}
result
obs1 <- xtable(result)
print(obs1)

#Loop over different treatment specifications
result2 <-c()
for (i in 1:length(all)){
  
dml_irm_forest = DoubleMLIRM$new(all[[i]],
                                 ml_g = randomForest,
                                 ml_m = randomForest_class,
                                 trimming_threshold = 0.01,
                                 n_folds = 3)
dml_irm_forest$fit()
b<- dml_irm_forest$summary()
result2 <-rbind(result2,b)
}
result2
obs2 <- xtable(result2)
print(obs2)

#A failed Lasso.
# lasso_learner <- lrn("regr.cv_glmnet", s = "lambda.min",nfolds=5)
# lasso_class = lrn("classif.cv_glmnet", nfolds = 5, s = "lambda.min")
# for (i in 1:length(all)){
#   doubleml_bonus = DoubleMLPLR$new(all[[i]],
#                                    ml_l =lasso_learner ,
#                                    ml_m =lasso_class ,
#                                    n_folds = 5)
#   
#   doubleml_bonus$fit()
#   doubleml_bonus$summary()
# }

