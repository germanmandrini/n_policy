setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents') #CPSC
# setwd("/home/germanm2")
# setwd('~')
source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')


library(tidyverse) # data manipulation
library(mlr)       # ML package (also some data manipulation)
library(knitr)     # just using this for kable() to make pretty tables
library(caret)

reg_model_stuff <- readRDS( "./vr_value_v2/Data/files_rds/reg_model_stuff.rds")

# ValidSet2 <- reg_model_stuff[['ValidSet']]
# TrainSet2 <- reg_model_stuff[['TrainSet']]

# no_cost_varb <- reg_model_stuff[['no_cost_var']] 
no_cost_varb_trf <- reg_model_stuff[['no_cost_varb_trf']]
ss_varb <- reg_model_stuff[['ss_var']] 
crop_varb <- reg_model_stuff[['crop_varb']] 

preprocessParams <- reg_model_stuff[['preprocessParams']]

TrainSet_eonr3 <- reg_model_stuff[['TrainSet_eonr3']]
ValidSet_eonr3 <- reg_model_stuff[['ValidSet_eonr3']]

#  ================ MODEL 1 =============================================
# Train a XGBOOST model 1 using MLR -------
#https://www.kaggle.com/dkyleward/xgboost-using-mlr-package-r


# Create the tasks encapsulate the data set and further relevant information
trainTask <- makeRegrTask(data = TrainSet_eonr3[,c("eonr", no_cost_varb_trf), with = FALSE], target = "eonr")
testTask <- makeRegrTask(data = ValidSet_eonr3[,c("eonr", no_cost_varb_trf), with = FALSE], target = "eonr")

# 4 Hyper-parameter -------------
# Tuning http://rstudio-pubs-static.s3.amazonaws.com/336732_52d1b0e682634b5eae42cf86e1fc2a98.html

# To see all the parameters of the xgboost classifier
getParamSet("regr.xgboost")           

# ## Tune hyperparameters
xgb_params <- makeParamSet(
  # The number of trees in the model (each one built sequentially)
  makeIntegerParam("nrounds", lower = 100, upper = 200),
  # number of splits in each tree
  makeIntegerParam("max_depth", lower = 1, upper = 10),
  # "shrinkage" - prevents overfitting
  makeNumericParam("eta", lower = .1, upper = .5),
  makeNumericParam("gamma", lower = 20, upper = 20000),
  # L2 regularization - prevents overfitting
  # makeNumericParam("lambda", lower = -1, upper = 0, trafo = function(x) 10^x)
  makeNumericParam("lambda", lower = 0, upper = 10)
)

xgb_learner <- makeLearner(cl="regr.xgboost",
                           predict.type = "response",
                           par.vals = list(eval_metric = "error",
                                           colsample_bytree=c(0.7),
                                           min_child_weight=2,
                                           subsample = 0.5))

# define how we will search (random, grid, etc.). We will do random search
control <- makeTuneControlRandom(maxit = 300)

# Create a description of the resampling plan
resample_desc <- makeResampleDesc("CV", iters = 4)

# we perform the tuning.

library("parallelMap")

parallelStartSocket(7)
tuned_params <- tuneParams(
  learner = xgb_learner,
  task = trainTask,
  resampling = resample_desc,
  par.set = xgb_params,
  control = control
)
parallelStop()


# Create a new model using tuned hyperparameters
xgb_tuned_learner <- setHyperPars(
  learner = xgb_learner,
  par.vals = tuned_params$x
)

# Re-train parameters using tuned hyperparameters (and full training set)
xgb_1 <- mlr::train(xgb_tuned_learner, trainTask)

# Make predictions
preds <- predict(xgb_1, trainTask)
TrainSet_eonr3$eonr_pred <-  preds$data$response
preds <- predict(xgb_1, testTask)
ValidSet_eonr3$eonr_pred <-  preds$data$response

# Model performance metrics Training Set &  Validation set
data.frame(
  RMSE_train = caret::RMSE(TrainSet_eonr3$eonr_pred, TrainSet_eonr3$eonr),
  Rsquare_train = caret::R2(TrainSet_eonr3$eonr_pred, TrainSet_eonr3$eonr),
  RMSE_valid = caret::RMSE(ValidSet_eonr3$eonr_pred, ValidSet_eonr3$eonr),
  Rsquare_valid = caret::R2(ValidSet_eonr3$eonr_pred, ValidSet_eonr3$eonr)
)

ggplot(TrainSet_eonr3, aes(x = eonr, y = eonr_pred))+
  geom_point()

ggplot()+
  geom_hex(data = TrainSet_eonr3, aes(x = eonr, y = eonr_pred), shape = 1, color = 'blue')+
  geom_hex(data = ValidSet_eonr3, aes(x = eonr, y = eonr_pred), shape = 2, color = 'green')  +
  coord_fixed() + geom_abline() + ylim(0, 350)+ xlim(0, 350) +
  theme(aspect.ratio=1, 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_bw()






#  ================ MODEL 2 =============================================

# Train a XGBOOST model 2 using MLR 
#https://www.kaggle.com/dkyleward/xgboost-using-mlr-package-r


# Create the tasks encapsulate the data set and further relevant information
trainTask <- makeRegrTask(data = TrainSet_eonr3[,c("eonr", no_cost_varb_trf, ss_varb), with = FALSE], target = "eonr")
testTask <- makeRegrTask(data = ValidSet_eonr3[,c("eonr", no_cost_varb_trf, ss_varb), with = FALSE], target = "eonr")

# 4 Hyper-parameter -------------
# Tuning http://rstudio-pubs-static.s3.amazonaws.com/336732_52d1b0e682634b5eae42cf86e1fc2a98.html

# To see all the parameters of the xgboost classifier
getParamSet("regr.xgboost")           

# ## Tune hyperparameters
xgb_params <- makeParamSet(
  # The number of trees in the model (each one built sequentially)
  makeIntegerParam("nrounds", lower = 100, upper = 200),
  # number of splits in each tree
  makeIntegerParam("max_depth", lower = 1, upper = 10),
  # "shrinkage" - prevents overfitting
  makeNumericParam("eta", lower = .1, upper = .5),
  makeNumericParam("gamma", lower = 20, upper = 20000),
  # L2 regularization - prevents overfitting
  # makeNumericParam("lambda", lower = -1, upper = 0, trafo = function(x) 10^x)
  makeNumericParam("lambda", lower = 0, upper = 10)
)

xgb_learner <- makeLearner(cl="regr.xgboost",
                           predict.type = "response",
                           par.vals = list(eval_metric = "error",
                                           colsample_bytree=c(0.7),
                                           min_child_weight=2,
                                           subsample = 0.5))

# define how we will search (random, grid, etc.). We will do random search
control <- makeTuneControlRandom(maxit = 300)

# Create a description of the resampling plan
resample_desc <- makeResampleDesc("CV", iters = 4)

# we perform the tuning.

library("parallelMap")

parallelStartSocket(7)
tuned_params <- tuneParams(
  learner = xgb_learner,
  task = trainTask,
  resampling = resample_desc,
  par.set = xgb_params,
  control = control
)
parallelStop()


# Create a new model using tuned hyperparameters
xgb_tuned_learner <- setHyperPars(
  learner = xgb_learner,
  par.vals = tuned_params$x
)

# Re-train parameters using tuned hyperparameters (and full training set)
xgb_2 <- mlr::train(xgb_tuned_learner, trainTask)

# Make predictions
preds <- predict(xgb_2, trainTask)
TrainSet_eonr3$eonr_pred <-  preds$data$response
preds <- predict(xgb_2, testTask)
ValidSet_eonr3$eonr_pred <-  preds$data$response

# Model performance metrics Training Set &  Validation set
data.frame(
  RMSE_train = caret::RMSE(TrainSet_eonr3$eonr_pred, TrainSet_eonr3$eonr),
  Rsquare_train = caret::R2(TrainSet_eonr3$eonr_pred, TrainSet_eonr3$eonr),
  RMSE_valid = caret::RMSE(ValidSet_eonr3$eonr_pred, ValidSet_eonr3$eonr),
  Rsquare_valid = caret::R2(ValidSet_eonr3$eonr_pred, ValidSet_eonr3$eonr)
)

ggplot(TrainSet_eonr3, aes(x = eonr, y = eonr_pred))+
  geom_point()

ggplot()+
  geom_hex(data = TrainSet_eonr3, aes(x = eonr, y = eonr_pred), shape = 1, color = 'blue')+
  geom_hex(data = ValidSet_eonr3, aes(x = eonr, y = eonr_pred), shape = 2, color = 'green')  +
  coord_fixed() + geom_abline() + ylim(0, 350)+ xlim(0, 350) +
  theme(aspect.ratio=1, 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_bw()


#  ================ MODEL 3 =============================================

# Train a XGBOOST model 2 using MLR 
#https://www.kaggle.com/dkyleward/xgboost-using-mlr-package-r


# Create the tasks encapsulate the data set and further relevant information
trainTask <- makeRegrTask(data = TrainSet_eonr3[,c("eonr", no_cost_varb_trf, crop_varb), with = FALSE], target = "eonr")
testTask <- makeRegrTask(data = ValidSet_eonr3[,c("eonr", no_cost_varb_trf, crop_varb), with = FALSE], target = "eonr")

# 4 Hyper-parameter -------------
# Tuning http://rstudio-pubs-static.s3.amazonaws.com/336732_52d1b0e682634b5eae42cf86e1fc2a98.html

# To see all the parameters of the xgboost classifier
getParamSet("regr.xgboost")           

# ## Tune hyperparameters
xgb_params <- makeParamSet(
  # The number of trees in the model (each one built sequentially)
  makeIntegerParam("nrounds", lower = 100, upper = 200),
  # number of splits in each tree
  makeIntegerParam("max_depth", lower = 1, upper = 10),
  # "shrinkage" - prevents overfitting
  makeNumericParam("eta", lower = .1, upper = .5),
  makeNumericParam("gamma", lower = 20, upper = 20000),
  # L2 regularization - prevents overfitting
  # makeNumericParam("lambda", lower = -1, upper = 0, trafo = function(x) 10^x)
  makeNumericParam("lambda", lower = 0, upper = 10)
)

xgb_learner <- makeLearner(cl="regr.xgboost",
                           predict.type = "response",
                           par.vals = list(eval_metric = "error",
                                           colsample_bytree=c(0.7),
                                           min_child_weight=2,
                                           subsample = 0.5))

# define how we will search (random, grid, etc.). We will do random search
control <- makeTuneControlRandom(maxit = 300)

# Create a description of the resampling plan
resample_desc <- makeResampleDesc("CV", iters = 4)

# we perform the tuning.

library("parallelMap")

parallelStartSocket(7)
tuned_params <- tuneParams(
  learner = xgb_learner,
  task = trainTask,
  resampling = resample_desc,
  par.set = xgb_params,
  control = control
)
parallelStop()


# Create a new model using tuned hyperparameters
xgb_tuned_learner <- setHyperPars(
  learner = xgb_learner,
  par.vals = tuned_params$x
)

# Re-train parameters using tuned hyperparameters (and full training set)
xgb_3 <- mlr::train(xgb_tuned_learner, trainTask)

# Make predictions
preds <- predict(xgb_2, trainTask)
TrainSet_eonr3$eonr_pred <-  preds$data$response
preds <- predict(xgb_2, testTask)
ValidSet_eonr3$eonr_pred <-  preds$data$response

# Model performance metrics Training Set &  Validation set
data.frame(
  RMSE_train = caret::RMSE(TrainSet_eonr3$eonr_pred, TrainSet_eonr3$eonr),
  Rsquare_train = caret::R2(TrainSet_eonr3$eonr_pred, TrainSet_eonr3$eonr),
  RMSE_valid = caret::RMSE(ValidSet_eonr3$eonr_pred, ValidSet_eonr3$eonr),
  Rsquare_valid = caret::R2(ValidSet_eonr3$eonr_pred, ValidSet_eonr3$eonr)
)

ggplot(TrainSet_eonr3, aes(x = eonr, y = eonr_pred))+
  geom_point()

ggplot()+
  geom_hex(data = TrainSet_eonr3, aes(x = eonr, y = eonr_pred), shape = 1, color = 'blue')+
  geom_hex(data = ValidSet_eonr3, aes(x = eonr, y = eonr_pred), shape = 2, color = 'green')  +
  coord_fixed() + geom_abline() + ylim(0, 350)+ xlim(0, 350) +
  theme(aspect.ratio=1, 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_bw()


reg_model_stuff[['xgb_1']] <-  xgb_1
reg_model_stuff[['xgb_2']] <-  xgb_2
reg_model_stuff[['xgb_3']] <-  xgb_3


saveRDS(reg_model_stuff, "./vr_value_v2/Data/files_rds/reg_model_stuff.rds")
