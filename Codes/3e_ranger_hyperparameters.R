library(ranger)
library(mlr)
library("parallelMap")
#------ TUNE HYPERPARAMETERS

## Create combined training data
valid_years <- sample(unique(training_eonr_dt$year), 5)
hyper_training_set_dt <- training_eonr_dt[!year %in% valid_years]
hyper_validation_set_dt <- training_eonr_dt[year %in% valid_years]

# train_data <- TrainSet_eonr[,c('eonr', no_cost_varb, ss_varb), with = FALSE]
# ValidSet3[, P := Yld * Pc - N_fert * Pn]
# # ValidSet3[, n_0_60cm_v5 := n_20cm_v5 + n_40cm_v5 + n_60cm_v5]
# validation_data <- ValidSet3[, .SD[ P == max( P)], by = .(id_10, mukey, z)]
# setnames(validation_data, 'N_fert', 'eonr')
# validation_data <- validation_data[,c('eonr', no_cost_varb, ss_varb), with = FALSE]
#
train_task_data <- rbind(hyper_training_set_dt, hyper_validation_set_dt)
size <- nrow(train_task_data)
train_ind <- seq_len(nrow(hyper_training_set_dt))
validation_ind <- seq.int(max(train_ind) + 1, size)


# Create training task
train_task <- makeRegrTask(data = train_task_data[,-'year'], target = "eonr") #task = data


## Tune hyperparameters
ctrl <- makeTuneControlRandom(maxit = 1000)
ps <- makeParamSet(
  makeIntegerParam("num.trees", lower=500, upper=3000),
  makeIntegerParam("min.node.size", lower=5, upper=50),
  makeIntegerParam("mtry", lower=2, upper=length(c(low_var, high_var))-5)
)


parallelStartSocket(30)

res <- tuneParams("regr.ranger",
                  task = train_task,
                  resampling = makeFixedHoldoutInstance(train_ind, validation_ind, size),
                  par.set = ps,
                  control = ctrl)


parallelStop()

# learner_rf = setHyperPars(makeLearner("regr.ranger"), par.vals = res$x)#instructions

#-----------------------------------------------------------------------------------------------------------------
# # Train a model
learner_rf = makeLearner("regr.ranger",
                         num.trees = 585,
                         min.node.size = 29,
                         mtry = 4) #instructions

regr_task = makeRegrTask(data = training_eonr_dt, target = "eonr", blocking = 'year') #task = data
tsk = subsetTask(regr_task, features = c(low_var, high_var))#specify the features to avoid using the year

mod_rf <- train(learner_rf, task = regr_task)

names(mod_rf)
mod_rf$learner.model

#Make preditions
task_pred = predict(mod_rf, task = regr_task)
prediction_set_aggregated_dt[,eonr_pred := round(predict(mod_rf, newdata = prediction_set_aggregated_dt)$data$response/10)*10] 
prediction_set_aggregated_dt[,eonr_pred := round(predict(rfhigh, prediction_set_aggregated_dt)/10,0)*10]

# task_pred$data$truth
# measureRMSE(truth = task_pred$data$truth, response = task_pred$data$response)
# cor(task_pred$data$truth, task_pred$data$response)