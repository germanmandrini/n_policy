setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents') #CPSC
# setwd("/home/germanm2")
# setwd('~')

# cochete alt 91 []
# llave { } alt 123
source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')

library(randomForest)
library(ranger)
library(mlr)
source('./vr_value_v2/Codes/parameters.R')


yc_yearly_dt3 <- readRDS("./vr_value_v2/Data/files_rds/yc_yearly_dt3.rds")
grid10_soils_dt5 <- readRDS("./vr_value_v2/Data/Grid/grid10_soils_dt5.rds") %>% data.table()

reg_model_stuff <- readRDS( "./vr_value_v2/Data/files_rds/reg_model_stuff.rds")
stations_dt2 <- reg_model_stuff[['stations']]
full_fields_dt2 <- reg_model_stuff[['full_fields']]
model_minimum_regional <- reg_model_stuff[['model_minimum_regional']]
ValidSet3 <- reg_model_stuff[['ValidSet']]
TrainSet <- reg_model_stuff[['TrainSet']]
table(TrainSet$z)
# rm(reg_model_stuff)
#==========================================================================================
yc_yearly_dt3[, .(n_deep_v5 = mean(n_deep_v5)), by = z][order(as.numeric(z))]

#===========================================================================================================================================================
# EPLORATORY PLOT PROFITS VS RESIDUALS
ValidSet3[, P := Yld * Pc - N_fert * Pn]
ValidSet_eonr <- ValidSet3[, .SD[ P == max( P)], by = .(id_10, mukey, z)] %>% .[,.(id_10, mukey, z, P_12 = P, eonr_12 = N_fert)]
ValidSet_comp <- merge(ValidSet3, ValidSet_eonr, by = c('id_10', 'mukey', 'z'))
ValidSet_comp[,N_res := N_fert - eonr_12]
ValidSet_comp[,P_diff := P - P_12]

p_plot_dt <- ValidSet_comp[,.(P_diff = mean(P_diff)), by = N_res]
p_plot_dt[order(N_res)]

ggplot(data = p_plot_dt, aes(x = N_res, y = P_diff)) +
  geom_line()



# =========================================================================================================================================================
# SET THE VARIABLES
no_cost_varb <- c('region', "rain_30", "rain_60", "rain_90",
                  "t_max_30", "t_max_60", "t_max_90", "t_min_30", "t_min_60",
                  "t_min_90", "Yld_prev", 'Yld_lt_avg', 'Yld_lt_min', 'Yld_lt_max', "lai_v5")

ss_varb <- c("n_0_60cm_v5","esw_pct_v5", "whc")

crop_varb <- c(  "biomass_n_v5_Nminus", "biomass_n_v5_Nrich", "green_biomass_n_v5_Nminus",
              "green_biomass_n_v5_Nrich", "greenn_v5_Nminus", "greenn_v5_Nrich", "leafgreennconc_v5_Nminus", 
              "leafgreennconc_v5_Nrich", "leafgreenn_v5_Nminus", "leafgreenn_v5_Nrich", "biomass_n_v5","green_biomass_n_v5" ,
              "greenn_v5", "leafgreennconc_v5", "leafgreenn_v5") 

# , "n_0_60cm_v5"

#"biomass_n_df", "green_biomass_n_df", "greenn_df", "leafgreennconc_df", "leafgreenn_df")

#----------------------------
# PREPARE THE TRAINING DATA WITH EONR
TrainSet[, Yld_response := max(Yld) - min(Yld), by = .(id_10, mukey,z)]
TrainSet[, P := Yld * Pc - N_fert * Pn]
TrainSet[, n_0_60cm_v5 := n_20cm_v5 + n_40cm_v5 + n_60cm_v5]

# TrainSet[, Yld_response := max(Yld) - min(Yld), by = .(id_10, mukey,z)]
# TrainSet_RMM <- TrainSet[Yld_response > 500]
# z_dry <- TrainSet[,.(Yld = mean(Yld), .N), by = z][Yld < 5000]$z
# TrainSet <- TrainSet[!z %in% z_dry]
#Add the coordinates
coordinates_dt <- grid10_soils_dt5[,.(lat = mean(lat), 
                                      long = mean(long)), by = .(id_10, mukey)]

TrainSet <- merge(TrainSet,coordinates_dt , by = c('id_10', 'mukey'))

TrainSet_eonr <- TrainSet[, .SD[ P == max( P)], by = .(id_10, mukey, z)]
setnames(TrainSet_eonr, 'N_fert', 'eonr')

TrainSet_eonr2 <- TrainSet_eonr[,c('eonr','Yld_response', no_cost_varb, ss_varb, crop_varb), with = FALSE]

#===========================================================================================================================================================

#Principal Component Analysis
pcaData <- princomp(TrainSet_eonr2, scores = TRUE, cor = TRUE)
summary(pcaData)
#loadings - This represents the contribution of variables in each factor. Higher the
#number higher is the contribution of a particular variable in a factor
loadings(pcaData)

#screeplot of eigen values ( Value of standard deviation is considered as eigen values)
screeplot(pcaData, type = 'line', main = 'Screeplot')
#Biplot of score variables
biplot(pcaData)
#Scores of the components
pcaData$scores[1:10,]

#==========================================================================================
# Check a regression w/ all variables and backwards stepwise elimination


formula_lm <- paste0('eonr ~ ', paste(no_cost_varb, collapse = ' + '), ' + ', paste(ss_varb, collapse = ' + '), ' + ', 
                     paste(crop_varb, collapse = ' + '))

var_remove <- 'start'
while(length(var_remove)==1){
  print(var_remove)
  reg_lm1 <- lm(data = TrainSet_eonr2, formula = as.formula(formula_lm))
  summary(reg_lm1)
  
  sum_reg_mt <- summary(reg_lm1)$coefficients
  sum_reg_dt <- data.table(sum_reg_mt)
  names(sum_reg_dt) <- c("estimate", "std_error", "t_value", "p_value")
  sum_reg_dt[ ,variable := rownames(sum_reg_mt)]
  sum_reg_dt2 <- sum_reg_dt[!variable == '(Intercept)'] %>% .[order(p_value)]
  var_remove <- sum_reg_dt2[p_value > 0.05] %>% .[.N, variable]
  variables_try <- sum_reg_dt2$variable[!sum_reg_dt2$variable %in% var_remove]
  formula_lm <- paste0('eonr ~ ', paste(variables_try, collapse = ' + '))
}

#==========================================================================================
# Check a regression w/ model2 variables and backwards stepwise elimination
# cochete alt 91 []
# llave { } alt 123

formula_lm <- paste0('eonr ~ ', paste(no_cost_varb, collapse = ' + '), ' + ', paste(ss_varb, collapse = ' + '))

var_remove <- 'start'
while(length(var_remove)==1){
  print(var_remove)
  reg_lm2 <- lm(data = TrainSet_eonr2, formula = as.formula(formula_lm))
  summary(reg_lm2)
  
  sum_reg_mt <- summary(reg_lm2)$coefficients
  sum_reg_dt <- data.table(sum_reg_mt)
  names(sum_reg_dt) <- c("estimate", "std_error", "t_value", "p_value")
  sum_reg_dt[ ,variable := rownames(sum_reg_mt)]
  sum_reg_dt2 <- sum_reg_dt[!variable == '(Intercept)'] %>% .[order(p_value)]
  var_remove <- sum_reg_dt2[p_value > 0.05] %>% .[.N, variable]
  variables_try <- sum_reg_dt2$variable[!sum_reg_dt2$variable %in% var_remove]
  formula_lm <- paste0('eonr ~ ', paste(variables_try, collapse = ' + '))
}


#==========================================================================================
#==========================================================================================
# Check a regression old school method
#  -----
#All together
reg_lm3 <- lm(data = TrainSet_eonr, formula = "eonr  ~  n_0_60cm_v5")
summary(reg_lm3)

#  -----
# By mukey
TrainSet_eonr_cat <- copy(TrainSet_eonr)
TrainSet_eonr_cat[,id_10_mukey := as.factor(paste(id_10, mykey, sep = '_'))]
reg_lm2_mukey <- lm(data = TrainSet_eonr_cat, formula = "eonr  ~  n_0_60cm_v5 +  id_10_mukey + n_0_60cm_v5 * id_10_mukey")
summary(reg_lm2_mukey)

reg_lm2_mukey_mt <- reg_lm2_mukey$coefficients
reg_lm2_mukey_dt <- data.table(t(reg_lm2_mukey_mt[1:2]))
names(reg_lm2_mukey_dt) <- c('intercept', 'n_0_60cm_v5')

# TrainSet_eonr[, eonr_reg2 := round(predict(reg_lm2, TrainSet_eonr)/10,0)*10]
# summary(TrainSet_eonr$eonr_reg2)
# 
# cor(TrainSet_eonr$eonr, TrainSet_eonr$eonr_reg2) # reg
# 
# 
# ggplot(data = TrainSet_eonr, aes(x = eonr, y = eonr_reg2)) +
#   geom_point()+
#   geom_smooth(se=FALSE)+
#   coord_fixed() + geom_abline() + ylim(0, 350)+ xlim(0, 350) +
#   theme(aspect.ratio=1, 
#         axis.text=element_text(size=12),
#         axis.title=element_text(size=14,face="bold"))+
#   theme_bw()
# 
# reg_2 <- lm(data =TrainSet_eonr, eonr ~ eonr_reg2) #check a fitted line between pred and obs
# summary(reg_2)


#==========================================================================================
# COR matrix
cormat <- round(cor(TrainSet_eonr2),2)
head(cormat)
melted_cormat <- melt(cormat)
head(melted_cormat)
# ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
#   geom_tile()
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
upper_tri
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

#==========================================================================================
# CHOOSE VARIABLES
table(TrainSet$z)


ggplot(data = TrainSet_eonr2, aes(x = Yld, y = eonr)) + 
  geom_point()

# ss_varb <- c("n_0_60cm_v5")
# TrainSet_eonr <- eonr_mukey_dt[,c('eonr', no_cost_varb, ss_varb), with = FALSE]

# =========================================================================================================================================================
# CREATE THE REGIONAL RF-EONR


# Select mtry value with minimum out of bag(OOB) error.



# Create a Random Forest model with default parameters

mtry <- tuneRF(TrainSet_eonr2[,c(no_cost_varb), with = FALSE],TrainSet_eonr$eonr, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]


model1_eonr <- randomForest(eonr ~ ., data = TrainSet_eonr2[,c('eonr',no_cost_varb), with = FALSE],
                        importance = TRUE , mtry = best.m, ntree=5000) # , ntree=500, nodesize = 20

# --------------------------------------
mtry <- tuneRF(TrainSet_eonr2[,c(no_cost_varb, ss_varb), with = FALSE],TrainSet_eonr2$eonr, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]


model2_eonr <- randomForest(eonr ~ ., data = TrainSet_eonr2[,c('eonr', no_cost_varb, ss_varb), with = FALSE],
                        importance = TRUE , mtry = best.m, ntree=5000)

# --------------------------------------
mtry <- tuneRF(TrainSet_eonr2[,c(no_cost_varb, crop_varb), with = FALSE],TrainSet_eonr2$eonr, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]


model3_eonr <- randomForest(eonr ~ ., data = TrainSet_eonr2[,c('eonr', no_cost_varb, crop_varb), with = FALSE],
                            importance = TRUE, mtry = best.m, ntree=5000)
varImpPlot(model3_eonr, type=2)
plot(model3_eonr)

# --------------------------------------
# How many trees are needed to reach the minimum error estimate? 

which.min(model3_eonr$mse)
model2b_eonr
importance(model2_eonr)
varImpPlot(model2_eonr)
plot(model2_eonr)

# --------------------------------------
# CATEGORICA APPROACH
TrainSet_eonr_categ <- TrainSet_eonr2[,c("eonr", no_cost_varb, ss_varb), with = FALSE]
summary(TrainSet_eonr_categ$eonr)
TrainSet_eonr_categ[eonr <= 50, eonr_cat := "50"]
TrainSet_eonr_categ[eonr > 50 & eonr <= 100, eonr_cat := "100"]
TrainSet_eonr_categ[eonr > 100 & eonr <= 150, eonr_cat := "150"]
TrainSet_eonr_categ[eonr > 150 & eonr <= 200, eonr_cat := "200"]
TrainSet_eonr_categ[eonr > 200, eonr_cat := "250"]

TrainSet_eonr_categ[, eonr_cat := factor(eonr_cat) ]

model2_eonr_cat <- randomForest(eonr_cat ~ ., data = TrainSet_eonr_categ[,c('eonr_cat', no_cost_varb, ss_varb), with = FALSE], ntree = 5000)

# --------------------------------------
# ESTIMATING RESPONSE

mtry <- tuneRF(TrainSet_eonr2[,c(no_cost_varb, ss_varb), with = FALSE], TrainSet_eonr$Yld_response, ntreeTry=500,
               stepFactor=1.5, trace=TRUE, plot=TRUE)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

model2_response <- randomForest(Yld_response ~ ., data = TrainSet_eonr2[,c('Yld_response', no_cost_varb, ss_varb), with = FALSE], ntree = 5000)
varImpPlot(model2_response)

ggplot(data = TrainSet_eonr2, aes(x = Yld_response, y = eonr)) +
  geom_point()+
  geom_smooth(method = 'lm', formula = 'y ~ x')

resp_to_eonr_mt <- lm(TrainSet_eonr2, formula = 'eonr ~ Yld_response')$coefficients
resp_to_eonr_dt <- data.table(t(resp_to_eonr_mt))
names(resp_to_eonr_dt) <- c('intercept', 'Yld_response')

# --------------------------------------


importance(model2_eonr_cat)

plot(model2_eonr_cat)


#
jpeg("./vr_value_v2/Data/figures/model1_eonr_variables.jpg")
varImpPlot(model1_eonr, type=2, main = 'a) Regional RF1')
dev.off()
#
jpeg("./vr_value_v2/Data/figures/model2_eonr_variables.jpg")
varImpPlot(model2_eonr, type=2, main = 'b) Regional RF2')
dev.off()

jpeg("./vr_value_v2/Data/figures/model3_eonr_variables.jpg")
varImpPlot(model3_eonr, type=2, main = 'c) Regional RF3')
dev.off()

# reg_model_stuff[['model1b_eonr']] <-  model1b_eonr
# reg_model_stuff[['model2b_eonr']] <-  model2b_eonr

#==========================================================================================
# Plot response to N in the soil
ggplot(data = TrainSet_eonr, aes(x = n_0_60cm_v5, y = eonr)) + 
  geom_point() + geom_smooth()

ggplot(data = TrainSet_eonr, aes(x = Yld, y = eonr)) + 
  geom_hex() + geom_smooth()


# Section One #############################
## Section Two =================================
### Section Three ---------------------------------

#  ====================================================================================================================================

# SECTION ML FROM PAPER  ##########################################################
## PRINCIPAL COMPONENT REGRESSION =================================================
# http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/152-principal-component-and-partial-least-squares-regression-essentials/
library(caret)
TrainSet_eonr2 <- TrainSet_eonr[,c('eonr', no_cost_varb, ss_varb, crop_varb), with = FALSE]
preProcValues <- preProcess(TrainSet_eonr2[,-'eonr'], method = c("center", "scale"))

TrainSet_eonr2_transformed <- predict(preProcValues, TrainSet_eonr2)

model <- train(
  eonr~., data = TrainSet_eonr2_transformed, method = "pcr",
  trControl = trainControl("cv", number = 10),
  tuneLength = 25
)

# Plot model RMSE vs different values of components
plot(model)
# Print the best tuning parameter ncomp that
# minimize the cross-validation error, RMSE
model$bestTune

summary(model$finalModel)

# Make predictions
predictions <- model %>% predict(TrainSet_eonr2_transformed)

# Model performance metrics
data.frame(
  RMSE = caret::RMSE(predictions, TrainSet_eonr2_transformed$eonr),
  Rsquare = caret::R2(predictions, TrainSet_eonr2_transformed$eonr)
)

setdiff(names(TrainSet_eonr2_transformed), names(ValidSet3_eonr2))

ValidSet3_eonr2_transformed <- predict(preProcValues, ValidSet3_eonr2) 
# Make predictions
ValidSet3_eonr2_transformed$eonr_pred <- model %>% predict(ValidSet3_eonr2_transformed)

# Model performance metrics
data.frame(
  RMSE = caret::RMSE(ValidSet3_eonr2_transformed$eonr_pred, ValidSet3_eonr2_transformed$eonr),
  Rsquare = caret::R2(ValidSet3_eonr2_transformed$eonr_pred, ValidSet3_eonr2_transformed$eonr)
)
ggplot(ValidSet3_eonr2_transformed, aes(x = eonr, y = eonr_pred))+
  geom_point()

## PRINCIPAL COMPONENT REGRESSION =================================================




#==========================================================================================
# Y plot with Yld at eonr
mukey_sample <- sample(unique(TrainSet$mukey), 2)

ic_field_plot <- TrainSet[mukey %in% mukey_sample]
performance_set_plot <- ic_field_plot[mukey %in% mukey_sample, .SD[ P == max( P)], by = .(id_10, mukey, z)]

(plot_n <- ggplot() +
    geom_point(data = performance_set_plot, aes(x = N_fert, y = P)) +
    # geom_point(data = performance_set_plot[rotation == 0 & model == 12], aes(x = N_fert, y = P), size = 3, show.legend = FALSE) +
    geom_line(data = ic_field_plot, aes(x = N_fert, y = P, group=interaction(mukey, z)), show.legend = FALSE) +
    ggtitle(paste('P plot with Yld at eonr')))

#===========================================================================================
# install.packages('mlr')
# install.packages('ranger')


# # TrainSet_eonr <- eonr_mukey_dt[,c('eonr', no_cost_varb, ss_varb), with = FALSE]
# 
# # Create the tasks encapsulate the data set and further relevant information
# regr_task = makeRegrTask(data = TrainSet_eonr2, target = "eonr") #task = data

# 
# # Create the RF learner, with some optimized hyperparameters:
# learner_rf = makeLearner("regr.ranger",
#                          num.trees = 100,
#                          min.node.size = 25) #instructions
# 
# # Train a model
# mod_rf <- train(learner_rf, task = regr_task)
# names(mod_rf)
# mod_rf$learner.model
# 
# 
# #Make preditions
# task_pred = predict(mod_rf, task = regr_task)
# 
# task_pred$data$truth
# measureRMSE(truth = task_pred$data$truth, response = task_pred$data$response)
# cor(task_pred$data$truth, task_pred$data$response)

#------ TUNE HYPERPARAMETERS

# ranger.learner <- makeLearner("classif.ranger", predict.type = "prob")
## Create combined training data
# train_data <- TrainSet_eonr[,c('eonr', no_cost_varb, ss_varb), with = FALSE]
# ValidSet3[, P := Yld * Pc - N_fert * Pn]
# # ValidSet3[, n_0_60cm_v5 := n_20cm_v5 + n_40cm_v5 + n_60cm_v5]
# validation_data <- ValidSet3[, .SD[ P == max( P)], by = .(id_10, mukey, z)]
# setnames(validation_data, 'N_fert', 'eonr')
# validation_data <- validation_data[,c('eonr', no_cost_varb, ss_varb), with = FALSE]
# 
# train_task_data <- rbind(train_data, validation_data)
# size <- nrow(train_task_data)
# train_ind <- seq_len(nrow(train_data))
# validation_ind <- seq.int(max(train_ind) + 1, size)
# 
# 
# # Create training task
# train_task <- makeRegrTask(data = train_task_data, target = "eonr") #task = data
# 
# ## Tune hyperparameters
# ctrl <- makeTuneControlRandom(maxit = 400) 
# ps <- makeParamSet(
#   makeIntegerParam("num.trees", lower=50, upper=300),
#   makeIntegerParam("min.node.size", lower=5, upper=50),
#   makeIntegerParam("mtry", lower=2, upper=12)
# )

# library("parallelMap")
# parallelStartSocket(3)
# 
# res <- tuneParams("regr.ranger", task = train_task, 
#                   resampling = makeFixedHoldoutInstance(train_ind, validation_ind, size), 
#                   par.set = ps, 
#                   control = ctrl) 
# 
# 
# parallelStop()

# learner_rf = setHyperPars(makeLearner("regr.ranger"), par.vals = res$x)#instructions

# learner_rf = makeLearner("regr.ranger",
#                          num.trees = 500,
#                          min.node.size = 50,
#                          mtry = 10) #instructions
# 
# 
# # Train a model
# mod_rf <- train(learner_rf, task = regr_task)
# names(mod_rf)
# mod_rf$learner.model
# 
# #Make preditions
# task_pred = predict(mod_rf, task = regr_task)
# 
# task_pred$data$truth
# measureRMSE(truth = task_pred$data$truth, response = task_pred$data$response)
# cor(task_pred$data$truth, task_pred$data$response)

# =========================================================================================================================================================
# N PREDICTIONS IN THE TRAINING SET =========================================================================================================================================================

TrainSet_eonr
TrainSet_eonr[,eonr_pred := ceiling(predict(model2_eonr)/10)*10]
TrainSet_eonr[,eonr_pred:= predictrf_quantile(model = model2_eonr, data = TrainSet_eonr, quant = 0.7)]
TrainSet_eonr[,eonr_pred := ceiling(predict(reg_lm2, TrainSet_eonr)/10)*10]

TrainSet_eonr[,Yld_response_pred := predict(model2_response, newdata = TrainSet_eonr)] %>% 
  .[,eonr_pred := ceiling((resp_to_eonr_dt$intercept + Yld_response_pred * resp_to_eonr_dt$Yld_response)/10)*10] 

ggplot(TrainSet_eonr) +
  geom_density(aes(x = eonr), colour = 'red') +
  geom_density(aes(x = eonr_pred), colour = 'blue')

ggplot(data = TrainSet_eonr, aes(x = eonr, y = eonr_pred)) +
  geom_point()+
  geom_smooth(se=FALSE)+
  coord_fixed() + geom_abline() + ylim(0, 350)+ xlim(0, 350) +
  theme(aspect.ratio=1, 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_bw()

reg_2 <- lm(data =TrainSet_eonr, eonr ~ eonr_pred) #check a fitted line between pred and obs
summary(reg_2)

summary(TrainSet_eonr$Yld_response)

ggplot(data = TrainSet_eonr, aes(x = Yld_response, y = Yld_response_pred)) +
  geom_point()+
  geom_smooth(se=FALSE)+
  coord_fixed() + geom_abline() + ylim(0, 10000)+ xlim(0, 10000) +
  theme(aspect.ratio=1, 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_bw()

# =========================================================================================================================================================
# PREPARE THE VALIDATION DATA WITH EONR
ValidSet3[, Yld_response := max(Yld) - min(Yld), by = .(id_10, mukey,z)]
ValidSet3[, P := Yld * Pc - N_fert * Pn]
ValidSet3[, n_0_60cm_v5 := n_20cm_v5 + n_40cm_v5 + n_60cm_v5]

#Add the coordinates
ValidSet3 <- merge(ValidSet3,coordinates_dt , by = c('id_10', 'mukey'))

ValidSet3_eonr <- ValidSet3[, .SD[ P == max( P)], by = .(id_10, mukey, z)]
setnames(ValidSet3_eonr, 'N_fert', 'eonr')

ValidSet3_eonr2 <- ValidSet3_eonr[,c('eonr', no_cost_varb, ss_varb, crop_varb), with = FALSE]

# =========================================================================================================================================================
# PREDICTIONS IN THE VALIDATION SET ================================================================

ValidSet3_eonr2[, eonr_pred := ceiling(predict(model2_eonr, ValidSet3_eonr2)/10)*10]
ValidSet3_eonr2[, eonr_pred:= predictrf_quantile(model = model2_eonr, data = ValidSet3_eonr2, quant = 0.7)]
ValidSet3_eonr2[, eonr_pred := predict(model2_eonr_cat, ValidSet3_eonr2)]
ValidSet3_eonr2[,Yld_response_pred := predict(model2_response, newdata = ValidSet3_eonr2)] %>% 
  .[,eonr_pred := ceiling((resp_to_eonr_dt$intercept + Yld_response_pred * resp_to_eonr_dt$Yld_response)/10)*10] 
ValidSet3_eonr2[, eonr_pred := ceiling(predict(reg_lm2, ValidSet3_eonr2)/10)*10]


ggplot(data = ValidSet3_eonr2, aes( x = eonr, y = eonr_pred)) +
  geom_point()

ggplot(ValidSet3_eonr2) +
  geom_density(aes(x = eonr), colour = 'red') +
  geom_density(aes(x = eonr_pred), colour = 'blue')

ggplot(data = ValidSet3_eonr2, aes(x = eonr, y = eonr_pred)) +
  geom_hex()+
  geom_smooth(se=FALSE)+
  coord_fixed() + geom_abline() + ylim(0, 350)+ xlim(0, 350) +
  theme(aspect.ratio=1, 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_bw()

reg_2 <- lm(data =ValidSet3_eonr2, eonr ~ eonr_pred) #check a fitted line between pred and obs
summary(reg_2)

measureRMSE(truth = ValidSet3_eonr2$eonr, response = ValidSet3_eonr2$eonr_pred)
#---------------------
ggplot(data = ValidSet3_eonr2, aes(x = n_0_60cm_v5, y = eonr)) + 
  geom_point() + geom_smooth()

ggplot(data = ValidSet3_eonr2, aes(x = Yld, y = eonr)) + 
  geom_hex() + geom_smooth()

#---------------------
#Profits in the training set
TrainSet

results_list <- list()
# results_list[[1]] <- TrainSet[,eonr_rf := ceiling(predict(model2b_eonr, TrainSet, type = "class")/10)*10] %>% 
#   .[ N_fert == eonr_rf] %>%
#   .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := '5']


results_list[[1]] <- merge(TrainSet[,-"eonr_pred"] , model_minimum_regional, by = c('region')) %>% #here we join back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "Yld", "leach_n2", "n_deep_v5","N_fert",'P')] %>% .[,model := '1']

results_list[[2]] <- TrainSet[,eonr_pred := round(predict(model1_eonr, newdata = TrainSet)/10)*10] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'rf1']

results_list[[3]] <- TrainSet[,eonr_pred := round(predict(model2_eonr, newdata = TrainSet)/10)*10] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'rf2']

results_list[[4]] <- TrainSet[,eonr_pred := predictrf_quantile(model = model2_eonr, data = TrainSet, quant = 0.7)] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'rf2_q70']

results_list[[5]] <- TrainSet[,eonr_pred := predict(model2_eonr_cat, newdata = TrainSet)] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'rf2_cat']

results_list[[6]] <- TrainSet[,eonr_pred := round(predict(model3_eonr, newdata = TrainSet)/10)*10] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'rf3']

results_list[[7]] <- TrainSet[,eonr_pred := ceiling(predict(reg_lm1, TrainSet)/10)*10] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'reg1']

results_list[[8]] <- TrainSet[,eonr_pred := ceiling(predict(reg_lm2, TrainSet)/10)*10] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'reg2']

results_list[[9]] <- TrainSet[,eonr_pred := ceiling((reg_lm2_mukey_dt$intercept + reg_lm2_mukey_dt$n_0_60cm_v5 * n_0_60cm_v5)/10)*10] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'reg2_corrected']

results_list[[10]] <- TrainSet[,eonr_pred := ceiling(predict(reg_lm3, TrainSet)/10)*10] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'reg3']

results_list[[11]] <- TrainSet[,Yld_response_pred := predict(model2_response, newdata = TrainSet)] %>% 
  .[,eonr_pred := ceiling((resp_to_eonr_dt$intercept + Yld_response_pred * resp_to_eonr_dt$Yld_response)/10)*10] %>%
  .[,c("mukey", "z", "id_10", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'rf2_resp']

results_list[[12]] <- TrainSet[, .SD[ P == max( P)], by = .(id_10, mukey, z)] %>%
  .[,c("mukey", "z", "id_10", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := '12']

perfomances_dt <- rbindlist(results_list)

perfomances_dt2 <- merge(perfomances_dt, perfomances_dt[model == '12', .(id_10, mukey, z, N_fert_12 = N_fert)], by = c("id_10", "mukey", "z"))

perfomances_dt2[, .(Yld =  mean(Yld),
                   leach_n2 = mean(leach_n2),
                   N_fert = mean(N_fert),
                   N_fert_min = min(N_fert),
                   N_fert_max = max(N_fert),
                   P = mean(P),
                   cor = cor(N_fert_12, N_fert),
                   RMSE = measureRMSE(truth = N_fert_12, response = N_fert)), by = .( model)]

# =========================================================================================================================================================
# =========================================================================================================================================================
# =========================================================================================================================================================
# COMPARE n predictions IN THE VALIDATION SET
            # ValidSet3[, P := Yld * Pc - N_fert * Pn]
            # 
            # ValidSet3_eonr <- ValidSet3[, .SD[ P == max( P)], by = .(id_10, mukey, z)]
            # setnames(ValidSet3_eonr, 'N_fert', 'eonr')
            # 
            # ValidSet3_eonr[,eonr_pred := round(predict(mod_rf, newdata = ValidSet3_eonr)$data$response/10)*10] 
            # ValidSet3_eonr[,.N, by = eonr_mlr][order(eonr_mlr)]
            # 
            # ggplot(ValidSet3_eonr) +
            #   geom_density(aes(x = eonr), colour = 'red') +
            #   geom_density(aes(x = eonr_mlr), colour = 'blue')
            # 
            # ggplot(data = ValidSet3_eonr, aes(x = eonr, y = eonr_mlr)) +
            #   geom_hex()+
            #   geom_smooth(se=FALSE)+
            #   coord_fixed() + geom_abline() + ylim(0, 350)+ xlim(0, 350) +
            #   theme(aspect.ratio=1, 
            #         axis.text=element_text(size=12),
            #         axis.title=element_text(size=14,face="bold"))+
            #   theme_bw()
            # 
            # ggplot(data = ValidSet3_eonr, aes(x = n_0_60cm_v5, y = eonr_mlr)) +
            #   geom_hex()+
            #   geom_smooth(se=FALSE)
            # 
            # reg_2 <- lm(data =ValidSet3_eonr, eonr ~ eonr_mlr) #check a fitted line between pred and obs
            # summary(reg_2)

#---------------------
#Profits in the validation set
results_list <- list()

results_list[[1]] <- merge(ValidSet3[,-'eonr_pred'] , model_minimum_regional, by = c('region')) %>% #here we join back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "Yld", "leach_n2", "n_deep_v5","N_fert",'P')] %>% .[,model := '1']

results_list[[2]] <- ValidSet3[,eonr_pred := round(predict(model1_eonr, newdata = ValidSet3)/10)*10] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'rf1']


results_list[[3]] <- ValidSet3[,eonr_pred := round(predict(model2_eonr, newdata = ValidSet3)/10)*10] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'rf2']

results_list[[4]] <- ValidSet3[,eonr_pred := predictrf_quantile(model = model2_eonr, data = ValidSet3, quant = 0.7)] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'rf2_q70']

results_list[[5]] <- ValidSet3[,eonr_pred := predict(model2_eonr_cat, newdata = ValidSet3)] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'rf2_cat']


results_list[[6]] <- ValidSet3[,eonr_pred := round(predict(model3_eonr, newdata = ValidSet3)/10)*10] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'rf3']

results_list[[7]] <- ValidSet3[,eonr_pred := ceiling(predict(reg_lm1, ValidSet3)/10)*10] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'reg1']


results_list[[8]] <- ValidSet3[,eonr_pred := ceiling(predict(reg_lm2, ValidSet3)/10)*10] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'reg2']

results_list[[9]] <- ValidSet3[,eonr_pred := ceiling((reg_lm2_mukey_dt$intercept + reg_lm2_mukey_dt$n_0_60cm_v5 * n_0_60cm_v5)/10)*10] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'reg2_corrected']


results_list[[10]] <- ValidSet3[,eonr_pred := ceiling(predict(reg_lm3, ValidSet3)/10)*10] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'reg3']

results_list[[11]] <- ValidSet3[,Yld_response_pred := predict(model2_response, newdata = ValidSet3)] %>% 
  .[,eonr_pred := ceiling((resp_to_eonr_dt$intercept + Yld_response_pred * resp_to_eonr_dt$Yld_response)/10)*10] %>%
  .[,c("mukey", "z", "id_10", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'rf2_resp']

results_list[[12]] <- ValidSet3[, .SD[ P == max( P)], by = .(id_10, mukey, z)] %>%
  .[,c("mukey", "z", "id_10", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := '12']

perfomances_dt <- rbindlist(results_list)

# Add the real eonr to calculate RMSE
perfomances_dt2 <- merge(perfomances_dt, perfomances_dt[model == '12', .(id_10, mukey, z, N_fert_12 = N_fert)], by = c("id_10", "mukey", "z"))

perfomances_dt2[, .(Yld =  mean(Yld),
                    leach_n2 = mean(leach_n2),
                    N_fert = mean(N_fert),
                    N_fert_min = min(N_fert),
                    N_fert_max = max(N_fert),
                    P = mean(P),
                    cor = cor(N_fert_12, N_fert),
                    RMSE = measureRMSE(truth = N_fert_12, response = N_fert)), by = .( model)]

reg_model_stuff2 <- list()
reg_model_stuff2[['model_minimum_regional']] <-  model_minimum_regional
reg_model_stuff2[['full_fields']] <-  full_fields_dt2[,-'rf']
reg_model_stuff2[['stations']] <-  stations_dt2[,-'rs']
reg_model_stuff2[['training_z']] <-  reg_model_stuff[['training_z']]

reg_model_stuff2[['no_cost_var']] <-  no_cost_varb
reg_model_stuff2[['ss_var']] <-  ss_varb
reg_model_stuff2[['crop_varb']] <-  crop_varb



reg_model_stuff2[['model1_eonr']] <-  model1_eonr
reg_model_stuff2[['model2_eonr']] <-  model2_eonr
reg_model_stuff2[['model3_eonr']] <-  model2_response
reg_model_stuff2[['model2_response']] <-  model3_eonr
reg_model_stuff2[['reg_lm1']] <-  reg_lm1
reg_model_stuff2[['reg_lm2']] <-  reg_lm2
reg_model_stuff2[['reg_lm3']] <-  reg_lm3
reg_model_stuff2[['reg_lm2_corrected']] <-  reg_lm2_mukey_dt




saveRDS(reg_model_stuff2, "./vr_value_v2/Data/files_rds/reg_model_stuff2.rds")

