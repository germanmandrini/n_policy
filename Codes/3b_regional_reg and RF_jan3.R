# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('C:/Users/germanm2/Box Sync/My_Documents') #CPSC
# setwd("/home/germanm2")
# setwd('~')

# corchete alt 91 []
# llave { } alt 123 125 
source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')

library(randomForest)
library(ranger)
library(mlr)
source('./vr_value_v2/Codes/parameters.R')


yc_yearly_dt3 <- readRDS("./vr_value_v2/Data/files_rds/yc_yearly_dt3.rds")
grid10_soils_dt5 <- readRDS("./vr_value_v2/Data/Grid/grid10_soils_dt5.rds") %>% data.table()

reg_model_stuff <- readRDS( "./vr_value_v2/Data/files_rds/reg_model_stuff.rds")
# stations_dt <- reg_model_stuff[['stations']]
# full_fields_dt <- reg_model_stuff[['full_fields']]
model_minimum_regional <- reg_model_stuff[['model_minimum_regional']]
ValidSet2 <- reg_model_stuff[['ValidSet']]
TrainSet2 <- reg_model_stuff[['TrainSet']]
table(TrainSet2$z)
# rm(reg_model_stuff)
#==========================================================================================
yc_yearly_dt3[, .(n_deep_v5 = mean(n_deep_v5)), by = z][order(as.numeric(z))]

#===========================================================================================================================================================
## EPLORATORY PLOT PROFITS VS RESIDUALS  ========
ValidSet2[, P := Yld * Pc - N_fert * Pn]
ValidSet_eonr <- ValidSet2[, .SD[ P == max( P)], by = .(id_10, mukey, z)] %>% .[,.(id_10, mukey, z, P_12 = P, eonr_12 = N_fert)]
ValidSet_comp <- merge(ValidSet2, ValidSet_eonr, by = c('id_10', 'mukey', 'z'))
ValidSet_comp[,N_res := N_fert - eonr_12]
ValidSet_comp[,P_diff := P - P_12]

p_plot_dt <- ValidSet_comp[,.(P_diff = mean(P_diff)), by = N_res]
p_plot_dt[order(N_res)]

ggplot(data = p_plot_dt, aes(x = N_res, y = P_diff)) +
  geom_line()


# =========================================================================================================================================================
## SET THE VARIABLES ========
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

# =========================================================================================================================================================
## PREPARE THE TRAINING DATA WITH EONR ========
if(FALSE){
  TrainSet2[, Yld_response := max(Yld) - min(Yld), by = .(id_10, mukey,z)]
  TrainSet2[, P := Yld * Pc - N_fert * Pn]
  TrainSet2[, n_0_60cm_v5 := n_20cm_v5 + n_40cm_v5 + n_60cm_v5]
  
  # TrainSet2[, Yld_response := max(Yld) - min(Yld), by = .(id_10, mukey,z)]
  # TrainSet2_RMM <- TrainSet2[Yld_response > 500]
  # z_dry <- TrainSet2[,.(Yld = mean(Yld), .N), by = z][Yld < 5000]$z
  # TrainSet2 <- TrainSet2[!z %in% z_dry]
  #Add the coordinates
  coordinates_dt <- grid10_soils_dt5[,.(lat = mean(lat), 
                                        long = mean(long)), by = .(id_10, mukey)]
  
  TrainSet2 <- merge(TrainSet2,coordinates_dt , by = c('id_10', 'mukey'))
  
  TrainSet_eonr <- TrainSet2[, .SD[ P == max( P)], by = .(id_10, mukey, z)]
  setnames(TrainSet_eonr, 'N_fert', 'eonr')
  
  TrainSet_eonr2 <- TrainSet_eonr[,c('eonr', no_cost_varb, ss_varb, crop_varb), with = FALSE]
  saveRDS(TrainSet_eonr2, "./vr_value_v2/Data/files_rds/TrainSet_eonr2.rds")
}
TrainSet_eonr2 <- readRDS("./vr_value_v2/Data/files_rds/TrainSet_eonr2.rds")

ggplot(data = TrainSet_eonr2[n_0_60cm_v5 <200], aes(x = n_0_60cm_v5, y = eonr)) + 
  geom_point() + geom_smooth(formula = 'y ~ x', method = lm)

# =========================================================================================================================================================
## PREPARE THE VALIDATION DATA WITH EONR ========
if(FALSE){
  ValidSet2[, Yld_response := max(Yld) - min(Yld), by = .(id_10, mukey,z)]
  ValidSet2[, P := Yld * Pc - N_fert * Pn]
  ValidSet2[, n_0_60cm_v5 := n_20cm_v5 + n_40cm_v5 + n_60cm_v5]
  
  #Add the coordinates
  ValidSet2 <- merge(ValidSet2,coordinates_dt , by = c('id_10', 'mukey'))
  
  ValidSet_eonr <- ValidSet2[, .SD[ P == max( P)], by = .(id_10, mukey, z)]
  setnames(ValidSet_eonr, 'N_fert', 'eonr')
  
  ValidSet_eonr2 <- ValidSet_eonr[,c('eonr', no_cost_varb, ss_varb, crop_varb), with = FALSE]
  saveRDS(ValidSet_eonr2, "./vr_value_v2/Data/files_rds/ValidSet_eonr2.rds")
}

ValidSet_eonr2 <- readRDS("./vr_value_v2/Data/files_rds/ValidSet_eonr2.rds")

ggplot(data = ValidSet_eonr2[n_0_60cm_v5 <200], aes(x = n_0_60cm_v5, y = eonr)) + 
  geom_point() + geom_smooth(formula = 'y ~ x', method = lm)

#===========================================================================================================================================================
library(caret)
library(knitr)     # just using this for kable() to make pretty tables

## STANDARIZE DATA ========
# Standarize the Training set ---------
# calculate the pre-process parameters from the dataset
# preprocessParams <- preProcess(TrainSet2_eonr2[,-c('eonr', 'region')], method=c("scale"))
yc_yearly_dt3[, n_0_60cm_v5 := n_20cm_v5 + n_40cm_v5 + n_60cm_v5]
sample_for_sandarization <- yc_yearly_dt3[N_fert == 160,c( no_cost_varb, ss_varb, crop_varb), with = FALSE][,-'region']

preprocessParams <- preProcess(sample_for_sandarization, method=c('scale'))
# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
TrainSet_eonr3 <- predict(preprocessParams, TrainSet_eonr2)

# summarize the transformed dataset
summarizeColumns(TrainSet_eonr3) %>%
  kable(digits = 2)

# Create dummy variables for region
TrainSet_eonr3[, region.2 := ifelse(region == 2, 1, 0)]
TrainSet_eonr3[, region.3 := ifelse(region == 3, 1, 0)]
TrainSet_eonr3[, region := NULL]
# TrainSet_eonr3[,region := factor(region)]
# 
# dummies <- dummyVars(" ~ .", data = TrainSet_eonr3)
# TrainSet_eonr3 <- data.table(predict(dummies, newdata = TrainSet_eonr3))

# Standarize the Validation set ---------
# transform the dataset using the parameters
ValidSet_eonr3 <- predict(preprocessParams, ValidSet_eonr2)

# Create dummy variables using caret
ValidSet_eonr3[, region.2 := ifelse(region == 2, 1, 0)]
ValidSet_eonr3[, region.3 := ifelse(region == 3, 1, 0)]
ValidSet_eonr3[, region := NULL]

no_cost_varb_trf <- c('region.2', 'region.3', "rain_30", "rain_60", "rain_90",
                  "t_max_30", "t_max_60", "t_max_90", "t_min_30", "t_min_60",
                  "t_min_90", "Yld_prev", 'Yld_lt_avg', 'Yld_lt_min', 'Yld_lt_max', "lai_v5")

#==========================================================================================
# COR matrix ------------
cormat <- round(cor(ValidSet_eonr3),2)
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
## Regression 1:  Check a regression w/ all variables and backwards stepwise elimination =========
formula_lm <- paste0('eonr ~ ', paste(no_cost_varb_trf, collapse = ' + '))

var_remove <- 'start'
while(length(var_remove)==1){
  print(var_remove)
  reg_lm1 <- lm(data = TrainSet_eonr3, formula = as.formula(formula_lm))
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

# Make predictions
TrainSet_eonr3$eonr_pred <- predict(reg_lm1, TrainSet_eonr3)
ValidSet_eonr3$eonr_pred <- predict(reg_lm1, ValidSet_eonr3)

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

#==========================================================================================
# Regression 2: Check a regression w/ model2 variables and backwards stepwise elimination
# cochete alt 91 []
# llave { } alt 123

formula_lm <- paste0('eonr ~ ', paste(no_cost_varb_trf, collapse = ' + '), ' + ', paste(ss_varb, collapse = ' + '))

var_remove <- 'start'
while(length(var_remove)==1){
  print(var_remove)
  reg_lm2 <- lm(data = TrainSet_eonr3, formula = as.formula(formula_lm))
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

# Make predictions
TrainSet_eonr3$eonr_pred <- predict(reg_lm2, TrainSet_eonr3)
ValidSet_eonr3$eonr_pred <- predict(reg_lm2, ValidSet_eonr3)

# Model performance metrics Training Set &  Validation set
data.frame(
  RMSE_train = caret::RMSE(TrainSet_eonr3$eonr_pred, TrainSet_eonr3$eonr),
  Rsquare_train = caret::R2(TrainSet_eonr3$eonr_pred, TrainSet_eonr3$eonr),
  RMSE_valid = caret::RMSE(ValidSet_eonr3$eonr_pred, ValidSet_eonr3$eonr),
  Rsquare_valid = caret::R2(ValidSet_eonr3$eonr_pred, ValidSet_eonr3$eonr)
)

ggplot(ValidSet_eonr3, aes(x = eonr, y = eonr_pred))+
  geom_point()

ggplot()+
  geom_hex(data = TrainSet_eonr3, aes(x = eonr, y = eonr_pred), shape = 1, color = 'blue')+
  geom_hex(data = ValidSet_eonr3, aes(x = eonr, y = eonr_pred), shape = 2, color = 'green') +
  coord_fixed() + geom_abline() + ylim(0, 350)+ xlim(0, 350) +
  theme(aspect.ratio=1, 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_bw()

#==========================================================================================
# Regression 3: Check a regression w/ model3 variables and backwards stepwise elimination
# cochete alt 91 []
# llave { } alt 123

formula_lm <- paste0('eonr ~ ', paste(no_cost_varb_trf, collapse = ' + '), ' + ', paste(crop_varb, collapse = ' + '))

var_remove <- 'start'
while(length(var_remove)==1){
  print(var_remove)
  reg_lm3 <- lm(data = TrainSet_eonr3, formula = as.formula(formula_lm))
  summary(reg_lm3)
  
  sum_reg_mt <- summary(reg_lm3)$coefficients
  sum_reg_dt <- data.table(sum_reg_mt)
  names(sum_reg_dt) <- c("estimate", "std_error", "t_value", "p_value")
  sum_reg_dt[ ,variable := rownames(sum_reg_mt)]
  sum_reg_dt2 <- sum_reg_dt[!variable == '(Intercept)'] %>% .[order(p_value)]
  var_remove <- sum_reg_dt2[p_value > 0.05] %>% .[.N, variable]
  variables_try <- sum_reg_dt2$variable[!sum_reg_dt2$variable %in% var_remove]
  formula_lm <- paste0('eonr ~ ', paste(variables_try, collapse = ' + '))
}

# Make predictions
TrainSet_eonr3$eonr_pred <- predict(reg_lm3, TrainSet_eonr3)
ValidSet_eonr3$eonr_pred <- predict(reg_lm3, ValidSet_eonr3)

# Model performance metrics Training Set &  Validation set
data.frame(
  RMSE_train = caret::RMSE(TrainSet_eonr3$eonr_pred, TrainSet_eonr3$eonr),
  Rsquare_train = caret::R2(TrainSet_eonr3$eonr_pred, TrainSet_eonr3$eonr),
  RMSE_valid = caret::RMSE(ValidSet_eonr3$eonr_pred, ValidSet_eonr3$eonr),
  Rsquare_valid = caret::R2(ValidSet_eonr3$eonr_pred, ValidSet_eonr3$eonr)
)

ggplot(ValidSet_eonr3, aes(x = eonr, y = eonr_pred))+
  geom_point()

ggplot()+
  geom_hex(data = TrainSet_eonr3, aes(x = eonr, y = eonr_pred), shape = 1, color = 'blue')+
  geom_hex(data = ValidSet_eonr3, aes(x = eonr, y = eonr_pred), shape = 2, color = 'green') +
  coord_fixed() + geom_abline() + ylim(0, 350)+ xlim(0, 350) +
  theme(aspect.ratio=1, 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_bw()

#==========================================================================================
# Regression 4: Check a regression old school method

reg_lm4 <- lm(data = TrainSet_eonr3, formula = "eonr  ~  n_0_60cm_v5")
summary(reg_lm4)

# Make predictions
TrainSet_eonr3$eonr_pred <- predict(reg_lm4, TrainSet_eonr3)
ValidSet_eonr3$eonr_pred <- predict(reg_lm4, ValidSet_eonr3)

# Model performance metrics Training Set &  Validation set
data.frame(
  RMSE_train = caret::RMSE(TrainSet_eonr3$eonr_pred, TrainSet_eonr3$eonr),
  Rsquare_train = caret::R2(TrainSet_eonr3$eonr_pred, TrainSet_eonr3$eonr),
  RMSE_valid = caret::RMSE(ValidSet_eonr3$eonr_pred, ValidSet_eonr3$eonr),
  Rsquare_valid = caret::R2(ValidSet_eonr3$eonr_pred, ValidSet_eonr3$eonr)
)

ggplot(ValidSet_eonr3, aes(x = n_0_60cm_v5, y = eonr_pred))+
  geom_point()

ggplot()+
  geom_hex(data = TrainSet_eonr3, aes(x = eonr, y = eonr_pred), shape = 1, color = 'blue')+
  geom_hex(data = ValidSet_eonr3, aes(x = eonr, y = eonr_pred), shape = 2, color = 'green') +
  coord_fixed() + geom_abline() + ylim(0, 350)+ xlim(0, 350) +
  theme(aspect.ratio=1, 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_bw()



#==========================================================================================
# By mukey
TrainSet_eonr_cat <- copy(TrainSet_eonr)

# Standarize 
# transform the dataset using the parameters
TrainSet_eonr_cat <- predict(preprocessParams, TrainSet_eonr_cat)

TrainSet_eonr_cat[,id_10_mukey := as.factor(paste(id_10, mukey, sep = '_'))]
reg_lm4_mukey <- lm(data = TrainSet_eonr_cat, formula = "eonr  ~  n_0_60cm_v5 +  id_10_mukey + n_0_60cm_v5 * id_10_mukey")
summary(reg_lm4_mukey)

reg_lm4_mukey_mt <- reg_lm4_mukey$coefficients
reg_lm4_mukey_dt <- data.table(t(reg_lm4_mukey_mt[1:2]))
names(reg_lm4_mukey_dt) <- c('intercept', 'n_0_60cm_v5')

# Make predictions
TrainSet_eonr3[,eonr_pred := reg_lm4_mukey_dt$intercept + reg_lm4_mukey_dt$n_0_60cm_v5 * n_0_60cm_v5]
ValidSet_eonr3[,eonr_pred := reg_lm4_mukey_dt$intercept + reg_lm4_mukey_dt$n_0_60cm_v5 * n_0_60cm_v5]

# Model performance metrics Training Set &  Validation set
data.frame(
  RMSE_train = caret::RMSE(TrainSet_eonr3$eonr_pred, TrainSet_eonr3$eonr),
  Rsquare_train = caret::R2(TrainSet_eonr3$eonr_pred, TrainSet_eonr3$eonr),
  RMSE_valid = caret::RMSE(ValidSet_eonr3$eonr_pred, ValidSet_eonr3$eonr),
  Rsquare_valid = caret::R2(ValidSet_eonr3$eonr_pred, ValidSet_eonr3$eonr)
)

ggplot(ValidSet_eonr3, aes(x = eonr, y = eonr_pred))+
  geom_point()

ggplot()+
  geom_hex(data = TrainSet_eonr3, aes(x = eonr, y = eonr_pred), shape = 1, color = 'blue')+
  geom_hex(data = ValidSet_eonr3, aes(x = eonr, y = eonr_pred), shape = 2, color = 'green') +
  coord_fixed() + geom_abline() + ylim(0, 350)+ xlim(0, 350) +
  theme(aspect.ratio=1, 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_bw()


#==========================================================================================
# Decision tree 1 -----
# https://dataaspirant.com/2017/02/03/decision-tree-classifier-implementation-in-r/
library(caret)
library('rpart.plot')

trctrl <- trainControl(method = "repeatedcv", number = 100, repeats = 4)

set.seed(3333)
dtree_1 <- caret::train(eonr ~., data = TrainSet_eonr3[,c('eonr',no_cost_varb_trf), with = FALSE], 
                   method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 100)


prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)

# Make predictions
TrainSet_eonr3$eonr_pred <- predict(dtree_1, TrainSet_eonr3)
ValidSet_eonr3$eonr_pred <- predict(dtree_1, ValidSet_eonr3)

# Model performance metrics Training Set &  Validation set
data.frame(
  RMSE_train = caret::RMSE(TrainSet_eonr3$eonr_pred, TrainSet_eonr3$eonr),
  Rsquare_train = caret::R2(TrainSet_eonr3$eonr_pred, TrainSet_eonr3$eonr),
  RMSE_valid = caret::RMSE(ValidSet_eonr3$eonr_pred, ValidSet_eonr3$eonr),
  Rsquare_valid = caret::R2(ValidSet_eonr3$eonr_pred, ValidSet_eonr3$eonr)
)

ggplot(ValidSet_eonr3, aes(x = eonr, y = eonr_pred))+
  geom_point()


ggplot()+
  geom_hex(data = TrainSet_eonr3, aes(x = eonr, y = eonr_pred), shape = 1, color = 'blue')+
  geom_hex(data = ValidSet_eonr3, aes(x = eonr, y = eonr_pred), shape = 2, color = 'green') +
  coord_fixed() + geom_abline() + ylim(0, 350)+ xlim(0, 350) +
  theme(aspect.ratio=1, 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_bw()

#==========================================================================================
# Decision tree 2 -----
# https://dataaspirant.com/2017/02/03/decision-tree-classifier-implementation-in-r/
trctrl <- trainControl(method = "repeatedcv", number = 100, repeats = 4)

set.seed(3333)
dtree_2 <- caret::train(eonr ~., data = TrainSet_eonr3[,c('eonr',no_cost_varb_trf, ss_varb), with = FALSE], 
                 method = "rpart",
                 parms = list(split = "information"),
                 trControl=trctrl,
                 tuneLength = 100)


prp(dtree_2$finalModel, box.palette = "Reds", tweak = 1.2)

# Make predictions
TrainSet_eonr3$eonr_pred <- predict(dtree_2, TrainSet_eonr3)
ValidSet_eonr3$eonr_pred <- predict(dtree_2, ValidSet_eonr3)

# Model performance metrics Training Set &  Validation set
data.frame(
  RMSE_train = caret::RMSE(TrainSet_eonr3$eonr_pred, TrainSet_eonr3$eonr),
  Rsquare_train = caret::R2(TrainSet_eonr3$eonr_pred, TrainSet_eonr3$eonr),
  RMSE_valid = caret::RMSE(ValidSet_eonr3$eonr_pred, ValidSet_eonr3$eonr),
  Rsquare_valid = caret::R2(ValidSet_eonr3$eonr_pred, ValidSet_eonr3$eonr)
)

ggplot(ValidSet_eonr3, aes(x = eonr, y = eonr_pred))+
  geom_point()

ggplot()+
  geom_hex(data = TrainSet_eonr3, aes(x = eonr, y = eonr_pred), shape = 1, color = 'blue')+
  geom_hex(data = ValidSet_eonr3, aes(x = eonr, y = eonr_pred), shape = 2, color = 'green') +
  coord_fixed() + geom_abline() + ylim(0, 350)+ xlim(0, 350) +
  theme(aspect.ratio=1, 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_bw()

#==========================================================================================
# Decision tree 3 -----
# https://dataaspirant.com/2017/02/03/decision-tree-classifier-implementation-in-r/

trctrl <- trainControl(method = "repeatedcv", number = 100, repeats = 4)

set.seed(3333)
dtree_3 <- caret::train(eonr ~., data = TrainSet_eonr3[,c('eonr',no_cost_varb_trf, crop_varb), with = FALSE], 
                        method = "rpart",
                        parms = list(split = "information"),
                        trControl=trctrl,
                        tuneLength = 100)


prp(dtree_3$finalModel, box.palette = "Reds", tweak = 1.2)

# Make predictions
TrainSet_eonr3$eonr_pred <- predict(dtree_3, TrainSet_eonr3)
ValidSet_eonr3$eonr_pred <- predict(dtree_3, ValidSet_eonr3)

# Model performance metrics Training Set &  Validation set
data.frame(
  RMSE_train = caret::RMSE(TrainSet_eonr3$eonr_pred, TrainSet_eonr3$eonr),
  Rsquare_train = caret::R2(TrainSet_eonr3$eonr_pred, TrainSet_eonr3$eonr),
  RMSE_valid = caret::RMSE(ValidSet_eonr3$eonr_pred, ValidSet_eonr3$eonr),
  Rsquare_valid = caret::R2(ValidSet_eonr3$eonr_pred, ValidSet_eonr3$eonr)
)

ggplot(ValidSet_eonr3, aes(x = eonr, y = eonr_pred))+
  geom_point()

ggplot()+
  geom_hex(data = TrainSet_eonr3, aes(x = eonr, y = eonr_pred), shape = 1, color = 'blue')+
  geom_hex(data = ValidSet_eonr3, aes(x = eonr, y = eonr_pred), shape = 2, color = 'green') +
  coord_fixed() + geom_abline() + ylim(0, 350)+ xlim(0, 350) +
  theme(aspect.ratio=1, 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_bw()
# =========================================================================================================================================================
# RF Model 1------------------------

# Create a Random Forest model with default parameters

mtry <- tuneRF(TrainSet_eonr3[,c(no_cost_varb_trf), with = FALSE],TrainSet_eonr3$eonr, ntreeTry=500,
               stepFactor=1.2,improve=0.01, trace=TRUE, plot=TRUE)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

rf1_eonr <- randomForest(eonr ~ ., data = TrainSet_eonr3[,c('eonr',no_cost_varb_trf), with = FALSE],
                        importance = TRUE , mtry = best.m, ntree=1000) # , ntree=500, nodesize = 20

names(TrainSet_eonr3[,c('eonr',no_cost_varb_trf), with = FALSE])

jpeg("./vr_value_v2/Data/figures/model1_eonr_variables.jpg")
varImpPlot(rf1_eonr, type=2, main = 'a) Regional RF1')
dev.off() 

# Make predictions
TrainSet_eonr3$eonr_pred <- predict(rf1_eonr, TrainSet_eonr3)
ValidSet_eonr3$eonr_pred <- predict(rf1_eonr, ValidSet_eonr3)

data.frame(
  RMSE_train = caret::RMSE(TrainSet_eonr3$eonr_pred, TrainSet_eonr3$eonr),
  Rsquare_train = caret::R2(TrainSet_eonr3$eonr_pred, TrainSet_eonr3$eonr),
  RMSE_valid = caret::RMSE(ValidSet_eonr3$eonr_pred, ValidSet_eonr3$eonr),
  Rsquare_valid = caret::R2(ValidSet_eonr3$eonr_pred, ValidSet_eonr3$eonr)
)

ggplot(ValidSet_eonr3, aes(x = eonr, y = eonr_pred))+
  geom_point()

ggplot()+
  geom_hex(data = TrainSet_eonr3, aes(x = eonr, y = eonr_pred), shape = 1, color = 'blue')+
  geom_hex(data = ValidSet_eonr3, aes(x = eonr, y = eonr_pred), shape = 2, color = 'green') +
  coord_fixed() + geom_abline() + ylim(0, 350)+ xlim(0, 350) +
  theme(aspect.ratio=1, 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_bw()

# --------------------------------------
# RF Model 2------------------------
mtry <- tuneRF(TrainSet_eonr3[,c(no_cost_varb_trf, ss_varb), with = FALSE],TrainSet_eonr3$eonr, ntreeTry=500,
               stepFactor=1.2,improve=0.01, trace=TRUE, plot=TRUE)
mtry
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

rf2_eonr <- randomForest(eonr ~ ., data = TrainSet_eonr3[,c('eonr', no_cost_varb_trf, ss_varb), with = FALSE],
                            importance = TRUE , mtry = best.m, ntree=1000, nodesize = 20)

names(TrainSet_eonr3[,c('eonr', no_cost_varb_trf, ss_varb), with = FALSE])
varImpPlot(rf2_eonr, type=2)

jpeg("./vr_value_v2/Data/figures/model2_eonr_variables.jpg")
varImpPlot(rf2_eonr, type=2, main = 'b) Regional RF2')
dev.off() 

# Make predictions
TrainSet_eonr3$eonr_pred <- predict(rf2_eonr, TrainSet_eonr3)
ValidSet_eonr3$eonr_pred <- predict(rf2_eonr, ValidSet_eonr3)

# Model performance metrics Training Set &  Validation set
data.frame(
  RMSE_train = caret::RMSE(TrainSet_eonr3$eonr_pred, TrainSet_eonr3$eonr),
  Rsquare_train = caret::R2(TrainSet_eonr3$eonr_pred, TrainSet_eonr3$eonr),
  RMSE_valid = caret::RMSE(ValidSet_eonr3$eonr_pred, ValidSet_eonr3$eonr),
  Rsquare_valid = caret::R2(ValidSet_eonr3$eonr_pred, ValidSet_eonr3$eonr)
)

ggplot(TrainSet_eonr3, aes(x = n_0_60cm_v5, y = eonr))+
  geom_hex() + geom_smooth()

ggplot(ValidSet_eonr3, aes(x = n_0_60cm_v5, y = eonr_pred))+
  geom_hex() + geom_smooth()

ggplot()+
  geom_hex(data = TrainSet_eonr3, aes(x = eonr, y = eonr_pred), shape = 1, color = 'blue')+
  geom_hex(data = ValidSet_eonr3, aes(x = eonr, y = eonr_pred), shape = 2, color = 'green') +
  coord_fixed() + geom_abline() + ylim(0, 350)+ xlim(0, 350) +
  theme(aspect.ratio=1, 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_bw()

# --------------------------------------
# RF Model 3------------------------
mtry <- tuneRF(TrainSet_eonr3[,c(no_cost_varb_trf, crop_varb), with = FALSE],TrainSet_eonr3$eonr, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]


rf3_eonr <- randomForest(eonr ~ ., data = TrainSet_eonr3[,c('eonr', no_cost_varb_trf, crop_varb), with = FALSE],
                            importance = TRUE , mtry = best.m, ntree=1000, nodesize = 20)
varImpPlot(rf3_eonr, type=2)
plot(rf3_eonr)


# Make predictions
TrainSet_eonr3$eonr_pred <- predict(rf3_eonr, TrainSet_eonr3)
ValidSet_eonr3$eonr_pred <- predict(rf3_eonr, ValidSet_eonr3)

# Model performance metrics Training Set &  Validation set
data.frame(
  RMSE_train = caret::RMSE(TrainSet_eonr3$eonr_pred, TrainSet_eonr3$eonr),
  Rsquare_train = caret::R2(TrainSet_eonr3$eonr_pred, TrainSet_eonr3$eonr),
  RMSE_valid = caret::RMSE(ValidSet_eonr3$eonr_pred, ValidSet_eonr3$eonr),
  Rsquare_valid = caret::R2(ValidSet_eonr3$eonr_pred, ValidSet_eonr3$eonr)
)

ggplot(TrainSet_eonr3, aes(x = n_0_60cm_v5, y = eonr))+
  geom_hex() + geom_smooth()

ggplot(ValidSet_eonr3, aes(x = n_0_60cm_v5, y = eonr_pred))+
  geom_hex() + geom_smooth()

ggplot()+
  geom_hex(data = TrainSet_eonr3, aes(x = eonr, y = eonr_pred), shape = 1, color = 'blue')+
  geom_hex(data = ValidSet_eonr3, aes(x = eonr, y = eonr_pred), shape = 2, color = 'green') +
  coord_fixed() + geom_abline() + ylim(0, 350)+ xlim(0, 350) +
  theme(aspect.ratio=1, 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_bw()

# --------------------------------------
# How many trees are needed to reach the minimum error estimate? 

which.min(rf3_eonr$mse)
model2b_eonr
importance(rf2_eonr)
varImpPlot(rf2_eonr)
plot(rf2_eonr)

 # --------------------------------------
## XBOOST 2----------------
# https://analyticsdataexploration.com/xgboost-model-tuning-in-crossvalidation-using-caret-in-r/
# https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/

ControlParamteres <- trainControl(method = "cv",
                                  number = 5,
                                  savePredictions = TRUE
)


parametersGrid <-  expand.grid(eta = 0.1, 
                               colsample_bytree=c(0.5,0.7),
                               max_depth=c(3,6),
                               nrounds=100,
                               gamma=2,
                               min_child_weight=2,
                               subsample = 0.5
)

parametersGrid <-  expand.grid(eta = 0.3, 
                               colsample_bytree=c(0.5),
                               max_depth=c(3),
                               nrounds=200,
                               gamma=2,
                               min_child_weight=2,
                               subsample = 0.5
)

xgboost_2 <- caret::train(eonr~., 
                      data = TrainSet_eonr3[,c("eonr", no_cost_varb_trf, ss_varb), with = FALSE],
                      method = "xgbTree",
                      trControl = ControlParamteres,
                      tuneGrid=parametersGrid)

# Make predictions
TrainSet_eonr3$eonr_pred <- predict(xgboost_2, TrainSet_eonr3)
ValidSet_eonr3$eonr_pred <- predict(xgboost_2, ValidSet_eonr3)

# Model performance metrics Training Set &  Validation set
data.frame(
  RMSE_train = caret::RMSE(TrainSet_eonr3$eonr_pred, TrainSet_eonr3$eonr),
  Rsquare_train = caret::R2(TrainSet_eonr3$eonr_pred, TrainSet_eonr3$eonr),
  RMSE_valid = caret::RMSE(ValidSet_eonr3$eonr_pred, ValidSet_eonr3$eonr),
  Rsquare_valid = caret::R2(ValidSet_eonr3$eonr_pred, ValidSet_eonr3$eonr)
)

ggplot(TrainSet_eonr3, aes(x = eonr, y = eonr_pred))+
  geom_point()

ggplot(ValidSet_eonr3, aes(x = eonr, y = eonr_pred))+
  geom_point()

ggplot()+
  geom_hex(data = TrainSet_eonr3, aes(x = eonr, y = eonr_pred), shape = 1, color = 'blue')+
  geom_hex(data = ValidSet_eonr3, aes(x = eonr, y = eonr_pred), shape = 2, color = 'green')

# --------------------------------------
importance(rf2_eonr_cat)

plot(rf2_eonr_cat)
#
jpeg("./vr_value_v2/Data/figures/rf1_eonr_variables.jpg")
varImpPlot(rf1_eonr, type=2, main = 'a) Regional RF1')
dev.off()
#
jpeg("./vr_value_v2/Data/figures/rf2_eonr_variables.jpg")
varImpPlot(rf2_eonr, type=2, main = 'b) Regional RF2')
dev.off()

jpeg("./vr_value_v2/Data/figures/rf3_eonr_variables.jpg")
varImpPlot(rf3_eonr, type=2, main = 'c) Regional RF3')
dev.off()

# reg_model_stuff[['model1b_eonr']] <-  model1b_eonr
# reg_model_stuff[['model2b_eonr']] <-  model2b_eonr

#==========================================================================================
# Plot response to N in the soil
ggplot(data = TrainSet2_eonr, aes(x = n_0_60cm_v5, y = eonr)) + 
  geom_point() + geom_smooth()

ggplot(data = TrainSet2_eonr, aes(x = Yld, y = eonr)) + 
  geom_hex() + geom_smooth()


# Section One #############################
## Section Two =================================
### Section Three ---------------------------------

#  ====================================================================================================================================

# SECTION ML FROM PAPER  ##########################################################
## PRINCIPAL COMPONENT REGRESSION =================================================
# http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/152-principal-component-and-partial-least-squares-regression-essentials/
library(caret)
TrainSet_eonr3 <- TrainSet2_eonr[,c('eonr', no_cost_varb, ss_varb, crop_varb), with = FALSE]
preProcValues <- preProcess(TrainSet_eonr3[,-'eonr'], method = c("center", "scale"))

TrainSet_eonr3_transformed <- predict(preProcValues, TrainSet_eonr3)

model <- train(
  eonr~., data = TrainSet_eonr3_transformed, method = "pcr",
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
predictions <- model %>% predict(TrainSet_eonr3_transformed)

# Model performance metrics
data.frame(
  RMSE = caret::RMSE(predictions, TrainSet_eonr3_transformed$eonr),
  Rsquare = caret::R2(predictions, TrainSet_eonr3_transformed$eonr)
)

setdiff(names(TrainSet_eonr3_transformed), names(ValidSet_eonr3))

ValidSet_eonr3_transformed <- predict(preProcValues, ValidSet_eonr3) 
# Make predictions
ValidSet_eonr3_transformed$eonr_pred <- model %>% predict(ValidSet_eonr3_transformed)

# Model performance metrics
data.frame(
  RMSE = caret::RMSE(ValidSet_eonr3_transformed$eonr_pred, ValidSet_eonr3_transformed$eonr),
  Rsquare = caret::R2(ValidSet_eonr3_transformed$eonr_pred, ValidSet_eonr3_transformed$eonr)
)
ggplot(ValidSet_eonr3_transformed, aes(x = eonr, y = eonr_pred))+
  geom_point()



#==========================================================================================
# Y plot with Yld at eonr
mukey_sample <- sample(unique(TrainSet2$mukey), 2)

ic_field_plot <- TrainSet2[mukey %in% mukey_sample]
performance_set_plot <- ic_field_plot[mukey %in% mukey_sample, .SD[ P == max( P)], by = .(id_10, mukey, z)]

(plot_n <- ggplot() +
    geom_point(data = performance_set_plot, aes(x = N_fert, y = P)) +
    # geom_point(data = performance_set_plot[rotation == 0 & model == 12], aes(x = N_fert, y = P), size = 3, show.legend = FALSE) +
    geom_line(data = ic_field_plot, aes(x = N_fert, y = P, group=interaction(mukey, z)), show.legend = FALSE) +
    ggtitle(paste('P plot with Yld at eonr')))

#===========================================================================================
# install.packages('mlr')
# install.packages('ranger')


# # TrainSet2_eonr <- eonr_mukey_dt[,c('eonr', no_cost_varb, ss_varb), with = FALSE]
# 
# # Create the tasks encapsulate the data set and further relevant information
# regr_task = makeRegrTask(data = TrainSet_eonr3, target = "eonr") #task = data

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
# train_data <- TrainSet2_eonr[,c('eonr', no_cost_varb, ss_varb), with = FALSE]
# ValidSet2[, P := Yld * Pc - N_fert * Pn]
# # ValidSet2[, n_0_60cm_v5 := n_20cm_v5 + n_40cm_v5 + n_60cm_v5]
# validation_data <- ValidSet2[, .SD[ P == max( P)], by = .(id_10, mukey, z)]
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
#---------------------
#Profits in the validation set ------
# transform the dataset using the parameters
ValidSet3 <- predict(preprocessParams, ValidSet2)

# Create dummy variables 
ValidSet3[, region.2 := ifelse(region == 2, 1, 0)]
ValidSet3[, region.3 := ifelse(region == 3, 1, 0)]
ValidSet3[, region := NULL]



results_list <- list()

results_list[[1]] <- merge(ValidSet3[,-'eonr_pred'] , model_minimum_regional, by = c('region')) %>% #here we join back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "Yld", "leach_n2", "n_deep_v5","N_fert",'P')] %>% .[,model := '1']

results_list[[2]] <- ValidSet3[,eonr_pred := round(predict(reg_lm1, newdata = ValidSet3)/10)*10] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'reg1']

results_list[[3]] <- ValidSet3[,eonr_pred := round(predict(reg_lm2, newdata = ValidSet3)/10)*10] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'reg2']

results_list[[4]] <- ValidSet3[,eonr_pred := round(predict(reg_lm3, newdata = ValidSet3)/10)*10] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'reg3']

results_list[[5]] <- ValidSet3[,eonr_pred := round(predict(reg_lm4, newdata = ValidSet3)/10)*10] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'reg4']

results_list[[6]] <- ValidSet3[,eonr_pred := round(predict(rf1_eonr, newdata = ValidSet3)/10)*10] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'rf1']

results_list[[7]] <- ValidSet3[,eonr_pred := round(predict(rf2_eonr, newdata = ValidSet3)/10)*10] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'rf2']

results_list[[8]] <- ValidSet3[,eonr_pred := round(predict(rf3_eonr, newdata = ValidSet3)/10)*10] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'rf3']

results_list[[9]] <- ValidSet3[,eonr_pred := round(predict(modelxgboost, newdata = ValidSet3)/10)*10] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'xgb2']

results_list[[10]] <- ValidSet3[,eonr_pred := predictrf_quantile(model = rf2_eonr, data = ValidSet3, quant = 0.70)] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'rf2_q70']

results_list[[11]] <- ValidSet3[,eonr_pred := predictrf_quantile(model = rf2_eonr, data = ValidSet3, quant = 0.8)] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'rf2_q80']

results_list[[13]] <- ValidSet3[,eonr_pred := predictrf_quantile(model = rf2_eonr, data = ValidSet3, quant = 0.6)] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'rf2_q60']

results_list[[NA]] <- ValidSet2[,eonr_pred := ceiling((reg_lm2_mukey_dt$intercept + reg_lm2_mukey_dt$n_0_60cm_v5 * n_0_60cm_v5)/10)*10] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'reg2_corrected']


results_list[[12]] <- ValidSet3[, .SD[ P == max( P)], by = .(id_10, mukey, z)] %>%
  .[,c("mukey", "z", "id_10", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := '12']

results_list[[14]] <- ValidSet3[,eonr_pred := round(predict(dtree_1, newdata = ValidSet3)/10)*10] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'dtree_1']

results_list[[15]] <- ValidSet3[,eonr_pred := round(predict(dtree_2, newdata = ValidSet3)/10)*10] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'dtree_2']

results_list[[16]] <- ValidSet3[,eonr_pred := round(predict(dtree_3, newdata = ValidSet3)/10)*10] %>% 
  .[ N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'dtree_3']


perfomances_dt <- rbindlist(results_list)

# Add the real eonr to calculate RMSE
perfomances_dt2 <- merge(perfomances_dt, perfomances_dt[model == '12', .(id_10, mukey, z, N_fert_12 = N_fert)], by = c("id_10", "mukey", "z"))

perfomances_dt2[,overpred := ifelse(N_fert > N_fert_12, 1, 0 )]


perfomances_dt2[, .(Yld =  mean(Yld),
                    leach_n2 = mean(leach_n2),
                    N_fert = mean(N_fert),
                    N_fert_min = min(N_fert),
                    N_fert_max = max(N_fert),
                    P = mean(P),
                    cor = cor(N_fert_12, N_fert),
                    RMSE = measureRMSE(truth = N_fert_12, response = N_fert),
                    overpred = sum(overpred)/.N), by = .( model)]

# reg_model_stuff <- list()
# reg_model_stuff[['model_minimum_regional']] <-  model_minimum_regional
# reg_model_stuff[['full_fields']] <-  full_fields_dt2[,-'rf']
# reg_model_stuff[['stations']] <-  stations_dt2[,-'rs']
# reg_model_stuff[['training_z']] <-  reg_model_stuff[['training_z']]

reg_model_stuff[['no_cost_var']] <-  no_cost_varb
reg_model_stuff[['ss_var']] <-  ss_varb
reg_model_stuff[['crop_varb']] <-  crop_varb
reg_model_stuff[['no_cost_varb_trf']] <-  no_cost_varb_trf
reg_model_stuff[['preprocessParams']] <-  preprocessParams

reg_model_stuff[['TrainSet_eonr3']] <-  TrainSet_eonr3
reg_model_stuff[['ValidSet_eonr3']] <-  ValidSet_eonr3

reg_model_stuff[['rf1_eonr']] <-  rf1_eonr
reg_model_stuff[['rf2_eonr']] <-  rf2_eonr
reg_model_stuff[['rf3_eonr']] <-  rf3_eonr
reg_model_stuff[['reg_lm1']] <-  reg_lm1
reg_model_stuff[['reg_lm2']] <-  reg_lm2
reg_model_stuff[['reg_lm3']] <-  reg_lm3
reg_model_stuff[['reg_lm4']] <-  reg_lm4
reg_model_stuff[['reg_lm4_mukey_dt']] <-  reg_lm4_mukey_dt
reg_model_stuff[['dtree_1']] <-  dtree_1
reg_model_stuff[['dtree_2']] <-  dtree_2
reg_model_stuff[['dtree_3']] <-  dtree_3
reg_model_stuff[['xgboost_2']] <-  xgboost_2

saveRDS(reg_model_stuff, "./vr_value_v2/Data/files_rds/reg_model_stuff.rds")
