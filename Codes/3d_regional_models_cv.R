# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents') #CPSC
# setwd("/home/germanm2")
# setwd('~')

# corchete alt 91 []
# llave { } alt 123 125 
# source('./Codes_useful/R.libraries.R')
# source('./Codes_useful/gm_functions.R')

library(randomForest)
# library(ranger)
library(mlr)
# source('./vr_value_v2/Codes/parameters.R')

yc_yearly_dt3 <- readRDS("./vr_value_v2/Data/files_rds/yc_yearly_dt3.rds")
# grid10_soils_dt5 <- readRDS("./vr_value_v2/Data/Grid/grid10_soils_dt5.rds") %>% data.table()

reg_model_stuff <- readRDS( "./vr_value_v2/Data/files_rds/reg_model_stuff.rds")
# stations_dt <- reg_model_stuff[['stations']]
# full_fields_dt <- reg_model_stuff[['full_fields']]

# ValidSet2 <- reg_model_stuff[['ValidSet']]
TrainSet2 <- reg_model_stuff[['TrainSet']]
# table(TrainSet2$z)
# rm(reg_model_stuff)
#==========================================================================================
yc_yearly_dt3[, .(n_deep_v5 = mean(n_deep_v5)), by = z][order(as.numeric(z))]

# =========================================================================================================================================================
# CREATE THE REGIONAL MINIMUM MODEL

#Analysis included only responsive sites (sawyer 2006)
TrainSet2[, Yld_response := max(Yld) - min(Yld), by = .(id_10, mukey,z)]
TrainSet_RMM <- TrainSet2[Yld_response > 500]


#Select a few rates
#Alll this comes from https://rcompanion.org/handbook/I_11.html
# N_rates_trial <- c(10, 90,170,250, 330)
N_rates_trial <- seq(10,330,10)

quadratic_dt <- TrainSet_RMM[,list(intercept=coef(lm(Yld~N_fert + I(N_fert^2)))[1], 
                                   coef1=coef(lm(Yld ~ N_fert + I(N_fert^2)))[2],
                                   coef2=coef(lm(Yld ~ N_fert + I(N_fert^2)))[3]),by=.(id_10, mukey,z, region)]

# Expand and calculate yield
N_rates_int <- seq(min(N_rates_trial),max(N_rates_trial), by = 10)
quadratic_dt2 <- quadratic_dt[rep(x = 1:nrow(quadratic_dt), each = length(N_rates_int))]



quadratic_dt2[,N_fert := rep(N_rates_int, nrow(quadratic_dt))]
quadratic_dt2[,Yld := intercept + coef1 * N_fert + coef2 * (N_fert^2)]
quadratic_dt2[,P:= Yld * Pc - N_fert * Pn]

#Average all curves
quadratic_dt3 <- quadratic_dt2[,.(P_avg = mean(P)), by = .(region, N_fert)]
ggplot(quadratic_dt3) + geom_point(aes(x = N_fert, y = P_avg, colour = interaction(region)))

#Select EONR
model_minimum_regional <- quadratic_dt3[, .SD[ P_avg == max( P_avg)], by = .(region)][,.( region, N_fert)]
setnames(model_minimum_regional, 'N_fert', 'eonr_pred')


# reg_model_stuff <- readRDS("./vr_value_v2/Data/files_rds/reg_model_stuff.rds")

reg_model_stuff_tmp[['model_minimum_regional']] <-  model_minimum_regional

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
if(TRUE){
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
  # saveRDS(TrainSet_eonr2, "./vr_value_v2/Data/files_rds/TrainSet_eonr2.rds")
}
# TrainSet_eonr2 <- readRDS("./vr_value_v2/Data/files_rds/TrainSet_eonr2.rds")

ggplot(data = TrainSet_eonr2[n_0_60cm_v5 <200], aes(x = n_0_60cm_v5, y = eonr)) + 
  geom_point() + geom_smooth(formula = 'y ~ x', method = lm)

# =========================================================================================================================================================
library(caret)
# library(knitr)     # just using this for kable() to make pretty tables

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
# ValidSet_eonr3 <- predict(preprocessParams, ValidSet_eonr2)
# 
# # Create dummy variables using caret
# ValidSet_eonr3[, region.2 := ifelse(region == 2, 1, 0)]
# ValidSet_eonr3[, region.3 := ifelse(region == 3, 1, 0)]
# ValidSet_eonr3[, region := NULL]

no_cost_varb_trf <- c('region.2', 'region.3', "rain_30", "rain_60", "rain_90",
                  "t_max_30", "t_max_60", "t_max_90", "t_min_30", "t_min_60",
                  "t_min_90", "Yld_prev", 'Yld_lt_avg', 'Yld_lt_min', 'Yld_lt_max', "lai_v5")

#==========================================================================================
# Regression 4: Check a regression old school method

reg_lm4 <- lm(data = TrainSet_eonr3, formula = "eonr  ~  n_0_60cm_v5")
summary(reg_lm4)


# =========================================================================================================================================================
# RF Model 1------------------------

# Create a Random Forest model with default parameters

mtry <- tuneRF(TrainSet_eonr3[,c(no_cost_varb_trf), with = FALSE],TrainSet_eonr3$eonr, ntreeTry=500,
               stepFactor=1.2,improve=0.01, trace=TRUE, plot=TRUE)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

rf1_eonr <- randomForest(eonr ~ ., data = TrainSet_eonr3[,c('eonr',no_cost_varb_trf), with = FALSE],
                        importance = TRUE , mtry = best.m, ntree=1000) # , ntree=500, nodesize = 20

names(TrainSet_eonr3[,c('eonr',no_cost_varb_trf), with = FALSE])



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



# --------------------------------------
# RF Model 3------------------------
mtry <- tuneRF(TrainSet_eonr3[,c(no_cost_varb_trf, crop_varb), with = FALSE],TrainSet_eonr3$eonr, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]


rf3_eonr <- randomForest(eonr ~ ., data = TrainSet_eonr3[,c('eonr', no_cost_varb_trf, crop_varb), with = FALSE],
                            importance = TRUE , mtry = best.m, ntree=1000, nodesize = 20)
varImpPlot(rf3_eonr, type=2)
plot(rf3_eonr)



#==========================================================================================

reg_model_stuff[['no_cost_var']] <-  no_cost_varb
reg_model_stuff[['ss_var']] <-  ss_varb
reg_model_stuff[['crop_varb']] <-  crop_varb
reg_model_stuff[['no_cost_varb_trf']] <-  no_cost_varb_trf
reg_model_stuff[['preprocessParams']] <-  preprocessParams

reg_model_stuff_tmp[['TrainSet_eonr3']] <-  TrainSet_eonr3

reg_model_stuff_tmp[['rf1_eonr']] <-  rf1_eonr
reg_model_stuff_tmp[['rf2_eonr']] <-  rf2_eonr
reg_model_stuff_tmp[['rf3_eonr']] <-  rf3_eonr
reg_model_stuff_tmp[['reg_lm4']] <-  reg_lm4

# saveRDS(reg_model_stuff, "./vr_value_v2/Data/files_rds/reg_model_stuff.rds")
