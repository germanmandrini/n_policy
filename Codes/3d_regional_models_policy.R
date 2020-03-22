# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents') #CPSC
# setwd("/home/germanm2")
setwd('~')

# corchete alt 91 []
# llave { } alt 123 125 
source('./Codes_useful/R.libraries.R')
# source('./Codes_useful/gm_functions.R')

library(randomForest)
# library(ranger)
# library(mlr)
source('./n_policy/Codes/parameters.R')

# yc_yearly_dt3 <- readRDS("./n_policy/Data/files_rds/yc_yearly_dt3.rds")
# grid10_soils_dt5 <- readRDS("./n_policy/Data/Grid/grid10_soils_dt5.rds") %>% data.table()

reg_model_stuff <- readRDS( "./n_policy/Data/files_rds/reg_model_stuff.rds")
names(reg_model_stuff)
reg_model_stuff[['yr_0.95']]

remove_list <- names(reg_model_stuff)[!names(reg_model_stuff) %in% c("full_fields", "stations", "TrainSet","training_z")]
for(n in remove_list){
  reg_model_stuff[[n]] <- NULL
}

# stations_dt <- reg_model_stuff[['stations']]
# full_fields_dt <- reg_model_stuff[['full_fields']]

# ValidSet2 <- reg_model_stuff[['ValidSet']]
TrainSet2 <- reg_model_stuff[['TrainSet']]
# table(TrainSet2$z)
# rm(reg_model_stuff)

# =========================================================================================================================================================
## SET THE VARIABLES ========
no_cost_varb <- c('region', "rain_30", "rain_60", "rain_90",
                  "t_max_30", "t_max_60", "t_max_90", "t_min_30", "t_min_60",
                  "t_min_90", "Yld_prev", 'Yld_lt_avg', 'Yld_lt_min', 'Yld_lt_max', "lai_v5")

ss_varb <- c("n_0_60cm_v5","esw_pct_v5", "whc")


TrainSet2[, n_0_60cm_v5 := n_20cm_v5 + n_40cm_v5 + n_60cm_v5]


# =========================================================================================================================================================
# CREATE THE N RATIO TAX MODEL

ratio_seq <- seq(1, 20, by = 1)
set.seed(123)

for(ratio_n in ratio_seq){
  # ratio_n = 10
  small_model_list <- list()
  Pn_tmp = ratio_n * Pc
  print(Pn_tmp/Pc)
  TrainSet2[, P := Yld * Pc - N_fert * Pn_tmp]  #update profits

  # =========================================================================================================================================================
  # CREATE THE REGIONAL MINIMUM MODEL
  
  #Analysis included only responsive sites (sawyer 2006)
  TrainSet2[, Yld_response := max(Yld) - min(Yld), by = .(id_10, mukey,z)]
  TrainSet_RMM <- TrainSet2[Yld_response > 500]
  
  
  #Select a few rates
  #Alll this comes from https://rcompanion.org/handbook/I_11.html
  # N_rates_trial <- c(10, 90,170,250, 330)
  N_rates_trial <- seq(10,330,10)
  
  quadratic_dt <- TrainSet_RMM[,list(intercept=coef(lm(P ~ N_fert + I(N_fert^2)))[1], 
                                     coef1=coef(lm(P ~ N_fert + I(N_fert^2)))[2],
                                     coef2=coef(lm(P ~ N_fert + I(N_fert^2)))[3]),by=.(id_10, mukey,z, region)]
  
  # Expand and calculate P
  N_rates_int <- seq(min(N_rates_trial),max(N_rates_trial), by = 10)
  quadratic_dt2 <- quadratic_dt[rep(x = 1:nrow(quadratic_dt), each = length(N_rates_int))]
  
  
  
  quadratic_dt2[,N_fert := rep(N_rates_int, nrow(quadratic_dt))]
  quadratic_dt2[,P := intercept + coef1 * N_fert + coef2 * (N_fert^2)]
  # quadratic_dt2[,P:= Yld * Pc - N_fert * Pn]
  
  #Average all curves
  quadratic_dt3 <- quadratic_dt2[,.(P_avg = mean(P)), by = .(region, N_fert)]
  ggplot(quadratic_dt3) + geom_point(aes(x = N_fert, y = P_avg, colour = interaction(region)))
  
  #Select EONR
  model_minimum_regional <- quadratic_dt3[, .SD[ P_avg == max( P_avg)], by = .(region)][,.( region, N_fert)]
  setnames(model_minimum_regional, 'N_fert', 'eonr_pred')
  
  name_model = paste0('minimum')
  small_model_list[[name_model]] <- model_minimum_regional
  
  # =========================================================================================================================================================
  # CREATE THE REGIONAL MINIMUM MODEL - OK
  model_minimum_ok <- TrainSet2[, .(P = mean(P)), by = .(region, N_fert)][, .SD[ P == max( P)], by = .(region)][, .(region, eonr_pred = N_fert )]
  
  name_model = paste0('minimum_ok')
  small_model_list[[name_model]] <- model_minimum_ok
  
  
  # =========================================================================================================================================================
  ## PREPARE THE TRAINING DATA WITH EONR ========
  TrainSet_eonr <- TrainSet2[, .SD[ P == max( P)], by = .(id_10, mukey, z)]
  setnames(TrainSet_eonr, 'N_fert', 'eonr')
  
  TrainSet_eonr2 <- TrainSet_eonr[,c('eonr', no_cost_varb, ss_varb), with = FALSE]
  
  # =========================================================================================================================================================
  # RF Model 1------------------------
  
  # Create a Random Forest model with default parameters
  
  mtry <- tuneRF(TrainSet_eonr2[,c(no_cost_varb), with = FALSE],TrainSet_eonr2$eonr, ntreeTry=1000,
                 stepFactor=1.2,improve=0.01, trace=TRUE, plot=TRUE)
  
  best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
  
  rf1_eonr <- randomForest(eonr ~ ., data = TrainSet_eonr2[,c('eonr',no_cost_varb), with = FALSE],
                          importance = TRUE , mtry = best.m, ntree=1000) # , ntree=500, nodesize = 20
  
  name_model = paste0('rf1')
  small_model_list[[name_model]] <- rf1_eonr
  # --------------------------------------
  # RF Model 2------------------------
  mtry <- tuneRF(TrainSet_eonr2[,c(no_cost_varb, ss_varb), with = FALSE],TrainSet_eonr2$eonr, ntreeTry=1000,
                 stepFactor=1.2,improve=0.01, trace=TRUE, plot=TRUE)
  best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
  
  rf2_eonr <- randomForest(eonr ~ ., data = TrainSet_eonr2[,c('eonr', no_cost_varb, ss_varb), with = FALSE],
                              importance = TRUE , mtry = best.m, ntree=1000, nodesize = 20)
  
  varImpPlot(rf2_eonr, type=2)
  name_model = paste0('rf2')
  small_model_list[[name_model]] <- rf2_eonr
  
  # --------------------------------------
  # Save it to the big list
  name_model = paste0('ratio_', ratio_n)
  reg_model_stuff[[name_model]] <- small_model_list
  names(reg_model_stuff)
}

# =========================================================================================================================================================
# CREATE THE LEACHING FEE MODEL
fee_seq <- seq(0, Pe_max, by = 2)
source('./n_policy/Codes/parameters.R')
set.seed(123)

for(fee_n in fee_seq){
  # fee_n = 0
  print(fee_n)
  small_model_list <- list()
  TrainSet2[, P := Yld * Pc - N_fert * Pn - leach_n2 * fee_n] #update profits
  
  # =========================================================================================================================================================
  # CREATE THE REGIONAL MINIMUM MODEL
  
  #Analysis included only responsive sites (sawyer 2006)
  TrainSet2[, Yld_response := max(Yld) - min(Yld), by = .(id_10, mukey,z)]
  TrainSet_RMM <- TrainSet2[Yld_response > 500]
  
  #Select a few rates
  #Alll this comes from https://rcompanion.org/handbook/I_11.html
  # N_rates_trial <- c(10, 90,170,250, 330)
  N_rates_trial <- seq(10,330,10)
  
  quadratic_dt <- TrainSet_RMM[,list(intercept=coef(lm(P ~ N_fert + I(N_fert^2)))[1], 
                                     coef1=coef(lm(P ~ N_fert + I(N_fert^2)))[2],
                                     coef2=coef(lm(P ~ N_fert + I(N_fert^2)))[3]),by=.(id_10, mukey,z, region)]
  
  # Expand and calculate P
  N_rates_int <- seq(min(N_rates_trial),max(N_rates_trial), by = 10)
  quadratic_dt2 <- quadratic_dt[rep(x = 1:nrow(quadratic_dt), each = length(N_rates_int))]
  quadratic_dt2[,N_fert := rep(N_rates_int, nrow(quadratic_dt))]
  quadratic_dt2[,P := intercept + coef1 * N_fert + coef2 * (N_fert^2)]
  # quadratic_dt2[,P:= Yld * Pc - N_fert * Pn]
  
  #Average all curves
  quadratic_dt3 <- quadratic_dt2[,.(P_avg = mean(P)), by = .(region, N_fert)]
  ggplot(quadratic_dt3) + geom_point(aes(x = N_fert, y = P_avg, colour = interaction(region)))
  
  #Select EONR
  model_minimum_regional <- quadratic_dt3[, .SD[ P_avg == max( P_avg)], by = .(region)][,.( region, eonr_pred = N_fert)]
  
  name_model = paste0('minimum')
  small_model_list[[name_model]] <- model_minimum_regional
  # =========================================================================================================================================================
  # CREATE THE REGIONAL MINIMUM MODEL - OK
  model_minimum_ok <- TrainSet2[, .(P = mean(P)), by = .(region, N_fert)] %>% 
    .[, .SD[ P == max( P)], by = .(region)] %>% .[,.(region, eonr_pred = N_fert)]
  
  name_model = paste0('minimum_ok')
  small_model_list[[name_model]] <- model_minimum_ok
  
  # =========================================================================================================================================================
  ## PREPARE THE TRAINING DATA WITH EONR ========
  TrainSet_eonr <- TrainSet2[, .SD[ P == max( P)], by = .(id_10, mukey, z)]
  setnames(TrainSet_eonr, 'N_fert', 'eonr')
  
  TrainSet_eonr2 <- TrainSet_eonr[,c('eonr', no_cost_varb, ss_varb), with = FALSE]
  
  # =========================================================================================================================================================
  # RF Model 1------------------------
  
  # Create a Random Forest model with default parameters
  
  mtry <- tuneRF(TrainSet_eonr2[,c(no_cost_varb), with = FALSE],TrainSet_eonr2$eonr, ntreeTry=1000,
                 stepFactor=1.2,improve=0.01, trace=TRUE, plot=TRUE)
  
  best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
  
  rf1_eonr <- randomForest(eonr ~ ., 
                           data = TrainSet_eonr2[,c('eonr',no_cost_varb), with = FALSE],
                           importance = TRUE , mtry = best.m, ntree=1000) # , ntree=500, nodesize = 20
  
  name_model = paste0('rf1')
  small_model_list[[name_model]] <- rf1_eonr
  
  # --------------------------------------
  # RF Model 2------------------------
  mtry <- tuneRF(TrainSet_eonr2[,c(no_cost_varb, ss_varb), with = FALSE],TrainSet_eonr2$eonr, ntreeTry=1000,
                 stepFactor=1.2,improve=0.01, trace=TRUE, plot=TRUE)
  best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
  
  rf2_eonr <- randomForest(eonr ~ ., data = TrainSet_eonr2[,c('eonr', no_cost_varb, ss_varb), with = FALSE],
                           importance = TRUE , mtry = best.m, ntree=1000, nodesize = 20)
  
  varImpPlot(rf2_eonr, type=2)
  
  name_model = paste0('rf2')
  small_model_list[[name_model]] <- rf2_eonr
  # --------------------------------------
  # Save it to the big list
  name_model = paste0('fee_', fee_n)
  reg_model_stuff[[name_model]] <- small_model_list
  # names(reg_model_stuff)
}

# =========================================================================================================================================================
# CREATE THE N LEACHING REDUCTION
set.seed(123)

## PREPARE THE TRAINING DATA WITH EONR N75========
# Part 1
TrainSet2[, P := Yld * Pc - N_fert * Pn] #update profits
baseline_leaching <- merge(TrainSet2, reg_model_stuff[['fee_0']]$minimum, by = 'region') %>% .[N_fert == eonr_pred] %>% .[,.(id_10, mukey, z, leach_base = leach_n2)]
TrainSet_nr <- merge(TrainSet2, baseline_leaching, by = c('id_10', 'mukey', 'z'))
TrainSet_nr[,leach_rel := leach_n2/leach_base]

red_seq <- seq(0.55, 1, by = 0.05)
for(n_red in red_seq){
  # n_red = 0.50
  print(n_red)
  small_model_list <- list()

  # CREATE THE REGIONAL MINIMUM MODEL
  model_minimum_ok <- TrainSet_nr[, .(leach_rel = mean(leach_rel)), by = .(N_fert , region)]
  
  ggplot(model_minimum_ok) + geom_line(aes(x = N_fert, y = leach_rel, colour = factor(region)))
  
  model_minimum_ok <- model_minimum_ok[leach_rel >= n_red] %>%
    .[, .SD[ leach_rel == min( leach_rel)], by = .(region)] %>% #select minimum leach_rel
    .[, .SD[ N_fert == min( N_fert)], by = .(region)] %>% #select minimum rate in case one is repeated
    .[,.(region, eonr_pred = N_fert)]
    
    
  name_model = paste0('minimum_ok')
  small_model_list[[name_model]] <- model_minimum_ok

  ## PREPARE THE TRAINING DATA WITH EONR ========
  # Part 2
  TrainSet_nr[,.N, by = .(id_10, mukey, z)]
  TrainSet_nr_tmp <- TrainSet_nr[leach_rel >= n_red & leach_rel <= 1] %>% 
    .[, .SD[ leach_rel == min( leach_rel)], by = .(id_10, mukey, z)] %>% #select minimum leach_rel
    .[, .SD[ N_fert == min( N_fert)], by = .(id_10, mukey, z)] %>% #select minimum rate in case one is repeated
    setnames('N_fert', 'eonr')
  TrainSet_nr_tmp <- TrainSet_nr_tmp[,c('eonr', no_cost_varb, ss_varb), with = FALSE]

  # =========================================================================================================================================================
  # RF Model 1------------------------
  
  # Create a Random Forest model with default parameters
  
  mtry <- tuneRF(TrainSet_nr_tmp[,c(no_cost_varb), with = FALSE],TrainSet_nr_tmp$eonr, ntreeTry=1000,
                 stepFactor=1.2,improve=0.01, trace=TRUE, plot=TRUE)
  
  best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
  
  rf1_eonr <- randomForest(eonr ~ ., data = TrainSet_nr_tmp[,c('eonr',no_cost_varb), with = FALSE],
                           importance = TRUE , mtry = best.m, ntree=1000) # , ntree=500, nodesize = 20
  
  name_model = paste0('rf1')
  small_model_list[[name_model]] <- rf1_eonr
  # --------------------------------------
  # RF Model 2------------------------
  mtry <- tuneRF(TrainSet_nr_tmp[,c(no_cost_varb, ss_varb), with = FALSE],TrainSet_nr_tmp$eonr, ntreeTry=1000,
                 stepFactor=1.2,improve=0.01, trace=TRUE, plot=TRUE)
  best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
  
  rf2_eonr <- randomForest(eonr ~ ., data = TrainSet_nr_tmp[,c('eonr', no_cost_varb, ss_varb), with = FALSE],
                           importance = TRUE , mtry = best.m, ntree=1000, nodesize = 20)
  
  varImpPlot(rf2_eonr, type=2)
  name_model = paste0('rf2')
  small_model_list[[name_model]] <- rf2_eonr

  name_model = paste0('nred_', n_red)
  reg_model_stuff[[name_model]] <- small_model_list
  names(reg_model_stuff)
}

#==========================================================================================
# CREATE THE YR MODEL
set.seed(123)
Y_rel_seq <- c(seq(0.5,0.9, by = 0.05), seq(0.91,1, by = 0.01))

TrainSet2[,Yld_max := max(Yld), by = .(id_10, mukey, z)]
TrainSet2[,Yld_rel := Yld/Yld_max]
TrainSet2[,P := Yld * Pc - N_fert * Pn]

for(yr_n in Y_rel_seq){
  # yr_n = 0.5
  print(yr_n)
  small_model_list <- list()
  # =========================================================================================================================================================
  # CREATE THE REGIONAL MINIMUM MODEL
  
  #Analysis included only responsive sites (sawyer 2006)
  TrainSet2[, Yld_response := max(Yld) - min(Yld), by = .(id_10, mukey,z)]
  TrainSet_RMM <- TrainSet2[Yld_response > 500]
  
  
  #Select a few rates
  #Alll this comes from https://rcompanion.org/handbook/I_11.html
  # N_rates_trial <- c(10, 90,170,250, 330)
  N_rates_trial <- seq(10,330,10)
  
  quadratic_dt <- TrainSet_RMM[,list(intercept=coef(lm(Yld_rel ~ N_fert + I(N_fert^2)))[1], 
                                     coef1=coef(lm(Yld_rel ~ N_fert + I(N_fert^2)))[2],
                                     coef2=coef(lm(Yld_rel ~ N_fert + I(N_fert^2)))[3]),by=.(id_10, mukey,z, region)]
  
  # Expand and calculate P
  N_rates_int <- seq(min(N_rates_trial),max(N_rates_trial), by = 10)
  quadratic_dt2 <- quadratic_dt[rep(x = 1:nrow(quadratic_dt), each = length(N_rates_int))]
  
  
  
  quadratic_dt2[,N_fert := rep(N_rates_int, nrow(quadratic_dt))]
  quadratic_dt2[,Yld := intercept + coef1 * N_fert + coef2 * (N_fert^2)]
  # quadratic_dt2[,P:= Yld * Pc - N_fert * Pn]
  
  #Average all curves
  quadratic_dt3 <- quadratic_dt2[,.(Yld_avg = mean(Yld)), by = .(region, N_fert)]
  ggplot(quadratic_dt3) + geom_point(aes(x = N_fert, y = Yld_avg, colour = interaction(region)))
  
  #Select EONR
  quadratic_dt3 <- quadratic_dt3[Yld_avg >= yr_n]
  quadratic_dt3 <- quadratic_dt3[, .SD[ N_fert == min( N_fert)], by = .(region)] #in case two rates had equal profits
  
  model_minimum_regional <- quadratic_dt3[,.( region, eonr_pred = N_fert)]
  
  name_model = paste0('minimum')
  small_model_list[[name_model]] <- model_minimum_regional
  
  # =========================================================================================================================================================
  # CREATE THE REGIONAL MINIMUM MODEL - OK
  model_minimum_ok <- TrainSet2[,.(Yld = mean(Yld)), by = .(region, N_fert)] 
  model_minimum_ok[,Yld_max := max(Yld), by = .(region)]
  model_minimum_ok[,Yld_rel := Yld/Yld_max]
  
  model_minimum_ok2 <- model_minimum_ok[Yld_rel >= yr_n]
  model_minimum_ok2 <- model_minimum_ok2[, .SD[ N_fert == min( N_fert)], by = .(region)][,.(region, eonr_pred = N_fert)] #in case two rates had equal profits
  
  name_model = paste0('minimum_ok')
  small_model_list[[name_model]] <- model_minimum_ok2
  
  # =========================================================================================================================================================
  ## PREPARE THE TRAINING DATA WITH EONR ========
  TrainSet_eonr <- TrainSet2[Yld_rel >= yr_n] %>% 
    .[, .SD[ N_fert == min( N_fert)], by = .(id_10, mukey, z)] #in case two rates had equal profits
  setnames(TrainSet_eonr, 'N_fert', 'eonr')
  summary(TrainSet_eonr$eonr)
  
  TrainSet_eonr2 <- TrainSet_eonr[,c('eonr', no_cost_varb, ss_varb), with = FALSE]
  
  # =========================================================================================================================================================
  # RF Model 1------------------------
  
  # Create a Random Forest model with default parameters
  
  mtry <- tuneRF(TrainSet_eonr2[,c(no_cost_varb), with = FALSE],TrainSet_eonr2$eonr, ntreeTry=100,
                 stepFactor=1.2,improve=0.01, trace=TRUE, plot=TRUE)
  
  best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
  
  rf1_eonr <- randomForest(eonr ~ ., data = TrainSet_eonr2[,c('eonr',no_cost_varb), with = FALSE],
                           importance = TRUE , mtry = best.m, ntree=1000) # , ntree=500, nodesize = 20
  
  name_model = paste0('rf1')
  small_model_list[[name_model]] <- rf1_eonr
  # --------------------------------------
  # RF Model 2------------------------
  mtry <- tuneRF(TrainSet_eonr2[,c(no_cost_varb, ss_varb), with = FALSE],TrainSet_eonr2$eonr, ntreeTry=1000,
                 stepFactor=1.2,improve=0.01, trace=TRUE, plot=TRUE)
  best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
  
  rf2_eonr <- randomForest(eonr ~ ., data = TrainSet_eonr2[,c('eonr', no_cost_varb, ss_varb), with = FALSE],
                           importance = TRUE , mtry = best.m, ntree=1000, nodesize = 20)
  
  varImpPlot(rf2_eonr, type=2)
  name_model = paste0('rf2')
  small_model_list[[name_model]] <- rf2_eonr
  # --------------------------------------
  # Save it to the big list
  name_model = paste0('yr_', yr_n)
  reg_model_stuff[[name_model]] <- small_model_list
  names(reg_model_stuff)
}

# =========================================================================================================================================================
names(reg_model_stuff)


reg_model_stuff[['no_cost_var']] <-  no_cost_varb
reg_model_stuff[['ss_var']] <-  ss_varb
# reg_model_stuff[['crop_varb']] <-  crop_varb
# reg_model_stuff[['no_cost_varb_trf']] <-  no_cost_varb
# reg_model_stuff[['preprocessParams']] <-  preprocessParams
# 
# reg_model_stuff_tmp[['TrainSet_eonr3']] <-  TrainSet_eonr3
# 
# reg_model_stuff_tmp[['rf1_eonr']] <-  rf1_eonr
# reg_model_stuff_tmp[['rf2_eonr']] <-  rf2_eonr
# reg_model_stuff_tmp[['rf3_eonr']] <-  rf3_eonr
# reg_model_stuff_tmp[['reg_lm4']] <-  reg_lm4

saveRDS(reg_model_stuff, "./n_policy/Data/files_rds/reg_model_stuff.rds")
