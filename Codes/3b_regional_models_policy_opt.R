# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents') #CPSC
# setwd("/home/germanm2")
setwd('~')
rm(list=ls())
# corchete alt 91 []
# llave { } alt 123 125 
source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')

library(randomForest)
# library(ranger)
# library(mlr)
source('./n_policy_git/Codes/parameters.R')

# yc_yearly_dt3 <- readRDS("./n_policy_box/Data/files_rds/yc_yearly_dt3.rds")
# grid10_soils_dt5 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt5.rds") %>% data.table()

reg_model_stuff <- readRDS( "./n_policy_box/Data/files_rds/reg_model_stuff.rds")
names(reg_model_stuff)
reg_model_stuff[['ratio_5']]


remove_list <- names(reg_model_stuff)[!names(reg_model_stuff) %in% c("full_fields", "stations", "TrainSet","training_z", "ratio_5")]
for(n in remove_list){
  reg_model_stuff[[n]] <- NULL
}

# stations_dt <- reg_model_stuff[['stations']]
# full_fields_dt <- reg_model_stuff[['full_fields']]

# ValidSet2 <- reg_model_stuff[['ValidSet']]

# table(TrainSet2$z)
# rm(reg_model_stuff)

# =========================================================================================================================================================
## SET THE VARIABLES ========
TrainSet2 <- reg_model_stuff[['TrainSet']]

# TrainSet2 <- TrainSet2[z!=4]

hist(TrainSet2$n_0_60cm_v5)

low_var <- c("rain_30", "rain_60", "rain_90",
             "t_max_30", "t_max_60", "t_max_90", "t_min_30", "t_min_60",
             "t_min_90", "Y_prev", 'Y_corn_lt_avg', "day_sow", "day_v5", "lai_v5")#'Y_corn_lt_min', 'Y_corn_lt_max', 

high_var <- c("whc",  "oc_20cm_v5", "sw_dep_v5", "n_0_60cm_v5",  "surfaceom_wt_v5", "sand_40cm", "clay_40cm") #"root_wt_v5",, "n_deep_v5", "esw_pct_v5", 

TrainSet2[, n_0_60cm_v5 := n_20cm_v5 + n_40cm_v5 + n_60cm_v5]
TrainSet2[,L := L1 + L2]
TrainSet2[, Yld_response := max(Y_corn) - min(Y_corn), by = .(id_10, mukey,z)]

# Limit the trials included in MRTN
soy_profits <- FALSE
if(soy_profits){
  Yld_response_threshold <- 1000 
}else{
  Yld_response_threshold <- 2000  
}

# =========================================================================================================================================================
# CREATE THE N RATIO TAX MODEL

ratio_seq <- sort(c(seq(5, 20, by = 1), 7.5, 8.5, 11.5,12.5))
# ratio_seq <- sort(c(seq(5, 20, by = 5)))
# ratio_seq <- c(5)
set.seed(123)

for(ratio_n in ratio_seq){
  # ratio_n = Pn/Pc
  # ratio_n = 5
  name_model = paste0('ratio_', ratio_n)
  if(name_model %in% names(reg_model_stuff)){next}
  
  small_model_list <- list()
  Pn_tmp = ratio_n * Pc
  print(Pn_tmp/Pc)
  # TrainSet2[, P := Y_corn * Pc + Y_soy * Ps - N_fert * Pn_tmp]  #update profits
  TrainSet2[, P := Y_corn * Pc - N_fert * Pn_tmp]  #update profits
  # =========================================================================================================================================================
  # CREATE THE REGIONAL MINIMUM MODEL
  
  # #Analysis included only responsive sites (sawyer 2006)
  # TrainSet2[, Y_corn_response := max(Y_corn) - min(Y_corn), by = .(id_10, mukey,z)]
  # TrainSet_RMM <- TrainSet2[Y_corn_response > 500]
  # 
  # #Select a few rates
  # #Alll this comes from https://rcompanion.org/handbook/I_11.html
  # # N_rates_trial <- c(10, 90,170,250, 330)
  # N_rates_trial <- seq(10,330,10)
  # 
  # quadratic_dt <- TrainSet_RMM[,list(intercept=coef(lm(P ~ N_fert + I(N_fert^2)))[1], 
  #                                    coef1=coef(lm(P ~ N_fert + I(N_fert^2)))[2],
  #                                    coef2=coef(lm(P ~ N_fert + I(N_fert^2)))[3]),by=.(id_10, mukey,z, region)]
  # 
  # # Expand and calculate P
  # N_rates_int <- seq(min(N_rates_trial),max(N_rates_trial), by = 10)
  # quadratic_dt2 <- quadratic_dt[rep(x = 1:nrow(quadratic_dt), each = length(N_rates_int))]
  # 
  # 
  # 
  # quadratic_dt2[,N_fert := rep(N_rates_int, nrow(quadratic_dt))]
  # quadratic_dt2[,P := intercept + coef1 * N_fert + coef2 * (N_fert^2)]
  # # quadratic_dt2[,P:= Y_corn * Pc - N_fert * Pn]
  # 
  # #Average all curves
  # quadratic_dt3 <- quadratic_dt2[,.(P_avg = mean(P)), by = .(region, N_fert)]
  # ggplot(quadratic_dt3) + geom_point(aes(x = N_fert, y = P_avg, colour = interaction(region)))
  # 
  # #Select EONR
  # model_minimum_regional <- quadratic_dt3[, .SD[ P_avg == max( P_avg)], by = .(region)][,.( region, N_fert)]
  # setnames(model_minimum_regional, 'N_fert', 'eonr_pred')
  # 
  # name_model = paste0('minimum')
  # small_model_list[[name_model]] <- model_minimum_regional
  
  # =========================================================================================================================================================
  # CREATE THE REGIONAL MINIMUM MODEL - OK
  TrainSet_RMM <- TrainSet2[Yld_response > Yld_response_threshold] #Needs to be here, to use updated profits 
  
  TrainSet2[,.N, .(id_10, mukey, z)] %>% nrow() #trials before (all of them)
  TrainSet_RMM[,.N, .(id_10, mukey, z)] %>% nrow()#trials after (whith response > threshold)
  
  model_minimum_ok  <- aggregate_by_area(data_dt = TrainSet_RMM, variables = c('P'), 
                                         weight = 'area_ha', by_c = c('region', 'N_fert')) %>% 
    .[, .SD[ P == max( P)], by = .(region)] %>% .[,.(region, eonr_pred = N_fert)]
  
  
  name_model = paste0('minimum_ok')
  small_model_list[[name_model]] <- model_minimum_ok
  
  # =========================================================================================================================================================
  ## PREPARE THE TRAINING DATA WITH EONR ========
  TrainSet_eonr <- TrainSet2[, .SD[ P == max( P)], by = .(id_10, mukey, z)]
  setnames(TrainSet_eonr, 'N_fert', 'eonr')
  
  TrainSet_eonr2 <- TrainSet_eonr[,c('eonr', low_var, high_var), with = FALSE]
  
  # =========================================================================================================================================================
  # RF Model 1------------------------
  
  # Create a Random Forest model with default parameters
  
  # mtry <- tuneRF(TrainSet_eonr2[,c(low_var), with = FALSE],TrainSet_eonr2$eonr, ntreeTry=1000,
  #                stepFactor=1.2,improve=0.01, trace=TRUE, plot=TRUE)
  # 
  # best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
  # best.m = 6
  # 
  # rf1_eonr <- randomForest(eonr ~ ., data = TrainSet_eonr2[,c('eonr',low_var), with = FALSE],
  #                         importance = TRUE , mtry = best.m, ntree=1000) # , ntree=500, nodesize = 20
  # 
  # varImpPlot(rf1_eonr, type=2)
  # name_model = paste0('rf1')
  # small_model_list[[name_model]] <- rf1_eonr
  
  # --------------------------------------
  # RF Model 2------------------------
  # mtry <- tuneRF(TrainSet_eonr2[,c(low_var, high_var), with = FALSE],TrainSet_eonr2$eonr, ntreeTry=1000,
  #                 stepFactor=1.2,improve=0.01, trace=TRUE, plot=TRUE) # ,mtryStart = 5
  # best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
  best.m = 6
  
  rf2_eonr <- randomForest(eonr ~ ., data = TrainSet_eonr2[,c('eonr', low_var, high_var), with = FALSE],
                           importance = TRUE , mtry = best.m, ntree=2000, nodesize = 30)
 
  varImpPlot(rf2_eonr, type=2)
  plot(rf2_eonr)
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
source('./n_policy_git/Codes/parameters.R')
# fee_seq <- sort(c(seq(0, 10, by = 2)))
fee_seq <- sort(c(seq(0, 15, by = 1)))
# fee_seq <- c(0,4)
length(fee_seq)
set.seed(123)

# CHECK IF THE DATA FOR CURRENT RATIO IS THE SAME THAN FEE_0
# test_comp_dt <- merge(test_comp[[1]][,.(id_10, mukey, z, N_fert, Y_corn, L)], 
#       test_comp[[2]][,.(id_10, mukey, z, N_fert, Y_corn, L)], by = c('id_10', 'mukey', 'z', 'N_fert'))
# 
# test_comp_dt[,Y_corn_same := (Y_corn.x == Y_corn.y)]
# test_comp_dt[,leach_same := (L.x == L.y)]
# table(test_comp_dt$Y_corn_same)
# table(test_comp_dt$leach_same)

for(fee_n in fee_seq){
  # fee_n = 0
  print(fee_n)
  
  name_model = paste0('fee_', fee_n)
  if(name_model %in% names(reg_model_stuff)){next}
  
  small_model_list <- list()
  TrainSet2[, P := Y_corn * Pc - N_fert * Pn - L * fee_n] #update profits
  
  # =========================================================================================================================================================
  # CREATE THE REGIONAL MINIMUM MODEL
  
  #Analysis included only responsive sites (sawyer 2006)
  # TrainSet2[, Y_corn_response := max(Y_corn) - min(Y_corn), by = .(id_10, mukey,z)]
  # TrainSet_RMM <- TrainSet2[Y_corn_response > 500]
  # 
  # #Removes N_fert closer to the max yield
  # TrainSet_RMM[ , Y_corn_plateau_max := max(Y_corn), by = .(id_10, mukey,z)]
  # TrainSet_RMM[ , Y_corn_plateau_dist := Y_corn_plateau_max - Y_corn]
  # TrainSet_RMM[ Y_corn_plateau_dist > 30, Y_corn_area := 'quadratic']
  # TrainSet_RMM[is.na(Y_corn_area), Y_corn_area := 'plateau']
  # 
  # #Select a few rates
  # #Alll this comes from https://rcompanion.org/handbook/I_11.html
  # 
  # unique(TrainSet_RMM$id_10)
  # ggplot(TrainSet_RMM[id_10 == 761]) + geom_point(aes(x = N_fert, y = Y_corn, colour = interaction(id_10, mukey, z)))
  # 
  # #Fit a line for P points in each trial (soil,z)
  # quadratic_dt <- TrainSet_RMM[Y_corn_area == 'quadratic', list(intercept=coef(lm(Y_corn ~ N_fert + I(N_fert^2)))[1], 
  #                                    coef1=coef(lm(Y_corn ~ N_fert + I(N_fert^2)))[2],
  #                                    coef2=coef(lm(Y_corn ~ N_fert + I(N_fert^2)))[3]), by=.(id_10, mukey,z)]
  # 
  # # Calculate Y_corn with new regression line
  # TrainSet_RMM <- merge(TrainSet_RMM, quadratic_dt, by = c('id_10', 'mukey', 'z'))
  # TrainSet_RMM[Y_corn_area == 'plateau', Y_corn := mean(Y_corn), by = c('id_10', 'mukey', 'z')]
  # TrainSet_RMM[Y_corn_area == 'quadratic', Y_corn := intercept + coef1 * N_fert + coef2 * (N_fert^2)]
  # 
  # #Average all curves profits by region
  # TrainSet_RMM[, P := Y_corn * Pc + Y_soy * Ps - N_fert * Pn - L * fee_n]
  # TrainSet_RMM2 <- TrainSet_RMM[,.(P_avg = mean(P)), by = .(region, N_fert)]
  # ggplot(TrainSet_RMM2) + geom_point(aes(x = N_fert, y = P_avg, colour = interaction(region)))
  # 
  # #Select EONR
  # model_minimum_regional <- TrainSet_RMM2[, .SD[ P_avg == max( P_avg)], by = .(region)][,.( region, eonr_pred = N_fert)]
  # 
  # name_model = paste0('minimum')
  # small_model_list[[name_model]] <- model_minimum_regional
  # =========================================================================================================================================================
  # CREATE THE REGIONAL MINIMUM MODEL - OK
  TrainSet_RMM <- TrainSet2[Yld_response > Yld_response_threshold] #Needs to be here, to use updated profits 
  
  TrainSet2[,.N, .(id_10, mukey, z)] %>% nrow() #trials before (all of them)
  TrainSet_RMM[,.N, .(id_10, mukey, z)] %>% nrow()#trials after (whith response > threshold)
  
  model_minimum_ok  <- aggregate_by_area(data_dt = TrainSet_RMM, variables = c('P'), 
                                         weight = 'area_ha', by_c = c('region', 'N_fert')) %>% 
    .[, .SD[ P == max( P)], by = .(region)] %>% .[,.(region, eonr_pred = N_fert)]
  
  name_model = paste0('minimum_ok')
  small_model_list[[name_model]] <- model_minimum_ok
  
  # =========================================================================================================================================================
  ## PREPARE THE TRAINING DATA WITH EONR ========
  TrainSet_eonr <- TrainSet2[, .SD[ P == max( P)], by = .(id_10, mukey, z)]
  setnames(TrainSet_eonr, 'N_fert', 'eonr')
  
  TrainSet_eonr2 <- TrainSet_eonr[,c('eonr', low_var, high_var), with = FALSE]
  
  # =========================================================================================================================================================
  # RF Model 2------------------------
  # mtry <- tuneRF(TrainSet_eonr2[,c(low_var, high_var), with = FALSE],TrainSet_eonr2$eonr, ntreeTry=1000,
  #                stepFactor=1.2,improve=0.01, trace=TRUE, plot=TRUE)
  # best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
  best.m = 6
  
  rf2_eonr <- randomForest(eonr ~ ., data = TrainSet_eonr2[,c('eonr', low_var, high_var), with = FALSE],
                           importance = TRUE , mtry = best.m, ntree=2000, nodesize = 30)
  
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
# CREATE THE TARGET N LEACHING REDUCTION (ORIGINAL)
set.seed(123)

## PREPARE THE TRAINING DATA ========
# Part 1
TrainSet2[, P := Y_corn * Pc - N_fert * Pn] #update profits

baseline_leaching <- merge(TrainSet2, reg_model_stuff$ratio_5$minimum_ok, by = 'region') %>% 
  .[N_fert == eonr_pred] %>% .[,.(id_10, mukey, z, leach_base = L)]

TrainSet_nr <- merge(TrainSet2, baseline_leaching, by = c('id_10', 'mukey', 'z'))
TrainSet_nr[,leach_rel := L/leach_base]
summary(TrainSet_nr$leach_rel)
baseline_leaching[leach_base == 0]
TrainSet_nr[L == 0 & leach_base == 0, leach_rel := 1] #avoid dividing by 0
TrainSet_nr[L > 0 & leach_base == 0, leach_rel := L/0.0001] #avoid dividing by 0

TrainSet_nr[,.(leach_rel = mean(leach_rel)), by = N_fert][order(N_fert)]
TrainSet_RMM <- TrainSet_nr[Yld_response > Yld_response_threshold]

target_seq <- sort(unique(c(seq(0.7,0.89, by = 0.04), seq(0.9,1, by = 0.02))))

length(target_seq)

for(target_n in target_seq){
  # target_n = 1
  name_model = paste0('target_', target_n)
  if(name_model %in% names(reg_model_stuff)){next}
  
  print(target_n)
  small_model_list <- list()
  
  # CREATE THE REGIONAL MINIMUM MODEL
  model_minimum_ok  <- aggregate_by_area(data_dt = TrainSet_RMM, variables = c('P','leach_rel'), 
                                         weight = 'area_ha', by_c = c('region', 'N_fert')) 
  
  
  ggplot(model_minimum_ok) + 
    geom_line(aes(x = N_fert, y = leach_rel*min(model_minimum_ok$P), colour = factor(region)))+ #shift up the curve
    geom_line(aes(x = N_fert, y = P, colour = factor(region)))
  
  
  model_minimum_ok1 <- model_minimum_ok[leach_rel <= target_n][order(N_fert )]
  
  #Chose the EONR below the target reduction L
  if(nrow(model_minimum_ok1)>0){
    model_minimum_ok1 <- model_minimum_ok1[, .SD[ P == max( P)], by = .(region)] #pick the EONR
    model_minimum_ok1 <- model_minimum_ok1[, .SD[ N_fert == min( N_fert)], by = .(region)] #in case more than one rate had the same P
  }
  
  #Type III: cases where the lowest L is higher than the target. Pick the rate with lowest L
  model_minimum_ok[,leach_rel_min := min(leach_rel), by = .(region)]
  model_minimum_ok2 <- model_minimum_ok[leach_rel_min > target_n]
  if(nrow(model_minimum_ok2)>0){
    model_minimum_ok2 <- model_minimum_ok2[, .SD[ leach_rel  == min(leach_rel )], by = .(region)] #pick the lowest L
    model_minimum_ok2 <- model_minimum_ok2[, .SD[ N_fert == min( N_fert)], by = .(region)] #in case more than one rate had the same L
  }
  model_minimum_ok <- rbind(model_minimum_ok1, model_minimum_ok2, fill = T) %>% .[,.(region, eonr_pred = N_fert)]
  
  # model_minimum_ok <- model_minimum_ok[leach_rel >= n_red] %>%
  #   .[, .SD[ leach_rel == min( leach_rel)], by = .(region)] %>% #select minimum leach_rel
  #   .[, .SD[ N_fert == min( N_fert)], by = .(region)] %>% #select minimum rate in case one is repeated
  #   .[,.(region, eonr_pred = N_fert)]
  
  name_model = paste0('minimum_ok')
  small_model_list[[name_model]] <- model_minimum_ok
  
  ## PREPARE THE TRAINING DATA WITH EONR ========
  
  # Type I and II: cases where there are rates with L below the target
  TrainSet_nr_tmp1 <- TrainSet_nr[leach_rel <= target_n][order(N_fert )]
  
  #Chose the EONR below the target reduction L
  TrainSet_nr_tmp1 <- TrainSet_nr_tmp1[, .SD[ P == max( P)], by = .(id_10, mukey, z)] #pick the EONR
  TrainSet_nr_tmp1 <- TrainSet_nr_tmp1[, .SD[ N_fert == min( N_fert)], by = .(id_10, mukey, z)] #in case more than one rate had the same P
  
  #Type III: cases where the lowest L is higher than the target. Pick the rate with lowest L
  TrainSet_nr[,leach_rel_min := min(leach_rel), by = .(id_10, mukey, z)]
  TrainSet_nr_tmp2 <- TrainSet_nr[leach_rel_min > target_n]
  if(nrow(TrainSet_nr_tmp2)>0){
    TrainSet_nr_tmp2 <- TrainSet_nr_tmp2[, .SD[ L == min(L)], by = .(id_10, mukey, z)] #pick the EONR
    TrainSet_nr_tmp2 <- TrainSet_nr_tmp2[, .SD[ N_fert == min( N_fert)], by = .(id_10, mukey, z)] #in case more than one rate had the same P
  }
  TrainSet_nr_tmp <- rbind(TrainSet_nr_tmp1, TrainSet_nr_tmp2, fill = T)
  setnames(TrainSet_nr_tmp, 'N_fert', 'eonr')
  table(TrainSet_nr_tmp[,.N, by = .(id_10, mukey, z)]$N)
  
  TrainSet_nr_tmp[,.N, by = .(id_10, mukey, z)]
  TrainSet_nr_tmp <- TrainSet_nr_tmp[,c('eonr', low_var, high_var), with = FALSE]
  
  # =========================================================================================================================================================
  # RF Model 2------------------------
  # mtry <- tuneRF(TrainSet_nr_tmp[,c(low_var, high_var), with = FALSE],TrainSet_nr_tmp$eonr, ntreeTry=1000,
  #                stepFactor=1.2,improve=0.01, trace=TRUE, plot=TRUE)
  # best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
  
  best.m = 6
  
  rf2_eonr <- randomForest(eonr ~ ., data = TrainSet_eonr2[,c('eonr', low_var, high_var), with = FALSE],
                           importance = TRUE , mtry = best.m, ntree=2000, nodesize = 30)
  
  varImpPlot(rf2_eonr, type=2)
  name_model = paste0('rf2')
  small_model_list[[name_model]] <- rf2_eonr
  
  name_model = paste0('target_', target_n)
  reg_model_stuff[[name_model]] <- small_model_list
  names(reg_model_stuff)
}

# =========================================================================================
# CREATE THE N LEACHING REDUCTION LONG TERM OPTIMIZATION
set.seed(123)

## PREPARE THE TRAINING DATA ========
# Part 1
TrainSet2[, P := Y_corn * Pc - N_fert * Pn] #update profits

baseline_leaching <- merge(TrainSet2, reg_model_stuff$ratio_5$minimum_ok, by = 'region') %>% 
  .[N_fert == eonr_pred] %>% .[,.(id_10, mukey, z, z_type, leach_base = L)]
sum(baseline_leaching[id_10 ==  47 & mukey == 1591917]$leach_base)
TrainSet2_nr <- merge(TrainSet2, baseline_leaching, by = c('id_10', 'mukey', 'z', 'z_type'))
TrainSet2_nr[,leach_rel := L/leach_base]
summary(TrainSet2_nr$leach_rel)
baseline_leaching[leach_base == 0]
TrainSet2_nr[L == 0 & leach_base == 0, leach_rel := 1] #avoid dividing by 0
TrainSet2_nr[L > 0 & leach_base == 0, leach_rel := L/0.0001] #avoid dividing by 0

TrainSet2_nr[,.(leach_rel = mean(leach_rel)), by = N_fert][order(N_fert)]
TrainSet_RMM <- TrainSet2_nr[Yld_response > Yld_response_threshold]

red_seq <- sort(unique(c(seq(0.7,0.89, by = 0.03), seq(0.9,1, by = 0.01))))
# red_seq <- c(0.8, 0.95, 1)
# red_seq <- c(0.85, 0.95)
length(red_seq)
all_optimized_training_sets_list <- list()
rm(Trainset_optimized_tmp)

for(n_red in sort(red_seq, decreasing = T)){
  # n_red = 0.8
  name_model = paste0('nred_', n_red)
  # if(name_model %in% names(reg_model_stuff)){next}
  
  print(n_red)
  small_model_list <- list()
  
  # CREATE THE REGIONAL MINIMUM MODEL
  model_minimum_ok  <- aggregate_by_area(data_dt = TrainSet_RMM, variables = c('P','leach_rel'), 
                                         weight = 'area_ha', by_c = c('region', 'N_fert')) 
  
  ggplot(model_minimum_ok) + 
    geom_line(aes(x = N_fert, y = leach_rel*min(model_minimum_ok$P), colour = factor(region)))+ #shift up the curve
    geom_line(aes(x = N_fert, y = P, colour = factor(region)))
  
  
  model_minimum_ok1 <- model_minimum_ok[leach_rel <= n_red][order(N_fert )]
  
  #Chose the EONR below the target reduction L
  if(nrow(model_minimum_ok1)>0){
    model_minimum_ok1 <- model_minimum_ok1[, .SD[ P == max( P)], by = .(region)] #pick the EONR
    model_minimum_ok1 <- model_minimum_ok1[, .SD[ N_fert == min( N_fert)], by = .(region)] #in case more than one rate had the same P
  }
  #Type III: cases where the lowest L is higher than the target. Pick the rate with lowest L
  model_minimum_ok[,leach_rel_min := min(leach_rel), by = .(region)]
  model_minimum_ok2 <- model_minimum_ok[leach_rel_min > n_red]
  if(nrow(model_minimum_ok2)>0){
    model_minimum_ok2 <- model_minimum_ok2[, .SD[ leach_rel  == min(leach_rel )], by = .(region)] #pick the lowest L
    model_minimum_ok2 <- model_minimum_ok2[, .SD[ N_fert == min( N_fert)], by = .(region)] #in case more than one rate had the same L
  }
  model_minimum_ok <- rbind(model_minimum_ok1, model_minimum_ok2, fill = T) %>% .[,.(region, eonr_pred = N_fert)]
  
  # model_minimum_ok <- model_minimum_ok[leach_rel >= n_red] %>%
  #   .[, .SD[ leach_rel == min( leach_rel)], by = .(region)] %>% #select minimum leach_rel
  #   .[, .SD[ N_fert == min( N_fert)], by = .(region)] %>% #select minimum rate in case one is repeated
  #   .[,.(region, eonr_pred = N_fert)]
  
  name_model = paste0('minimum_ok')
  small_model_list[[name_model]] <- model_minimum_ok
  
  # =========================================================================================================================================================
  # ## PREPARE THE TRAINING DATA WITH EONR ========
  soils_training <- unique(TrainSet2_nr[,.(id_10, mukey, z_type)])
  TrainSet_nr_tmp_list <- list()
  
  library("foreach")
  library("doParallel")
  registerDoParallel(20) # register the cluster
  # registerDoParallel(cores = 10)
  TrainSet_nr_tmp_list = foreach(i = 1:nrow(soils_training), .combine = "c", .packages = c("data.table")) %dopar% {
    # i= 419
    print(i)
    soils_training_n <- soils_training[i]
    TrainSet2_nr_field <- filter_dt_in_dt(TrainSet2_nr, soils_training_n, return_table = T)
    leach_base_field <- filter_dt_in_dt(baseline_leaching, soils_training_n, return_table = T)
    leach_base_sum <- sum(leach_base_field$leach_base)
    
    # if(!exists( 'Trainset_optimized_tmp')){
    #start with the rates that maximize profits
    opt_dt <- TrainSet2_nr_field[,.SD[P == max(P)],by = z] %>% .[,.SD[N_fert == min( N_fert)],by = z]
    sum(opt_dt$L)/leach_base_sum
    # }else{
    #   #start with the rates from last optimization
    #   opt_dt <- filter_dt_in_dt(Trainset_optimized_tmp, soils_training_n, return_table = T)
    #   setnames(opt_dt, 'eonr', 'N_fert')
    #   if(n_red > unique(opt_dt$target)){stop()}
    #   opt_dt <- opt_dt[,-'target']
    #   sum(opt_dt$L)/leach_base_sum
    # }
    
    #find what z can have a lower rate with the lowest hurt in P
    hit_the_target <- sum(opt_dt$L) <= leach_base_sum * n_red
    previous<- sum(opt_dt$L)
    lower_than_before <- TRUE
    
    while(lower_than_before & !hit_the_target){
      #1- Lower the rates by 10 kg
      next_step_rates <- opt_dt[,.(id_10, mukey, z, N_fert_new = N_fert)]
      next_step_rates[, N_fert_new := ifelse(N_fert_new <= 10, 10, N_fert_new - 10)]
      
      #2- go get the data for the lower rates
      next_step_data <- merge(TrainSet2_nr_field, next_step_rates, by = c('id_10', 'mukey', 'z'))[N_fert == N_fert_new]
      next_step_efficiency <- merge(opt_dt[,.(id_10, mukey, z, N_fert, P, L)],
                                    next_step_data[,.(id_10, mukey, z, N_fert, P, L)], by = c('id_10', 'mukey', 'z'))
      next_step_efficiency[,P_diff := P.y - P.x]
      next_step_efficiency[,L_diff := L.y - L.x]
      next_step_efficiency[,P_L_efficiency := P_diff/L_diff]
      
      if(all(!(next_step_efficiency$L_diff < 0))){break}
      #3 - find what z can have a lower rate with the lowest hurt in P
      decrease_me <- next_step_efficiency[L_diff < 0][P_L_efficiency == min(P_L_efficiency)]
      next_step_rates <- opt_dt[,.(id_10, mukey, z, N_fert_new = N_fert)]
      next_step_rates[z == decrease_me$z, N_fert_new := N_fert_new-10]
      opt_dt <- merge(TrainSet2_nr_field, next_step_rates, by = c('id_10', 'mukey', 'z')) 
      opt_dt <- opt_dt[N_fert == N_fert_new, -'N_fert_new']
      lower_than_before <- sum(opt_dt$L) < previous
      previous = sum(opt_dt$L)
      # print(sum(opt_dt$L))
      hit_the_target <- sum(opt_dt$L) <= leach_base_sum * n_red
      # print(sum(opt_dt$L)/leach_base_sum)
    } #end of while loop
    
    # return the results
    list(opt_dt)
    
  }#end of dopar loop
  
  stopImplicitCluster()
  
  Trainset_optimized_tmp <- rbindlist(TrainSet_nr_tmp_list, use.names=TRUE)
  setnames(Trainset_optimized_tmp, 'N_fert', 'eonr')
  
  all_optimized_training_sets_list[[length(all_optimized_training_sets_list)+1]] <- Trainset_optimized_tmp[,target := n_red]
  # # Type I and II: cases where there are rates with L below the target
  # TrainSet_nr_tmp1 <- TrainSet_nr[leach_rel <= n_red][order(N_fert )]
  # TrainSet_nr_tmp1[mukey == 1591902 & z == 3]
  # #Chose the EONR below the target reduction L
  # TrainSet_nr_tmp1 <- TrainSet_nr_tmp1[, .SD[ P == max( P)], by = .(id_10, mukey, z)] #pick the EONR
  # TrainSet_nr_tmp1 <- TrainSet_nr_tmp1[, .SD[ N_fert == min( N_fert)], by = .(id_10, mukey, z)] #in case more than one rate had the same P
  # 
  # #Type III: cases where the lowest L is higher than the target. Pick the rate with lowest L
  # TrainSet_nr[,leach_rel_min := min(leach_rel), by = .(id_10, mukey, z)]
  # TrainSet_nr_tmp2 <- TrainSet_nr[leach_rel_min > n_red]
  # if(nrow(TrainSet_nr_tmp2)>0){
  # TrainSet_nr_tmp2 <- TrainSet_nr_tmp2[, .SD[ L == min(L)], by = .(id_10, mukey, z)] #pick the EONR
  # TrainSet_nr_tmp2 <- TrainSet_nr_tmp2[, .SD[ N_fert == min( N_fert)], by = .(id_10, mukey, z)] #in case more than one rate had the same P
  # }
  # TrainSet_nr_tmp <- rbind(TrainSet_nr_tmp1, TrainSet_nr_tmp2, fill = T)
  # setnames(TrainSet_nr_tmp, 'N_fert', 'eonr')
  # table(TrainSet_nr_tmp[,.N, by = .(id_10, mukey, z)]$N)
  # 
  # TrainSet_nr_tmp[,.N, by = .(id_10, mukey, z)]
  # TrainSet_nr_tmp <- TrainSet_nr_tmp[,c('eonr', low_var, high_var), with = FALSE]
  
  # =========================================================================================================================================================
  # RF Model 2------------------------
  # mtry <- tuneRF(TrainSet_nr_tmp[,c(low_var, high_var), with = FALSE],TrainSet_nr_tmp$eonr, ntreeTry=1000,
  #                stepFactor=1.2,improve=0.01, trace=TRUE, plot=TRUE)
  # best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
  
  best.m <- 5
  
  rf2_eonr <- randomForest(eonr ~ ., data = Trainset_optimized_tmp[,c('eonr', low_var, high_var), with = FALSE],
                           importance = TRUE , mtry = best.m, ntree=1000, nodesize = 20)
  # plot(rf2_eonr)
  varImpPlot(rf2_eonr, type=2)
  name_model = paste0('rf2')
  small_model_list[[name_model]] <- rf2_eonr
  
  name_model = paste0('nred_', n_red)
  reg_model_stuff[[name_model]] <- small_model_list
  names(reg_model_stuff)
}
# =========================================================================================================================================================
# CREATE THE N LEACHING REDUCTION WITH SHADOW VALUE
set.seed(123)

## PREPARE THE TRAINING DATA ========
# Part 1
TrainSet2[, P := Y_corn * Pc - N_fert * Pn] #update profits

baseline_leaching <- merge(TrainSet2, reg_model_stuff$ratio_5$minimum_ok, by = 'region') %>% 
  .[N_fert == eonr_pred] %>% .[,.(id_10, mukey, z, z_type, leach_base = L)]
sum(baseline_leaching[id_10 ==  47 & mukey == 1591917]$leach_base)
TrainSet2_nr <- merge(TrainSet2, baseline_leaching, by = c('id_10', 'mukey', 'z', 'z_type'))
TrainSet2_nr[,leach_rel := L/leach_base]
summary(TrainSet2_nr$leach_rel)
baseline_leaching[leach_base == 0]
TrainSet2_nr[L == 0 & leach_base == 0, leach_rel := 1] #avoid dividing by 0
TrainSet2_nr[L > 0 & leach_base == 0, leach_rel := L/0.0001] #avoid dividing by 0

TrainSet2_nr[,.(leach_rel = mean(leach_rel)), by = N_fert][order(N_fert)]
TrainSet_RMM <- TrainSet2_nr[Yld_response > Yld_response_threshold]

shadow_seq <- sort(unique(c(seq(0.7,0.89, by = 0.03), seq(0.9,1, by = 0.01))))

length(shadow_seq)

for(shadow_n in sort(shadow_seq, decreasing = T)){
  # shadow_n = 1
  name_model = paste0('shadow', shadow_n)
  # if(name_model %in% names(reg_model_stuff)){next}
  
  print(shadow_n)
  small_model_list <- list()
  
  # CREATE THE REGIONAL MINIMUM MODEL
  model_minimum_ok  <- aggregate_by_area(data_dt = TrainSet_RMM, variables = c('P','leach_rel'), 
                                         weight = 'area_ha', by_c = c('region', 'N_fert')) 
  
  
  ggplot(model_minimum_ok) + 
    geom_line(aes(x = N_fert, y = leach_rel*min(model_minimum_ok$P), colour = factor(region)))+ #shift up the curve
    geom_line(aes(x = N_fert, y = P, colour = factor(region)))
  
  
  model_minimum_ok1 <- model_minimum_ok[leach_rel <= shadow_n][order(N_fert )]
  
  #Chose the EONR below the target reduction L
  if(nrow(model_minimum_ok1)>0){
    model_minimum_ok1 <- model_minimum_ok1[, .SD[ P == max( P)], by = .(region)] #pick the EONR
    model_minimum_ok1 <- model_minimum_ok1[, .SD[ N_fert == min( N_fert)], by = .(region)] #in case more than one rate had the same P
  }
  #Type III: cases where the lowest L is higher than the target. Pick the rate with lowest L
  model_minimum_ok[,leach_rel_min := min(leach_rel), by = .(region)]
  model_minimum_ok2 <- model_minimum_ok[leach_rel_min > shadow_n]
  if(nrow(model_minimum_ok2)>0){
    model_minimum_ok2 <- model_minimum_ok2[, .SD[ leach_rel  == min(leach_rel )], by = .(region)] #pick the lowest L
    model_minimum_ok2 <- model_minimum_ok2[, .SD[ N_fert == min( N_fert)], by = .(region)] #in case more than one rate had the same L
  }
  model_minimum_ok <- rbind(model_minimum_ok1, model_minimum_ok2, fill = T) %>% .[,.(region, eonr_pred = N_fert)]
  
  # model_minimum_ok <- model_minimum_ok[leach_rel >= shadow_n] %>%
  #   .[, .SD[ leach_rel == min( leach_rel)], by = .(region)] %>% #select minimum leach_rel
  #   .[, .SD[ N_fert == min( N_fert)], by = .(region)] %>% #select minimum rate in case one is repeated
  #   .[,.(region, eonr_pred = N_fert)]
  
  name_model = paste0('minimum_ok')
  small_model_list[[name_model]] <- model_minimum_ok
  
  # =========================================================================================================================================================
  # ## PREPARE THE TRAINING DATA WITH EONR ========
  # Type I and II: cases where there are rates with L below the target
  TrainSet_nr_tmp1 <- TrainSet2_nr[leach_rel <= shadow_n][order(N_fert )]
  
  #Chose the EONR below the target reduction L
  TrainSet_nr_tmp1 <- TrainSet_nr_tmp1[, .SD[ P == max( P)], by = .(id_10, mukey, z)] #pick the EONR
  TrainSet_nr_tmp1 <- TrainSet_nr_tmp1[, .SD[ N_fert == min( N_fert)], by = .(id_10, mukey, z)] #in case more than one rate had the same P
  
  #Type III: cases where the lowest L is higher than the target. Pick the rate with lowest L
  TrainSet2_nr[,leach_rel_min := min(leach_rel), by = .(id_10, mukey, z)]
  TrainSet_nr_tmp2 <- TrainSet2_nr[leach_rel_min > shadow_n]
  if(nrow(TrainSet_nr_tmp2)>0){
    TrainSet_nr_tmp2 <- TrainSet_nr_tmp2[, .SD[ L == min(L)], by = .(id_10, mukey, z)] #pick the EONR
    TrainSet_nr_tmp2 <- TrainSet_nr_tmp2[, .SD[ N_fert == min( N_fert)], by = .(id_10, mukey, z)] #in case more than one rate had the same P
  }
  TrainSet_nr_tmp <- rbind(TrainSet_nr_tmp1, TrainSet_nr_tmp2, fill = T)
  setnames(TrainSet_nr_tmp, 'N_fert', 'eonr')
  table(TrainSet_nr_tmp[,.N, by = .(id_10, mukey, z)]$N)
  
  TrainSet_nr_tmp[,.N, by = .(id_10, mukey, z)]
  # TrainSet_nr_tmp <- TrainSet_nr_tmp[,c('eonr', low_var, high_var), with = FALSE]
  hist(TrainSet_nr_tmp$eonr)
  #-----------
  #Add the shadow value
  rates_0 <- TrainSet_nr_tmp[,.(id_10, mukey, z, z_type, N_fert = eonr, P_const = P)]
  rates_0[,N_fert := N_fert + 10] 
  hist(rates_0$N_fert)
  shadow_dt <- merge(TrainSet2_nr, rates_0, by = c('id_10', 'mukey', 'z', 'z_type', 'N_fert'))
  shadow_dt[,shadow_val := P - P_const]
  hist(shadow_dt$shadow_val)
  shadow_dt[,shadow_val_rel := (shadow_val - min(shadow_val)) / (max(shadow_val) - min(shadow_val)), by =  .(id_10, mukey, z_type)]
  hist(shadow_dt$shadow_val_rel)
  
  
  # =========================================================================================================================================================
  # RF Model 2------------------------
  # mtry <- tuneRF(TrainSet_nr_tmp[,c(low_var, high_var), with = FALSE],TrainSet_nr_tmp$eonr, ntreeTry=1000,
  #                stepFactor=1.2,improve=0.01, trace=TRUE, plot=TRUE)
  # best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
  
  best.m <- 5
  
  rf2_eonr <- randomForest(eonr ~ ., data = TrainSet_nr_tmp[,c('eonr', low_var, high_var), with = FALSE],
                           importance = TRUE , mtry = best.m, ntree=1000, nodesize = 20)
  
  # varImpPlot(rf2_eonr, type=2)
  name_model = paste0('rf2')
  small_model_list[[name_model]] <- rf2_eonr
  
  # mtry <- tuneRF(shadow_dt[,c('shadow_val_rel', low_var, high_var), with = FALSE],shadow_dt$shadow_val_rel, ntreeTry=1000,
  #                                stepFactor=1.2,improve=0.1, trace=TRUE, plot=TRUE)
  best.m <- 10           
  setnames(shadow_dt, 'N_fert', 'eonr_pred_cut') #when we predict the shadow value, we will do it using already predicted eonr_pred
  rf2_shadow <- randomForest(shadow_val_rel ~ ., data = shadow_dt[,c('shadow_val_rel', 'eonr_pred_cut' ,low_var, high_var), with = FALSE],
                             importance = TRUE , mtry = best.m, ntree=1000, nodesize = 20)
  
  # plot(rf2_shadow)
  # varImpPlot(rf2_shadow, type=2)
  
  name_model = paste0('rf2_shadow')
  small_model_list[[name_model]] <- rf2_shadow
  
  name_model = paste0('shadow_', shadow_n)
  reg_model_stuff[[name_model]] <- small_model_list
  names(reg_model_stuff)
}

# =========================================================================================================================================================
names(reg_model_stuff)


reg_model_stuff[['no_cost_var']] <-  low_var
reg_model_stuff[['ss_var']] <-  high_var
# reg_model_stuff[['crop_varb']] <-  crop_varb
# reg_model_stuff[['low_var_trf']] <-  low_var
# reg_model_stuff[['preprocessParams']] <-  preprocessParams
# 
# reg_model_stuff_tmp[['TrainSet_eonr3']] <-  TrainSet_eonr3
# 
# reg_model_stuff_tmp[['rf1_eonr']] <-  rf1_eonr
# reg_model_stuff_tmp[['rf2_eonr']] <-  rf2_eonr
# reg_model_stuff_tmp[['rf3_eonr']] <-  rf3_eonr
# reg_model_stuff_tmp[['reg_lm4']] <-  reg_lm4

reg_model_stuff$ratio_5$minimum_ok
reg_model_stuff$fee_0$minimum_ok
reg_model_stuff$nred_1$minimum_ok

saveRDS(reg_model_stuff, "./n_policy_box/Data/files_rds/reg_model_stuff.rds")
