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

prediction_set_aggregated_dt <- readRDS("./n_policy_box/Data/files_rds/prediction_set_aggregated_dt.rds")
testing_set_dt <- readRDS("./n_policy_box/Data/files_rds/testing_set_dt.rds")


prediction_set_aggregated_dt <- prediction_set_aggregated_dt[z %in% 15:30]
testing_set_dt <- testing_set_dt[z %in% 15:30]
testing_set_dt[, P := Y_corn * Pc - N_fert * Pn]


remove_list <- names(reg_model_stuff)[!names(reg_model_stuff) %in% c("full_fields", "stations", "TrainSet","training_z")]
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
TrainSet2 <- TrainSet2[z %in% 1:14]
hist(TrainSet2$n_0_60cm_v5)

no_cost_varb <- c('region', "rain_30", "rain_60", "rain_90",
                  "t_max_30", "t_max_60", "t_max_90", "t_min_30", "t_min_60",
                  "t_min_90", "Yld_prev", 'Y_corn_lt_avg', 'Y_corn_lt_min', 'Y_corn_lt_max', "lai_v5")

ss_varb <- c("n_0_60cm_v5","esw_pct_v5", "whc")


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

TrainSet2[, P := Y_corn * Pc - N_fert * Pn]  #update profits
print(Pn/Pc)
# =========================================================================================================================================================
# CREATE THE N RATIO TAX MODEL

results_list <- list()

for(z_n in c(1:14)){

  # z_n = 3
  print(z_n)
  # TrainSet2[, P := Y_corn * Pc + Y_soy * Ps - N_fert * Pn_tmp]  #update profits
  
  TrainSet2_tmp <- TrainSet2[!z %in% z_n]
  TrainSet2_tmp[!z %in% c(5,6)]
  # =========================================================================================================================================================
  # CREATE THE REGIONAL MINIMUM MODEL - OK
  TrainSet_RMM <- TrainSet2_tmp[Yld_response > Yld_response_threshold] #Needs to be here, to use updated profits 
  
  TrainSet2_tmp[,.N, .(id_10, mukey, z)] %>% nrow() #trials before (all of them)
  TrainSet_RMM[,.N, .(id_10, mukey, z)] %>% nrow()#trials after (whith response > threshold)
  
  model_minimum_ok  <- aggregate_by_area(data_dt = TrainSet_RMM, variables = c('P'), 
                                         weight = 'area_ha', by_c = c('region', 'N_fert')) %>% 
    .[, .SD[ P == max( P)], by = .(region)] %>% .[,.(region, eonr_pred = N_fert)]
  
  
  # =========================================================================================================================================================
  ## PREPARE THE TRAINING DATA WITH EONR ========
  TrainSet_eonr <- TrainSet2_tmp[, .SD[ P == max( P)], by = .(id_10, mukey, z)]
  setnames(TrainSet_eonr, 'N_fert', 'eonr')
  
  TrainSet_eonr2 <- TrainSet_eonr[,c('eonr', no_cost_varb, ss_varb), with = FALSE]
  
  # =========================================================================================================================================================
  
  # RF Model 2------------------------
  # mtry <- tuneRF(TrainSet_eonr2[,c(no_cost_varb, ss_varb), with = FALSE],TrainSet_eonr2$eonr, ntreeTry=1000,
  #                 stepFactor=1.2,improve=0.01, trace=TRUE, plot=TRUE) # ,mtryStart = 5
  # best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
  best.m = 5
  
  rf2_eonr <- randomForest(eonr ~ ., data = TrainSet_eonr2[,c('eonr', no_cost_varb, ss_varb), with = FALSE],
                           importance = TRUE , mtry = best.m, ntree=1000, nodesize = 30)
  
  varImpPlot(rf2_eonr, type=2)
  
  #===================================================================================================================
  # 1b) MINIMUM OK-
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  # the NMS is trained with z1-10 and testing is evaluated with z11-25
  testing_set_tmp <- merge(testing_set_dt,
                           model_minimum_ok, #because I forgot to set the name to eonr_pred
                           by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    .[N_fert == eonr_pred] %>%
    .[,c("region", "id_10", 'id_field',"mukey", "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert", "P")]
  
  testing_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
  
  policy_n <- paste0('ztest_', z_n)
  results_list[[length(results_list)+1]] <- testing_set_tmp[,NMS := '1'][,policy := policy_n]
  
  #===================================================================================================================
  # 4) PREDICT WITH REGIONAL RF 2 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  
  prediction_set_aggregated_dt[,eonr_pred := ceiling(predict(rf2_eonr, prediction_set_aggregated_dt)/10)*10]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set_tmp <- merge(testing_set_dt,
                           prediction_set_aggregated_dt[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
    .[N_fert == eonr_pred] %>%
    .[,c("region", "id_10", 'id_field',"mukey", "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P')]
  
  results_list[[length(results_list)+1]] <- testing_set_tmp[,NMS := '2'][,policy := policy_n]
}

perfomances_dt <- rbindlist(results_list)
perfomances_dt[,G := 0]

perfomances_dt[,.N, .(id_10, mukey,id_field)] %>% 
  .[,.N, .(id_10, id_field)] %>% .[,.N, .(id_10)] %>% .[,N] %>% table() #number of fields by cell

perfomances_dt[,.N, .(id_10, mukey,id_field, policy, NMS)]$N %>% table() #number of z by all the other things

saveRDS(perfomances_dt, "./n_policy_box/Data/files_rds/perfomances_dt.rds")
# perfomances_dt <- readRDS( "./n_policy_box/Data/files_rds/perfomances_dt.rds")