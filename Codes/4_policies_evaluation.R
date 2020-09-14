# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
setwd('~')
rm(list=ls())

source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')
source('./n_policy_git/Codes/parameters.R')

library("foreach")
library("doParallel")

# install.packages('reticulate')
# library(reticulate)
# 
# use_condaenv('GEOANN', conda = '/opt/anaconda3/condabin/conda')
# conda_install("GEOANN", "pyreadr")
# 
# py_run_file("./n_policy_git/Codes/3c_cnn.py")
# 
# #Activate condas from terminal
# system('source activate GEOANN')  
# system('/home/germanm2/n_policy_git/Codes/3c_cnn.py')


# #Create condas environment from R
# library(reticulate)
# conda_create("r-reticulate")
# conda_install("r-reticulate", "pyreadr")
# conda_install("r-reticulate", "numpy")
# conda_install("r-reticulate", "pandas")
# conda_install("r-reticulate", "scipy")
# conda_install("r-reticulate", "torch")
# conda_install("r-reticulate", "torchvision")
# conda_install("r-reticulate", "sklearn")

#Run python in condas environment from R
# https://cran.r-project.org/web/packages/reticulate/vignettes/calling_python.html
library(reticulate)
use_condaenv('GEOANN', conda = '/opt/anaconda3/condabin/conda')
source_python("./n_policy_git/Codes/3c_cnn_functions_sep10.py")


reg_model_stuff <- readRDS( "./n_policy_box/Data/files_rds/reg_model_stuff.rds")
# grid10_soils_sf2 <- readRDS('./n_policy_box/Data/Grid/grid10_soils_sf2.rds')

prediction_set_aggregated_dt <- readRDS("./n_policy_box/Data/files_rds/prediction_set_aggregated_dt.rds")
pred_vars <- readRDS("./n_policy_box/Data/files_rds/pred_vars.rds")
# #---------------------------------------------------------------------------------
# # Evaluate CNN
# 
# prediction_set_aggregated_cnn_dt <- readRDS("./n_policy_box/Data/files_rds/prediction_set_aggregated_cnn_dt.rds") %>% data.table()
# nrow(prediction_set_aggregated_cnn_dt)
# 
# prediction_set_aggregated_dt[,eonr_pred_rf := ceiling(predict(reg_model_stuff[['ratio_5']]$rf2, prediction_set_aggregated_dt)/10)*10]
# 
# compare_dt <- merge(prediction_set_aggregated_dt, prediction_set_aggregated_cnn_dt[,.(id_10, id_field, region, z, eonr_pred_cnn = eonr_pred)], by = c('id_10', 'id_field', 'region', 'z'))
# compare_dt[,eonr_pred_cnn := ceiling(as.numeric(eonr_pred_cnn)/10)*10]
# 
# ggplot(compare_dt[sample(1:nrow(compare_dt), 1000)]) + geom_point(aes(x = eonr_pred_rf, y= eonr_pred_cnn))
#---------------------------------------------------------------------------------
testing_set_dt <- readRDS("./n_policy_box/Data/files_rds/testing_set_dt.rds")
testing_set_dt[, region := factor(region)]

testing_set_dt[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z, N_fert)]$area_ha %>% summary()

names(reg_model_stuff)
training_z <- reg_model_stuff$training_z
prediction_set_aggregated_dt <- prediction_set_aggregated_dt[!z %in% training_z]
testing_set_dt <- testing_set_dt[!z %in% training_z]

results_list <- list()
# yc_yearly_dt3[,L := L1 + L2] #update leaching adding corn and soy
# yc_yearly_dt3[, P := Y_corn * Pc + Y_soy * Ps - N_fert * Pn] #update profits adding corn and soy

#Get profits relative to zero Nitrogen Rate
# data_zero_dt <- yc_yearly_dt3[N_fert == 10, .(id_10, mukey, z, Y_corn_zero = Y_corn, Y_soy_zero = Y_soy, L_zero = L, N_fert_zero = N_fert)] %>% unique()
# yc_yearly_dt3 <- merge(yc_yearly_dt3, data_zero_dt, by = c('id_10', 'mukey', 'z'))

#======================================================================================
# RATIO POLICY
policies_ratios <- names(reg_model_stuff)[str_detect(names(reg_model_stuff), pattern = 'ratio_')]

for(policy_n in policies_ratios){
  # policy_n = policies_ratios[[1]]
  ratio_n <- as.numeric(str_extract(policy_n,pattern = '[0-9.]+'))
  Pn_tmp = ratio_n * Pc
  print(Pn_tmp/Pc)
  testing_set_dt[, P := Y_corn * Pc - N_fert * Pn_tmp]  #update profits
  # testing_set_dt[, P_2 := Y_soy * Ps]  #update profits
  testing_set_dt[, G := N_fert * (Pn_tmp - Pn)] #gov collection
  
  #===================================================================================================================
  # 1b) MINIMUM OK-
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  # the NMS is trained with z1-10 and testing is evaluated with z11-25
  testing_set_tmp <- merge(testing_set_dt[!z %in% training_z],
                       reg_model_stuff[[policy_n]]$minimum_ok, #because I forgot to set the name to eonr_pred
                       by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    .[N_fert == eonr_pred] %>%
    .[,c("region", "id_10", 'id_field',"mukey", "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
  
  testing_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
  
  results_list[[length(results_list)+1]] <- testing_set_tmp[,NMS := 'static'][,policy := policy_n]
  
  #===================================================================================================================
  # 2) PREDICT WITH REGIONAL RF 2 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  
  prediction_set_aggregated_dt[,eonr_pred := flood(predict(reg_model_stuff[[policy_n]]$rf2, prediction_set_aggregated_dt)/10)*10]
  
  # if(ratio_n == 5){
  #   save_me <- copy(prediction_set_aggregated_dt)
  #   setnames(save_me, 'eonr_pred', 'eonr_pred_rf')
  #   saveRDS(save_me, "./n_policy_box/Data/files_rds/prediction_set_aggregated_dt.rds")
  #   rm(save_me)
  # }
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set_tmp <- merge(testing_set_dt[!z %in% training_z],
                       prediction_set_aggregated_dt[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
    .[N_fert == eonr_pred] %>%
    .[,c("region", "id_10", 'id_field',"mukey", "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
  
  results_list[[length(results_list)+1]] <- testing_set_tmp[,NMS := 'dynamic1'][,policy := policy_n]
  
  #===================================================================================================================
  # 3) PREDICT CNN
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  # source_python("./n_policy_git/Codes/3c_cnn_functions_sep10.py")
  prediction_set_aggregated_dt2 <- predict_cnn(prediction_set_aggregated_dt, policy_n, pred_vars) %>% data.table()
  
  prediction_set_aggregated_dt2[,eonr_pred := flood(eonr_pred/10)*10] %>%
    .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))]
  
  ggplot(prediction_set_aggregated_dt2[sample(1:nrow(prediction_set_aggregated_dt2), 1000)]) + 
    geom_point(aes(x = eonr_pred_rf, y= eonr_pred))+ 
    theme(aspect.ratio=1) + coord_fixed() + geom_abline() + ylim(0, 260)+ xlim(0, 260) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"))
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set_tmp <- merge(testing_set_dt[!z %in% training_z],
                           prediction_set_aggregated_dt2[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
    .[N_fert == eonr_pred] %>%
    .[,c("region", "id_10", 'id_field',"mukey", "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
  
  
  
  results_list[[length(results_list)+1]] <- testing_set_tmp[,NMS := 'dynamic2'][,policy := policy_n]
  
}
#---------------------------------------------------------------------------
# FEE POLICY

policies_fee <- names(reg_model_stuff)[str_detect(names(reg_model_stuff), pattern = 'fee_')]

for(policy_n in policies_fee){
  # policy_n = policies_fee[[3]]
  fee_n <- as.numeric(str_extract(policy_n,pattern = '[0-9.]+'))
  # print(fee_n)
  # ic_field_dt[, P_1 := Y_corn * Pc - N_fert * Pn - L1 * fee_n]  #update profits
  # ic_field_dt[, P_2 := Y_soy * Ps - L2 * fee_n]  #update profits
  testing_set_dt[, P := Y_corn * Pc - N_fert * Pn - L * fee_n]  #update profits
  testing_set_dt[, G := L * fee_n] #gov collectionn
  
  
  #===================================================================================================================
  # 1b) MINIMUM OK-
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  # the NMS is trained with z1-10 and testing is evaluated with z11-25
  testing_set_tmp <- merge(testing_set_dt[!z %in% training_z],
                           reg_model_stuff[[policy_n]]$minimum_ok, #because I forgot to set the name to eonr_pred
                           by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    .[N_fert == eonr_pred] %>%
    .[,c("region", "id_10", 'id_field',"mukey", "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
  
  testing_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
  
  results_list[[length(results_list)+1]] <- testing_set_tmp[,NMS := 'static'][,policy := policy_n]
  
  #===================================================================================================================
  # 2) PREDICT WITH REGIONAL RF 2 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  
  prediction_set_aggregated_dt[,eonr_pred := flood(predict(reg_model_stuff[[policy_n]]$rf2, prediction_set_aggregated_dt)/10)*10]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set_tmp <- merge(testing_set_dt[!z %in% training_z],
                           prediction_set_aggregated_dt[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
    .[N_fert == eonr_pred] %>%
    .[,c("region", "id_10", 'id_field',"mukey", "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
  
  results_list[[length(results_list)+1]] <- testing_set_tmp[,NMS := 'dynamic1'][,policy := policy_n]
  
  #===================================================================================================================
  # 3) PREDICT CNN
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  prediction_set_aggregated_dt2 <- predict_cnn(prediction_set_aggregated_dt, policy_n, pred_vars) %>% data.table()
  
  prediction_set_aggregated_dt2[,eonr_pred := flood(eonr_pred/10)*10] %>%
    .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set_tmp <- merge(testing_set_dt[!z %in% training_z],
                           prediction_set_aggregated_dt2[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
    .[N_fert == eonr_pred] %>%
    .[,c("region", "id_10", 'id_field',"mukey", "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
  
  
  
  results_list[[length(results_list)+1]] <- testing_set_tmp[,NMS := 'dynamic2'][,policy := policy_n]
  
}
#-------------------------------------------------------
# LEACHING TARGET POLICY (ORIGINAL)
testing_set_dt[, P := Y_corn * Pc - N_fert * Pn]  #update profits
testing_set_dt[, G := 0] #gov collection
policies_target <- names(reg_model_stuff)[str_detect(names(reg_model_stuff), pattern = 'target_')]

for(policy_n in policies_target){
  # policy_n = policies_nred[[1]]
  print(policy_n)
  #===================================================================================================================
  # 1b) MINIMUM OK-
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  # the NMS is trained with z1-10 and testing is evaluated with z11-25
  testing_set_tmp <- merge(testing_set_dt[!z %in% training_z],
                           reg_model_stuff[[policy_n]]$minimum_ok, #because I forgot to set the name to eonr_pred
                           by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    .[N_fert == eonr_pred] %>%
    .[,c("region", "id_10", 'id_field',"mukey", "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
  
  testing_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
  
  results_list[[length(results_list)+1]] <- testing_set_tmp[,NMS := 'static'][,policy := policy_n]
  
  #===================================================================================================================
  # 2) PREDICT WITH REGIONAL RF 2 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  
  prediction_set_aggregated_dt[,eonr_pred := flood(predict(reg_model_stuff[[policy_n]]$rf2, prediction_set_aggregated_dt)/10)*10]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set_tmp <- merge(testing_set_dt[!z %in% training_z],
                           prediction_set_aggregated_dt[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
    .[N_fert == eonr_pred] %>%
    .[,c("region", "id_10", 'id_field',"mukey", "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
  
  results_list[[length(results_list)+1]] <- testing_set_tmp[,NMS := 'dynamic1'][,policy := policy_n]
  
  #===================================================================================================================
  # 3) PREDICT CNN
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  prediction_set_aggregated_dt2 <- predict_cnn(prediction_set_aggregated_dt, policy_n, pred_vars) %>% data.table()
  
  prediction_set_aggregated_dt2[,eonr_pred := flood(eonr_pred/10)*10] %>%
    .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set_tmp <- merge(testing_set_dt[!z %in% training_z],
                           prediction_set_aggregated_dt2[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
    .[N_fert == eonr_pred] %>%
    .[,c("region", "id_10", 'id_field',"mukey", "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
  
  results_list[[length(results_list)+1]] <- testing_set_tmp[,NMS := 'dynamic2'][,policy := policy_n]
  
}

#-------------------------------------------------------
# LEACHING TARGET POLICY LONG TERM OPTIMIZATION
testing_set_dt[, P := Y_corn * Pc - N_fert * Pn]  #update profits
testing_set_dt[, G := 0] #gov collection
policies_nred <- names(reg_model_stuff)[str_detect(names(reg_model_stuff), pattern = 'nred_')]

for(policy_n in policies_nred){
  # policy_n = policies_nred[[16]]
  nred_n <- as.numeric(str_extract(policy_n,pattern = '[0-9.]+'))
  print(policy_n)
  #===================================================================================================================
  # 1b) MINIMUM OK-
  # testing_set_dt <- testing_set_dt[id_10 == 1436 & id_field == 2 & mukey == 178851 & z == 20]
  # prediction_set_aggregated_dt <- prediction_set_aggregated_dt[id_10 == 1436 & id_field == 2 & z == 20]
  # #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  # the NMS is trained with z1-10 and testing is evaluated with z11-25
  testing_set_tmp <- merge(testing_set_dt[!z %in% training_z],
                           reg_model_stuff[[policy_n]]$minimum_ok, #because I forgot to set the name to eonr_pred
                           by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    .[N_fert == eonr_pred] %>%
    .[,c("region", "id_10", 'id_field',"mukey", "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
  
  testing_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
  
  results_list[[length(results_list)+1]] <- testing_set_tmp[,NMS := 'static'][,policy := policy_n]
  
  #===================================================================================================================
  # 2) PREDICT WITH REGIONAL RF 2 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  
  prediction_set_aggregated_dt[,eonr_pred := flood(predict(reg_model_stuff[[policy_n]]$rf2, prediction_set_aggregated_dt)/10)*10]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set_tmp <- merge(testing_set_dt[!z %in% training_z],
                           prediction_set_aggregated_dt[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
    .[N_fert == eonr_pred] %>%
    .[,c("region", "id_10", 'id_field',"mukey", "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
  
  results_list[[length(results_list)+1]] <- testing_set_tmp[,NMS := 'dynamic1'][,policy := policy_n]
  
  #===================================================================================================================
  # 3) PREDICT CNN
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  prediction_set_aggregated_dt2 <- predict_cnn(prediction_set_aggregated_dt, policy_n, pred_vars) %>% data.table()
  
  prediction_set_aggregated_dt2[,eonr_pred := flood(eonr_pred/10)*10] %>%
    .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set_tmp <- merge(testing_set_dt[!z %in% training_z],
                           prediction_set_aggregated_dt2[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
    .[N_fert == eonr_pred] %>%
    .[,c("region", "id_10", 'id_field',"mukey", "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
  
  results_list[[length(results_list)+1]] <- testing_set_tmp[,NMS := 'dynamic2'][,policy := policy_n]
  
}

#-------------------------------------------------------
# LEACHING TARGET POLICY SHADOW VALUE

# testing_set_dt[, P := Y_corn * Pc - N_fert * Pn]  #update profits
# testing_set_dt[, G := 0] #gov collection
# 
# baseline_leaching_dt <- merge(testing_set_dt,
#                               reg_model_stuff[['ratio_5']]$minimum_ok, 
#                               by = c('region')) %>% 
#   .[N_fert == eonr_pred] %>%  aggregate_by_area(data_dt = ., variables = c('L'), 
#                                                 weight = 'area_ha', by_c = c('region', 'id_10', 'id_field','z'))
# setnames(baseline_leaching_dt, 'L', 'leach_base')
# policies_shadow <- names(reg_model_stuff)[str_detect(names(reg_model_stuff), pattern = 'shadow_')]
# 
# for(policy_n in sort(policies_shadow)){
#   # policy_n = policies_shadow[[1]]
#   nred_n <- as.numeric(str_extract(policy_n,pattern = '[0-9.]+'))
#   print(policy_n)
#   
#   #===================================================================================================================
#   # 1b) MINIMUM OK-
#   
#   #---------------------------------------------------------------------------
#   # PERFORMANCE EVALUATION
#   # the NMS is trained with z1-10 and testing is evaluated with z11-25
#   testing_set_tmp <- merge(testing_set_dt[!z %in% training_z],
#                            reg_model_stuff[[policy_n]]$minimum_ok, #because I forgot to set the name to eonr_pred
#                            by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
#     .[N_fert == eonr_pred] %>%
#     .[,c("region", "id_10", 'id_field',"mukey", "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
#   
#   testing_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
#   
#   results_list[[length(results_list)+1]] <- testing_set_tmp[,NMS := 'static'][,policy := policy_n]
#   
#   #===================================================================================================================
#   # 4) PREDICT WITH REGIONAL RF 2 - UR 
#   # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
#   
#   # prediction_set_aggregated_dt[,eonr_pred := ceiling(predict(reg_model_stuff[[policy_n]]$rf2, prediction_set_aggregated_dt)/10)*10]
#   # 
#   # prediction_set_aggregated_dt[,shadow_val_rel := predict(reg_model_stuff[["shadow_0.8"]]$rf2_shadow, prediction_set_aggregated_dt)]
#   # summary(prediction_set_aggregated_dt$shadow_val_rel)
# 
#   #---------------------------------------------------------------------------
#   # PERFORMANCE EVALUATION
#   fields_testing <- unique(testing_set_dt[,.(id_10, id_field)])
#   registerDoParallel(12) # register the cluster
#   # registerDoParallel(cores = 10)
#   testing_set_tmp_list = foreach(i = 1:nrow(fields_testing), .combine = "c", .packages = c("data.table")) %dopar% {
#     # i= 35
#     print(i)
#     fields_testing_n <- fields_testing[i]
#     
#     prediction_set_field <- filter_dt_in_dt(prediction_set_aggregated_dt[!z %in% training_z], fields_testing_n, return_table = T)
#     
#     prediction_set_field[,eonr_pred_econ := ceiling(predict(reg_model_stuff[['ratio_5']]$rf2, prediction_set_field)/10)*10]
#     prediction_set_field[,eonr_pred_cut := ceiling(predict(reg_model_stuff[[policy_n]]$rf2, prediction_set_field)/10)*10]
#     prediction_set_field[,shadow_val_rel := predict(reg_model_stuff[[policy_n]]$rf2_shadow, prediction_set_field)]
#     
#     prediction_set_field[,eonr_pred_1 := ceiling((shadow_val_rel * eonr_pred_econ + (1-shadow_val_rel) * eonr_pred_cut)/10)*10]
#     
#     testing_set_field <- filter_dt_in_dt(testing_set_dt[!z %in% training_z], fields_testing_n, return_table = T)
#     
#     
#     baseline_leaching_field <- filter_dt_in_dt(baseline_leaching_dt[!z %in% training_z], fields_testing_n, return_table = T)
#     
#     
#     leaching_balance = 0
#     for(z_n in unique(testing_set_field$z)){
#       prediction_set_field[z == z_n, eonr_pred := eonr_pred_1 + round(leaching_balance/10)*10]
#       prediction_set_field[z == z_n, leaching_bal := leaching_balance]
#     
#       testing_set_tmp_field <- merge(testing_set_field,
#                                prediction_set_field[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
#         .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
#         .[N_fert == eonr_pred] %>%
#         .[,c("region", "id_10", 'id_field',"mukey", "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
#       
#       
#       testing_set_tmp_leaching <- aggregate_by_area(data_dt = testing_set_tmp_field, variables = c('L'), 
#                                                     weight = 'area_ha', by_c = c('region', 'id_10', 'id_field','z'))
#   
#       leaching_balance <- sum(baseline_leaching_field[z <= z_n]$leach_base) * nred_n - sum(testing_set_tmp_leaching[z <= z_n]$L)
#       leaching_balance
#       round(leaching_balance/10)*10
#   }#end z_n loop
#       sum(testing_set_tmp_leaching$L) / sum(baseline_leaching_field$L)
#       list(testing_set_tmp_field)
#   }#end dopar loop  
#   stopImplicitCluster()
#   
#   testing_set_tmp <- rbindlist(testing_set_tmp_list)
#   
#   # shadow_results_dt <- aggregate_by_area(data_dt = testing_set_tmp, variables = c('L'),
#   #                                        weight = 'area_ha', by_c = c('z'))
#   # 
#   # baseline_leaching_dt2 <- aggregate_by_area(data_dt = baseline_leaching_dt, variables = c('leach_base'),
#   #                                          weight = 'area_ha', by_c = c('z'))
#   # 
#   # shadow_results_dt2 <- merge(shadow_results_dt[,-'area_ha'], baseline_leaching_dt2[,-'area_ha'], by = c('z'))
#   # shadow_results_dt2[,L_rel := L/leach_base]
#   # shadow_results_dt2[,z := as.numeric(z)]
#   # shadow_results_dt2[order(z)]
#   # shadow_results_dt3 <- shadow_results_dt2[,.(L_rel = sum(L)/sum(leach_base)), by = .(id_10, id_field)]
#   # 
#   # summary(shadow_results_dt3$L_rel)
#   # 
#   # testing_set_tmp[,.N, .(id_10, id_field)]
#   
#   results_list[[length(results_list)+1]] <- testing_set_tmp[,NMS := 'dynamic'][,policy := policy_n]
#   
# }

perfomances_dt <- rbindlist(results_list)

perfomances_dt[,.N, .(id_10, mukey,id_field)] %>% 
  .[,.N, .(id_10, id_field)] %>% .[,.N, .(id_10)] %>% .[,N] %>% table() #number of fields by cell

perfomances_dt[,.N, .(id_10, mukey,id_field, policy, NMS)]$N %>% table() #number of z by all the other things

saveRDS(perfomances_dt, "./n_policy_box/Data/files_rds/perfomances_dt.rds")


# perfomances_dt2 <- readRDS( "./n_policy_box/Data/files_rds/perfomances_dt.rds")
# perfomances_dt <- rbind(perfomances_dt, perfomances_dt2)
# perfomances_dt[,.N, by = policy]
# saveRDS(perfomances_dt, "./n_policy_box/Data/files_rds/perfomances_dt.rds")
# perfomances_dt <- readRDS( "./n_policy_box/Data/files_rds/perfomances_dt.rds")
# 
# #Add the eonr ex_post
# yc_yearly_dt3 <- readRDS("./n_policy_box/Data/files_rds/yc_yearly_dt3.rds")
# yc_yearly_dt3[, P := Y_corn * Pc - N_fert * Pn]
# yc_yearly_dt3_eonr <- yc_yearly_dt3[, .SD[ P == max( P)], by = .(id_10, mukey, z)]
# yc_yearly_dt3_eonr <- yc_yearly_dt3_eonr[, .SD[ N_fert == min( N_fert)], by = .(id_10, mukey, z)]
# setnames(yc_yearly_dt3_eonr, 'N_fert', 'N_fert_12')
# 
# perfomances_dt <- merge(perfomances_dt, yc_yearly_dt3_eonr[,.(id_10, mukey, z, N_fert_12)], by = c('id_10', 'mukey', 'z'))
# perfomances_dt[,.(N_fert = mean(N_fert)), by=.(region, NMS)]
# perfomances_dt[, P_diff := (P[NMS == 2] - P[NMS == 1]), by = .(id_10, id_field, mukey, z)]
# 
# perfomances_dt[region == 3 & N_fert == 170 & NMS == 2]$P_diff %>% summary()
# 
# 
# 
# soy_dt <- yc_yearly_dt3_eonr[,.(id_10, mukey, z, Y_soy = Yld_prev)]
# soy_dt[,z := as.numeric(z)-1]
# corn_dt <- yc_yearly_dt3_eonr[,.(id_10, mukey, z, Y_corn)]
# corn_dt[,z := as.numeric(z)]
# yield_test <- merge(soy_dt, corn_dt, by = c('id_10', 'mukey', 'z'))
# 
# plot_dt <- yield_test[sample(nrow(yield_test), 2000)]
# 
# ggplot(plot_dt) + geom_point(aes(x=Y_soy, y = Y_corn))
# 
# #What years is the model failing
# test <- perfomances_dt[NMS == 2]
# test[,.(P_diff = mean(P_diff)), by = z][order(P_diff)]
# test[,.(P_diff = mean(P_diff)), by = region][order(P_diff)]
# 
# #What happened in z14?
# test[region == 3,.(P_diff = mean(P_diff), 
#                          N_fert = mean(N_fert),
#                          n_deep_v5 = mean(n_deep_v5),
#                          N_fert_12 = mean(N_fert_12),
#         Y_corn = mean(Y_corn)), by = z]
# 
# test[,.(P_diff = mean(P_diff), 
#                N_fert = mean(N_fert),
#                N_fert_12 = mean(N_fert_12)), by = z]
# 
# no_cost_varb <- c('region', "rain_30", "rain_60", "rain_90",
#                   "t_max_30", "t_max_60", "t_max_90", "", "t_min_60",
#                   "t_min_90", "Yld_prev", 'Y_corn_lt_avg', 'Y_corn_lt_min', 'Y_corn_lt_max', "lai_v5")
# 
# ss_varb <- c("","esw_pct_v5", "whc")
# 
# prediction_set_aggregated_dt[region == 3,.(n_0_60cm_v5 = mean(n_0_60cm_v5),
#                                 t_min_30 = mean(t_min_30),
#                                 Yld_prev = mean(Yld_prev),
#                                 rain_90 = mean(rain_90)), by = z][order(z)]
# 
# 
# 
# varImpPlot(reg_model_stuff[['ratio_5']]$rf2, type=2)
# 
# plot_dt <- test[sample(nrow(test), 1000)]
# plot_dt[, region := factor(region)]
# 
# ggplot(plot_dt[region == 3]) + geom_point(aes(x=N_fert, y = P_diff, color = region))
# plot_dt[region == 3 & N_fert == 170 & NMS == 2]
# 
# ggplot(plot_dt) + geom_point(aes(x=n_deep_v5, y = P_diff, color = region))
# ggplot(plot_dt) + geom_point(aes(x=n_deep_v5, y = N_fert, color = region))
# 
