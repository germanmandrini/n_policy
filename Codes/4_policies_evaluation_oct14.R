rm(list=ls())

# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# codes_folder <-'C:/Users/germa/Documents'#Dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# codes_folder <-'C:/Users/germanm2/Documents'#CPSC
setwd('~')#Server
codes_folder <-'~' #Server



source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')
source(paste0(codes_folder, '/n_policy_git/Codes/parameters.R'))

library("foreach")
library("doParallel")

#Run python in condas environment from R
# https://cran.r-project.org/web/packages/reticulate/vignettes/calling_python.html
# install.packages('reticulate')
# library(reticulate)
# conda_install("GEOANN", "pyreadr")
# use_condaenv('GEOANN', conda = '/opt/anaconda3/condabin/conda')
# source_python("./n_policy_git/Codes/3c_cnn_functions_sep10.py")

reg_model_stuff <- readRDS( "./n_policy_box/Data/files_rds/reg_model_stuff.rds")

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
  
  prediction_set_aggregated_dt[,eonr_pred := ceiling(predict(reg_model_stuff[[policy_n]]$rf2, prediction_set_aggregated_dt)/10)*10]
  
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
  # prediction_set_aggregated_dt2 <- predict_cnn(prediction_set_aggregated_dt, policy_n, pred_vars) %>% data.table()
  # 
  # prediction_set_aggregated_dt2[,eonr_pred := round(eonr_pred/10)*10] %>%
  #   .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))]
  # 
  # ggplot(prediction_set_aggregated_dt2[sample(1:nrow(prediction_set_aggregated_dt2), 1000)]) + 
  #   geom_point(aes(x = eonr_pred_rf, y= eonr_pred))+ 
  #   theme(aspect.ratio=1) + coord_fixed() + geom_abline() + ylim(0, 260)+ xlim(0, 260) +
  #   theme(axis.text=element_text(size=12),
  #         axis.title=element_text(size=14,face="bold"))
  # 
  # #---------------------------------------------------------------------------
  # # PERFORMANCE EVALUATION
  # testing_set_tmp <- merge(testing_set_dt[!z %in% training_z],
  #                          prediction_set_aggregated_dt2[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  #   .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
  #   .[N_fert == eonr_pred] %>%
  #   .[,c("region", "id_10", 'id_field',"mukey", "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
  # 
  
  
  # results_list[[length(results_list)+1]] <- testing_set_tmp[,NMS := 'dynamic2'][,policy := policy_n]
  
}
#---------------------------------------------------------------------------
# LEACHING POLICY

policies_leach <- names(reg_model_stuff)[str_detect(names(reg_model_stuff), pattern = 'leach_[0-9.]')]

for(policy_n in policies_leach){
  # policy_n = policies_leach[[3]]
  leach_n <- as.numeric(str_extract(policy_n,pattern = '[0-9.]+'))
  # print(leach_n)
  
  
  no_pay_limit <- reg_model_stuff$leach_threshold #- leach_n
  
  testing_set_dt[,L_extra := L - no_pay_limit[region]]
  testing_set_dt[L_extra <= 0, L_extra := 0]
  
  testing_set_dt[, P := Y_corn * Pc - N_fert * Pn - L_extra * leach_n] #update profits
  testing_set_dt[, G := L_extra * leach_n] #gov collectionn
  
  
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
  
  prediction_set_aggregated_dt[,eonr_pred := ceiling(predict(reg_model_stuff[[policy_n]]$rf2, prediction_set_aggregated_dt)/10)*10]
  
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
  # prediction_set_aggregated_dt2 <- predict_cnn(prediction_set_aggregated_dt, policy_n, pred_vars) %>% data.table()
  # 
  # prediction_set_aggregated_dt2[,eonr_pred := round(eonr_pred/10)*10] %>%
  #   .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))]
  # 
  # #---------------------------------------------------------------------------
  # # PERFORMANCE EVALUATION
  # testing_set_tmp <- merge(testing_set_dt[!z %in% training_z],
  #                          prediction_set_aggregated_dt2[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  #   .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
  #   .[N_fert == eonr_pred] %>%
  #   .[,c("region", "id_10", 'id_field',"mukey", "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
  # 
  
  
  # results_list[[length(results_list)+1]] <- testing_set_tmp[,NMS := 'dynamic2'][,policy := policy_n]
  
}
#---------------------------------------------------------------------------
# BALANCE POLICY

policies_bal <- names(reg_model_stuff)[str_detect(names(reg_model_stuff), pattern = 'bal_[0-9.]')]

testing_set_dt[,N_balance := N_fert - Y_corn * 11/1000]

for(policy_n in policies_bal){
  # policy_n = policies_bal[[3]]
  bal_n <- as.numeric(str_extract(policy_n,pattern = '[0-9.]+'))
  # print(bal_n)
  
  no_pay_limit <- reg_model_stuff$bal_threshold# - bal_n
  
  testing_set_dt[,N_extra := N_balance - no_pay_limit[region]]
  testing_set_dt[N_extra <= 0, N_extra := 0]
  
  testing_set_dt[, P := Y_corn * Pc - N_fert * Pn - N_extra * bal_n]#update profits
  testing_set_dt[, G := N_extra * bal_n] #gov collectionn
  
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
  
  prediction_set_aggregated_dt[,eonr_pred := ceiling(predict(reg_model_stuff[[policy_n]]$rf2, prediction_set_aggregated_dt)/10)*10]
  
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
  # prediction_set_aggregated_dt2 <- predict_cnn(prediction_set_aggregated_dt, policy_n, pred_vars) %>% data.table()
  # 
  # prediction_set_aggregated_dt2[,eonr_pred := round(eonr_pred/10)*10] %>%
  #   .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))]
  # 
  # #---------------------------------------------------------------------------
  # # PERFORMANCE EVALUATION
  # testing_set_tmp <- merge(testing_set_dt[!z %in% training_z],
  #                          prediction_set_aggregated_dt2[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  #   .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
  #   .[N_fert == eonr_pred] %>%
  #   .[,c("region", "id_10", 'id_field',"mukey", "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
  # 
  
  
  # results_list[[length(results_list)+1]] <- testing_set_tmp[,NMS := 'dynamic2'][,policy := policy_n]
  
}

#-------------------------------------------------------
# N FERTILIZER REDUCTION (CUT RATIO 5 RECOMENDATIONS)
testing_set_dt[, P := Y_corn * Pc - N_fert * Pn]  #update profits
testing_set_dt[, G := 0] #gov collection

policies_red <- paste0('red_', seq(0,30, by = 1))

for(policy_n in policies_red){
# policy_n = policies_red[[11]]
red_n <- as.numeric(str_extract(policy_n,pattern = '[0-9.]+'))
print(policy_n)
#===================================================================================================================
# 1b) MINIMUM OK-
# testing_set_dt <- testing_set_dt[id_10 == 1436 & id_field == 2 & mukey == 178851 & z == 20]
# prediction_set_aggregated_dt <- prediction_set_aggregated_dt[id_10 == 1436 & id_field == 2 & z == 20]
# #---------------------------------------------------------------------------
# PERFORMANCE EVALUATION
# the NMS is trained with z1-10 and testing is evaluated with z11-25
minimum_ok_dt <- copy(reg_model_stuff[["ratio_5"]]$minimum_ok)
minimum_ok_dt[, eonr_pred := round(eonr_pred*((100-red_n)/100)/10)*10]

testing_set_tmp <- merge(testing_set_dt[!z %in% training_z],
                         minimum_ok_dt, #because I forgot to set the name to eonr_pred
                         by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[N_fert == eonr_pred] %>%
  .[,c("region", "id_10", 'id_field',"mukey", "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]

testing_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()

results_list[[length(results_list)+1]] <- testing_set_tmp[,NMS := 'static'][,policy := policy_n]

#===================================================================================================================
# 2) PREDICT WITH REGIONAL RF 2 - UR 
# GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY

prediction_set_aggregated_dt[,eonr_pred := ceiling(predict(reg_model_stuff[["ratio_5"]]$rf2, prediction_set_aggregated_dt)/10)*10]
prediction_set_aggregated_dt[, eonr_pred := round(eonr_pred*((100-red_n)/100)/10)*10]
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
    # prediction_set_aggregated_dt2 <- predict_cnn(prediction_set_aggregated_dt, policy_n, pred_vars) %>% data.table()
    # 
    # prediction_set_aggregated_dt2[,eonr_pred := round(eonr_pred/10)*10] %>%
    #   .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))]
    # 
    # #---------------------------------------------------------------------------
    # # PERFORMANCE EVALUATION
    # testing_set_tmp <- merge(testing_set_dt[!z %in% training_z],
    #                          prediction_set_aggregated_dt2[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    #   .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
    #   .[N_fert == eonr_pred] %>%
    #   .[,c("region", "id_10", 'id_field',"mukey", "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    # 
    # results_list[[length(results_list)+1]] <- testing_set_tmp[,NMS := 'dynamic2'][,policy := policy_n]
    
}

#-------------------------------------------------------------------------------------------------------
perfomances_dt <- rbindlist(results_list)

# perfomances_dt <- merge(perfomances_dt[policy_name != 'red'], perfomances_dt2, fill = T)

perfomances_dt[,.N, .(id_10, mukey,id_field)] %>% 
  .[,.N, .(id_10, id_field)] %>% .[,.N, .(id_10)] %>% .[,N] %>% table() #number of fields by cell

perfomances_dt[,.N, .(id_10, mukey,id_field, policy, NMS)]$N %>% table() #number of z by all the other things

# saveRDS(perfomances_dt, "./n_policy_box/Data/files_rds/perfomances_dt.rds")

perfomances_dt2 <- readRDS("./n_policy_box/Data/files_rds/perfomances_dt.rds")
perfomances_dt2 <- perfomances_dt2[!str_detect(perfomances_dt2$policy, 'bal_')]
table(perfomances_dt2$policy)
perfomances_dt2 <- rbind(perfomances_dt2, perfomances_dt)
saveRDS(perfomances_dt2, "./n_policy_box/Data/files_rds/perfomances_dt.rds")
