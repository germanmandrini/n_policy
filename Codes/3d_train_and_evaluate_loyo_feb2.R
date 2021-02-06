rm(list=ls())
# 
# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# codes_folder <-'C:/Users/germa/Documents'#Dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# codes_folder <-'C:/Users/germanm2/Documents'#CPSC
setwd('~')#Server
codes_folder <-'~' #Server

source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')
source(paste0(codes_folder, '/n_policy_git/Codes/parameters.R'))
library(randomForest)
library(reticulate)

# use_condaenv('GEOANN', conda = '/opt/anaconda3/condabin/conda')
# source_python("./n_policy_git/Codes/3c_cnn_functions_sep10.py")

# yc_yearly_dt3 <- readRDS("./n_policy_box/Data/files_rds/yc_yearly_dt3.rds")
# grid10_soils_dt5 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt5.rds") %>% data.table()

reg_model_stuff <- readRDS( "./n_policy_box/Data/files_rds/reg_model_stuff.rds")
yc_field_dt <- readRDS("./n_policy_box/Data/files_rds/yc_field_dt.rds")
thresholds_dt <- readRDS( "./n_policy_box/Data/files_rds/thresholds_dt.rds") #this is tricky. Are the mean values after running this script and using the ratio_5 policy for all the z. I do it this way to use always the same

# =========================================================================================================================================================
#Run python in condas environment from R
# https://cran.r-project.org/web/packages/reticulate/vignettes/calling_python.html
# install.packages('reticulate')
# library(reticulate)
# conda_install("GEOANN", "pyreadr")
# use_condaenv('GEOANN', conda = '/opt/anaconda3/condabin/conda')
# source_python("./n_policy_git/Codes/3c_cnn_functions_dec9.py")
# =========================================================================================================================================================
## PREPARE THE DATA ========


# pred_vars <- c("rain_30", "rain_60", "rain_90",
#                "t_max_30", "t_max_60", "t_max_90", "t_min_30", "t_min_60",
#                "t_min_90", 'Y_corn_lt_avg', "day_sow", "day_v5", "lai_v5",
#                "whc",  "oc_20cm_v5", "sw_dep_v5", "n_0_60cm_v5",  "surfaceom_wt_v5", 
#                "sand_40cm", "clay_40cm") #"root_wt_v5",, "n_deep_v5", "esw_pct_v5", 
# 
# saveRDS(pred_vars, "./n_policy_box/Data/files_rds/pred_vars.rds")



# names(reg_model_stuff)
# training_z <- 1:10
# validation_z <- 11:15

# training_z <- sample(1:30, 10)
# validation_z <- sample(setdiff(1:30, training_z), 5)

# reg_model_stuff[['training_z']] <-  training_z
# reg_model_stuff[['validation_z']] <-  validation_z

stations_dt <- reg_model_stuff[['stations']] %>% 
  .[,.(id_10, id_field)] %>% unique() %>% .[,station := 1]

yc_field_dt2 <- merge(yc_field_dt, stations_dt, by = c('id_10', 'id_field'), all.x = TRUE)
yc_field_dt2[is.na(station), station := 0]
yc_field_dt2[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z, N_fert)]$area_ha %>% summary()
yc_field_dt2[,year := as.numeric(z)+1988]
#-----------------------------
# #Add the weather
# weather_historic_dt <- readRDS('./n_policy_box/Data/met_files/weather_historic_dt2019.rds')
# weather_historic_dt[,date := as.Date(day-1, origin = paste0(year, "-01-01"))]
# weather_historic_dt[,month := month(date)]
# weather_historic_dt[,quarter := ceiling(month/3)]
# 
# weather_quarter_dt <- weather_historic_dt[year > 1988, .(radn = mean(radn),
#                                                          maxt = mean(maxt),
#                                                          mint = mean(mint),
#                                                          rain = sum(rain)), by = .(id_10, year, quarter)]
# 
# weather_wide_dt <- dcast(weather_quarter_dt, id_10 + year ~ quarter, value.var=c("radn", "maxt","mint", "rain"))
# names(weather_wide_dt)
# yc_field_dt2 <- merge(yc_field_dt2, weather_wide_dt, by = c('id_10','year'))
# yc_field_dt2[, Yld_response := max(Y_corn) - min(Y_corn), by = .(id_10, id_field,z)]
saveRDS(yc_field_dt2, "./n_policy_box/Data/files_rds/yc_field_dt2.rds")
#-----------------------------
# yc_field_dt2[station == 1 & z %in% training_z, set := 'training']
# yc_field_dt2[station == 1 & z %in% validation_z, set := 'validation']
# yc_field_dt2[station == 0 & (!z %in% c(training_z, validation_z)), set := 'evaluation']
# yc_field_dt2 <- yc_field_dt2[!is.na(set)][,-'station']
# yc_field_dt2[,.N, .(set, id_10, id_field)][,.N, set]#fields by set
# saveRDS(yc_field_dt2, "./n_policy_box/Data/files_rds/yc_field_dt2.rds")
# 
# training_set_dt <- yc_field_dt2[set == 'training']
# testvalid_set_dt <- yc_field_dt2[!set == 'training']
# validation_set_dt <- yc_field_dt2[set == 'validation']
# prediction_set_aggregated_dt <- testvalid_set_dt[N_fert == 180][,-'N_fert'] #one line per field, not yield curve
# 
# rm(yc_field_dt2)
#----------------------------------------------------------------------------------------------


z_n = 10
z_seq <- 1:30
for(z_n in z_seq){
  print(z_n)
  results_list <- list()
  # yc_field_dt2[station == 1 & z %in% training_z, set := 'training']
  # yc_field_dt2[station == 1 & z %in% validation_z, set := 'validation']
  # yc_field_dt2[station == 0 & (!z %in% c(training_z, validation_z)), set := 'evaluation']
  # yc_field_dt2 <- yc_field_dt2[!is.na(set)][,-'station']
  # yc_field_dt2[,.N, .(set, id_10, id_field)][,.N, set]#fields by set
  # saveRDS(yc_field_dt2, "./n_policy_box/Data/files_rds/yc_field_dt2.rds")
  # =========================================================================================================================================================
  # CREATE THE N RATIO TAX MODEL
  
  #Load datasets again
  training_set_dt <- yc_field_dt2[station == 1 & z != z_n]
  evaluation_set_dt <- yc_field_dt2[station != 1 & z == z_n]
  prediction_set_aggregated_dt <- evaluation_set_dt[N_fert == 180][,-c('N_fert')] #one line per field, not yield curve
  
  
  ratio_seq <- sort(c(seq(5, 20, by = 1)))

  for(level_n in ratio_seq){
     # level_n = 5
    
    Pn_tmp = level_n * Pc
    print(Pn_tmp/Pc)
    
    training_set_dt[, P := Y_corn * Pc - N_fert * Pn_tmp]  #update profits
    # =========================================================================================================================================================
    # CREATE THE STATIC MRTN
    # training_mrtn_dt <- training_set_dt[Yld_response > Yld_response_threshold] #Needs to be here, to use updated profits 
    # 
    # static_data <- aggregate_by_area(data_dt = training_mrtn_dt, variables = c('P'), 
    #                                  weight = 'area_ha', by_c = c('region', 'N_fert'))
    # 
    # static_mrtn_dt  <- static_data %>% 
    #   .[, .SD[ P == max( P)], by = .(region)] %>%
    #   .[, .SD[ N_fert == min( N_fert)], by = .(region)] %>%
    #   .[,.(region, eonr_pred = N_fert)] %>%
    #   .[order(region)]
    # 
    # 
    # (plot1 <- ggplot() +
    #     geom_line(data = static_data, aes(x = N_fert, y = P, color = region), size = 1.5)+
    #     geom_point(data = static_data[,.SD[P == max(P)], by = region], aes(x = N_fert, y = P), size = 3) +
    #     # geom_point(data = static_data[P_diff >= -level_n][, .SD[ N_fert == min( N_fert)], by = .(region)], aes(x = N_fert, y = P), shape = 2, size = 3)+
    #     ylab('Profits ($/ha)')+
    #     theme_bw() +
    #     xlab('N rate (kg/ha)'))
    
    
    # =========================================================================================================================================================
    # CREATE THE RF-HIGH
    training_eonr_dt  <- training_set_dt[, .SD[ P == max(P)], by = .(id_10, id_field, z)] %>%
      .[, .SD[ N_fert == min( N_fert)], by = .(id_10, id_field, z)] %>%
      .[,c('N_fert', low_var, high_var, 'year'), with = FALSE]
    
    setnames(training_eonr_dt, 'N_fert', 'eonr')
    
    # RF Model 2------------------------
    # mtry <- tuneRF(training_eonr_dt2[,c(pred_vars), with = FALSE],training_eonr_dt2$eonr, mtryStart = 6, ntreeTry=1000,
    #                 stepFactor=1.1,improve=0.01, trace=TRUE, plot=TRUE) # ,mtryStart = 5
    # best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
    
    best.m = 6
    
    dynamic <- randomForest(formula = as.formula(paste('eonr ~ ', paste(c(low_var, high_var), collapse = ' + '))), 
                           data = training_eonr_dt[,c('eonr', low_var, high_var, 'year'), with = FALSE],
                           strata = year, #I think it will bootstrap by year
                           importance = TRUE , mtry = best.m, ntree=2000, nodesize = 30)
    
    varImpPlot(dynamic, type=2)
    plot(dynamic)
    
    if(level_n == 5){
      pdf("./n_policy_box/Data/figures/VarImportancePlot_ratio.pdf")
      varImpPlot(dynamic, type=2, main = '')
      dev.off() 
    }
    
   
    #===================================================================================================================
    # EVALUATION
    
    #Prepare the data
    evaluation_set_dt[, P := Y_corn * Pc - N_fert * Pn_tmp]  #update profits
    evaluation_set_dt[, G := N_fert * (Pn_tmp - Pn)] #gov collection
    #===================================================================================================================
    # 1) STATIC MRTN
    
    # the NRT is trained with z1-10 and testing is evaluated with z11-25
    # evaluation_set_tmp <- merge(evaluation_set_dt, static_mrtn_dt, 
    #                          by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    #   .[N_fert == eonr_pred] %>%
    #   .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    # 
    # results_list[[length(results_list)+1]] <- evaluation_set_tmp[,NRT := 'static'][,policy := paste0('ratio_', level_n)]
    # 
    # if(level_n == 5){ratio5_recommendations_list[['static_mrtn']] <- static_mrtn_dt}
    #===================================================================================================================
    # 2) dynamic
    # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH id_field
    
    prediction_set_aggregated_dt[,eonr_pred := round(predict(dynamic, prediction_set_aggregated_dt)/10,0)*10]
    
    evaluation_set_tmp <- merge(evaluation_set_dt,
                             prediction_set_aggregated_dt[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
      .[N_fert == eonr_pred] %>%
      .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    
    evaluation_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
    
    results_list[[length(results_list)+1]] <- evaluation_set_tmp[,NRT := 'dynamic'][,policy := paste0('ratio_', level_n)]
    
    if(level_n == 5){
      setnames(prediction_set_aggregated_dt, 'eonr_pred', 'eonr_ratio5')
      saveRDS(prediction_set_aggregated_dt, paste0("./n_policy_box/Data/files_rds/field_performances_tmp/ratio5_recommendations_",z_n, ".rds"))
    }
    
  }#end of ratio loop
  
  
  if(TRUE){
    # =========================================================================================================================================================
    # CREATE THE LEACHING FEE MODEL
    source(paste0(codes_folder, '/n_policy_git/Codes/parameters.R'))
    
    #Load datasets again
    training_set_dt <- yc_field_dt2[station == 1 & z != z_n]
    evaluation_set_dt <- yc_field_dt2[station != 1 & z == z_n]
    prediction_set_aggregated_dt <- evaluation_set_dt[N_fert == 180][,-c('N_fert')] #one line per field, not yield curve
    
    #Update threholds
    # leach_threshold <- yc_field_dt2[station == 1 & N_fert == 100] %>%
    #   .[, .(L_thr = quantile(L, probs = 0.5)), region] %>%
    #   .[order(region)] # not change with z_n
    thresholds_dt[,L_thr := round(L * 0.6, 0)]
    
    training_set_dt <- merge(training_set_dt, thresholds_dt[,.(region, L_thr)], by = 'region')
    
    training_set_dt[,L_extra := L - L_thr]
    training_set_dt[L_extra <= 0, L_extra := 0]
    training_set_dt$L_extra %>% summary()
    
    evaluation_set_dt <- merge(evaluation_set_dt, thresholds_dt[,.(region, L_thr)], by = 'region')
    
    evaluation_set_dt[,L_extra := L - L_thr]
    evaluation_set_dt[L_extra <= 0, L_extra := 0]
    
    leach_seq <-  sort(c(seq(0, 30, by = 2)))
    
    set.seed(123)
    
    for(level_n in leach_seq){
      # level_n = 5
      print(level_n)
      
      # training_set_dt[, P := Y_corn * Pc - N_fert * Pn]  #update profits
      
      training_set_dt[, P := Y_corn * Pc - N_fert * Pn - L_extra * level_n] #update profits
      
      training_set_dt[, P1 := Y_corn * Pc - N_fert * Pn]
      training_set_dt[, P2 := Y_corn * Pc - N_fert * Pn - L_extra * level_n] #update profits
      
      plot_dt <- training_set_dt[, .(P1 = mean(P1), 
                               P2 = mean(P2), 
                               L_extra = round(mean(L_extra),0)), by = .(region, N_fert)][order(region, N_fert)]
      
      ggplot() + 
        geom_line(data = plot_dt, aes(x = N_fert, y = P1, color = region))+
        geom_point(data = plot_dt[,.SD[P1 == max(P1)], by = region], aes(x = N_fert, y = P1))+
        geom_line(data = plot_dt, aes(x = N_fert, y = P2, color = region), linetype = 'dashed')+
        geom_point(data = plot_dt[,.SD[P2 == max(P2)], by = region], aes(x = N_fert, y = P2))
      
      
      # =========================================================================================================================================================
      # CREATE THE STATIC MRTN
      # training_mrtn_dt <- training_set_dt[Yld_response > Yld_response_threshold] #Needs to be here, to use updated profits 
      # 
      # static_data <- aggregate_by_area(data_dt = training_mrtn_dt, variables = c('P'), 
      #                                  weight = 'area_ha', by_c = c('region', 'N_fert'))
      # 
      # static_mrtn_dt  <- static_data %>% 
      #   .[, .SD[ P == max( P)], by = .(region)] %>%
      #   .[, .SD[ N_fert == min( N_fert)], by = .(region)] %>%
      #   .[,.(region, eonr_pred = N_fert)] %>%
      #   .[order(region)]
      # 
      # 
      # (plot1 <- ggplot() +
      #     geom_line(data = static_data, aes(x = N_fert, y = P, color = region), size = 1.5)+
      #     geom_point(data = static_data[,.SD[P == max(P)], by = region], aes(x = N_fert, y = P), size = 3) +
      #     # geom_point(data = static_data[P_diff >= -level_n][, .SD[ N_fert == min( N_fert)], by = .(region)], aes(x = N_fert, y = P), shape = 2, size = 3)+
      #     ylab('Profits ($/ha)')+
      #     theme_bw() +
      #     xlab('N rate (kg/ha)'))
      
      # =========================================================================================================================================================
      # CREATE THE RF-HIGH
      training_eonr_dt  <- training_set_dt[, .SD[ P == max(P)], by = .(id_10, id_field, z)] %>%
        .[, .SD[ N_fert == min( N_fert)], by = .(id_10, id_field, z)] %>%
        .[,c('N_fert', low_var, high_var, 'year'), with = FALSE]
      
      setnames(training_eonr_dt, 'N_fert', 'eonr')
      
      # RF Model 2------------------------
      # mtry <- tuneRF(training_eonr_dt2[,c(pred_vars), with = FALSE],training_eonr_dt2$eonr, mtryStart = 6, ntreeTry=1000,
      #                 stepFactor=1.1,improve=0.01, trace=TRUE, plot=TRUE) # ,mtryStart = 5
      # best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
      
      best.m = 6
      
      dynamic <- randomForest(formula = as.formula(paste('eonr ~ ', paste(c(low_var, high_var), collapse = ' + '))), 
                             data = training_eonr_dt[,c('eonr', low_var, high_var, 'year'), with = FALSE],
                             strata = year, #I think it will bootstrap by year
                             importance = TRUE , mtry = best.m, ntree=2000, nodesize = 30)
      
      varImpPlot(dynamic, type=2)
      plot(dynamic)
      
      if(level_n == 5){
        pdf("./n_policy_box/Data/figures/VarImportancePlot_leach.pdf")
        varImpPlot(dynamic, type=2, main = '')
        dev.off() 
      }
      
      #===================================================================================================================
      # EVALUATION
      
      #Prepare the data
      evaluation_set_dt[, P := Y_corn * Pc - N_fert * Pn - L_extra * level_n] #update profits
      evaluation_set_dt[, G := L_extra * level_n] #gov collectionn
      #===================================================================================================================
      # # 1) STATIC MRTN
      # 
      # # the NRT is trained with z1-10 and testing is evaluated with z11-25
      # evaluation_set_tmp <- merge(evaluation_set_dt, static_mrtn_dt, 
      #                          by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      #   .[N_fert == eonr_pred] %>%
      #   .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
      # 
      # 
      # results_list[[length(results_list)+1]] <- evaluation_set_tmp[,NRT := 'static'][,policy := paste0('leach_', level_n)]
      #===================================================================================================================
      # 2) dynamic
      # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH id_field
      
      prediction_set_aggregated_dt[,eonr_pred := round(predict(dynamic, prediction_set_aggregated_dt)/10,0)*10]
      
      evaluation_set_tmp <- merge(evaluation_set_dt,
                               prediction_set_aggregated_dt[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
        .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
        .[N_fert == eonr_pred] %>%
        .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
      
      evaluation_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
      
      results_list[[length(results_list)+1]] <- evaluation_set_tmp[,NRT := 'dynamic'][,policy := paste0('leach_', level_n)]
      
      
      #===================================================================================================================
    } #end of leaching loop
    
    # =========================================================================================================================================================
    # CREATE THE BALANCE FEE MODEL
    source(paste0(codes_folder, '/n_policy_git/Codes/parameters.R'))
    
    #Load datasets again
    training_set_dt <- yc_field_dt2[station == 1 & z != z_n]
    evaluation_set_dt <- yc_field_dt2[station != 1 & z == z_n]
    prediction_set_aggregated_dt <- evaluation_set_dt[N_fert == 180][,-c('N_fert')] #one line per field, not yield curve
    
    #Update threholds
  
    training_set_dt[,N_balance := N_fert - Y_corn * 11.5/1000]
    
    # bal_threshold <- yc_field_dt2[station == 1 & N_fert == 100] %>%
    #   .[,N_balance := N_fert - Y_corn * 11.5/1000] %>%
    #   .[, .(N_balance_thr = quantile(N_balance, probs = 0.5)), region] %>%
    #   .[order(region)] # not change with z_n
    
    thresholds_dt[, N_balance_thr := round(N_balance - 60,  0)]
    
    training_set_dt <- merge(training_set_dt[,-'N_balance_thr'], thresholds_dt[,.(region, N_balance_thr)], by = 'region')
    
    training_set_dt[,N_extra := N_balance - N_balance_thr]
    training_set_dt[N_extra <= 0, N_extra := 0]
    
    #evaluation set
    evaluation_set_dt[,N_balance := N_fert - Y_corn * 11.5/1000]
    evaluation_set_dt <- merge(evaluation_set_dt[,-'N_balance_thr'], thresholds_dt[,.(region, N_balance_thr)], by = 'region')
  
    evaluation_set_dt[,N_extra := N_balance - N_balance_thr]
    evaluation_set_dt[N_extra <= 0, N_extra := 0]
    
    
    bal_seq <- sort(seq(0, 5, by = 0.25))
    
    set.seed(123)
    
    for(level_n in bal_seq){
      # level_n = 5
      print(level_n)
      
      # training_set_dt[, P := Y_corn * Pc - N_fert * Pn]  #update profits
      training_set_dt[, P := Y_corn * Pc - N_fert * Pn - N_extra * level_n]#update profits
      training_set_dt[, P1 := Y_corn * Pc - N_fert * Pn]
      training_set_dt[, P2 := Y_corn * Pc - N_fert * Pn - N_extra * level_n]
      
      plot_dt <- training_set_dt[, .(P1 = mean(P1), 
                               P2 = mean(P2), 
                               N_extra = round(mean(N_extra),0)), by = .(region, N_fert)][order(region, N_fert)]
      ggplot() + 
        geom_line(data = plot_dt, aes(x = N_fert, y = P1, color = region))+
        geom_point(data = plot_dt[,.SD[P1 == max(P1)], by = region], aes(x = N_fert, y = P1))+
        geom_line(data = plot_dt, aes(x = N_fert, y = P2, color = region), linetype = 'dashed')+
        geom_point(data = plot_dt[,.SD[P2 == max(P2)], by = region], aes(x = N_fert, y = P2))
      
      # =========================================================================================================================================================
      # CREATE THE STATIC MRTN
      # training_mrtn_dt <- training_set_dt[Yld_response > Yld_response_threshold] #Needs to be here, to use updated profits 
      # 
      # static_data <- aggregate_by_area(data_dt = training_mrtn_dt, variables = c('P'), 
      #                                  weight = 'area_ha', by_c = c('region', 'N_fert'))
      # 
      # static_mrtn_dt  <- static_data %>% 
      #   .[, .SD[ P == max( P)], by = .(region)] %>%
      #   .[, .SD[ N_fert == min( N_fert)], by = .(region)] %>%
      #   .[,.(region, eonr_pred = N_fert)] %>%
      #   .[order(region)]
      # 
      # 
      # (plot1 <- ggplot() +
      #     geom_line(data = static_data, aes(x = N_fert, y = P, color = region), size = 1.5)+
      #     geom_point(data = static_data[,.SD[P == max(P)], by = region], aes(x = N_fert, y = P), size = 3) +
      #     # geom_point(data = static_data[P_diff >= -level_n][, .SD[ N_fert == min( N_fert)], by = .(region)], aes(x = N_fert, y = P), shape = 2, size = 3)+
      #     ylab('Profits ($/ha)')+
      #     theme_bw() +
      #     xlab('N rate (kg/ha)'))
      
      # =========================================================================================================================================================
      # CREATE THE RF-HIGH
      training_eonr_dt  <- training_set_dt[, .SD[ P == max(P)], by = .(id_10, id_field, z)] %>%
        .[, .SD[ N_fert == min( N_fert)], by = .(id_10, id_field, z)] %>%
        .[,c('N_fert', low_var, high_var, 'year'), with = FALSE]
      
      setnames(training_eonr_dt, 'N_fert', 'eonr')
      
      # RF Model 2------------------------
      # mtry <- tuneRF(training_eonr_dt2[,c(pred_vars), with = FALSE],training_eonr_dt2$eonr, mtryStart = 6, ntreeTry=1000,
      #                 stepFactor=1.1,improve=0.01, trace=TRUE, plot=TRUE) # ,mtryStart = 5
      # best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
      
      best.m = 6
      
      dynamic <- randomForest(formula = as.formula(paste('eonr ~ ', paste(c(low_var, high_var), collapse = ' + '))), 
                             data = training_eonr_dt[,c('eonr', low_var, high_var, 'year'), with = FALSE],
                             strata = year, #I think it will bootstrap by year
                             importance = TRUE , mtry = best.m, ntree=2000, nodesize = 30)
      
      varImpPlot(dynamic, type=2)
      plot(dynamic)
      
      if(level_n == 2){
        pdf("./n_policy_box/Data/figures/VarImportancePlot_bal.pdf")
        varImpPlot(dynamic, type=2, main = '')
        dev.off() 
      }
      
       #===================================================================================================================
      # EVALUATION
      
      #Prepare the data
      evaluation_set_dt[, P := Y_corn * Pc - N_fert * Pn - N_extra * level_n]#update profits
      evaluation_set_dt[, G := N_extra * level_n] #gov collection
      
      #===================================================================================================================
      # 1) STATIC MRTN
      
      # the NRT is trained with z1-10 and testing is evaluated with z11-25
      # evaluation_set_tmp <- merge(evaluation_set_dt, static_mrtn_dt, 
      #                             by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      #   .[N_fert == eonr_pred] %>%
      #   .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
      # 
      # 
      # results_list[[length(results_list)+1]] <- evaluation_set_tmp[,NRT := 'static'][,policy := paste0('bal_', level_n)]
      #===================================================================================================================
      # 2) dynamic
      # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH id_field
      
      prediction_set_aggregated_dt[,eonr_pred := round(predict(dynamic, prediction_set_aggregated_dt)/10,0)*10]
      
      evaluation_set_tmp <- merge(evaluation_set_dt,
                                  prediction_set_aggregated_dt[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
        .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
        .[N_fert == eonr_pred] %>%
        .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
      
      evaluation_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
      
      results_list[[length(results_list)+1]] <- evaluation_set_tmp[,NRT := 'dynamic'][,policy := paste0('bal_', level_n)]
      
      
      #===================================================================================================================
    } #end of balance loop
  
  }#end of if false chunk
  # =========================================================================================================================================================
  # VOLUNTARY REDUCTION (REDUCE RATIO 5 RECOMENDATIONS)
  source(paste0(codes_folder, '/n_policy_git/Codes/parameters.R'))
  
  #Load datasets again
  training_set_dt <- yc_field_dt2[station == 1 & z != z_n]
  evaluation_set_dt <- yc_field_dt2[station != 1 & z == z_n]
  prediction_set_aggregated_dt <- readRDS(paste0("./n_policy_box/Data/files_rds/field_performances_tmp/ratio5_recommendations_",
                                                 z_n, ".rds")) #we lower the ratio5 dynamic recommendation
  
  red_seq <- unique(sort(c(seq(0,30, by = 1), seq(18,19, by = 0.1))))
  
  for(level_n in red_seq){
    # level_n = 10
    ratio5_recommendations_dt <- readRDS(paste0("./n_policy_box/Data/files_rds/field_performances_tmp/ratio5_recommendations_",z_n, ".rds"))
    #===================================================================================================================
    # EVALUATION
    
    #Prepare the data
    evaluation_set_dt[, P := Y_corn * Pc - N_fert * Pn]  #update profits
    evaluation_set_dt[, G := 0] #gov collection
    # #===================================================================================================================
    # # 1) STATIC MRTN
    # static_mrtn_dt <- copy(ratio5_recommendations_list$static_mrtn)
    # static_mrtn_dt[, eonr_pred := round(eonr_pred*((100-level_n)/100)/10)*10]
    # 
    # # the NRT is trained with z1-10 and testing is evaluated with z11-25
    # evaluation_set_tmp <- merge(evaluation_set_dt, static_mrtn_dt, 
    #                             by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    #   .[N_fert == eonr_pred] %>%
    #   .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    # 
    # results_list[[length(results_list)+1]] <- evaluation_set_tmp[,NRT := 'static'][,policy := paste0('red_', level_n)]
    #===================================================================================================================
    # 2) dynamic
    # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH id_field
    
    prediction_set_aggregated_dt[, eonr_pred := round(eonr_ratio5*((100-level_n)/100)/10)*10]
    
    evaluation_set_tmp <- merge(evaluation_set_dt,
                                prediction_set_aggregated_dt[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
      .[N_fert == eonr_pred] %>%
      .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    
    evaluation_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
    
    results_list[[length(results_list)+1]] <- evaluation_set_tmp[,NRT := 'dynamic'][,policy := paste0('red_', level_n)]
    
    
    #===================================================================================================================
    
  }#end of reduction loop
  
  perfomances_z_tmp <- rbindlist(results_list)
  
  saveRDS(perfomances_z_tmp, paste0("./n_policy_box/Data/files_rds/field_performances_tmp/field_performances_", z_n,'.rds'))

}#end of z_n

# 
# load all the results
perfomances_list <- list()
files_path <- list.files("./n_policy_box/Data/files_rds/field_performances_tmp", full.names = TRUE, pattern = 'field_performances_')

for(file_n in files_path){
  # file_n = files_path[1]
  perfomances_list[[length(perfomances_list)+1]] <- readRDS(file_n)

}
field_perfomances_dt <- rbindlist(perfomances_list)

#add region_eq
grid10_soils_dt5 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt5.rds") %>% data.table()
field_perfomances_dt <- merge(field_perfomances_dt, unique(grid10_soils_dt5[,.(id_10, region_eq)]), by = 'id_10')

field_perfomances_dt[,.N, z][order(z)]

field_perfomances_dt[,.N, .(id_10, id_field)] %>% .[,.N, .(id_10)] %>% .[,N] %>% table() #number of fields by cell
field_perfomances_dt[,.N, .(id_10, id_field, policy, NRT, z)] %>% .[,.N, .(NRT)] #field x year by NRT

saveRDS(field_perfomances_dt, "./n_policy_box/Data/files_rds/field_perfomances_dt.rds")

field_perfomances_dt <- readRDS("./n_policy_box/Data/files_rds/field_perfomances_dt.rds")

thresholds_dt <- field_perfomances_dt[policy == 'ratio_5' & NRT == 'dynamic']

thresholds_dt[,N_balance := N_fert - Y_corn * 11.5/1000]


thresholds_dt <- thresholds_dt[,
                           .(N_balance = mean(N_balance),
                             L = mean(L)), by = region]

saveRDS(thresholds_dt, "./n_policy_box/Data/files_rds/thresholds_dt.rds")




