rm(list=ls())
# 
# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# codes_folder <-'C:/Users/germa/Documents'#Dell
setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
codes_folder <-'C:/Users/germanm2/Documents'#CPSC
# setwd('~')#Server
# codes_folder <-'~' #Server

source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')
source(paste0(codes_folder, '/n_policy_git/Codes/parameters.R'))
library(randomForest)
library(reticulate)
library(ranger)
library(mlr)
# use_condaenv('GEOANN', conda = '/opt/anaconda3/condabin/conda')
# source_python("./n_policy_git/Codes/3c_cnn_functions_sep10.py")

# yc_yearly_dt3 <- readRDS("./n_policy_box/Data/files_rds/yc_yearly_dt3.rds")
# grid10_soils_dt5 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt5.rds") %>% data.table()

reg_model_stuff <- readRDS( "./n_policy_box/Data/files_rds/reg_model_stuff.rds")
yc_field_dt <- readRDS("./n_policy_box/Data/files_rds/yc_field_dt.rds")
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
results_list <- list()

z_n = 10
for(z_n in 1:30){
  print(z_n)
  # yc_field_dt2[station == 1 & z %in% training_z, set := 'training']
  # yc_field_dt2[station == 1 & z %in% validation_z, set := 'validation']
  # yc_field_dt2[station == 0 & (!z %in% c(training_z, validation_z)), set := 'evaluation']
  # yc_field_dt2 <- yc_field_dt2[!is.na(set)][,-'station']
  # yc_field_dt2[,.N, .(set, id_10, id_field)][,.N, set]#fields by set
  # saveRDS(yc_field_dt2, "./n_policy_box/Data/files_rds/yc_field_dt2.rds")

  training_set_dt <- yc_field_dt2[station == 1 & z != z_n]
  evaluation_set_dt <- yc_field_dt2[station != 1 & z == z_n]
  prediction_set_aggregated_dt <- evaluation_set_dt[N_fert == 180][,-c('N_fert')] #one line per field, not yield curve
  
  # =========================================================================================================================================================
  # CREATE THE N RATIO TAX MODEL
  
  ratio_seq <- sort(c(seq(5, 20, by = 1)))
  # ratio_seq <- sort(c(seq(5, 20, by = 5)))
  # ratio_seq <- c(5)
  set.seed(123)
  
  for(level_n in ratio_seq){
     # level_n = 5
    
    Pn_tmp = level_n * Pc
    print(Pn_tmp/Pc)
    # training_set_dt[, P := Y_corn * Pc + Y_soy * Ps - N_fert * Pn_tmp]  #update profits
    training_set_dt[, P := Y_corn * Pc - N_fert * Pn_tmp]  #update profits
    # =========================================================================================================================================================
    # CREATE THE STATIC MRTN
    training_mrtn_dt <- training_set_dt[Yld_response > Yld_response_threshold] #Needs to be here, to use updated profits 
    
    static_data <- aggregate_by_area(data_dt = training_mrtn_dt, variables = c('P'), 
                                     weight = 'area_ha', by_c = c('region', 'N_fert'))
    
    static_mrtn_dt  <- static_data %>% 
      .[, .SD[ P == max( P)], by = .(region)] %>%
      .[, .SD[ N_fert == min( N_fert)], by = .(region)] %>%
      .[,.(region, eonr_pred = N_fert)] %>%
      .[order(region)]
    
    
    (plot1 <- ggplot() +
        geom_line(data = static_data, aes(x = N_fert, y = P, color = region), size = 1.5)+
        geom_point(data = static_data[,.SD[P == max(P)], by = region], aes(x = N_fert, y = P), size = 3) +
        # geom_point(data = static_data[P_diff >= -level_n][, .SD[ N_fert == min( N_fert)], by = .(region)], aes(x = N_fert, y = P), shape = 2, size = 3)+
        ylab('Profits ($/ha)')+
        theme_bw() +
        xlab('N rate (kg/ha)'))
    
    
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
    
    rfhigh <- randomForest(formula = as.formula(paste('eonr ~ ', paste(c(low_var, high_var), collapse = ' + '))), 
                           data = training_eonr_dt[,c('eonr', low_var, high_var, 'year'), with = FALSE],
                           strata = year, #I think it will bootstrap by year
                           importance = TRUE , mtry = best.m, ntree=2000, nodesize = 30)
    
    varImpPlot(rfhigh, type=2)
    plot(rfhigh)
    
    if(level_n == 5){
      pdf("./n_policy_box/Data/figures/VarImportancePlot.pdf")
      varImpPlot(rfhigh, type=2, main = '')
      dev.off() 
    }
    
    # =========================================================================================================================================================
    # CREATE THE RANGER-RF
    # source('~/n_policy_git/Codes/3e_ranger_hyperparameters.R')
    # # Train a model
    blocking_year <- factor(training_eonr_dt$year)
    
    
    learner_rf = makeLearner("regr.ranger",
                             num.trees = 1700,
                             min.node.size = 50,
                             mtry = 5) #instructions
    
    regr_task = mlr::makeRegrTask(data = training_eonr_dt[,c('eonr', low_var, high_var), with = FALSE], 
                                  target = "eonr", blocking = blocking_year) #task = data
    
    tsk = subsetTask(regr_task, features = c(low_var, high_var))#specify the features to avoid using the year
    
    # mcaffinity(1:20)
    ranger_rf <- train(learner_rf, task = regr_task)
    #===================================================================================================================
    # EVALUATION
    
    #Prepare the data
    evaluation_set_dt[, P := Y_corn * Pc - N_fert * Pn_tmp]  #update profits
    evaluation_set_dt[, G := N_fert * (Pn_tmp - Pn)] #gov collection
    #===================================================================================================================
    # 1) STATIC MRTN
    
    # the NRS is trained with z1-10 and testing is evaluated with z11-25
    evaluation_set_tmp <- merge(evaluation_set_dt, static_mrtn_dt, 
                             by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      .[N_fert == eonr_pred] %>%
      .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    
    results_list[[length(results_list)+1]] <- evaluation_set_tmp[,NRS := 'staticmrtn'][,policy := paste0('ratio_', level_n)]
    
    if()
    #===================================================================================================================
    # 2) RFHIGH
    # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH id_field
    
    prediction_set_aggregated_dt[,eonr_pred := round(predict(rfhigh, prediction_set_aggregated_dt)/10,0)*10]
    
    evaluation_set_tmp <- merge(evaluation_set_dt,
                             prediction_set_aggregated_dt[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
      .[N_fert == eonr_pred] %>%
      .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    
    evaluation_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
    
    results_list[[length(results_list)+1]] <- evaluation_set_tmp[,NRS := 'rfhigh'][,policy := paste0('ratio_', level_n)]
    
    #===================================================================================================================
    # 3) RANGER-RF
    # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH id_field
    
    prediction_set_aggregated_dt[,eonr_pred := round(predict(ranger_rf , newdata = prediction_set_aggregated_dt[,c(low_var, high_var), with = FALSE])$data$response/10)*10] 
    
    
    evaluation_set_tmp <- merge(evaluation_set_dt,
                             prediction_set_aggregated_dt[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
      .[N_fert == eonr_pred] %>%
      .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    
    evaluation_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
    
    results_list[[length(results_list)+1]] <- evaluation_set_tmp[,NRS := 'rfranger'][,policy := paste0('ratio_', level_n)]
    
    
    #===================================================================================================================
    # 3) EXPOST

    evaluation_set_tmp <-  evaluation_set_dt %>% 
      .[, .SD[ P == max( P)], by = .(id_10, id_field, z)] %>%
      .[, .SD[ N_fert == min(N_fert)], by = .(id_10, id_field, z)] %>%
      .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    
    evaluation_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
    
    results_list[[length(results_list)+1]] <- evaluation_set_tmp[,NRS := 'expost'][,policy := paste0('ratio_', level_n)]
    #===================================================================================================================

  }#end of ratio loop
  
  
  
  # =========================================================================================================================================================
  # CREATE THE LEACHING FEE MODEL
  source(paste0(codes_folder, '/n_policy_git/Codes/parameters.R'))
  
  #Load datasets again
  training_set_dt <- yc_field_dt2[station == 1 & z != z_n]
  evaluation_set_dt <- yc_field_dt2[station != 1 & z == z_n]
  prediction_set_aggregated_dt <- evaluation_set_dt[N_fert == 180][,-c('N_fert')] #one line per field, not yield curve
  
  #Update threholds
  leach_threshold <- training_set_dt[N_fert == 100, .(L_thr = quantile(L, probs = 0.5)), region][order(region)]
  
  training_set_dt <- merge(training_set_dt, leach_threshold, by = 'region')
  
  training_set_dt[,L_extra := L - L_thr]
  training_set_dt[L_extra <= 0, L_extra := 0]
  training_set_dt$L_extra %>% summary()
  
  evaluation_set_dt <- merge(evaluation_set_dt, leach_threshold, by = 'region')
  
  evaluation_set_dt[,L_extra := L - L_thr]
  evaluation_set_dt[L_extra <= 0, L_extra := 0]
  
  leach_seq <-  sort(c(seq(0, 30, by = 2)))
  
  set.seed(123)
  
  for(level_n in leach_seq){
    # level_n = 5
    print(level_n)
    # training_set_dt[, P := Y_corn * Pc + Y_soy * Ps - N_fert * Pn_tmp]  #update profits
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
    training_mrtn_dt <- training_set_dt[Yld_response > Yld_response_threshold] #Needs to be here, to use updated profits 
    
    static_data <- aggregate_by_area(data_dt = training_mrtn_dt, variables = c('P'), 
                                     weight = 'area_ha', by_c = c('region', 'N_fert'))
    
    static_mrtn_dt  <- static_data %>% 
      .[, .SD[ P == max( P)], by = .(region)] %>%
      .[, .SD[ N_fert == min( N_fert)], by = .(region)] %>%
      .[,.(region, eonr_pred = N_fert)] %>%
      .[order(region)]
    
    
    (plot1 <- ggplot() +
        geom_line(data = static_data, aes(x = N_fert, y = P, color = region), size = 1.5)+
        geom_point(data = static_data[,.SD[P == max(P)], by = region], aes(x = N_fert, y = P), size = 3) +
        # geom_point(data = static_data[P_diff >= -level_n][, .SD[ N_fert == min( N_fert)], by = .(region)], aes(x = N_fert, y = P), shape = 2, size = 3)+
        ylab('Profits ($/ha)')+
        theme_bw() +
        xlab('N rate (kg/ha)'))
    
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
    
    rfhigh <- randomForest(formula = as.formula(paste('eonr ~ ', paste(c(low_var, high_var), collapse = ' + '))), 
                           data = training_eonr_dt[,c('eonr', low_var, high_var, 'year'), with = FALSE],
                           strata = year, #I think it will bootstrap by year
                           importance = TRUE , mtry = best.m, ntree=2000, nodesize = 30)
    
    varImpPlot(rfhigh, type=2)
    plot(rfhigh)
    
    if(level_n == 5){
      pdf("./n_policy_box/Data/figures/VarImportancePlot.pdf")
      varImpPlot(rfhigh, type=2, main = '')
      dev.off() 
    }
    
    # =========================================================================================================================================================
    # CREATE THE RANGER-RF
    # source('~/n_policy_git/Codes/3e_ranger_hyperparameters.R')
    # # Train a model
    blocking_year <- factor(training_eonr_dt$year)
    
    
    learner_rf = makeLearner("regr.ranger",
                             num.trees = 1700,
                             min.node.size = 50,
                             mtry = 5) #instructions
    
    regr_task = mlr::makeRegrTask(data = training_eonr_dt[,c('eonr', low_var, high_var), with = FALSE], 
                                  target = "eonr", blocking = blocking_year) #task = data
    
    tsk = subsetTask(regr_task, features = c(low_var, high_var))#specify the features to avoid using the year
    
    # mcaffinity(1:20)
    ranger_rf <- train(learner_rf, task = regr_task)
    #===================================================================================================================
    # EVALUATION
    
    #Prepare the data
    evaluation_set_dt[, P := Y_corn * Pc - N_fert * Pn - L_extra * level_n] #update profits
    evaluation_set_dt[, G := L_extra * level_n] #gov collectionn
    #===================================================================================================================
    # 1) STATIC MRTN
    
    # the NRS is trained with z1-10 and testing is evaluated with z11-25
    evaluation_set_tmp <- merge(evaluation_set_dt, static_mrtn_dt, 
                             by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      .[N_fert == eonr_pred] %>%
      .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]

    
    results_list[[length(results_list)+1]] <- evaluation_set_tmp[,NRS := 'staticmrtn'][,policy := paste0('leach_', level_n)]
    #===================================================================================================================
    # 2) RFHIGH
    # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH id_field
    
    prediction_set_aggregated_dt[,eonr_pred := round(predict(rfhigh, prediction_set_aggregated_dt)/10,0)*10]
    
    evaluation_set_tmp <- merge(evaluation_set_dt,
                             prediction_set_aggregated_dt[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
      .[N_fert == eonr_pred] %>%
      .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    
    evaluation_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
    
    results_list[[length(results_list)+1]] <- evaluation_set_tmp[,NRS := 'rfhigh'][,policy := paste0('leach_', level_n)]
    
    #===================================================================================================================
    # 3) RANGER-RF
    # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH id_field
    
    prediction_set_aggregated_dt[,eonr_pred := round(predict(ranger_rf , newdata = prediction_set_aggregated_dt[,c(low_var, high_var), with = FALSE])$data$response/10)*10] 
    
    
    evaluation_set_tmp <- merge(evaluation_set_dt,
                             prediction_set_aggregated_dt[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
      .[N_fert == eonr_pred] %>%
      .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    
    evaluation_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
    
    results_list[[length(results_list)+1]] <- evaluation_set_tmp[,NRS := 'rfranger'][,policy := paste0('leach_', level_n)]
    
    
    #===================================================================================================================
    # 3) EXPOST
    
    evaluation_set_tmp <-  evaluation_set_dt %>% 
      .[, .SD[ P == max( P)], by = .(id_10, id_field, z)] %>%
      .[, .SD[ N_fert == min(N_fert)], by = .(id_10, id_field, z)] %>%
      .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    
    evaluation_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
    
    results_list[[length(results_list)+1]] <- evaluation_set_tmp[,NRS := 'expost'][,policy := paste0('leach_', level_n)]
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
  
  bal_threshold <- training_set_dt[N_fert == 100, .(N_balance_thr = quantile(N_balance, probs = 0.5)), region][order(region)]
  
  training_set_dt[,N_balance := N_fert - Y_corn * 11.5/1000]
  training_set_dt <- merge(training_set_dt[,-'N_balance_thr'], bal_threshold, by = 'region')
  
  training_set_dt[,N_extra := N_balance - N_balance_thr]
  training_set_dt[N_extra <= 0, N_extra := 0]
  #evaluation set
  evaluation_set_dt[,N_balance := N_fert - Y_corn * 11.5/1000]
  evaluation_set_dt <- merge(evaluation_set_dt[,-'N_balance_thr'], bal_threshold, by = 'region')

  evaluation_set_dt[,N_extra := N_balance - N_balance_thr]
  evaluation_set_dt[N_extra <= 0, N_extra := 0]
  
  
  bal_seq <- sort(seq(0, 5, by = 0.25))
  
  set.seed(123)
  
  for(level_n in bal_seq){
    # level_n = 5
    print(level_n)
    # training_set_dt[, P := Y_corn * Pc + Y_soy * Ps - N_fert * Pn_tmp]  #update profits
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
    training_mrtn_dt <- training_set_dt[Yld_response > Yld_response_threshold] #Needs to be here, to use updated profits 
    
    static_data <- aggregate_by_area(data_dt = training_mrtn_dt, variables = c('P'), 
                                     weight = 'area_ha', by_c = c('region', 'N_fert'))
    
    static_mrtn_dt  <- static_data %>% 
      .[, .SD[ P == max( P)], by = .(region)] %>%
      .[, .SD[ N_fert == min( N_fert)], by = .(region)] %>%
      .[,.(region, eonr_pred = N_fert)] %>%
      .[order(region)]
    
    
    (plot1 <- ggplot() +
        geom_line(data = static_data, aes(x = N_fert, y = P, color = region), size = 1.5)+
        geom_point(data = static_data[,.SD[P == max(P)], by = region], aes(x = N_fert, y = P), size = 3) +
        # geom_point(data = static_data[P_diff >= -level_n][, .SD[ N_fert == min( N_fert)], by = .(region)], aes(x = N_fert, y = P), shape = 2, size = 3)+
        ylab('Profits ($/ha)')+
        theme_bw() +
        xlab('N rate (kg/ha)'))
    
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
    
    rfhigh <- randomForest(formula = as.formula(paste('eonr ~ ', paste(c(low_var, high_var), collapse = ' + '))), 
                           data = training_eonr_dt[,c('eonr', low_var, high_var, 'year'), with = FALSE],
                           strata = year, #I think it will bootstrap by year
                           importance = TRUE , mtry = best.m, ntree=2000, nodesize = 30)
    
    varImpPlot(rfhigh, type=2)
    plot(rfhigh)
    
    if(level_n == 5){
      pdf("./n_policy_box/Data/figures/VarImportancePlot.pdf")
      varImpPlot(rfhigh, type=2, main = '')
      dev.off() 
    }
    
    # =========================================================================================================================================================
    # CREATE THE RANGER-RF
    # source('~/n_policy_git/Codes/3e_ranger_hyperparameters.R')
    # # Train a model
    blocking_year <- factor(training_eonr_dt$year)
    
    
    learner_rf = makeLearner("regr.ranger",
                             num.trees = 1700,
                             min.node.size = 50,
                             mtry = 5) #instructions
    
    regr_task = mlr::makeRegrTask(data = training_eonr_dt[,c('eonr', low_var, high_var), with = FALSE], 
                                  target = "eonr", blocking = blocking_year) #task = data
    
    tsk = subsetTask(regr_task, features = c(low_var, high_var))#specify the features to avoid using the year
    
    # mcaffinity(1:20)
    ranger_rf <- train(learner_rf, task = regr_task)
    #===================================================================================================================
    # EVALUATION
    
    #Prepare the data
    evaluation_set_dt[, P := Y_corn * Pc - N_fert * Pn - N_extra * level_n]#update profits
    evaluation_set_dt[, G := N_extra * level_n] #gov collection
    
    #===================================================================================================================
    # 1) STATIC MRTN
    
    # the NRS is trained with z1-10 and testing is evaluated with z11-25
    evaluation_set_tmp <- merge(evaluation_set_dt, static_mrtn_dt, 
                                by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      .[N_fert == eonr_pred] %>%
      .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    
    
    results_list[[length(results_list)+1]] <- evaluation_set_tmp[,NRS := 'staticmrtn'][,policy := paste0('bal_n', level_n)]
    #===================================================================================================================
    # 2) RFHIGH
    # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH id_field
    
    prediction_set_aggregated_dt[,eonr_pred := round(predict(rfhigh, prediction_set_aggregated_dt)/10,0)*10]
    
    evaluation_set_tmp <- merge(evaluation_set_dt,
                                prediction_set_aggregated_dt[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
      .[N_fert == eonr_pred] %>%
      .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    
    evaluation_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
    
    results_list[[length(results_list)+1]] <- evaluation_set_tmp[,NRS := 'rfhigh'][,policy := paste0('bal_n', level_n)]
    
    #===================================================================================================================
    # 3) RANGER-RF
    # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH id_field
    
    prediction_set_aggregated_dt[,eonr_pred := round(predict(ranger_rf , newdata = prediction_set_aggregated_dt[,c(low_var, high_var), with = FALSE])$data$response/10)*10] 
    
    
    evaluation_set_tmp <- merge(evaluation_set_dt,
                                prediction_set_aggregated_dt[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
      .[N_fert == eonr_pred] %>%
      .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    
    evaluation_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
    
    results_list[[length(results_list)+1]] <- evaluation_set_tmp[,NRS := 'rfranger'][,policy := paste0('bal_n', level_n)]
    
    
    #===================================================================================================================
    # 3) EXPOST
    
    evaluation_set_tmp <-  evaluation_set_dt %>% 
      .[, .SD[ P == max( P)], by = .(id_10, id_field, z)] %>%
      .[, .SD[ N_fert == min(N_fert)], by = .(id_10, id_field, z)] %>%
      .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    
    evaluation_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
    
    results_list[[length(results_list)+1]] <- evaluation_set_tmp[,NRS := 'expost'][,policy := paste0('bal_n', level_n)]
    #===================================================================================================================
  } #end of balance loop
  
 
  # =========================================================================================================================================================
  # VOLUNTARY LAG REDUCTION (REDUCE RATIO 5 RECOMENDATIONS)
  source(paste0(codes_folder, '/n_policy_git/Codes/parameters.R'))
  
  #Load datasets again
  training_set_dt <- yc_field_dt2[station == 1 & z != z_n]
  evaluation_set_dt <- yc_field_dt2[station != 1 & z == z_n]
  prediction_set_aggregated_dt <- evaluation_set_dt[N_fert == 180][,-c('N_fert')] #one line per field, not yield curve
  
  red_seq <- unique(sort(c(seq(0,30, by = 1), seq(18,19, by = 0.1))))
  
  for(level_n in red_seq){
    # level_n = 5
    
    Pn_tmp = level_n * Pc
    print(Pn_tmp/Pc)
    # training_set_dt[, P := Y_corn * Pc + Y_soy * Ps - N_fert * Pn_tmp]  #update profits
    training_set_dt[, P := Y_corn * Pc - N_fert * Pn_tmp]  #update profits
    # =========================================================================================================================================================
    # CREATE THE STATIC MRTN
    training_mrtn_dt <- training_set_dt[Yld_response > Yld_response_threshold] #Needs to be here, to use updated profits 
    
    static_data <- aggregate_by_area(data_dt = training_mrtn_dt, variables = c('P'), 
                                     weight = 'area_ha', by_c = c('region', 'N_fert'))
    
    static_mrtn_dt  <- static_data %>% 
      .[, .SD[ P == max( P)], by = .(region)] %>%
      .[, .SD[ N_fert == min( N_fert)], by = .(region)] %>%
      .[,.(region, eonr_pred = N_fert)] %>%
      .[order(region)]
    
    
    (plot1 <- ggplot() +
        geom_line(data = static_data, aes(x = N_fert, y = P, color = region), size = 1.5)+
        geom_point(data = static_data[,.SD[P == max(P)], by = region], aes(x = N_fert, y = P), size = 3) +
        # geom_point(data = static_data[P_diff >= -level_n][, .SD[ N_fert == min( N_fert)], by = .(region)], aes(x = N_fert, y = P), shape = 2, size = 3)+
        ylab('Profits ($/ha)')+
        theme_bw() +
        xlab('N rate (kg/ha)'))
    
    
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
    
    rfhigh <- randomForest(formula = as.formula(paste('eonr ~ ', paste(c(low_var, high_var), collapse = ' + '))), 
                           data = training_eonr_dt[,c('eonr', low_var, high_var, 'year'), with = FALSE],
                           strata = year, #I think it will bootstrap by year
                           importance = TRUE , mtry = best.m, ntree=2000, nodesize = 30)
    
    varImpPlot(rfhigh, type=2)
    plot(rfhigh)
    
    if(level_n == 5){
      pdf("./n_policy_box/Data/figures/VarImportancePlot.pdf")
      varImpPlot(rfhigh, type=2, main = '')
      dev.off() 
    }
    
    # =========================================================================================================================================================
    # CREATE THE RANGER-RF
    # source('~/n_policy_git/Codes/3e_ranger_hyperparameters.R')
    # # Train a model
    blocking_year <- factor(training_eonr_dt$year)
    
    
    learner_rf = makeLearner("regr.ranger",
                             num.trees = 1700,
                             min.node.size = 50,
                             mtry = 5) #instructions
    
    regr_task = mlr::makeRegrTask(data = training_eonr_dt[,c('eonr', low_var, high_var), with = FALSE], 
                                  target = "eonr", blocking = blocking_year) #task = data
    
    tsk = subsetTask(regr_task, features = c(low_var, high_var))#specify the features to avoid using the year
    
    # mcaffinity(1:20)
    ranger_rf <- train(learner_rf, task = regr_task)
    #===================================================================================================================
    # EVALUATION
    
    #Prepare the data
    evaluation_set_dt[, P := Y_corn * Pc - N_fert * Pn_tmp]  #update profits
    evaluation_set_dt[, G := N_fert * (Pn_tmp - Pn)] #gov collection
    #===================================================================================================================
    # 1) STATIC MRTN
    
    # the NRS is trained with z1-10 and testing is evaluated with z11-25
    evaluation_set_tmp <- merge(evaluation_set_dt, static_mrtn_dt, 
                                by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      .[N_fert == eonr_pred] %>%
      .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    
    results_list[[length(results_list)+1]] <- evaluation_set_tmp[,NRS := 'staticmrtn'][,policy := paste0('ratio_', level_n)]
    #===================================================================================================================
    # 2) RFHIGH
    # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH id_field
    
    prediction_set_aggregated_dt[,eonr_pred := round(predict(rfhigh, prediction_set_aggregated_dt)/10,0)*10]
    
    evaluation_set_tmp <- merge(evaluation_set_dt,
                                prediction_set_aggregated_dt[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
      .[N_fert == eonr_pred] %>%
      .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    
    evaluation_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
    
    results_list[[length(results_list)+1]] <- evaluation_set_tmp[,NRS := 'rfhigh'][,policy := paste0('ratio_', level_n)]
    
    #===================================================================================================================
    # 3) RANGER-RF
    # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH id_field
    
    prediction_set_aggregated_dt[,eonr_pred := round(predict(ranger_rf , newdata = prediction_set_aggregated_dt[,c(low_var, high_var), with = FALSE])$data$response/10)*10] 
    
    
    evaluation_set_tmp <- merge(evaluation_set_dt,
                                prediction_set_aggregated_dt[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
      .[N_fert == eonr_pred] %>%
      .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    
    evaluation_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
    
    results_list[[length(results_list)+1]] <- evaluation_set_tmp[,NRS := 'rfranger'][,policy := paste0('ratio_', level_n)]
    
    
    #===================================================================================================================
    # 3) EXPOST
    
    evaluation_set_tmp <-  evaluation_set_dt %>% 
      .[, .SD[ P == max( P)], by = .(id_10, id_field, z)] %>%
      .[, .SD[ N_fert == min(N_fert)], by = .(id_10, id_field, z)] %>%
      .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    
    evaluation_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
    
    results_list[[length(results_list)+1]] <- evaluation_set_tmp[,NRS := 'expost'][,policy := paste0('ratio_', level_n)]
    #===================================================================================================================
    
  }#end of ratio loop
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  for(lag_n in lag_seq){
    # lag_n = 10
    print(lag_n)
    
    # =========================================================================================================================================================
    # CREATE THE STATIC MRTN
    
    training_mrtn_dt <- training_set_dt[Yld_response > Yld_response_threshold] #Needs to be here, to use updated profits 
    
    static_data <- aggregate_by_area(data_dt = training_mrtn_dt, variables = c('P'), 
                                     weight = 'area_ha', by_c = c('region', 'N_fert')) %>% 
      .[,P_max := max(P), region] %>%
      .[,P_diff := P - P_max]
    
    static_mrtn_dt  <- static_data[P_diff >= -lag_n] %>% # get out of the flat zone: lowest rate withing diff_n usd difference from max Profit
      .[, .SD[ N_fert == min( N_fert)], by = .(region)] %>%
      .[,.(region, eonr_pred = N_fert)] %>%
      .[order(region)]
    
    (plot1 <- ggplot() + 
        geom_line(data = static_data, aes(x = N_fert, y = P, color = region), size = 1.5)+
        geom_point(data = static_data[,.SD[P == max(P)], by = region], aes(x = N_fert, y = P), size = 3) +
        geom_point(data = static_data[P_diff >= -lag_n][, .SD[ N_fert == min( N_fert)], by = .(region)], aes(x = N_fert, y = P), shape = 2, size = 3)+
        ylab('Profits ($/ha)')+
        theme_bw() +
        xlab('N rate (kg/ha)'))
    
    # =========================================================================================================================================================
    #Dynamic1 (lag_n usd below P max)
    ## PREPARE THE TRAINING DATA WITH EONR ========
    #Dynamic (lag_n usd below P max)
    ## PREPARE THE TRAINING DATA WITH EONR ========
    training_eonr_dt <- copy(training_set_dt)
    
    training_eonr_dt[,P_max := max(P), .(id_10, id_field, z)]
    training_eonr_dt[,P_diff := P - P_max]
    
    training_eonr_dt  <- training_eonr_dt[P_diff >= -lag_n] %>% # get out of the flat zone: lowest rate withing 10 usd difference from max Profit
      .[, .SD[ N_fert == min( N_fert)], by = .(id_10, id_field, z)] 
    
    setnames(training_eonr_dt, 'N_fert', 'eonr')
    
    # RF Model 2------------------------
    # mtry <- tuneRF(training_eonr_dt2[,c(pred_vars), with = FALSE],training_eonr_dt2$eonr, mtryStart = 6, ntreeTry=1000,
                    # stepFactor=1.1,improve=0.01, trace=TRUE, plot=TRUE) # ,mtryStart = 5
    # best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
    
    best.m = 6
    
    rflow <- randomForest(eonr ~ ., data = training_eonr_dt[,c('eonr', low_var), with = FALSE],
                            importance = TRUE , mtry = best.m, ntree=2000, nodesize = 30)
    
    rfhigh <- randomForest(eonr ~ ., data = training_eonr_dt[,c('eonr', low_var, high_var), with = FALSE],
                           importance = TRUE , mtry = best.m, ntree=2000, nodesize = 30)
    
    if(lag_n == 0){
      png("./n_policy_box/Data/figures/varimportancedynamic.png", width = 900, height = 480,units = "px")
      varImpPlot(rfhigh, type=2)
      dev.off()
    }
    plot(rfhigh)
    
    # =========================================================================================================================================================
    #Dynamic Long Term (lag_n usd below P max)
    ## PREPARE THE TRAINING DATA WITH EONR ========
    #Dynamic (lag_n usd below P max)
    ## PREPARE THE TRAINING DATA WITH EONR ========
    training_lt_dt <- copy(training_set_dt)
    
    pred_vars_lt <- c("Y_corn_lt_avg", "whc", "sand_40cm", "clay_40cm", "oc_20cm_v5")
    training_lt_dt <- training_lt_dt[,c('id_10', 'id_field', 'N_fert', 'P', pred_vars_lt), with = FALSE]
    training_lt_dt <- training_lt_dt[, lapply(.SD, mean, na.rm = T), by = .(id_10, id_field, N_fert)]
    
    training_lt_dt[,P_max := max(P), .(id_10, id_field)]
    training_lt_dt[,P_diff := P - P_max]
    
    training_lt_dt  <- training_lt_dt[P_diff >= -lag_n] %>% # get out of the flat zone: lowest rate withing 10 usd difference from max Profit
      .[, .SD[ N_fert == min( N_fert)], by = .(id_10, id_field)] 
    
    
    setnames(training_lt_dt, 'N_fert', 'eonr')
    
    training_lt_dt2 <- training_lt_dt[,c('eonr', pred_vars_lt), with = FALSE]
    
    # RF LT------------------------
    # mtry <- tuneRF(training_eonr_dt2[,c(pred_vars), with = FALSE],training_eonr_dt2$eonr, mtryStart = 6, ntreeTry=1000,
    # stepFactor=1.1,improve=0.01, trace=TRUE, plot=TRUE) # ,mtryStart = 5
    # best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
    
    best.m = 6
    
    rfLT <- randomForest(eonr ~ ., data = training_lt_dt2,
                            importance = TRUE , ntree=500)
    
    varImpPlot(rfLT)
    
    plot(rfLT)
    
    # =========================================================================================================================================================
    # Static mean
    static_mean_dt <- training_eonr_dt[,.(eonr_pred = round(mean(eonr)/10,0)*10), by = region] %>%
                                      .[order(region)]
    
    # =========================================================================================================================================================
    #Future (lag_n usd below P max)
    ## PREPARE THE TRAINING DATA WITH EONR ========
    #Dynamic (lag_n usd below P max)
    ## PREPARE THE TRAINING DATA WITH EONR ========
    
    # pred_vars2 <- c(low_var, high_var, 'Y_corn', 'n_uptake', 'LAI_max', 'water_table_fw', 'swdef_photo_fw', 'swdef_expan_fw', 'swdef_pheno_fw') 
    
    pred_vars2 <- c(low_var, high_var, "radn_1", "radn_2", "radn_3", "radn_4", "maxt_1", "maxt_2", "maxt_3", "maxt_4", "mint_1", "mint_2", "mint_3", "mint_4", "rain_1", "rain_2",
    "rain_3", "rain_4")
    
    # RF Model 2------------------------
    # mtry <- tuneRF(training_eonr_dt2[,c(pred_vars), with = FALSE],training_eonr_dt2$eonr, ntreeTry=2000,
    #                 trace=TRUE, plot=TRUE) # ,mtryStart = 5
    # 
    # best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
    
    best.m = 12
    
    forecast <- randomForest(eonr ~ ., data = training_eonr_dt[,c('eonr', pred_vars2), with = FALSE],
                           importance = TRUE , mtry = best.m, ntree=2000, nodesize = 30)
    
    if(lag_n == 0){
      png("./n_policy_box/Data/figures/varimportanceforecast.png", width = 900, height = 480,units = "px")
      varImpPlot(forecast, type=2)
      dev.off()
    }
    # varImpPlot(dynamic2, type=2)
    # plot(dynamic2)
    # =========================================================================================================================================================
    #Yield at harvest rule
    training_eonr_dt2 <- copy(training_eonr_dt)
    training_eonr_dt2[,regionC := ifelse(region == '2-Central', 1,0)]
    training_eonr_dt2[,regionN := ifelse(region == '3-North', 1,0)]
    yieldgoal <- lm(data = training_eonr_dt2, 'eonr ~ Y_corn + regionC + regionN + I(Y_corn * regionC) + I(Y_corn * regionN)')
    summary(yieldgoal)
    
    ggplot(data = training_eonr_dt, aes(x = Y_corn, y = eonr))+
      geom_point()+
      geom_smooth(se = FALSE, formula = 'y ~ x', method = 'lm')+
      facet_wrap(.~region)
    
    # =========================================================================================================================================================
    #Yield response at harvest rule
    # training_eonr_dt2 <- copy(training_eonr_dt)
    # training_eonr_dt2[,regionC := ifelse(region == '2-Central', 1,0)]
    # training_eonr_dt2[,regionN := ifelse(region == '3-North', 1,0)]
    # yieldresponse <- lm(data = training_eonr_dt2, 'eonr ~ Yld_response + regionC + regionN + I(Yld_response * regionC) + I(Yld_response * regionN)')
    # summary(yieldresponse)
    # 
    # 
    # ggplot(data = training_eonr_dt, aes(x = Yld_response, y = eonr))+
    #   geom_point()+
    #   geom_smooth(se = FALSE, formula = 'y ~ x', method = 'lm')+
    #   facet_wrap(.~region)
    
    #===================================================================================================================
    #XGBoost
    # https://www.datatechnotes.com/2020/08/regression-example-with-xgboost-in-r.html
    
    # library(xgboost)
    # library(caret)
    # 
    # training_eonr_dt2 <- training_eonr_dt[,c('eonr', pred_vars), with = FALSE]
    # 
    # train_x = data.matrix(training_eonr_dt2[, pred_vars, with = F])
    # train_y = training_eonr_dt2$eonr
    # 
    # test_x = data.matrix(prediction_set_aggregated_dt[, pred_vars, with = F])
    # # test_y = prediction_set_aggregated_dt$eonr_12
    # 
    # xgb_train = xgb.DMatrix(data = train_x, label = train_y)
    # xgb_test = xgb.DMatrix(data = test_x)
    # 
    # xgbc = xgboost(data = xgb_train, nrounds = 2000, verbose = FALSE)
    # print(xgbc)
    # #---------------------------------------------------------------------------
    # # PERFORMANCE EVALUATION
    # 
    # prediction_set_aggregated_dt[,eonr_pred := ceiling(predict(xgbc, xgb_test)/10)*10]
    # caret::RMSE(prediction_set_aggregated_dt$eonr_12, prediction_set_aggregated_dt$eonr_pred)
    # 
    # evaluation_set_tmp <- merge(evaluation_set_dt,
    #                          prediction_set_aggregated_dt[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    #   .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
    #   .[N_fert == eonr_pred] %>%
    #   .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    # 
    # evaluation_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
    # 
    # results_list[[length(results_list)+1]] <- evaluation_set_tmp[,lag := lag_n][,NRS := 'xgboost']
    
    #===================================================================================================================
    # EVALUATION
    
    #===================================================================================================================
    # 1) STATIC MRTN
    
    # the NRS is trained with z1-10 and testing is evaluated with z11-25
    evaluation_set_tmp <- merge(evaluation_set_dt, static_mrtn_dt, 
                             by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      .[N_fert == eonr_pred] %>%
      .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]

    results_list[[length(results_list)+1]] <- evaluation_set_tmp[,lag := lag_n][,NRS := 'staticmrtn']
    
    #===================================================================================================================
    # 1) extra20 (MRTN + 20)
    
    # the NRS is trained with z1-10 and testing is evaluated with z11-25
    evaluation_set_tmp <- merge(evaluation_set_dt, static_mrtn_dt, 
                             by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      .[N_fert == eonr_pred+20] %>%
      .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    
    evaluation_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
    
    results_list[[length(results_list)+1]] <- evaluation_set_tmp[,lag := lag_n][,NRS := 'extra20']
    
    #===================================================================================================================
    # 1) STATIC MEAN
    
    # the NRS is trained with z1-10 and testing is evaluated with z11-25
    evaluation_set_tmp <- merge(evaluation_set_dt, static_mean_dt, 
                             by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      .[N_fert == eonr_pred] %>%
      .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    
    evaluation_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
    
    results_list[[length(results_list)+1]] <- evaluation_set_tmp[,lag := lag_n][,NRS := 'staticmean']
    #===================================================================================================================
    # 2) RFLOW
    # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH id_field
    
    prediction_set_aggregated_dt[,eonr_pred := round(predict(rflow, prediction_set_aggregated_dt)/10,0)*10]
    
    #---------------------------------------------------------------------------
    # PERFORMANCE EVALUATION
    evaluation_set_tmp <- merge(evaluation_set_dt,
                             prediction_set_aggregated_dt[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
      .[N_fert == eonr_pred] %>%
      .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    
    evaluation_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
    
    results_list[[length(results_list)+1]] <- evaluation_set_tmp[,lag := lag_n][,NRS := 'rflow']
    
    #===================================================================================================================
    # 2) RFHIGH
    # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH id_field
    
    prediction_set_aggregated_dt[,eonr_pred := round(predict(rfhigh, prediction_set_aggregated_dt)/10,0)*10]
    
    if(FALSE){
      varImpPlot(rfhigh)
      
      plot_1 <- ggplot(data = prediction_set_aggregated_dt, aes(x = n_0_60cm_v5  , y = eonr_pred)) + 
        geom_point() + geom_smooth(formula = 'y ~ x', method = lm)
      
      plot_2 <- ggplot(data = prediction_set_aggregated_dt, aes(x = t_min_60, y = eonr_pred)) + 
        geom_point() + geom_smooth(formula = 'y ~ x', method = lm)
      
      plot_3 <- ggplot(data = prediction_set_aggregated_dt, aes(x = day_sow, y = eonr_pred)) + 
        geom_point() + geom_smooth(formula = 'y ~ x', method = lm)

      plot_4 <- ggplot(data = prediction_set_aggregated_dt, aes(x = t_max_60, y = eonr_pred)) + 
        geom_point() + geom_smooth(formula = 'y ~ x', method = lm)
      
      grid.arrange(plot_1, plot_2, plot_3, plot_4)
    }
    
    #---------------------------------------------------------------------------
    # PERFORMANCE EVALUATION
    evaluation_set_tmp <- merge(evaluation_set_dt,
                             prediction_set_aggregated_dt[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
      .[N_fert == eonr_pred] %>%
      .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    
    evaluation_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
    
    results_list[[length(results_list)+1]] <- evaluation_set_tmp[,lag := lag_n][,NRS := 'rfhigh']
    
    #===================================================================================================================
    # 2) DYNAMIC LONG TERM
    # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH id_field
    
    prediction_set_aggregated_dt[,eonr_pred := round(predict(rfLT, prediction_set_aggregated_dt)/10,0)*10]
    
   
    if(FALSE){
      
      plot_1 <- ggplot(data = prediction_set_aggregated_dt, aes(x = oc_20cm_v5  , y = eonr_pred)) + 
        geom_point() + geom_smooth(formula = 'y ~ x', method = lm)
      
      plot_2 <- ggplot(data = prediction_set_aggregated_dt, aes(x = Y_corn_lt_avg, y = eonr_pred)) + 
        geom_point() + geom_smooth(formula = 'y ~ x', method = lm)
      
      plot_3 <- ggplot(data = prediction_set_aggregated_dt, aes(x = whc, y = eonr_pred)) + 
        geom_point() + geom_smooth(formula = 'y ~ x', method = lm)
      
      plot_4 <- ggplot(data = prediction_set_aggregated_dt, aes(x = sand_40cm, y = eonr_pred)) + 
        geom_point() + geom_smooth(formula = 'y ~ x', method = lm)
      
      grid.arrange(plot_1, plot_2, plot_3, plot_4)
    }
    
    #---------------------------------------------------------------------------
    # PERFORMANCE EVALUATION
    evaluation_set_tmp <- merge(evaluation_set_dt,
                             prediction_set_aggregated_dt[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
      .[N_fert == eonr_pred] %>%
      .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    
    evaluation_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
    
    results_list[[length(results_list)+1]] <- evaluation_set_tmp[,lag := lag_n][,NRS := 'rfLT']
    # #===================================================================================================================
    # # 2) DYNAMICYEAR
    # # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH id_field
    # 
    # prediction_set_aggregated_dt[,eonr_pred := round(predict(dynamic, prediction_set_aggregated_dt)/10,0)*10]
    # 
    # dynamicyear_dt <- prediction_set_aggregated_dt[,.(eonr_pred = round(mean(eonr_pred)/10,0)*10), by = .(region, z)]
    # 
    # #---------------------------------------------------------------------------
    # # PERFORMANCE EVALUATION
    # evaluation_set_tmp <- merge(evaluation_set_dt,
    #                          dynamicyear_dt[,.(region, z, eonr_pred)], by = c('region','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    #   .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
    #   .[N_fert == eonr_pred] %>%
    #   .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    # 
    # results_list[[length(results_list)+1]] <- evaluation_set_tmp[,lag := lag_n][,NRS := 'dynamicyear']
    # 
    # 
    # #===================================================================================================================
    # # 3) PREDICT CNN
    # # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH id_field
    # prediction_set_aggregated_dt2 <- predict_cnn(prediction_set_aggregated_dt, as.character(lag_n), pred_vars) %>% data.table()
    # 
    # prediction_set_aggregated_dt2[,eonr_pred := round(eonr_pred/10)*10] %>%
    #   .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))]
    # 
    # # #---------------------------------------------------------------------------
    # # # PERFORMANCE EVALUATION
    # evaluation_set_tmp <- merge(evaluation_set_dt,
    #                          prediction_set_aggregated_dt2[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    #   .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
    #   .[N_fert == eonr_pred] %>%
    #   .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    # 
    # results_list[[length(results_list)+1]] <- evaluation_set_tmp[,lag := lag_n][,NRS := 'cnn'] 
    
    
    #===================================================================================================================
    # 3) FUTURE
    # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH id_field
    
    prediction_set_aggregated_dt[,eonr_pred := round(predict(forecast, prediction_set_aggregated_dt)/10,0)*10]
    
    #---------------------------------------------------------------------------
    # PERFORMANCE EVALUATION
    evaluation_set_tmp <- merge(evaluation_set_dt,
                             prediction_set_aggregated_dt[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
      .[N_fert == eonr_pred] %>%
      .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    
    evaluation_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
    
    results_list[[length(results_list)+1]] <- evaluation_set_tmp[,lag := lag_n][,NRS := 'forecast']
    
    #===================================================================================================================
    # 3) CORRECTED
    # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH id_field
    prediction_set_aggregated_dt[,eonr_pred := round(predict(rfhigh, prediction_set_aggregated_dt)/10,0)*10]
    prediction_set_aggregated_dt <- merge(prediction_set_aggregated_dt,
                                          static_mrtn_dt[,.(region, eonr_pred_static = eonr_pred)], by = c('region'))
    
    prediction_set_aggregated_dt[region == '1-South', eonr_pred := eonr_pred_static]
    prediction_set_aggregated_dt[region == '2-Central', eonr_pred := eonr_pred_static]
    # prediction_set_aggregated_dt[region == '2-Central' & eonr_pred > eonr_pred_static, eonr_pred := eonr_pred_static]
    # prediction_set_aggregated_dt[region == '3-North' & eonr_pred > eonr_pred_static, eonr_pred := eonr_pred_static]
    prediction_set_aggregated_dt[, eonr_pred_static := NULL]
    #
    # #---------------------------------------------------------------------------
    # # PERFORMANCE EVALUATION
    evaluation_set_tmp <- merge(evaluation_set_dt,
                             prediction_set_aggregated_dt[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
      .[N_fert == eonr_pred] %>%
      .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    
    evaluation_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
    
    results_list[[length(results_list)+1]] <- evaluation_set_tmp[,lag := lag_n][,NRS := 'corrected']
    # #===================================================================================================================
    # # 3) METAMODEL
    # 
    # #Predict with static
    # static_validation_dt <- merge(validation_set_dt[, -'eonr_pred'], static_mrtn_dt, 
    #                          by = c('region')) %>% 
    #   .[z %in% validation_z] %>%
    #   .[N_fert == eonr_pred] %>%
    #   setnames(c('P', 'N_fert'), c('P_static', 'N_static'))
    # 
    # #Predict with dynamic
    # dynamic_validation_dt <- validation_set_dt[,eonr_pred := ceiling(predict(dynamic, validation_set_dt)/10)*10] %>%
    #   .[z %in% validation_z] %>%
    #   .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
    #   .[N_fert == eonr_pred]%>% 
    #   .[, .SD[ N_fert == min( N_fert)], by = .(id_10, id_field, z)] %>%
    #   setnames(c('P', 'N_fert'), c('P_dynamic', 'n_policy'))
    # 
    # #Merge results and calculate P_diff
    # validation_dt <- merge(static_validation_dt, dynamic_validation_dt[,.(id_10, id_field, z, n_policy, P_dynamic)], by = c('id_10', 'id_field', 'z'))
    # validation_dt[,P_diff := P_dynamic - P_static]
    # summary(validation_dt$P_diff)
    # 
    # #Train a model that predicts P_loss
    # 
    # validation_dt <- validation_dt[,c('P_diff', 'n_policy','N_static',pred_vars), with = FALSE]
    # 
    # rf_metamodel <- randomForest(P_diff ~ ., data = validation_dt,
    #                         importance = TRUE, ntree=1000, nodesize = 30)
    # varImpPlot(rf_metamodel, type=2)
    # 
    # # Get the recommendation for each model
    # prediction_set_aggregated_dt[,n_policy := ceiling(predict(dynamic, prediction_set_aggregated_dt)/10)*10]
    # prediction_set_aggregated_dt <- merge(prediction_set_aggregated_dt[!z %in% training_z],
    #                                       static_mrtn_dt[,.(region, N_static = eonr_pred)], by = c('region'))
    # #Predict P_diff
    # prediction_set_aggregated_dt[,P_diff := predict(rf_metamodel, prediction_set_aggregated_dt)]
    # summary(prediction_set_aggregated_dt$P_diff)
    # ggplot(data = prediction_set_aggregated_dt, aes(x=n_policy, y=P_diff))+
    #   geom_point()+
    #   facet_wrap(region~.)
    # 
    # prediction_set_aggregated_dt[, eonr_pred := ifelse(P_diff > 0, n_policy, N_static)]
    # 
    # ggplot(data = prediction_set_aggregated_dt, aes(x=P_diff, y=eonr_pred))+
    #   geom_point()
    # 
    # prediction_set_aggregated_dt[, N_static := NULL]
    # prediction_set_aggregated_dt[, n_policy := NULL]
    # prediction_set_aggregated_dt[, P_diff := NULL]
    # summary(prediction_set_aggregated_dt$eonr_pred)
    # #---------------------------------------------------------------------------
    # # PERFORMANCE EVALUATION
    # evaluation_set_tmp <- merge(evaluation_set_dt,
    #                          prediction_set_aggregated_dt[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    #   .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
    #   .[N_fert == eonr_pred] %>%
    #   .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    # 
    # evaluation_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
    # 
    # results_list[[length(results_list)+1]] <- evaluation_set_tmp[,lag := lag_n][,NRS := 'metamodel']
    
    #===================================================================================================================
    # 3) YIELD GOAL
    # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH id_field
    
    prediction_set_aggregated_dt[,regionC := ifelse(region == '2-Central', 1,0)]
    prediction_set_aggregated_dt[,regionN := ifelse(region == '3-North', 1,0)]
    
    prediction_set_aggregated_dt[,eonr_pred := round(predict(yieldgoal, prediction_set_aggregated_dt)/10,0)*10]
    
    # #---------------------------------------------------------------------------
    # # PERFORMANCE EVALUATION
    evaluation_set_tmp <- merge(evaluation_set_dt,
                             prediction_set_aggregated_dt[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
      .[N_fert == eonr_pred] %>%
      .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    
    evaluation_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
    
    results_list[[length(results_list)+1]] <- evaluation_set_tmp[,lag := lag_n][,NRS := 'yieldgoal']
    #===================================================================================================================
    # 3) EXPOST
    predictions_expost_dt  <- aggregate_by_area(data_dt = evaluation_set_dt, variables = c('P'),
                                                    weight = 'area_ha', by_c = c('id_10', 'id_field','z', 'N_fert')) %>% 
      .[, .SD[ P == max( P)], by = .(id_10, id_field, z)] %>%
      .[, .SD[ N_fert == min(N_fert)], by = .(id_10, id_field, z)] %>% setnames('N_fert', 'eonr_pred')
    
    # #---------------------------------------------------------------------------
    # # PERFORMANCE EVALUATION
    evaluation_set_tmp <- merge(evaluation_set_dt,
                             predictions_expost_dt[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
      .[N_fert == eonr_pred] %>%
      .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    
    evaluation_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
    
    results_list[[length(results_list)+1]] <- evaluation_set_tmp[,lag := lag_n][,NRS := 'expost']
    #===================================================================================================================
    # 3) YIELD RESPONSE
    # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH id_field
    
    # prediction_set_aggregated_dt[,eonr_pred := ceiling(predict(yieldresponse, prediction_set_aggregated_dt)/10)*10]
    # 
    # # #---------------------------------------------------------------------------
    # # # PERFORMANCE EVALUATION
    # evaluation_set_tmp <- merge(evaluation_set_dt,
    #                          prediction_set_aggregated_dt[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    #   .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
    #   .[N_fert == eonr_pred] %>%
    #   .[,c("region", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    # 
    # evaluation_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
    # 
    # results_list[[length(results_list)+1]] <- evaluation_set_tmp[,lag := lag_n][,NRS := 'yieldresponse']
    
  }#end of lag_n
}#end of z_n

perfomances_dt <- rbindlist(results_list)
perfomances_dt[,.N, z][order(z)]

perfomances_dt[,.N, .(id_10, id_field)] %>% .[,.N, .(id_10)] %>% .[,N] %>% table() #number of fields by cell
perfomances_dt[,.N, .(id_10, id_field, lag, NRS, z)] %>% .[,.N, .(NRS)] #field x year by NRS
# perfomances_dt[,.N, .(id_10, id_field, lag, NRS, z)] %>% .[,.N, .(set)]#field x year by set

# perfomances_dt[z %in% validation_z, set := 'validation']
# perfomances_dt[!z %in% c(training_z, validation_z), set := 'testing']
# #-------------------------------------------------------------------------
# # AGGREGATE THE DATA TO FIELD X Z LEVEL CONSIDERING THE AREA
# names(perfomances_dt)
# do_not_aggregate = c('region','id_10', 'lag','NRS', 'z', 'id_field', 'set')
# do_aggregate =  c("Y_corn", 'Y_soy', 'L1', 'L2', "L", "N_fert","P")
# 
# if(FALSE){
#   
#   perfomances_dt2 <- aggregate_by_area(data_dt = perfomances_dt, variables = do_aggregate, 
#                                        weight = 'area_ha', by_c = do_not_aggregate) #field x z level (id_field is out)
# }else{
#   split_list <- split(perfomances_dt,perfomances_dt$z)
#   split_list_output <- list()
#   for(split_list_n in split_list){
#     split_list_output[[unique(split_list_n$z)]] <- aggregate_by_area(data_dt = split_list_n, variables = do_aggregate, 
#                                                                      weight = 'area_ha', by_c = do_not_aggregate) #field x z level (id_field is out)
#   }
#   
#   field_perfomances_dt <- rbindlist(split_list_output)
# }

#--------------------------------------------------------------------------------
# Add the eonr_12
#--------------------------------------------------------------------------------
# Add the eonr_12
field_ex_post_dt <- yc_field_dt[, .SD[ P == max( P)], by = .(id_10, z, id_field)] %>% 
  .[, .SD[ N_fert == min( N_fert)], by = .(id_10, z, id_field)] %>%
  .[,.(id_10, z, id_field, eonr_12 = N_fert, P_12 = P, L_12 = L)]

field_perfomances_dt <- merge(perfomances_dt, field_ex_post_dt, by =  c('id_10', 'z', 'id_field'))

field_perfomances_dt[,eonr_12 := round(eonr_12,0)]
field_perfomances_dt[,N_fert := round(N_fert,0)]
field_perfomances_dt[,N_dlt := N_fert - eonr_12]
field_perfomances_dt[,overpred := ifelse(N_fert > eonr_12, 1, 0 )]
field_perfomances_dt[,subpred := ifelse(N_fert < eonr_12, 1, 0 )]
field_perfomances_dt[,exact := ifelse(N_fert == eonr_12, 1, 0 )]


saveRDS(field_perfomances_dt, "./n_policy_box/Data/files_rds/field_perfomances_dt.rds")
saveRDS(reg_model_stuff,"./n_policy_box/Data/files_rds/reg_model_stuff.rds")
#-------------------------------------------------------
# 
# perfomances_dt2 <- readRDS("./n_policy_box/Data/files_rds/perfomances_twecon_dt.rds")
# perfomances_dt3 <- rbind(perfomances_dt, perfomances_dt2)
# 
# saveRDS(perfomances_dt3, "./n_policy_box/Data/files_rds/perfomances_twecon_dt.rds")




