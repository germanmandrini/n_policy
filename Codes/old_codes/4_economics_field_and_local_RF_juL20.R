# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
setwd('~')

source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')
source('./vr_value/Codes/parameters.R')

library(randomForest)

eonr_mukey_dt3 <- readRDS("./vr_value/Data/files_rds/eonr_mukey_dt3.rds")

grid10_soils_sf6 <- readRDS("./vr_value/Data/Grid/grid10_soils_sf6.rds")

grid10_tiles_sf2 <- readRDS("./vr_value/Data/Grid/grid10_tiles_sf2.rds") 

yc_yearly_dt2 <- readRDS("./vr_value/Data/files_rds/yc_yearly_dt2.rds")

reg_model_stuff <- readRDS( "./vr_value/Data/files_rds/reg_model_stuff.rds")

#======================================================================================
# GET THE FIELDS THAT CAN BE RUN
stations_dt2 <- reg_model_stuff$stations
full_fields_dt2 <- reg_model_stuff$full_fields
training_z <- reg_model_stuff$training_z
trial_rates <- reg_model_stuff$trial_rates



# mukey_n = 242963
# which(full_fields_dt2$id_10 == 475 & full_fields_dt2$id_field == 3)
# sample_fields <- sample(1:nrow(full_fields_dt2), 32, replace = FALSE)

process_field_economics <- function(j){
  # j =  999
  small_list <- list()
  print(j)
  field_n <- full_fields_dt2[j,]
  one_field_sf <- grid10_soils_sf6[grid10_soils_sf6$id_10 == field_n$id_10 &
                                     grid10_soils_sf6$id_field == field_n$id_field,]
  
  #======================================================================================
  # ADD GEOGRAPHIC INFORMATION 
  one_field_dt <- data.table(one_field_sf, st_coordinates(st_centroid(one_field_sf))) %>% .[,-'geometry']
  setnames(one_field_dt, c('X', 'Y'), c('long', 'lat'))
  one_field_dt <- one_field_dt[,.(area_ha = sum(area_ha),
                                         region = max(region), 
                                         long = mean(long), 
                                          lat = mean(lat)), by = .(id_tile, id_10, state_name, county_name, mukey)]
  sum(one_field_dt$area_ha)
  #---------------------------------------------------------------------------
  #MAKE A MAP OF THE FIELD
  if(FALSE){
    (field <- tm_shape(one_field_sf) + tm_polygons("mukey") + 
       tm_layout(legend.text.size = 0.7,
                 main.title = paste('ONE FIELD MAP -', round(sum(one_field_sf$area_ha),1),' ha'),
                 main.title.position = "center",
                 main.title.size = 1))
    tmap_save(field, filename = "./vr_value/Data/figures/field.jpg", scale = 2)  
  }
  #---------------------------------------------------------------------------
  # FILTER APSIM DATA AND ADD VARIABLES NEEDED FOR PREDICTIONS
  ic_field_dt <- yc_yearly_dt2[id_10 == field_n$id_10 & mukey %in% unique(one_field_sf$mukey)]
  ic_field_dt[,.N, by = .(rotation, z, mukey)]
  # if(!all(unique(one_field_dt$mukey) %in% unique(ic_field_dt$mukey))){next} #if not all the mukeys of the field are in the apsim files skip
  
  no_cost_var <- reg_model_stuff$no_cost_var
  ss_var <- reg_model_stuff$ss_var
  
  keep_cols <- c('id_10', 'mukey', 'z', 'Yld', 'leach_n', 'N_fert', 'n_deep_v5', no_cost_var, ss_var)
  keep_cols <- base::intersect(names(ic_field_dt), keep_cols)
  
  # Add geographic variables
  ic_field_dt <- merge(ic_field_dt[, keep_cols, with = FALSE], one_field_dt, by = c('id_10', 'mukey'))
  
  # Add long term yield
  long_term_yld_dt <- eonr_mukey_dt3[id_10 == field_n$id_10 & mukey %in% unique(one_field_sf$mukey)] %>% 
    .[,.(id_10, mukey, Yld_lt_avg, Yld_lt_min, Yld_lt_max)] %>% unique() #it's already calculated using the eonr
  
  ic_field_dt <- merge(ic_field_dt, long_term_yld_dt, by = c('id_10', 'mukey'))
  ic_field_dt[,rotation := ifelse(rotation == 'MSM', 0, 1)]
  ic_field_dt[, P := Yld * Pc - N_fert * Pn]
  
  summary(ic_field_dt[,.(area_ha = sum(area_ha)),  by = .(id_10, z, N_fert, rotation)]$area_ha)

  #===================================================================================================================
  # 1) PREDICT WITH REGIONAL MINIMUM MODEL UR
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  # the model is trained with z1-10 and performance is evaluated with z11-25
  performance_set <- merge(ic_field_dt[!z %in% training_z],
                           reg_model_stuff$model_minimum_regional, by = c('region', 'rotation')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure performance
    .[N_fert == eonr_pred] %>%
    .[,c("mukey", "z", "rotation", "id_10", "area_ha", "Yld", "leach_n", "n_deep_v5","N_fert",'P')]
  
  small_list[[length(small_list)+1]] <- performance_set[,model := '1'][,tech := 'UR']
  
  #===================================================================================================================
  # 2) PREDICT WITH REGIONAL RF 1 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  no_cost_varb <-reg_model_stuff$no_cost_var
  
  prediction_set <- data.table(unique(ic_field_dt[!z %in% training_z,
                                                  c('mukey', 'z','area_ha', 'Yld', no_cost_var, ss_var), with = FALSE])) #this is unique v5 conditions, doesn't have the different N rates
  
  test <- copy(prediction_set) #need to be here, before the columns are updated
  # We need to aggregate at the field level because is UR
  
  do_not_aggregate = c("mukey", "z", "area_ha", "rotation")
  do_aggregate = setdiff(c(no_cost_var, ss_var, 'Yld'), do_not_aggregate)
  
  prediction_set_aggregated  <- aggregate_by_area(data_dt = prediction_set, variables = do_aggregate, 
                                                  weight = 'area_ha', by_c = c('z', 'rotation'))# %>% .[,-'area_ha']
  
  prediction_set_aggregated <- eonr_from_rf(prediction_set_aggregated, trial_rates, rf_model = reg_model_stuff$model1b)
  
  test[,n_20cm_v5 := n_20cm_v5 * area_ha]
  
  test2 <- merge(test[,.(n_20cm_v5 = sum(n_20cm_v5) / sum(area_ha)),by= c("z", "rotation")], 
                 prediction_set_aggregated[,.(z, rotation, n_20cm_v5)], by = c('z', 'rotation'))
  
  test2[, ok := round(n_20cm_v5.x,1) == round(n_20cm_v5.y,1)]
  
  all(test2$ok )
  
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  performance_set <- merge(ic_field_dt[!z %in% training_z],
                           prediction_set_aggregated[,.(z, rotation, eonr_pred)], by = c('z', 'rotation')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure performance
    .[N_fert == eonr_pred] %>%
    .[,c("mukey", "z", "rotation", "id_10", "area_ha", "Yld", "leach_n", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- performance_set[,model := '2'][,tech := 'UR']
  
  
  #===================================================================================================================
  # 3) PREDICT WITH REGIONAL RF 1 -VR
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  prediction_set <- data.table(unique(ic_field_dt[!z %in% training_z ,
                            c('mukey', 'z', no_cost_var, ss_var), with = FALSE])) #this is unique v5 conditions, doesn't have the different N rates

  
  prediction_set <- eonr_from_rf(prediction_set, trial_rates, rf_model = reg_model_stuff$model1b)
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  performance_set <- merge(ic_field_dt[!z %in% training_z],
                           prediction_set[,.(mukey, z, rotation, eonr_pred)], by = c('mukey', 'z', 'rotation')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure performance
    .[N_fert == eonr_pred] %>% 
    .[,c("mukey", "z", "rotation", "id_10", "area_ha", "Yld", "leach_n", "n_deep_v5", "N_fert", 'P' )] %>% .[order(mukey, z, rotation)]
  
  small_list[[length(small_list)+1]] <- performance_set[,model := '3'][,tech := 'VR']
  
  #===================================================================================================================
  # 4) PREDICT WITH REGIONAL MODEL 2 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  
  prediction_set_aggregated <- eonr_from_rf(prediction_set_aggregated[,-'eonr_pred'], trial_rates, rf_model = reg_model_stuff$model2b)
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  performance_set <- merge(ic_field_dt[!z %in% training_z],
                           prediction_set_aggregated[,.(z, rotation, eonr_pred)], by = c('z', 'rotation')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure performance
    .[N_fert == eonr_pred] %>%
    .[,c("mukey", "z", "rotation", "id_10", "area_ha", "Yld", "leach_n", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- performance_set[,model := '4'][,tech := 'UR']
  
  #===================================================================================================================
  # 4_eonr) PREDICT WITH REGIONAL MODEL 2 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  
  prediction_set_aggregated$eonr_pred <- ceiling(predict(reg_model_stuff$model2b_eonr, prediction_set_aggregated[,-'eonr_pred'], type = "class")/10)*10
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  performance_set <- merge(ic_field_dt[!z %in% training_z],
                           prediction_set_aggregated[,.(z, rotation, eonr_pred)], by = c('z', 'rotation')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure performance
    .[N_fert == eonr_pred] %>%
    .[,c("mukey", "z", "rotation", "id_10", "area_ha", "Yld", "leach_n", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- performance_set[,model := '4_eonr'][,tech := 'UR']
  
  #===================================================================================================================
  # 5) PREDICT WITH REGIONAL MODEL 2 -VR
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  prediction_set <- data.table(unique(ic_field_dt[!z %in% training_z ,
                                                  c('mukey', 'z', no_cost_var, ss_var), with = FALSE])) #this is unique, doesn't have the different N rates
  
  prediction_set <- eonr_from_rf(prediction_set, trial_rates, rf_model = reg_model_stuff$model2b)
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  performance_set <- merge(ic_field_dt[!z %in% training_z],
                           prediction_set[,.(mukey, z, rotation, eonr_pred)], by = c('mukey', 'z', 'rotation')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure performance
    .[N_fert == eonr_pred] %>%
    .[,c("mukey", "z", "rotation", "id_10", "area_ha", "Yld", "leach_n", "n_deep_v5", "N_fert", 'P' )]
  small_list[[length(small_list)+1]] <- performance_set[,model := '5'][,tech := 'VR']
  #===================================================================================================================
  # 5_eonr) PREDICT WITH REGIONAL MODEL 2 -VR
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  prediction_set$eonr_pred <- ceiling(predict(reg_model_stuff$model2b_eonr, prediction_set[,-'eonr_pred'], type = "class")/10)*10
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  performance_set <- merge(ic_field_dt[!z %in% training_z],
                           prediction_set[,.(mukey, z, rotation, eonr_pred)], by = c('mukey', 'z', 'rotation')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure performance
    .[N_fert == eonr_pred] %>%
    .[,c("mukey", "z", "rotation", "id_10", "area_ha", "Yld", "leach_n", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- performance_set[,model := '5_eonr'][,tech := 'VR']
  
  #===================================================================================================================
  # 6) PREDICT WITH LOCAL MINIMUM MODEL UR
  prediction_set <- ic_field_dt[!z %in% training_z]
  model_minimum_local <- aggregate_by_area(data_dt = prediction_set, variables = c('P'), weight = 'area_ha', by_c = c('N_fert', 'rotation')) %>%
    .[, .SD[ P == max( P)], by = .(rotation)] %>% .[,.(rotation, eonr_pred = N_fert)]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  performance_set <- merge(ic_field_dt[!z %in% training_z],
                           model_minimum_local, by = c('rotation')) %>% #here we join back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure performance
    .[N_fert == eonr_pred] %>%
    .[,c("mukey", "z", "rotation", "id_10", "area_ha", "Yld", "leach_n", "n_deep_v5", "N_fert", 'P')]
  
  small_list[[length(small_list)+1]] <- performance_set[,model := '6'][,tech := 'UR']
  
  #===================================================================================================================
  #===================================      LOCAL MODEL       ========================================================
  #===================================================================================================================
  no_cost_var_local <-  c(no_cost_var[!no_cost_var %in% c("region", "long", "lat")]) #replace location variables like lat, long and region by mukey
  ss_var_local <-  ss_var[!ss_var %in% c("dul_dep", "ll15_dep","whc")] 
  
  #===================================================================================================================
  # 7) PREDICT WITH LOCAL RF 1 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  # Need to train with aggregated data. We will aggregate the data and select the best N rate for the whole field
  
  do_not_aggregate = c("mukey", "z", "area_ha", "rotation")
  do_aggregate = c('Yld', setdiff(c(no_cost_var, ss_var), do_not_aggregate))
  
  TrainSet_aggregated2 <- aggregate_by_area(data_dt = ic_field_dt[z %in% training_z], 
                                            variables = c(do_aggregate, 'P'), 
                                            weight = 'area_ha', by_c = c('N_fert','z', 'rotation')) %>% 
    .[N_fert %in% trial_rates, c('Yld', 'N_fert' , no_cost_var_local, ss_var_local), with = FALSE]
  
  #now we train the model
  model1_local <- randomForest(Yld ~ ., data = TrainSet_aggregated2[,c('Yld', 'N_fert',no_cost_var_local), with = FALSE], importance = TRUE)
  
  #Get the predictions for the prediction set aggregated
  prediction_set_aggregated <- eonr_from_rf(prediction_set_aggregated[,-'eonr_pred'], trial_rates, rf_model = model1_local)
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  #Predictions were made for the aggregated data, eates are obtained and performance is evaluated with data not aggregated
  performance_set <- merge(ic_field_dt[!z %in% training_z],
                           prediction_set_aggregated[,.(z, rotation, eonr_pred)], by = c('z', 'rotation')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure performance
    .[N_fert == eonr_pred] %>%
    .[,c("mukey", "z", "rotation", "id_10", "area_ha", "Yld", "leach_n", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- performance_set[,model := '7'][,tech := 'UR']
  
  #===================================================================================================================
  # 8) PREDICT WITH LOCAL RF 1 - VR 
  
  TrainSet <- ic_field_dt[z %in% training_z] %>% 
    .[N_fert %in% trial_rates, c('Yld', 'N_fert', 'mukey' , no_cost_var_local, ss_var_local), with = FALSE]
  
  # Create a Random Forest model with default parameters
  model1_local <- randomForest(Yld ~ ., data = TrainSet[,c('Yld', 'N_fert', 'mukey', no_cost_var_local), with = FALSE], importance = TRUE)
  
  # varImpPlot(model1_local, type=2)
  
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  prediction_set <- data.table(unique(ic_field_dt[!z %in% training_z ,
                                                  c('mukey', 'z', no_cost_var), with = FALSE])) #this is unique, doesn't have the different N rates
  
  prediction_set <- eonr_from_rf(prediction_set, trial_rates, rf_model = model1_local)
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  performance_set <- merge(ic_field_dt[!z %in% training_z],
                           prediction_set[,.(mukey, z, rotation, eonr_pred)], by = c('mukey', 'z', 'rotation')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure performance
    .[N_fert == eonr_pred] %>%
    .[,c("mukey", "z", "rotation", "id_10", "area_ha", "Yld", "leach_n", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- performance_set[,model := '8'][,tech := 'VR']
  
  #===================================================================================================================
  # 9) PREDICT WITH LOCAL MODEL 2 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  # Need to train with aggregated data
  model2_local <- randomForest(Yld ~ ., data = TrainSet_aggregated2[,c('Yld', 'N_fert',no_cost_var_local, ss_var_local), with = FALSE], importance = TRUE)
  
  
  # varImpPlot(model2_local, type=2)
  prediction_set_aggregated <- eonr_from_rf(prediction_set_aggregated[,-'eonr_pred'], trial_rates, rf_model = model2_local)
  
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  performance_set <- merge(ic_field_dt[!z %in% training_z],
                           prediction_set_aggregated[,.(z, rotation, eonr_pred)], by = c('z', 'rotation')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure performance
    .[N_fert == eonr_pred] %>%
    .[,c("mukey", "z", "rotation", "id_10", "area_ha", "Yld", "leach_n", "n_deep_v5", "N_fert", 'P' )]
  small_list[[length(small_list)+1]] <- performance_set[,model := '9'][,tech := 'UR']
  #===================================================================================================================
  # 10) PREDICT WITH LOCAL RF 2 - VR 
  
  # Create a Random Forest model with default parameters
  model2_local <- randomForest(Yld ~ ., data = TrainSet[,c('Yld', 'N_fert', 'mukey', no_cost_var_local, ss_var_local), with = FALSE], importance = TRUE)
  
  
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  prediction_set <- data.table(unique(ic_field_dt[!z %in% training_z ,
                                                  c('mukey', 'z', no_cost_var, ss_var), with = FALSE])) #this is unique, doesn't have the different N rates
  
  prediction_set <- eonr_from_rf(prediction_set, trial_rates, rf_model = model2_local)
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  performance_set <- merge(ic_field_dt[!z %in% training_z],
                           prediction_set[,.(mukey, z, rotation, eonr_pred)], by = c('mukey', 'z', 'rotation')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure performance
    .[N_fert == eonr_pred] %>%
    .[,c("mukey", "z", "rotation", "id_10", "area_ha", "Yld", "leach_n", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- performance_set[,model := '10'][,tech := 'VR']
  
  #===================================================================================================================
  # 11) EX POST UR - Best rate at the field level
  
  # Need to get EONR with aggregated data
  
  prediction_set_aggregated <- aggregate_by_area(data_dt = ic_field_dt[!z %in% training_z], 
                                            variables = c(do_aggregate, 'P'), 
                                            weight = 'area_ha', by_c = c('N_fert','z', 'rotation'))  %>%
    .[, .SD[ P == max( P)], by = .(z, rotation)] %>% .[,-c('area_ha', 'P')] %>% setnames('N_fert', 'eonr_pred')
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  performance_set <- merge(ic_field_dt[!z %in% training_z],
                           prediction_set_aggregated[,.(z, rotation, eonr_pred)], by = c('z', 'rotation')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure performance
    .[N_fert == eonr_pred] %>%
    .[,c("mukey", "z", "rotation", "id_10", "area_ha", "Yld", "leach_n", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- performance_set[,model := '11'][,tech := 'UR']
  #===================================================================================================================
  # 12) EX POST VR
  
  # PERFORMANCE EVALUATION
  performance_set <- ic_field_dt[!z %in% training_z] %>% 
    .[, .SD[ P == max( P)], by = .(id_10, mukey, z, rotation)] %>%
    .[,c("mukey", "z", "rotation", "id_10", "area_ha", "Yld", "leach_n", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- performance_set[,model := '12'][,tech := 'VR']
  #===================================================================================================================
  performance_set_dt <- rbindlist(small_list)
  
  # performance_set_dt[, .(Yld =  mean(Yld),
  #                     leach_n = mean(leach_n),
  #                     N_fert = mean(N_fert),
  #                     P = mean(P), 
  #                     area_ha = sum(area_ha)), by = .( model, tech, rotation)][order(rotation, model)]
  performance_set_dt <- data.table(field_n[,.(id_10, id_field, region)], performance_set_dt[,-'id_10'])
  performance_set_dt[,.N, by = model]
  # big_list[[length(big_list)+1]] <- data.table(field_n[,.(id_10, id_field, region)], performance_set_dt[,-'id_10'])
  
  #===================================================================================================================
  #PLOT ME
  
  if(FALSE){
    mukey_n <- one_field_dt[area_ha == max(area_ha)]$mukey[1]
    performance_set_plot <- performance_set_dt[mukey == mukey_n]
    performance_set_plot[,model := as.character(model)]
    ic_field_plot <- ic_field_dt[!z %in% training_z]

    
    # Y plot with Yld at eonr
    (plot_n1 <- ggplot() +
        geom_point(data = performance_set_plot[mukey == mukey_n & rotation == 1 & model != 12], aes(x = N_fert, y = Yld, colour = model)) +
        geom_point(data = performance_set_plot[mukey == mukey_n & rotation == 1 & model == 12], aes(x = N_fert, y = Yld), size = 3, show.legend = FALSE) +
        geom_line(data = ic_field_plot[mukey == mukey_n & rotation == 1], aes(x = N_fert, y = Yld, group=interaction(z, rotation)), show.legend = FALSE) +
        # ggtitle(paste('Yld plot with Yld at eonr', mukey_n)))
        ggtitle('Yield'))
    
    ic_field_dt[mukey == mukey_n & rotation == 0][order(P)]
    ggsave(plot_n, filename = "./vr_value/Data/figures/yield_curve_example.jpg")
    
    
    # P plot with P at eonr
    (plot_n <- ggplot() +
        geom_point(data = performance_set_plot[mukey == mukey_n & rotation == 1 & model != 12], aes(x = N_fert, y = P, colour = model)) +
        geom_point(data = performance_set_plot[mukey == mukey_n & rotation == 1 & model == 12], aes(x = N_fert, y = P), size = 3, show.legend = FALSE) +
        geom_line(data = ic_field_plot[mukey == mukey_n & rotation == 1], aes(x = N_fert, y = P, group=interaction(z, rotation)), show.legend = FALSE) +
        ggtitle(paste('P plot with P at eonr', mukey_n)))
    
    ic_field_dt[mukey == mukey_n & rotation == 0][order(P)]
    ggsave(plot_n, filename = "./vr_value/Data/figures/yield_curve_example.jpg")
    
    # leach_no3 plot with leaching at eonr
    (plot_n2 <- ggplot() +
        geom_point(data = performance_set_plot[mukey == mukey_n & rotation == 0 & model != 12], aes(x = N_fert, y = leach_n, colour = model)) +
        geom_point(data = performance_set_plot[mukey == mukey_n & rotation == 0 & model == 12], aes(x = N_fert, y = leach_n), size = 3, show.legend = FALSE) +
        geom_line(data = ic_field_plot[mukey == mukey_n & rotation == 0], aes(x = N_fert, y = leach_n, group=interaction(z, rotation)), show.legend = FALSE) +
        # ggtitle(paste('leach_no3 plot with leaching at eonr', mukey_n)))
        ggtitle('N Leaching'))
    
    ggsave(plot_n, filename = "./vr_value/Data/figures/leaching_curve_example.jpg")
    #---------------------------------------------------------------------------
    # ESL510
    (plot_n1 <- ggplot() +
        geom_point(data = performance_set_plot[mukey == mukey_n & rotation == 1 & model != 12], aes(x = N_fert, y = Yld), size = 0.2) +
        geom_point(data = performance_set_plot[mukey == mukey_n & rotation == 1 & model == 12], aes(x = N_fert, y = Yld), size = 3, show.legend = FALSE) +
        geom_line(data = ic_field_plot[mukey == mukey_n & rotation == 1], aes(x = N_fert, y = Yld, colour=z)) +
        # ggtitle(paste('Yld plot with Yld at eonr', mukey_n)))
        ggtitle('Yield'))
    
    (plot_n2 <- ggplot() +
        geom_point(data = performance_set_plot[mukey == mukey_n & rotation == 1 & model != 12], aes(x = N_fert, y = leach_n), size = 0.2) +
        geom_point(data = performance_set_plot[mukey == mukey_n & rotation == 1 & model == 12], aes(x = N_fert, y = leach_n), size = 3, show.legend = FALSE) +
        geom_line(data = ic_field_plot[mukey == mukey_n & rotation == 1], aes(x = N_fert, y = leach_n, colour=z)) +
        # ggtitle(paste('Yld plot with Yld at eonr', mukey_n)))
        ggtitle('N Leaching'))
    
    grid.arrange(plot_n1, plot_n2)
    
    #---------------------------------------------------------------------------
    #NOW WITH N_TOTAL (FERT + SOIL)
    ic_field_plot[,N_total := N_fert + n_deep_v5]
    performance_set_plot[,N_total := N_fert + n_deep_v5]
    
    # P plot with P at eonr
    (plot_n <- ggplot() +
        geom_point(data = performance_set_plot[mukey == mukey_n & rotation == 0 & model != 12], aes(x = N_total, y = P, colour = model)) +
        geom_point(data = performance_set_plot[mukey == mukey_n & rotation == 0 & model == 12], aes(x = N_total, y = P), size = 3, show.legend = FALSE) +
        geom_line(data = ic_field_plot[mukey == mukey_n & rotation == 0], aes(x = N_total, y = P, group=interaction(z, rotation)), show.legend = FALSE) +
        ggtitle(paste('P plot with P at eonr', mukey_n)))
    
    ic_field_dt[mukey == mukey_n & rotation == 0][order(P)]
    ggsave(plot_n, filename = "./vr_value/Data/figures/yield_curve_example_ntotal.jpg")
    
    # leach_no3 plot with leaching at eonr
    (plot_n <- ggplot() +
        geom_point(data = performance_set_plot[mukey == mukey_n & rotation == 0 & model != 12], aes(x = N_total, y = leach_n, colour = model)) +
        geom_point(data = performance_set_plot[mukey == mukey_n & rotation == 0 & model == 12], aes(x = N_total, y = leach_n), size = 3, show.legend = FALSE) +
        geom_line(data = ic_field_plot[mukey == mukey_n & rotation == 0], aes(x = N_total, y = leach_n, group=interaction(z, rotation)), show.legend = FALSE) +
        ggtitle(paste('leach_no3 plot with leaching at eonr', mukey_n)))
    
    ggsave(plot_n, filename = "./vr_value/Data/figures/leaching_curve_example_ntotal.jpg")
  }#end of PLOT ME
  
  return(performance_set_dt)
}

library('foreach')

time1 <- Sys.time()
big_list <- foreach(j=1:nrow(full_fields_dt2)) %do% {
  process_field_economics(j)}
time2 <- Sys.time()

nopar <- time2 - time1

perfomances_dt <- rbindlist(big_list)

perfomances_dt[, .(Yld =  mean(Yld),
                       leach_n = mean(leach_n),
                       N_fert = mean(N_fert),
                       P = mean(P), 
                       area_ha = sum(area_ha)), by = .( model, tech, rotation)][order(rotation, model)]


saveRDS(perfomances_dt, "./vr_value/Data/files_rds/perfomances_ntree500_split_dt.rds")
  


