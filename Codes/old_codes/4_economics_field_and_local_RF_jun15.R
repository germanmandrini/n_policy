# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
# setwd('~')

source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')
source('./vr_value/Codes/parameters.R')


grid10_soils_sf5 <- readRDS("./vr_value/Data/Grid/grid10_soils_sf5.rds") 
grid10_tiles_sf2 <- readRDS("./vr_value/Data/Grid/grid10_tiles_sf2.rds") 

yc_yearly_dt <- readRDS('./vr_value/Data/files_rds/yc_yearly_dt.rds')

eonr_mukey_dt3 <- readRDS("./vr_value/Data/files_rds/eonr_mukey_dt3.rds")
reg_model_stuff <- readRDS( "./vr_value/Data/files_rds/reg_model_stuff.rds")
#======================================================================================
# GET THE FIELDS THAT CAN BE RUN
stations_dt <- reg_model_stuff$stations
full_fields_dt <- reg_model_stuff$full_fields

performance_all_list <- list()

for(j in 1:nrow(full_fields_dt)){
  # j = 31
  performance_set_list <- list()
  print(j)
  field_n <- full_fields_dt[j,]
  one_field_sf <- grid10_soils_sf5[grid10_soils_sf5$id_10 == field_n$id_10 &
                                     grid10_soils_sf5$id_field == field_n$id_field,]
  
  #======================================================================================
  # ADD GEOGRAPHIC INFORMATION 
  one_field_dt <- data.table(one_field_sf, st_coordinates(st_centroid(one_field_sf))) %>% .[,-'geometry']
  setnames(one_field_dt, c('X', 'Y'), c('long', 'lat'))
  one_field_dt <- one_field_dt[,.(area_ha = sum(area_ha),
                                         region = max(region), 
                                         long = mean(long), lat = mean(lat)), by = .(id_tile, id_10, state_name, county_name, mukey)]
  
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
  ic_field_dt <- yc_yearly_dt[id_10 == field_n$id_10 & mukey %in% unique(one_field_sf$mukey)]
  if(!all(unique(one_field_dt$mukey) %in% unique(ic_field_dt$mukey))){next} #if not all the mukeys of the field are in the apsim files skip
  
  no_cost_var <- reg_model_stuff$no_cost_var
  ss_var <- reg_model_stuff$ss_var
  
  keep_cols <- c('id_10', 'mukey', 'z', 'Yld', 'leach_n', 'leach_n2', 'N_fert', no_cost_var, ss_var)
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
  #==================================================================================================================
  # VR plots
  if(FALSE){
    mukey_n <- sample(ic_field_dt$mukey,1)
    
    eonr_plot <- ic_field_dt[mukey == mukey_n & rotation == 0] %>% 
      .[, .SD[ P == max( P)], by = .(id_10, mukey, z, rotation)] %>%
      .[,c("mukey", "z", "rotation", "id_10", "area_ha", "Yld", "leach_n", "leach_n2", "N_fert", 'P' )]
    
    # P plot with P at eonr
    (plot_n <- ggplot() +
      geom_line(data = ic_field_dt[mukey == mukey_n & rotation == 0], aes(x = N_fert, y = P, color = z, group=interaction(z, rotation))) +
      geom_point(data = eonr_plot[mukey == mukey_n & rotation == 0], aes(x = N_fert, y = P)) +
      ggtitle(paste('P plot with P at eonr', mukey_n)))
    
    ggsave(plot_n, filename = "./vr_value/Data/figures/yield_curve_example.jpg")
    
    # leach_no3 plot with leaching at eonr
    (plot_n <- ggplot() +
             geom_line(data = ic_field_dt[mukey == mukey_n & rotation == 0], aes(x = N_fert, y = leach_no3, color = z, group=interaction(z, rotation))) +
      geom_point(data = eonr_plot[mukey == mukey_n & rotation == 0], aes(x = N_fert, y = leach_no3)) +
      ggtitle(paste('leach_no3 plot with leaching at eonr', mukey_n)))
    
    ggsave(plot_n, filename = "./vr_value/Data/figures/leaching_curve_example.jpg")
    
  }
  
  #===================================================================================================================
  # 1) PREDICT WITH REGIONAL MINIMUM MODEL UR
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  # the model is trained with z1-10 and performance is evaluated with z11-25
  
  performance_set <- merge(ic_field_dt[!z %in% c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10" )],
                           reg_model_stuff$model_minimum_regional, by = c('region', 'rotation')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure performance
    .[N_fert == eonr_pred] %>%
    .[,c("mukey", "z", "rotation", "id_10", "area_ha", "Yld", "leach_n", "leach_n2", "N_fert",'P')]
  
  performance_set_list[[length(performance_set_list)+1]] <- performance_set[,model := 1][,tech := 'UR']

  
  #===================================================================================================================
  # 2) PREDICT WITH LOCAL MINIMUM MODEL UR
  prediction_set <- ic_field_dt[!z %in% c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10" )]
  model_minimum_local <- aggregate_by_area(data_dt = prediction_set, variables = c('P'), weight = 'area_ha', by_c = c('N_fert', 'rotation')) %>%
    .[, .SD[ P == max( P)], by = .(rotation)] %>% .[,.(rotation, eonr_pred = N_fert)]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  performance_set <- merge(ic_field_dt[!z %in% c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10" )],
                           model_minimum_local, by = c('rotation')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure performance
    .[N_fert == eonr_pred] %>%
    .[,c("mukey", "z", "rotation", "id_10", "area_ha", "Yld", "leach_n", "leach_n2", "N_fert", 'P')]
  
  performance_set_list[[length(performance_set_list)+1]] <- performance_set[,model := 2][,tech := 'UR']
  
  #===================================================================================================================
  #===================================================================================================================
  # 3) PREDICT WITH REGIONAL MODEL 1 -VR
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  
  # prediction_set <- data.table(unique(ic_field_dt[!z %in% c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10" ) ,
  #                                                 c('mukey', 'z', no_cost_var, ss_var), with = FALSE])) #this is unique v5 conditions, doesn't have the different N rates
  # 
  # 
  # prediction_set$eonr_pred <- round(predict(reg_model_stuff$model1b, prediction_set, type = "class")/10,0)*10
  # 
  performance_set1 <-ic_field_dt[!z %in% c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10")]
  
  performance_set1$eonr_pred <- round(predict(reg_model_stuff$model1b, performance_set1, type = "class")/10,0)*10
  performance_set1 <- performance_set1[N_fert == eonr_pred] %>%
    .[,c("mukey", "z", "rotation", "id_10", "area_ha", "Yld", "leach_n", "leach_n2", "N_fert", 'P' )]
  
  merge(prediction_set[,.(mukey, z, eonr_pred, rotation)], performance_set1[,.(mukey, z,rotation, N_fert)], by = c('mukey', 'z', 'rotation')) %>% .[ eonr_pred != N_fert]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  performance_set <- merge(ic_field_dt[!z %in% c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10" )],
                           prediction_set[,.(mukey, z, rotation, eonr_pred)], by = c('mukey', 'z', 'rotation')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure performance
    .[N_fert == eonr_pred] %>%
    .[,c("mukey", "z", "rotation", "id_10", "area_ha", "Yld", "leach_n", "leach_n2", "N_fert", 'P' )]
  
  performance_set_list[[length(performance_set_list)+1]] <- performance_set[,model := 3][,tech := 'VR']
  
  #===================================================================================================================
  # 4) PREDICT WITH REGIONAL MODEL 1 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  no_cost_varb <-reg_model_stuff$no_cost_var
  
  prediction_set <- data.table(unique(ic_field_dt[!z %in% c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10" ),
                                                  c('mukey', 'z','area_ha', no_cost_var, ss_var), with = FALSE])) #this is unique v5 conditions, doesn't have the different N rates
  
  test <- copy(prediction_set) #need to be here, before the columns are updated
  # We need to aggregate at the field level because is UR
  
  do_not_aggregate = c("mukey", "z", "area_ha", "rotation")
  do_aggregate = setdiff(c(no_cost_var, ss_var), do_not_aggregate)
  
  prediction_set_aggregated  <- aggregate_by_area(data_dt = prediction_set, variables = do_aggregate, 
                                                  weight = 'area_ha', by_c = c('z', 'rotation'))# %>% .[,-'area_ha']
  prediction_set_aggregated$eonr_pred <- round(predict(reg_model_stuff$model1b, prediction_set_aggregated, type = "class")/10,0)*10
  
  
  test[,n_20cm_v5 := n_20cm_v5 * area_ha]
  all(test[,.(n_20cm_v5 = sum(n_20cm_v5) / sum(area_ha)),by= c("z", "rotation")]$n_20cm_v5 == prediction_set_aggregated$n_20cm_v5 )
  
  prediction_set_aggregated$eonr_pred <- round(predict(reg_model_stuff$model1b, prediction_set_aggregated, type = "class")/10,0)*10
 
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  performance_set <- merge(ic_field_dt[!z %in% c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10" )],
                           prediction_set_aggregated[,.(z, rotation, eonr_pred)], by = c('z', 'rotation')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure performance
    .[N_fert == eonr_pred] %>%
    .[,c("mukey", "z", "rotation", "id_10", "area_ha", "Yld", "leach_n", "leach_n2", "N_fert", 'P' )]

  performance_set_list[[length(performance_set_list)+1]] <- performance_set[,model := 4][,tech := 'UR']
  

  #===================================================================================================================
  # 5) PREDICT WITH REGIONAL MODEL 2 -VR
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  prediction_set <- data.table(unique(ic_field_dt[!z %in% c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10" ) ,
                                                  c('mukey', 'z', no_cost_var, ss_var), with = FALSE])) #this is unique, doesn't have the different N rates
  
  
  prediction_set$eonr_pred <- round(predict(reg_model_stuff$model2b, prediction_set, type = "class")/10,0)*10
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  performance_set <- merge(ic_field_dt[!z %in% c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10" )],
                           prediction_set[,.(mukey, z, rotation, eonr_pred)], by = c('mukey', 'z', 'rotation')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure performance
    .[N_fert == eonr_pred] %>%
    .[,c("mukey", "z", "rotation", "id_10", "area_ha", "Yld", "leach_n", "leach_n2", "N_fert", 'P' )]
  performance_set_list[[length(performance_set_list)+1]] <- performance_set[,model := 5][,tech := 'VR']
  
  #===================================================================================================================
  # 6) PREDICT WITH REGIONAL MODEL 2 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY

  prediction_set_aggregated$eonr_pred <- round(predict(reg_model_stuff$model2b, prediction_set_aggregated, type = "class")/10,0)*10
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  performance_set <- merge(ic_field_dt[!z %in% c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10" )],
                           prediction_set_aggregated[,.(z, rotation, eonr_pred)], by = c('z', 'rotation')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure performance
    .[N_fert == eonr_pred] %>%
    .[,c("mukey", "z", "rotation", "id_10", "area_ha", "Yld", "leach_n", "leach_n2", "N_fert", 'P' )]
  
  performance_set_list[[length(performance_set_list)+1]] <- performance_set[,model := 6][,tech := 'UR']
  
  
  #===================================================================================================================
  #===================================      LOCAL MODEL       ========================================================
  #===================================================================================================================
  # 7) PREDICT WITH LOCAL MODEL 1 - VR 
  
  TrainSet <- ic_field_dt[z %in% c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10" )] %>% 
    .[, .SD[ P == max( P)], by = .(id_10, mukey, z, rotation)] %>%
    setnames('N_fert', 'eonr')
  
  no_cost_var_local <-  c(no_cost_var[!no_cost_var %in% c("region", "long", "lat")]) #replace location variables like lat, long and region by mukey
  ss_var_local <-  ss_var[!ss_var %in% c("dul_dep", "ll15_dep","whc")]
  
  # Create a Random Forest model with default parameters
  model1_local <- randomForest(eonr ~ ., data = TrainSet[,c('eonr', c('mukey', no_cost_var_local)), with = FALSE], importance = TRUE)
  # varImpPlot(model1_local, type=2)
  
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  prediction_set <- data.table(unique(ic_field_dt[!z %in% c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10" ) ,
                                                  c('mukey', 'z', no_cost_var, ss_var), with = FALSE])) #this is unique, doesn't have the different N rates
  
  
  prediction_set$eonr_pred <- round(predict(model1_local, prediction_set, type = "class")/10,0)*10
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  performance_set <- merge(ic_field_dt[!z %in% c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10" )],
                           prediction_set[,.(mukey, z, rotation, eonr_pred)], by = c('mukey', 'z', 'rotation')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure performance
    .[N_fert == eonr_pred] %>%
    .[,c("mukey", "z", "rotation", "id_10", "area_ha", "Yld", "leach_n", "leach_n2", "N_fert", 'P' )]
  performance_set_list[[length(performance_set_list)+1]] <- performance_set[,model := 7][,tech := 'VR']
  
  #===================================================================================================================
  # 8) PREDICT WITH LOCAL MODEL 1 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  # Need to train with aggregated data. We will aggregate the data and select the best N rate for the whole field

  do_not_aggregate = c("mukey", "z", "area_ha", "rotation")
  do_aggregate = setdiff(c(no_cost_var, ss_var), do_not_aggregate)

  TrainSet_aggregated2 <- aggregate_by_area(data_dt = ic_field_dt[z %in% c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10" )], 
                                           variables = c(do_aggregate, 'P'), 
                    weight = 'area_ha', by_c = c('N_fert','z', 'rotation'))  %>%
    .[, .SD[ P == max( P)], by = .(z, rotation)] %>% .[,-c('area_ha', 'P')] %>% setnames('N_fert', 'eonr')
  
  
  #now we train the model
  model1_local <- randomForest(eonr ~ ., data = TrainSet_aggregated2[,c('eonr', no_cost_var_local), with = FALSE], importance = TRUE)
  
  # varImpPlot(model1_local, type=2)
  
  #Get the predictions for the prediction set aggregated
  prediction_set_aggregated$eonr_pred <- round(predict(model1_local, prediction_set_aggregated, type = "class")/10,0)*10
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  #Predictions were made for the aggregated data, eates are obtained and performance is evaluated with data not aggregated
  performance_set <- merge(ic_field_dt[!z %in% c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10" )],
                           prediction_set_aggregated[,.(z, rotation, eonr_pred)], by = c('z', 'rotation')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure performance
    .[N_fert == eonr_pred] %>%
    .[,c("mukey", "z", "rotation", "id_10", "area_ha", "Yld", "leach_n", "leach_n2", "N_fert", 'P' )]
  
  performance_set_list[[length(performance_set_list)+1]] <- performance_set[,model := 8][,tech := 'UR']
  
  
  
  #===================================================================================================================
  # 9) PREDICT WITH LOCAL MODEL 2 - VR 
  
  TrainSet <- ic_field_dt[z %in% c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10" )] %>% 
    .[, .SD[ P == max( P)], by = .(id_10, mukey, z, rotation)] %>%
    setnames('N_fert', 'eonr')
  
  no_cost_var_local <-  c(no_cost_var[!no_cost_var %in% c("region", "long", "lat")]) #replace location variables like lat, long and region by mukey
  ss_var_local <-  ss_var[!ss_var %in% c("dul_dep", "ll15_dep","whc")]
  
  # Create a Random Forest model with default parameters
  model2_local <- randomForest(eonr ~ ., data = TrainSet[,c('eonr', c('mukey', no_cost_var_local, ss_var_local)), with = FALSE], importance = TRUE)
  
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  prediction_set <- data.table(unique(ic_field_dt[!z %in% c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10" ) ,
                                                  c('mukey', 'z', no_cost_var, ss_var), with = FALSE])) #this is unique, doesn't have the different N rates
  
  
  prediction_set$eonr_pred <- round(predict(model2_local, prediction_set, type = "class")/10,0)*10
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  performance_set <- merge(ic_field_dt[!z %in% c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10" )],
                           prediction_set[,.(mukey, z, rotation, eonr_pred)], by = c('mukey', 'z', 'rotation')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure performance
    .[N_fert == eonr_pred] %>%
    .[,c("mukey", "z", "rotation", "id_10", "area_ha", "Yld", "leach_n", "leach_n2", "N_fert", 'P' )]
  
  performance_set_list[[length(performance_set_list)+1]] <- performance_set[,model := 9][,tech := 'VR']
  
  
  #===================================================================================================================
  # 10) PREDICT WITH LOCAL MODEL 2 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  # Need to train with aggregated data

  model2_local <- randomForest(eonr ~ ., data = TrainSet_aggregated2[,c('eonr', no_cost_var_local, ss_var_local), with = FALSE], importance = TRUE)
  # varImpPlot(model2_local, type=2)
  
  prediction_set_aggregated$eonr_pred <- round(predict(model2_local, prediction_set_aggregated, type = "class")/10,0)*10
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  performance_set <- merge(ic_field_dt[!z %in% c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10" )],
                           prediction_set_aggregated[,.(z, rotation, eonr_pred)], by = c('z', 'rotation')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure performance
    .[N_fert == eonr_pred] %>%
    .[,c("mukey", "z", "rotation", "id_10", "area_ha", "Yld", "leach_n", "leach_n2", "N_fert", 'P' )]
  performance_set_list[[length(performance_set_list)+1]] <- performance_set[,model := 10][,tech := 'UR']
  
  
  #===================================================================================================================
  # 11) EX POST
  
  # PERFORMANCE EVALUATION
  performance_set <- ic_field_dt[!z %in% c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10" )] %>% 
    .[, .SD[ P == max( P)], by = .(id_10, mukey, z, rotation)] %>%
    .[,c("mukey", "z", "rotation", "id_10", "area_ha", "Yld", "leach_n", "leach_n2", "N_fert", 'P' )]
  
  performance_set_list[[length(performance_set_list)+1]] <- performance_set[,model := 11][,tech := 'VR']
  #===================================================================================================================
  performance_set_dt <- rbindlist(performance_set_list)
  performance_all_list[[length(performance_all_list)+1]] <- data.table(field_n[,.(id_10, id_field, region)], performance_set_dt)
  
  #===================================================================================================================
  #PLOT ME
  
  if(FALSE){
    mukey_n <- one_field_dt[area_ha == max(area_ha)]$mukey[1]
    
    performance_set_dt <- performance_set_dt[mukey == mukey_n]
    performance_set_dt[,model := as.character(model)]
    
    # P plot with P at eonr
    (plot_n <- ggplot() +
        geom_point(data = performance_set_dt[mukey == mukey_n & rotation == 0 & model != 11], aes(x = N_fert, y = P, colour = model)) +
        geom_point(data = performance_set_dt[mukey == mukey_n & rotation == 0 & model == 11], aes(x = N_fert, y = P), size = 3, show.legend = FALSE) +
        geom_line(data = ic_field_dt[mukey == mukey_n & rotation == 0], aes(x = N_fert, y = P, group=interaction(z, rotation)), show.legend = FALSE) +
        ggtitle(paste('P plot with P at eonr', mukey_n)))
    
    ggsave(plot_n, filename = "./vr_value/Data/figures/yield_curve_example.jpg")
    
    # leach_no3 plot with leaching at eonr
    (plot_n <- ggplot() +
        geom_point(data = performance_set_dt[mukey == mukey_n & rotation == 0 & model != 11], aes(x = N_fert, y = leach_n2, colour = model)) +
        geom_point(data = performance_set_dt[mukey == mukey_n & rotation == 0 & model == 11], aes(x = N_fert, y = leach_n2), size = 3, show.legend = FALSE) +
        geom_line(data = ic_field_dt[mukey == mukey_n & rotation == 0], aes(x = N_fert, y = leach_n2, group=interaction(z, rotation)), show.legend = FALSE) +
        ggtitle(paste('leach_no3 plot with leaching at eonr', mukey_n)))
    
    ggsave(plot_n, filename = "./vr_value/Data/figures/leaching_curve_example.jpg")
    
    # performance_set_dt[,.(Yld = mean(Yld), leach_n2 = mean(leach_n2), leach_peak = max(leach_n2), P = mean(P)), by = model]
    
  }#end of PLOT ME
}
  
perfomances_dt <- rbindlist(performance_set_list)

saveRDS(perfomances_dt, "./vr_value/Data/files_rds/perfomances_dt.rds")
  


