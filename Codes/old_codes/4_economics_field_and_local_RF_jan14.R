# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
# setwd('~')

source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')
source('./vr_value_v2/Codes/parameters.R')

library(randomForest)
library(mlr)
# eonr_mukey_dt3 <- readRDS("./vr_value_v2/Data/files_rds/eonr_mukey_dt3.rds")
  
yc_yearly_dt3 <- readRDS("./vr_value_v2/Data/files_rds/yc_yearly_dt3.rds")
grid10_tiles_sf6 <- readRDS("./vr_value_v2/Data/Grid/grid10_tiles_sf6.rds") 
grid10_soils_dt5 <- readRDS("./vr_value_v2/Data/Grid/grid10_soils_dt5.rds") %>% data.table()
grid10_fields_sf2 <- readRDS('./vr_value_v2/Data/Grid/grid10_fields_sf2.rds')
reg_model_stuff <- readRDS( "./vr_value_v2/Data/files_rds/reg_model_stuff.rds")
grid10_soils_sf2 <- readRDS('./vr_value_v2/Data/Grid/grid10_soils_sf2.rds')

#======================================================================================
# GET THE FIELDS THAT CAN BE RUN
# stations_dt2 <- reg_model_stuff2$stations
full_fields_dt <- reg_model_stuff$full_fields #one row by field x soil

# Clean the fields, leaving only 2 by cell
full_fields_dt2 <- full_fields_dt[,.(.N), by = .(id_10, id_field, region)][,-'N'] #one row by field
table(full_fields_dt2[,.N, .(id_10)]$N)

full_fields_dt2[,N := .N, .(id_10)]
# full_fields_wide_dt <- dcast(full_fields_dt2, id_10 + region +  N ~ id_field, value.var = "id_field")
Cnames(full_fields_wide_dt)[4:7] <- paste('field', names(full_fields_wide_dt)[4:7], sep = '_')
  # full_fields_wide_dt[N == 4,field_3 := NA ]
  # full_fields_wide_dt[N == 4,field_4 := NA ]
  
  # full_fields_wide_dt[N == 3 & is.na(field_1), field_2 := NA]
  # full_fields_wide_dt[N == 3 & is.na(field_2), field_1 := NA]
  # full_fields_wide_dt[N == 3 & is.na(field_3), field_4 := NA]
  # full_fields_wide_dt[N == 3 & is.na(field_4), field_3 := NA]
  
  # full_fields_dt3 <- melt(full_fields_wide_dt, id.vars = c("id_10", "region","N"), 
  #              measure.vars = c("field_1", "field_2", "field_3", "field_4"), value.name = 'id_field') %>% 
  #   .[!is.na(id_field)] %>% .[,variable := NULL]
  
  # fields_list_dt <- full_fields_dt3[N != 1][order(id_10)]
fields_list_dt <- full_fields_dt2[N != 1][order(id_10)]
fields_list_dt[,N := .N, .(id_10)]
table(fields_list_dt$N)
unique(fields_list_dt)

# ----------------
training_z <- reg_model_stuff$training_z
no_cost_varb_trf <- reg_model_stuff$no_cost_varb_trf
ss_var <- reg_model_stuff$ss_var
crop_varb <- reg_model_stuff$crop_varb


# sampled_field <- grid10_soils_sf6[grid10_soils_sf6$county_name == 'Champaign',] 
# sampled_field2 <- sampled_field %>% group_by(mukey) %>% summarize(area_ha = sum(area_ha)) %>% arrange(desc(area_ha))
# mukey_n <- sampled_field2$mukey[1]
# sampled_field3 <- sampled_field[sampled_field$mukey == mukey_n,] %>% arrange(desc(corn5_cell))
# 
# which(full_fields_dt2$id_10 == sampled_field3[1,]$id_10 & full_fields_dt2$id_field == sampled_field3[1,]$id_field)


# mukey_n = 242963
# which(fields_list_dt$id_10 ==  5 & fields_list_dt$id_field == 1)
# sample_fields <- sample(1:nrow(full_fields_dt2), 32, replace = FALSE)

process_field_economics <- function(j){
  # j = 1
  print(j)
  field_info <- fields_list_dt[j]
  id_10_info <- fields_list_dt[id_10 == field_info$id_10]
  
  small_list <- list()
  
  ic_field_dt_both <- data.table()
  for(field_n in id_10_info$id_field){
  # field_n <- id_10_info$id_field[1]
  
    field_soils_dt <- grid10_soils_dt5[id_10 == field_info$id_10 &
                      id_field == field_n,]
    
    
    
    sum(field_soils_dt$area_ha)
    
    #======================================================================================
    # # ADD GEOGRAPHIC INFORMATION 
    # one_field_dt <- data.table(one_field_sf, st_coordinates(st_centroid(one_field_sf))) %>% .[,-'geometry']
    # setnames(one_field_dt, c('X', 'Y'), c('long', 'lat'))
    # one_field_dt <- one_field_dt[,.(area_ha = sum(area_ha),
    #                                        region = max(region), 
    #                                        long = mean(long), 
    #                                         lat = mean(lat)), by = .(id_tile, id_10, state_name, county_name, mukey)]
    # sum(one_field_dt$area_ha)
    #---------------------------------------------------------------------------
    #MAKE A MAP OF THE FIELD
    if(FALSE){
  
      one_field_sf <- grid10_soils_sf2[grid10_soils_sf2$id_10 == field_n$id_10 &
                                         grid10_soils_sf2$id_field == field_n$id_field,]
      one_field_sf2 <- one_field_sf[one_field_sf$mukey %in% field_soils_dt$mukey,]
      
      (field <- tm_shape(one_field_sf2) + tm_polygons("mukey") + 
         tm_layout(legend.text.size = 0.7,
                   main.title = paste('ONE FIELD MAP -', round(sum(one_field_sf$area_ha),1),' ha'),
                   main.title.position = "center",
                   main.title.size = 1))
      tmap_save(field, filename = "./vr_value_v2/Data/figures/field.jpg", scale = 2)  
    }
    #---------------------------------------------------------------------------
    # FILTER APSIM DATA AND ADD VARIABLES NEEDED FOR PREDICTIONS
    z_odd = c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)
    z_even = z_odd+1
    z_select <- if(field_n %in% c(1,3)){z_odd} else{z_even}
    ic_field_dt <- yc_yearly_dt3[id_10 == field_info$id_10 & 
                                   mukey %in% unique(field_soils_dt$mukey) & z %in% z_select]
    ic_field_dt[,.N, by = z]
    
    # Update area and coordinates using field level information
    ic_field_dt <- merge(ic_field_dt, field_soils_dt[,.(id_10, mukey, area_ha, lat, long)], by = c('id_10', 'mukey'))
    ic_field_dt[, P := Yld * Pc - N_fert * Pn]
    ic_field_dt[, n_0_60cm_v5 := n_20cm_v5 + n_40cm_v5 + n_60cm_v5]
    ic_field_dt[,.N, by = .(z, mukey)]
    
    #Stantadarize the data
    ic_field_dt <- predict(reg_model_stuff$preprocessParams, ic_field_dt)
    # Create dummy variables for region
    ic_field_dt[, region.2 := ifelse(region == 2, 1, 0)]
    ic_field_dt[, region.3 := ifelse(region == 3, 1, 0)]
    
    ic_field_dt_both <- rbind(ic_field_dt_both, ic_field_dt[,id_field := field_n])
  } #end of field_n loop
  
  # Select the information for the field we are analyzing now
  ic_field_dt <- ic_field_dt_both[id_field == field_info$id_field]
  
  #===================================================================================================================
  # 1) PREDICT WITH REGIONAL MINIMUM MODEL UR ----

  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  # the model is trained with z1-10 and testing is evaluated with z11-25
  testing_set <- merge(ic_field_dt[!z %in% training_z],
                         reg_model_stuff$model_minimum_regional, by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5","N_fert",'P')]
  
  areas <- testing_set[,.(mukey, area_ha)] %>% unique()
  
  sum(areas$area_ha)
  
  small_list[[length(small_list)+1]] <- testing_set[,model := '1'][,tech := 'UR']
  
  #===================================================================================================================
  # 2) PREDICT WITH REGIONAL RF 1 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  # no_cost_var <-reg_model_stuff$no_cost_var
  table(ic_field_dt$z)
  
  prediction_set <- data.table(unique(ic_field_dt[!z %in% training_z,
                                                c('mukey', 'z','area_ha', no_cost_varb_trf, ss_var, crop_varb), with = FALSE])) #this is unique v5 conditions, doesn't have the different N rates
  table(prediction_set$z)
  
  test <- copy(prediction_set) #need to be here, before the columns are updated
  
  # We need to aggregate at the field level because is UR
  do_not_aggregate = c("mukey", "z", "area_ha")
  do_aggregate = setdiff(c(no_cost_varb_trf, ss_var, crop_varb), do_not_aggregate)
  
  prediction_set_aggregated  <- aggregate_by_area(data_dt = prediction_set, variables = do_aggregate, 
                                                weight = 'area_ha', by_c = c('z'))# %>% .[,-'area_ha']
  
  prediction_set_aggregated[,eonr_pred := ceiling(predict(reg_model_stuff$rf1_eonr, prediction_set_aggregated)/10)*10]
  
  test[,n_0_60cm_v5 := n_0_60cm_v5 * area_ha]
  
  test2 <- merge(test[,.(n_0_60cm_v5 = sum(n_0_60cm_v5) / sum(area_ha)),by= c("z")], 
               prediction_set_aggregated[,.(z, n_0_60cm_v5)], by = c('z'))
  
  test2[, ok := round(n_0_60cm_v5.x,1) == round(n_0_60cm_v5.y,1)]
  
  all( test2$ok )
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <- merge(ic_field_dt[!z %in% training_z],
                         prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we join back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'rf1'][,tech := 'UR']
  
  #===================================================================================================================
  # 3) PREDICT WITH REGIONAL RF 1 -VR
  
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY AND PERFORMANCE EVALUATION
  testing_set <- ic_field_dt[!z %in% training_z] %>% 
  .[,eonr_pred := ceiling(predict(reg_model_stuff$rf1_eonr, .)/10)*10] %>%
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>% 
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[order(mukey, z)]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'rf1'][,tech := 'VR']
  
  #===================================================================================================================
  # 4) PREDICT WITH REGIONAL RF 2 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  
  prediction_set_aggregated[,eonr_pred := ceiling(predict(reg_model_stuff$rf2_eonr, prediction_set_aggregated)/10)*10]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <- merge(ic_field_dt[!z %in% training_z],
                         prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'rf2'][,tech := 'UR']
  
  #===================================================================================================================
  # 5) PREDICT WITH REGIONAL MODEL 2 -VR
  
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY AND PERFORMANCE EVALUATION
  testing_set <- ic_field_dt[!z %in% training_z] %>% 
  .[,eonr_pred := ceiling(predict(reg_model_stuff$rf2_eonr, . )/10)*10] %>%
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>% 
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[order(mukey, z)]
  
  # predict(reg_model_stuff$model2b_eonr,type="prob")
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'rf2'][,tech := 'VR']
  
  #===================================================================================================================
  # 6) PREDICT WITH REGIONAL RF 3 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  
  prediction_set_aggregated[,eonr_pred := ceiling(predict(reg_model_stuff$rf3_eonr, prediction_set_aggregated)/10)*10]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <- merge(ic_field_dt[!z %in% training_z],
                     prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'rf3'][,tech := 'UR']
  
  #===================================================================================================================
  # 7) PREDICT WITH REGIONAL MODEL 3 -VR
  
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY AND PERFORMANCE EVALUATION
  testing_set <- ic_field_dt[!z %in% training_z] %>% 
  .[,eonr_pred := ceiling(predict(reg_model_stuff$rf3_eonr, . )/10)*10] %>%
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>% 
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[order(mukey, z)]
  
  # predict(reg_model_stuff$model2b_eonr,type="prob")
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'rf3'][,tech := 'VR']
  
  #===================================================================================================================
  # 8) PREDICT WITH REGIONAL REG1 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  
  prediction_set_aggregated[,eonr_pred := ceiling(predict(reg_model_stuff$reg_lm1, prediction_set_aggregated)/10)*10]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <- merge(ic_field_dt[!z %in% training_z],
                     prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'reg1'][,tech := 'UR']
  
  #===================================================================================================================
  # 9) PREDICT WITH REGIONAL REG1 -VR
  
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY AND PERFORMANCE EVALUATION
  testing_set <- ic_field_dt[!z %in% training_z] %>% 
  .[,eonr_pred := ceiling(predict(reg_model_stuff$reg_lm1, . )/10)*10] %>%
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>% 
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[order(mukey, z)]
  
  # predict(reg_model_stuff$model2b_eonr,type="prob")
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'reg1'][,tech := 'VR']
  
  #===================================================================================================================
  # 10) PREDICT WITH REGIONAL REG 2 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  
  prediction_set_aggregated[,eonr_pred := ceiling(predict(reg_model_stuff$reg_lm2, prediction_set_aggregated)/10)*10]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <- merge(ic_field_dt[!z %in% training_z],
                     prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'reg2'][,tech := 'UR']
  
  #===================================================================================================================
  # 11) PREDICT WITH REGIONAL REG 2 -VR
  
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY AND PERFORMANCE EVALUATION
  testing_set <- ic_field_dt[!z %in% training_z] %>% 
  .[,eonr_pred := ceiling(predict(reg_model_stuff$reg_lm2, . )/10)*10] %>%
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>% 
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[order(mukey, z)]
  
  # predict(reg_model_stuff$model2b_eonr,type="prob")
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'reg2'][,tech := 'VR']
  
  #===================================================================================================================
  # 12) PREDICT WITH REGIONAL REG 3 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  
  prediction_set_aggregated[,eonr_pred := ceiling(predict(reg_model_stuff$reg_lm3, prediction_set_aggregated)/10)*10]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <- merge(ic_field_dt[!z %in% training_z],
                     prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'reg3'][,tech := 'UR']
  
  #===================================================================================================================
  # 13) PREDICT WITH REGIONAL  REG 3 -VR
  
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY AND PERFORMANCE EVALUATION
  testing_set <- ic_field_dt[!z %in% training_z] %>% 
  .[,eonr_pred := ceiling(predict(reg_model_stuff$reg_lm3, . )/10)*10] %>%
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>% 
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[order(mukey, z)]
  
  # predict(reg_model_stuff$model2b_eonr,type="prob")
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'reg3'][,tech := 'VR']
  
  #===================================================================================================================
  # 14) PREDICT WITH REGIONAL  REG 4 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  
  prediction_set_aggregated[,eonr_pred := ceiling(predict(reg_model_stuff$reg_lm4, prediction_set_aggregated)/10)*10]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <- merge(ic_field_dt[!z %in% training_z],
                     prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'reg4'][,tech := 'UR']
  
  #===================================================================================================================
  # 15) PREDICT WITH REGIONAL  REG 4 -VR
  
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY AND PERFORMANCE EVALUATION
  testing_set <- ic_field_dt[!z %in% training_z] %>% 
  .[,eonr_pred := ceiling(predict(reg_model_stuff$reg_lm4, . )/10)*10] %>%
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>% 
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[order(mukey, z)]
  
  # predict(reg_model_stuff$model2b_eonr,type="prob")
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'reg4'][,tech := 'VR']
  
  #===================================================================================================================
  # 16) PREDICT WITH REGIONAL REG 4 BY MUKEY - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  names(reg_model_stuff)
  prediction_set_aggregated[,eonr_pred := ceiling((reg_model_stuff$reg_lm4_mukey_dt$n_0_60cm_v5 * n_0_60cm_v5 +
  reg_model_stuff$reg_lm4_mukey_dt$intercept)/10)*10]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <- merge(ic_field_dt[!z %in% training_z],
                     prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'reg4_mukey'][,tech := 'UR']
  
  #===================================================================================================================
  # 17) PREDICT WITH REGIONALREG 4 BY MUKEY -VR
  
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY AND PERFORMANCE EVALUATION
  testing_set <- ic_field_dt[!z %in% training_z] %>% 
  .[,eonr_pred := ceiling((reg_model_stuff$reg_lm4_mukey_dt$n_0_60cm_v5 * n_0_60cm_v5 +
                             reg_model_stuff$reg_lm4_mukey_dt$intercept)/10)*10] %>%
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>% 
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[order(mukey, z)]
  
  # predict(reg_model_stuff$model2b_eonr,type="prob")
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'reg4_mukey'][,tech := 'VR']
  
  #===================================================================================================================
  # 18) PREDICT WITH REGIONAL DTREE 1 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  
  prediction_set_aggregated[,eonr_pred := ceiling(predict(reg_model_stuff$dtree_1, prediction_set_aggregated)/10)*10]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <- merge(ic_field_dt[!z %in% training_z],
                     prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'dtree1'][,tech := 'UR']
  
  #===================================================================================================================
  # 19) PREDICT WITH REGIONAL DTREE 1 -VR
  
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY AND PERFORMANCE EVALUATION
  testing_set <- ic_field_dt[!z %in% training_z] %>% 
  .[,eonr_pred := ceiling(predict(reg_model_stuff$dtree_1, . )/10)*10] %>%
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>% 
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[order(mukey, z)]
  
  # predict(reg_model_stuff$model2b_eonr,type="prob")
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'dtree1'][,tech := 'VR']
  #===================================================================================================================
  # 20) PREDICT WITH REGIONAL DTREE 2 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  
  prediction_set_aggregated[,eonr_pred := ceiling(predict(reg_model_stuff$dtree_2, prediction_set_aggregated)/10)*10]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <- merge(ic_field_dt[!z %in% training_z],
                     prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'dtree2'][,tech := 'UR']
  
  #===================================================================================================================
  # 21) PREDICT WITH REGIONAL DTREE 2 -VR
  
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY AND PERFORMANCE EVALUATION
  testing_set <- ic_field_dt[!z %in% training_z] %>% 
  .[,eonr_pred := ceiling(predict(reg_model_stuff$dtree_2, . )/10)*10] %>%
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>% 
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[order(mukey, z)]
  
  # predict(reg_model_stuff$model2b_eonr,type="prob")
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'dtree2'][,tech := 'VR']
  #===================================================================================================================
  # 22) PREDICT WITH REGIONAL DTREE 3 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  
  prediction_set_aggregated[,eonr_pred := ceiling(predict(reg_model_stuff$dtree_3, prediction_set_aggregated)/10)*10]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <- merge(ic_field_dt[!z %in% training_z],
                     prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'dtree3'][,tech := 'UR']
  
  #===================================================================================================================
  # 23) PREDICT WITH REGIONAL DTREE 3 -VR
  
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY AND PERFORMANCE EVALUATION
  testing_set <- ic_field_dt[!z %in% training_z] %>% 
  .[,eonr_pred := ceiling(predict(reg_model_stuff$dtree_3, . )/10)*10] %>%
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>% 
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[order(mukey, z)]
  
  # predict(reg_model_stuff$model2b_eonr,type="prob")
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'dtree3'][,tech := 'VR']
  
  #===================================================================================================================
  # 24) PREDICT WITH REGIONAL RF2_Q70 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  
  prediction_set_aggregated %>% .[,eonr_pred := predictrf_quantile(model = reg_model_stuff$rf2_eonr, data = ., quant = 0.7)]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <- merge(ic_field_dt[!z %in% training_z],
                     prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'rf2_q70'][,tech := 'UR']
  
  #===================================================================================================================
  # 25) PREDICT WITH REGIONAL RF2_Q70 -VR
  
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY AND PERFORMANCE EVALUATION
  testing_set <- ic_field_dt[!z %in% training_z] %>% 
  .[,eonr_pred := predictrf_quantile(model = reg_model_stuff$rf2_eonr, data = ., quant = 0.7)] %>%
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>% 
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[order(mukey, z)]
  
  # predict(reg_model_stuff$model2b_eonr,type="prob")
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'rf2_q70'][,tech := 'VR']
  
  #===================================================================================================================
  # 26) PREDICT WITH REGIONAL RF2_Q80- UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  
  prediction_set_aggregated %>% .[,eonr_pred := predictrf_quantile(model = reg_model_stuff$rf2_eonr, data = ., quant = 0.80)]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <- merge(ic_field_dt[!z %in% training_z],
                     prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'rf2_q80'][,tech := 'UR']
  
  #===================================================================================================================
  # 27) PREDICT WITH REGIONAL RF2_Q80-VR
  
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY AND PERFORMANCE EVALUATION
  testing_set <- ic_field_dt[!z %in% training_z] %>% 
  .[,eonr_pred := predictrf_quantile(model = reg_model_stuff$rf2_eonr, data = ., quant = 0.80)] %>%
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>% 
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[order(mukey, z)]
  
  # predict(reg_model_stuff$model2b_eonr,type="prob")
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'rf2_q80'][,tech := 'VR']
  #===================================================================================================================
  # 26b) PREDICT WITH REGIONAL RF2_Q80- UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  
  prediction_set_aggregated %>% .[,eonr_pred := predictrf_quantile(model = reg_model_stuff$rf2_eonr, data = ., quant = 0.90)]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <- merge(ic_field_dt[!z %in% training_z],
                     prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'rf2_q90'][,tech := 'UR']
  
  #===================================================================================================================
  # 27b) PREDICT WITH REGIONAL RF2_Q80-VR
  
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY AND PERFORMANCE EVALUATION
  testing_set <- ic_field_dt[!z %in% training_z] %>% 
  .[,eonr_pred := predictrf_quantile(model = reg_model_stuff$rf2_eonr, data = ., quant = 0.90)] %>%
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>% 
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[order(mukey, z)]
  
  # predict(reg_model_stuff$model2b_eonr,type="prob")
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'rf2_q90'][,tech := 'VR']
  
  #===================================================================================================================
  # 27c) PREDICT WITH REGIONAL RF1_Q70 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  
  prediction_set_aggregated %>% .[,eonr_pred := predictrf_quantile(model = reg_model_stuff$rf1_eonr, data = ., quant = 0.7)]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <- merge(ic_field_dt[!z %in% training_z],
                     prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'rf1_q70'][,tech := 'UR']
  
  #===================================================================================================================
  # 27d) PREDICT WITH REGIONAL RF1_Q70 -VR
  
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY AND PERFORMANCE EVALUATION
  testing_set <- ic_field_dt[!z %in% training_z] %>% 
  .[,eonr_pred := predictrf_quantile(model = reg_model_stuff$rf1_eonr, data = ., quant = 0.7)] %>%
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>% 
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[order(mukey, z)]
  
  # predict(reg_model_stuff$model2b_eonr,type="prob")
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'rf1_q70'][,tech := 'VR']
  
  #===================================================================================================================
  # 27e) PREDICT WITH REGIONAL RF3_Q70 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  
  prediction_set_aggregated %>% .[,eonr_pred := predictrf_quantile(model = reg_model_stuff$rf3_eonr, data = ., quant = 0.7)]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <- merge(ic_field_dt[!z %in% training_z],
                     prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'rf3_q70'][,tech := 'UR']
  
  #===================================================================================================================
  # 27f) PREDICT WITH REGIONAL RF2_Q70 -VR
  
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY AND PERFORMANCE EVALUATION
  testing_set <- ic_field_dt[!z %in% training_z] %>% 
  .[,eonr_pred := predictrf_quantile(model = reg_model_stuff$rf3_eonr, data = ., quant = 0.7)] %>%
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>% 
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[order(mukey, z)]
  
  # predict(reg_model_stuff$model2b_eonr,type="prob")
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'rf3_q70'][,tech := 'VR']
  
  #===================================================================================================================
  # 27g) PREDICT WITH REGIONAL RF1_Q80 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  
  prediction_set_aggregated %>% .[,eonr_pred := predictrf_quantile(model = reg_model_stuff$rf1_eonr, 
                                                                 data = ., quant = 0.8)]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <- merge(ic_field_dt[!z %in% training_z],
                     prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'rf1_q80'][,tech := 'UR']
  
  #===================================================================================================================
  # 27h) PREDICT WITH REGIONAL RF1_Q80 -VR
  
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY AND PERFORMANCE EVALUATION
  testing_set <- ic_field_dt[!z %in% training_z] %>% 
  .[,eonr_pred := predictrf_quantile(model = reg_model_stuff$rf1_eonr, data = ., quant = 0.8)] %>%
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>% 
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[order(mukey, z)]
  
  # predict(reg_model_stuff$model2b_eonr,type="prob")
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'rf1_q80'][,tech := 'VR']
  
  #===================================================================================================================
  # 27i) PREDICT WITH REGIONAL RF3_Q70 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  
  prediction_set_aggregated %>% .[,eonr_pred := predictrf_quantile(model = reg_model_stuff$rf3_eonr, 
                                                                 data = ., quant = 0.8)]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <- merge(ic_field_dt[!z %in% training_z],
                     prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'rf3_q80'][,tech := 'UR']
  
  #===================================================================================================================
  # 27f) PREDICT WITH REGIONAL RF2_Q70 -VR
  
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY AND PERFORMANCE EVALUATION
  testing_set <- ic_field_dt[!z %in% training_z] %>% 
  .[,eonr_pred := predictrf_quantile(model = reg_model_stuff$rf3_eonr, data = ., quant = 0.8)] %>%
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>% 
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[order(mukey, z)]
  
  # predict(reg_model_stuff$model2b_eonr,type="prob")
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'rf3_q80'][,tech := 'VR']
  
  #===================================================================================================================
  # 28) PREDICT WITH REGIONAL xgboost_2- UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  
  prediction_set_aggregated[,eonr_pred := ceiling(predict(reg_model_stuff$xgboost_2, prediction_set_aggregated)/10)*10]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <- merge(ic_field_dt[!z %in% training_z],
                     prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'xgboost_2'][,tech := 'UR']
  
  #===================================================================================================================
  # 29) PREDICT WITH REGIONAL xgboost_2-VR
  
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY AND PERFORMANCE EVALUATION
  testing_set <- ic_field_dt[!z %in% training_z] %>% 
  .[,eonr_pred := ceiling(predict(reg_model_stuff$xgboost_2, . )/10)*10] %>%
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>% 
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[order(mukey, z)]
  
  
  # predict(reg_model_stuff$model2b_eonr,type="prob")
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'xgboost_2'][,tech := 'VR']
  #===================================================================================================================
  # 30) PREDICT WITH REGIONAL XGB_1- UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  
  prediction_set_aggregated[,eonr_pred := ceiling(
  mlr::getPredictionResponse(
  predict(reg_model_stuff$xgb_1, newdata = data.frame(prediction_set_aggregated[, c(no_cost_varb_trf), with = FALSE])))
                                                        /10)*10]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <- merge(ic_field_dt[!z %in% training_z],
                     prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'xgb_1'][,tech := 'UR']
  
  #===================================================================================================================
  # 31) PREDICT WITH REGIONAL XGB_1-VR
  
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY AND PERFORMANCE EVALUATION
  testing_set <- ic_field_dt[!z %in% training_z]
  preds <- predict(reg_model_stuff$xgb_1, newdata =  data.frame(testing_set[, c(no_cost_varb_trf), with = FALSE]))
  testing_set[,eonr_pred := ceiling(preds$data$response/10)*10]
  testing_set[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))]
  testing_set <- testing_set[N_fert == eonr_pred] %>% 
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[order(mukey, z)]
  
  # predict(reg_model_stuff$model2b_eonr,type="prob")
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'xgb_1'][,tech := 'VR']
  
  #===================================================================================================================
  # 32) PREDICT WITH REGIONAL XGB_2 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  
  prediction_set_aggregated[,eonr_pred := ceiling(
  mlr::getPredictionResponse(
    predict(reg_model_stuff$xgb_2, newdata = data.frame(prediction_set_aggregated[, c(no_cost_varb_trf, ss_var), with = FALSE])))
  /10)*10] 
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <- merge(ic_field_dt[!z %in% training_z],
                     prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
  
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'xgb_2'][,tech := 'UR']
  
  #===================================================================================================================
  # 33) PREDICT WITH REGIONAL XGB_2 -VR
  
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY AND PERFORMANCE EVALUATION
  testing_set <- ic_field_dt[!z %in% training_z]
  preds <- predict(reg_model_stuff$xgb_2, 
                 newdata = data.frame(testing_set[, c(no_cost_varb_trf, ss_var), with = FALSE]))
  testing_set[,eonr_pred := ceiling(preds$data$response/10)*10]
  testing_set[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))]
  testing_set <- testing_set[N_fert == eonr_pred] %>% 
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[order(mukey, z)]
  
  
  # predict(reg_model_stuff$model2b_eonr,type="prob")
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'xgb_2'][,tech := 'VR']
  
  #===================================================================================================================
  # 34) PREDICT WITH REGIONAL XGB_3 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  
  prediction_set_aggregated[,eonr_pred := ceiling(
  mlr::getPredictionResponse(
    predict(reg_model_stuff$xgb_3, newdata = data.frame(prediction_set_aggregated[, c(no_cost_varb_trf, crop_varb), with = FALSE])))
  /10)*10] 
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <- merge(ic_field_dt[!z %in% training_z],
                     prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'xgb_3'][,tech := 'UR']
  
  #===================================================================================================================
  # 35) PREDICT WITH REGIONAL XGB_3 -VR
  
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY AND PERFORMANCE EVALUATION
  testing_set <- ic_field_dt[!z %in% training_z]
  preds <- predict(reg_model_stuff$xgb_3, newdata =  data.frame(testing_set[, c(no_cost_varb_trf, crop_varb), with = FALSE]))
  testing_set[,eonr_pred := ceiling(preds$data$response/10)*10]
  testing_set[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))]
  testing_set <- testing_set[N_fert == eonr_pred] %>% 
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[order(mukey, z)]
  
  
  # predict(reg_model_stuff$model2b_eonr,type="prob")
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'xgb_3'][,tech := 'VR']
  
  #===================================================================================================================
  #===================================      LOCAL MODEL       ========================================================
  #===================================================================================================================
  # 6) PREDICT WITH LOCAL MINIMUM MODEL UR
  no_cost_var_local <-  c(no_cost_varb_trf[!no_cost_varb_trf %in% c("long", "lat", "Yld_lt_avg", "Yld_lt_min", "Yld_lt_max", "region.2", "region.3" )]) #this is captured by the mukey
  ss_var_local <-  ss_var[!ss_var %in% c("dul_dep", "ll15_dep","whc")] #this is captured by the mukey
  crop_varb
  
  training_set_yield <- ic_field_dt[z %in% training_z, c('mukey','area_ha', 'z','N_fert', 'Yld', 'P', no_cost_var_local, ss_var_local), with = FALSE] #this is the first time we are training in this field
  table(training_set_yield$z)
  training_set_yield_aggregated  <- aggregate_by_area(data_dt = training_set_yield, variables = c('P', 'Yld'), weight = 'area_ha', by_c = c('N_fert','z'))
  table(training_set_yield_aggregated$z)
  
  training_set_yield_aggregated[, Yld_response := max(Yld) - min(Yld), by = .(z)]
  training_set_yield_aggregated <- training_set_yield_aggregated[Yld_response > 500]
  quadratic_dt <- training_set_yield_aggregated[,list(intercept=coef(lm(Yld~N_fert + I(N_fert^2)))[1], 
                                coef1=coef(lm(Yld ~ N_fert + I(N_fert^2)))[2],
                                coef2=coef(lm(Yld ~ N_fert + I(N_fert^2)))[3]),by=.(z)]
  N_rates_trial <- seq(10,330,10)
  N_rates_int <- seq(min(N_rates_trial),max(N_rates_trial), by = 10)
  quadratic_dt2 <- quadratic_dt[rep(x = 1:nrow(quadratic_dt), each = length(N_rates_int))]
  
  quadratic_dt2[,N_fert := rep(N_rates_int, nrow(quadratic_dt))]
  quadratic_dt2[,Yld := intercept + coef1 * N_fert + coef2 * (N_fert^2)]
  quadratic_dt2[,P:= Yld * Pc - N_fert * Pn]
  
  #Average all curves
  quadratic_dt3 <- quadratic_dt2[,.(P_avg = mean(P)), by = .(N_fert)]
  ggplot(quadratic_dt3) + geom_point(aes(x = N_fert, y = P_avg))
  
  #Select EONR
  model_minimum_local <- quadratic_dt3 %>%
  .[P_avg == max( P_avg)] %>% .[,.(eonr_pred = N_fert)]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <- cbind(ic_field_dt[!z %in% training_z],
                         model_minimum_local) %>% #here we join back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P')]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := '6'][,tech := 'UR']
  
  #===================================================================================================================
  # 7) ONE BEST RATE
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  # Need to train with aggregated data. We will aggregate the data and select the best N rate for the whole field
  training_set_yield_aggregated  <- aggregate_by_area(data_dt = training_set_yield, variables = c('P', 'Yld'), weight = 'area_ha', by_c = c('N_fert'))
  
  local_best_rate_model <- training_set_yield_aggregated[, .SD[ P == max( P)]] %>%
  setnames('N_fert', 'eonr_pred') %>%
  .[1, c('eonr_pred'), with = FALSE]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  #Predictions were made for the aggregated data, eates are obtained and testing is evaluated with data not aggregated
  # PERFORMANCE EVALUATION
  testing_set <- cbind(ic_field_dt[!z %in% training_z],
                     local_best_rate_model) %>% #here we join back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P')]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'Local_onebestrate'][,tech := 'UR']
  
  
  #===================================================================================================================
  # 8) PREDICT WITH LOCAL REG4 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  # Need to train with aggregated data. We will aggregate the data and select the best N rate for the whole field
  do_not_aggregate = c("prev_crop")
  do_aggregate = c('P', setdiff(c(no_cost_var_local, ss_var_local), do_not_aggregate))
  table(training_set_yield$z)
  training_set_eonr_aggregated <- aggregate_by_area(data_dt = training_set_yield, 
                                                  variables = do_aggregate, 
                                                  weight = 'area_ha', by_c = c('z', 'N_fert')) %>% 
  .[, .SD[ P == max( P)], by = .(z)] %>%
  setnames('N_fert', 'eonr') %>%
  .[, c('eonr', no_cost_var_local, ss_var_local), with = FALSE]
  
  #now we train the model
  reg4_local_ur <- lm(eonr ~ n_0_60cm_v5, data = training_set_eonr_aggregated[,c('eonr' ,'n_0_60cm_v5'), with = FALSE])
  # varImpPlot(model1_local_ur, type=2)
  # plot(model1_local_ur)
  
  #Get the predictions for the prediction set aggregated
  prediction_set_aggregated[,eonr_pred := ceiling(predict(reg4_local_ur, prediction_set_aggregated)/10)*10]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  #Predictions were made for the aggregated data, eates are obtained and testing is evaluated with data not aggregated
  testing_set <- merge(ic_field_dt[!z %in% training_z],
                     prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'reg4_local'][,tech := 'UR']
  
  
  
  #===================================================================================================================
  # 9) PREDICT WITH LOCAL RF 1 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  # Need to train with aggregated data. We will aggregate the data and select the best N rate for the whole field
  #now we train the model
  rf1_local_ur <- randomForest(eonr ~ ., data = training_set_eonr_aggregated[,c('eonr' ,no_cost_var_local), with = FALSE], importance = TRUE, 
                             ntree=50)
  # varImpPlot(rf1_local_ur, type=2)
  # plot(model1_local_ur)
  
  #Get the predictions for the prediction set aggregated
  prediction_set_aggregated[,eonr_pred := ceiling(predict(rf1_local_ur, prediction_set_aggregated)/10)*10]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  #Predictions were made for the aggregated data, eates are obtained and testing is evaluated with data not aggregated
  testing_set <- merge(ic_field_dt[!z %in% training_z],
                         prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := '7'][,tech := 'UR']
  
  #===================================================================================================================
  # 8) PREDICT WITH LOCAL RF 1 - VR 
  
  training_set_eonr <- training_set_yield[, .SD[ P == max( P)], by = .(mukey, z)] %>%
  setnames('N_fert', 'eonr') %>%
  .[, c('mukey','eonr', no_cost_var_local, ss_var_local), with = FALSE]
  
  sapply(training_set_eonr, class)
  # Create a Random Forest model with default parameters
  rf1_local_vr <- randomForest(eonr ~ ., data = training_set_eonr[,c('eonr', 'mukey' ,no_cost_var_local), with = FALSE], importance = TRUE, 
                             ntree=50)
  # varImpPlot(rf1_local_vr, type=2)
  # plot(model1_local_vr)
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <- ic_field_dt[!z %in% training_z] %>% 
  .[,eonr_pred := ceiling(predict(rf1_local_vr, .)/10)*10] %>%
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>% 
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[order(mukey, z)]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := '8'][,tech := 'VR']
  
  #===================================================================================================================
  # 9) PREDICT WITH LOCAL MODEL 2 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  # Need to train with aggregated data
  
  rf2_local_ur <- randomForest(eonr ~ ., data = training_set_eonr_aggregated[,c('eonr' ,no_cost_var_local, ss_var_local), with = FALSE], importance = TRUE, 
                                ntree=50)
  
  # plot(rf2_local_ur)
  # varImpPlot(rf2_local_ur, type=2)
  prediction_set_aggregated[,eonr_pred := ceiling(predict(rf2_local_ur, prediction_set_aggregated)/10)*10]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <- merge(ic_field_dt[!z %in% training_z],
                         prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := '9'][,tech := 'UR']
  #===================================================================================================================
  # 10) PREDICT WITH LOCAL RF 2 - VR 
  
  # Create a Random Forest model with default parameters
  rf2_local_vr <- randomForest(eonr ~ ., data = training_set_eonr[,c('eonr', 'mukey' ,no_cost_var_local, ss_var_local), with = FALSE], importance = TRUE, 
                                ntree=50)
  # varImpPlot(rf2_local_vr, type=2)
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <- ic_field_dt[!z %in% training_z] %>% 
  .[,eonr_pred := ceiling(predict(rf2_local_vr, .)/10)*10] %>%
  .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
  .[N_fert == eonr_pred] %>% 
  .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[order(mukey, z)]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := '10'][,tech := 'VR']
  #===================================================================================================================
  # 11) EX POST UR - Best rate at the field level
  
  # Need to get EONR with aggregated data
  prediction_set_aggregated_profits <- aggregate_by_area(data_dt = ic_field_dt[!z %in% training_z], 
                                                 variables = c('P'), 
                                                 weight = 'area_ha', by_c = c('N_fert','z'))  %>%
    .[, .SD[ P == max( P)], by = .(z)] %>% .[,-c('area_ha', 'P')] %>% setnames('N_fert', 'eonr_pred')
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <- merge(ic_field_dt[!z %in% training_z],
                       prediction_set_aggregated_profits[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    .[N_fert == eonr_pred] %>%
    .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := '11'][,tech := 'UR']
  #===================================================================================================================
  # 12) EX POST VR
  
  # PERFORMANCE EVALUATION
  testing_set <- ic_field_dt[!z %in% training_z] %>% 
    .[, .SD[ P == max( P)], by = .(id_10, mukey, z)] %>%
    .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- testing_set[, model := '12'][,tech := 'VR']
  #===================================================================================================================
  
  
  
  
  
  
  
  
  
  #===================================================================================================================
  #===================================      LOCAL MODEL  COMBINED     ========================================================
  #===================================================================================================================
  # 6_comb) PREDICT WITH LOCAL MINIMUM MODEL UR
  no_cost_var_local <-  c('id_field',no_cost_varb_trf[!no_cost_varb_trf %in% c("long", "lat", "Yld_lt_avg", "Yld_lt_min", "Yld_lt_max", "region.2", "region.3" )]) #this is captured by the mukey
  ss_var_local <-  ss_var[!ss_var %in% c("dul_dep", "ll15_dep","whc")] #this is captured by the mukey
  crop_varb
  
  training_set_yield <- ic_field_dt_both[z %in% training_z, c('id_field','mukey','area_ha', 'z','N_fert', 'Yld', 'P', no_cost_var_local, ss_var_local), with = FALSE] #this is the first time we are training in this field
  table(training_set_yield$z)
  training_set_yield_aggregated  <- aggregate_by_area(data_dt = training_set_yield, variables = c('P', 'Yld'), weight = 'area_ha', by_c = c('id_field','N_fert','z'))
  table(training_set_yield_aggregated$z)
  
  training_set_yield_aggregated[, Yld_response := max(Yld) - min(Yld), by = .(z)]
  training_set_yield_aggregated <- training_set_yield_aggregated[Yld_response > 500]
  quadratic_dt <- training_set_yield_aggregated[,list(intercept=coef(lm(Yld~N_fert + I(N_fert^2)))[1], 
                                                      coef1=coef(lm(Yld ~ N_fert + I(N_fert^2)))[2],
                                                      coef2=coef(lm(Yld ~ N_fert + I(N_fert^2)))[3]),by=.(id_field, z)]
  N_rates_trial <- seq(10,330,10)
  N_rates_int <- seq(min(N_rates_trial),max(N_rates_trial), by = 10)
  quadratic_dt2 <- quadratic_dt[rep(x = 1:nrow(quadratic_dt), each = length(N_rates_int))]
  
  quadratic_dt2[,N_fert := rep(N_rates_int, nrow(quadratic_dt))]
  quadratic_dt2[,Yld := intercept + coef1 * N_fert + coef2 * (N_fert^2)]
  quadratic_dt2[,P:= Yld * Pc - N_fert * Pn]
  
  #Average all curves
  quadratic_dt3 <- quadratic_dt2[,.(P_avg = mean(P)), by = .(N_fert)]
  ggplot(quadratic_dt3) + geom_point(aes(x = N_fert, y = P_avg))
  
  #Select EONR
  model_minimum_local <- quadratic_dt3 %>%
    .[P_avg == max( P_avg)] %>% .[,.(eonr_pred = N_fert)]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <- cbind(ic_field_dt[!z %in% training_z],
                       model_minimum_local) %>% #here we join back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
    .[N_fert == eonr_pred] %>%
    .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P')]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := '6_comb'][,tech := 'UR']
  
  #===================================================================================================================
  # 7_comb) ONE BEST RATE
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  # Need to train with aggregated data. We will aggregate the data and select the best N rate for the whole field
  training_set_yield_aggregated  <- aggregate_by_area(data_dt = training_set_yield, variables = c('P', 'Yld'), weight = 'area_ha', by_c = c('N_fert'))
  
  local_best_rate_model <- training_set_yield_aggregated[, .SD[ P == max( P)]] %>%
    setnames('N_fert', 'eonr_pred') %>%
    .[1, c('eonr_pred'), with = FALSE]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  #Predictions were made for the aggregated data, eates are obtained and testing is evaluated with data not aggregated
  # PERFORMANCE EVALUATION
  testing_set <- cbind(ic_field_dt[!z %in% training_z],
                       local_best_rate_model) %>% #here we join back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
    .[N_fert == eonr_pred] %>%
    .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P')]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'Local_onebestrate_comb'][,tech := 'UR']
  
  
  #===================================================================================================================
  # 8_comb) PREDICT WITH LOCAL REG4 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  # Need to train with aggregated data. We will aggregate the data and select the best N rate for the whole field
  # do_not_aggregate = c("prev_crop")
  do_aggregate = c('P', no_cost_var_local, ss_var_local)
  table(training_set_yield$z)
  training_set_eonr_aggregated <- aggregate_by_area(data_dt = training_set_yield, 
                                                    variables = do_aggregate, 
                                                    weight = 'area_ha', by_c = c('id_field','z', 'N_fert')) %>% 
    .[, .SD[ P == max( P)], by = .(id_field, z)] %>%
    setnames('N_fert', 'eonr') %>%
    .[, c('eonr', no_cost_var_local, ss_var_local), with = FALSE]
  
  #now we train the model
  reg4_local_ur <- lm(eonr ~ n_0_60cm_v5, 
                      data = training_set_eonr_aggregated[,c('eonr' ,'n_0_60cm_v5'), with = FALSE])
  # varImpPlot(model1_local_ur, type=2)
  # plot(model1_local_ur)
  
  #Get the predictions for the prediction set aggregated
  prediction_set_aggregated[,eonr_pred := ceiling(predict(reg4_local_ur, prediction_set_aggregated)/10)*10]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  #Predictions were made for the aggregated data, eates are obtained and testing is evaluated with data not aggregated
  testing_set <- merge(ic_field_dt[!z %in% training_z],
                       prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
    .[N_fert == eonr_pred] %>%
    .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := 'reg4_local_comb'][,tech := 'UR']
  
  #===================================================================================================================
  # 9_comb) PREDICT WITH LOCAL RF 1 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  # Need to train with aggregated data. We will aggregate the data and select the best N rate for the whole field
  #now we train the model
  rf1_local_ur <- randomForest(eonr ~ ., data = training_set_eonr_aggregated[,c('eonr', no_cost_var_local), with = FALSE], importance = TRUE, 
                               ntree=50)
  # varImpPlot(model1_local_ur, type=2)
  # plot(model1_local_ur)
  
  #Get the predictions for the prediction set aggregated
  prediction_set_aggregated[,id_field := field_n]
  prediction_set_aggregated[,eonr_pred := ceiling(predict(rf1_local_ur, prediction_set_aggregated)/10)*10]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  #Predictions were made for the aggregated data, eates are obtained and testing is evaluated with data not aggregated
  testing_set <- merge(ic_field_dt[!z %in% training_z],
                       prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
    .[N_fert == eonr_pred] %>%
    .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := '7_comb'][,tech := 'UR']
  
  #===================================================================================================================
  # 8_comb) PREDICT WITH LOCAL RF 1 - VR 
  
  training_set_eonr <- training_set_yield[, .SD[ P == max( P)], by = .(id_field, mukey, z)] %>%
    setnames('N_fert', 'eonr') %>%
    .[, c('mukey','eonr', no_cost_var_local, ss_var_local), with = FALSE]
  
  sapply(training_set_eonr, class)
  # Create a Random Forest model with default parameters
  rf1_local_vr <- randomForest(eonr ~ ., data = training_set_eonr[,c('eonr', 'mukey' ,no_cost_var_local), with = FALSE], importance = TRUE, 
                               ntree=50)
  # varImpPlot(model1_local_vr, type=2)
  # plot(model1_local_vr)
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <- ic_field_dt[!z %in% training_z] %>% 
    .[,eonr_pred := ceiling(predict(rf1_local_vr, .)/10)*10] %>%
    .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
    .[N_fert == eonr_pred] %>% 
    .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[order(mukey, z)]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := '8_comb'][,tech := 'VR']
  
  #===================================================================================================================
  # 9_comb) PREDICT WITH LOCAL MODEL 2 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  # Need to train with aggregated data
  
  rf2_local_ur <- randomForest(eonr ~ ., data = training_set_eonr_aggregated[,c('eonr' ,no_cost_var_local, ss_var_local), with = FALSE], importance = TRUE, 
                               ntree=50)
  
  # plot(rf2_local_ur)
  # varImpPlot(rf2_local_ur, type=2)
  prediction_set_aggregated[,eonr_pred := ceiling(predict(rf2_local_ur, prediction_set_aggregated)/10)*10]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <- merge(ic_field_dt[!z %in% training_z],
                       prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
    .[N_fert == eonr_pred] %>%
    .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := '9_comb'][,tech := 'UR']
  #===================================================================================================================
  # 10_comb) PREDICT WITH LOCAL RF 2 - VR 
  
  # Create a Random Forest model with default parameters
  rf2_local_vr <- randomForest(eonr ~ ., data = training_set_eonr[,c('eonr', 'mukey' ,no_cost_var_local, ss_var_local), with = FALSE], importance = TRUE, 
                               ntree=50)
  # varImpPlot(rf2_local_vr, type=2)
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <- ic_field_dt[!z %in% training_z] %>% 
    .[,eonr_pred := ceiling(predict(rf2_local_vr, .)/10)*10] %>%
    .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
    .[N_fert == eonr_pred] %>% 
    .[,c("mukey", "z", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[order(mukey, z)]
  
  small_list[[length(small_list)+1]] <- testing_set[,model := '10_comb'][,tech := 'VR']
  
  
  
  
  
  #===================================================================================================================
  testing_set_dt <- rbindlist(small_list)
  testing_set_dt
  # testing_set_dt[,model := as.numeric(model)]
  testing_set_dt[z == 12,.N, .(z, model)]
  testing_set_dt[, .(Yld =  mean(Yld),
                    leach_n2 = mean(leach_n2),
                    N_fert = mean(N_fert),
                    P = mean(P),
                    area_ha = sum(area_ha)), by = .( model, tech)][order(-P)]
  
  testing_set_dt <- cbind(field_info[1,.(id_10, id_field, region)], testing_set_dt[,-'id_10'])
  testing_set_dt[,.N, by = .(model, tech)]
  
  #===================================================================================================================
  #PLOT ME
  
  if(FALSE){
  
  mukey_n <- field_soils_dt[area_ha == max(area_ha)]
  mukey_n <- mukey_n$mukey[1]
  testing_set_plot <- testing_set_dt[mukey == mukey_n ]
  testing_set_plot[,method := factor(model, levels= c('1', '2', '3','4', '5', '6', '7', '8', '9', '10', '11', '12'))]
  ic_field_plot <- ic_field_dt[!z %in% training_z & mukey == mukey_n ]
  
  # library(RColorBrewer)
  # n <- 12
  # qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  # col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  # colors_sample =sample(col_vector, n)
  # pie(rep(1,n), colors_sample)
  
  colors_sample=c( "#7570B3", "#FFED6F", "#666666", "#7FC97F", "#386CB0", "#B3B3B3", "#FFFFCC", "#A65628", "#F4CAE4", "#E41A1C", "#E6AB02", "black")
  
  # Y plot with Yld at eonr
  z_labels <- ic_field_plot[N_fert == max(ic_field_plot$N_fert), .(N_fert, Yld, z)][order(-Yld)]
  z_labels[seq(1, nrow(z_labels), by = 2), N_fert := N_fert - 20]
  
  (plot_n1 <- ggplot() +
      geom_point(data = testing_set_plot, aes(x = N_fert, y = Yld, colour = method, size = method)) +
      geom_line(data = ic_field_plot, aes(x = N_fert, y = Yld, group=z), show.legend = FALSE) +
      scale_size_manual(values=c(rep(2, 11), 4)) +
      scale_color_manual(values=colors_sample)+
      ylab('Yield (kg/ha)')+
      xlab('N rate (kg/ha)')+
      geom_text(data = z_labels, aes(x = N_fert, y = Yld, label = z))+
      theme_bw()+
      theme(panel.grid = element_blank()))
  
  # Y plot with Yld at eonr
  z_labels <- ic_field_plot[N_fert == max(ic_field_plot$N_fert), .(N_fert, Yld, z, leach_n2)][order(-leach_n2)]
  z_labels[seq(1, nrow(z_labels), by = 2), N_fert := N_fert - 20]
  
  (plot_n2 <- ggplot() +
      geom_point(data = testing_set_plot, aes(x = N_fert, y = leach_n2, colour = method, size = method)) +
      geom_line(data = ic_field_plot, aes(x = N_fert, y = leach_n2, group=z), show.legend = FALSE) +
      scale_size_manual(values=c(rep(2, 11), 4)) +
      scale_color_manual(values=colors_sample)+
      ylab('N Leaching (kg/ha)')+
      xlab('N rate (kg/ha)')+
      geom_text(data = z_labels, aes(x = N_fert, y = leach_n2, label = z))+
      theme_bw()+
      theme(panel.grid = element_blank()))
  
  
  ggsave(grid.arrange(plot_n1, plot_n2), filename = "./vr_value_v2/Data/figures/yield_curve_example.jpg")
  
  # Y plot with Yld at eonr
  (plot_n1 <- ggplot() +
      geom_point(data = testing_set_plot[prev_crop == 1 & model != 12], aes(x = N_fert, y = Yld, colour = model)) +
      geom_point(data = testing_set_plot[prev_crop == 1 & model == 12], aes(x = N_fert, y = Yld), colour = 'black', size = 3, show.legend = FALSE) +
      geom_line(data = ic_field_plot[mukey == mukey_n & prev_crop == 1], aes(x = N_fert, y = Yld, group=interaction(z)), show.legend = FALSE) +
      # ggtitle(paste('Yld plot with Yld at eonr', mukey_n)))
      ggtitle('Yield'))
  
  ic_field_dt[mukey == mukey_n & prev_crop == 0][order(P)]
  ggsave(plot_n1, filename = "./vr_value_v2/Data/figures/yield_curve_example.jpg")
  
  
  # P plot with P at eonr
  (plot_n <- ggplot() +
      geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 1 & model != 12], aes(x = N_fert, y = P, colour = model)) +
      geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 1 & model == 12], aes(x = N_fert, y = P), size = 3, show.legend = FALSE) +
      geom_line(data = ic_field_plot[mukey == mukey_n & prev_crop == 1], aes(x = N_fert, y = P, group=interaction(z)), show.legend = FALSE) +
      ggtitle(paste('P plot with P at eonr', mukey_n)))
  
  ic_field_dt[mukey == mukey_n & prev_crop == 0][order(P)]
  ggsave(plot_n, filename = "./vr_value_v2/Data/figures/yield_curve_example.jpg")
  
  # leach_n2o3 plot with leaching at eonr
  (plot_n2 <- ggplot() +
      geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 0 & model != 12], aes(x = N_fert, y = leach_n2, colour = model)) +
      geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 0 & model == 12], aes(x = N_fert, y = leach_n2), size = 3, show.legend = FALSE) +
      geom_line(data = ic_field_plot[mukey == mukey_n & prev_crop == 0], aes(x = N_fert, y = leach_n2, group=interaction(z)), show.legend = FALSE) +
      # ggtitle(paste('leach_n2o3 plot with leaching at eonr', mukey_n)))
      ggtitle('N Leaching'))
  
  ggsave(plot_n2, filename = "./vr_value_v2/Data/figures/leaching_curve_example.jpg")
  #---------------------------------------------------------------------------
  # ESL510
  (plot_n1 <- ggplot() +
      geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 1 & model != 12], aes(x = N_fert, y = Yld), size = 0.2) +
      geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 1 & model == 12], aes(x = N_fert, y = Yld), size = 3, show.legend = FALSE) +
      geom_line(data = ic_field_plot[mukey == mukey_n & prev_crop == 1], aes(x = N_fert, y = Yld, colour=z)) +
      # ggtitle(paste('Yld plot with Yld at eonr', mukey_n)))
      ggtitle('Yield'))
  
  (plot_n2 <- ggplot() +
      geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 1 & model != 12], aes(x = N_fert, y = leach_n2), size = 0.2) +
      geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 1 & model == 12], aes(x = N_fert, y = leach_n2), size = 3, show.legend = FALSE) +
      geom_line(data = ic_field_plot[mukey == mukey_n & prev_crop == 1], aes(x = N_fert, y = leach_n2, colour=z)) +
      # ggtitle(paste('Yld plot with Yld at eonr', mukey_n)))
      ggtitle('N Leaching'))
  
  grid.arrange(plot_n1, plot_n2)
  
  #---------------------------------------------------------------------------
  #NOW WITH N_TOTAL (FERT + SOIL)
  ic_field_plot[,N_total := N_fert + n_deep_v5]
  testing_set_plot[,N_total := N_fert + n_deep_v5]
  
  # P plot with P at eonr
  (plot_n <- ggplot() +
      geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 0 & model != 12], aes(x = N_total, y = P, colour = model)) +
      geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 0 & model == 12], aes(x = N_total, y = P), size = 3, show.legend = FALSE) +
      geom_line(data = ic_field_plot[mukey == mukey_n & prev_crop == 0], aes(x = N_total, y = P, group=interaction(z)), show.legend = FALSE) +
      ggtitle(paste('P plot with P at eonr', mukey_n)))
  
  ic_field_dt[mukey == mukey_n & prev_crop == 0][order(P)]
  ggsave(plot_n, filename = "./vr_value_v2/Data/figures/yield_curve_example_ntotal.jpg")
  
  # leach_n2o3 plot with leaching at eonr
  (plot_n <- ggplot() +
      geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 0 & model != 12], aes(x = N_total, y = leach_n2, colour = model)) +
      geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 0 & model == 12], aes(x = N_total, y = leach_n2), size = 3, show.legend = FALSE) +
      geom_line(data = ic_field_plot[mukey == mukey_n & prev_crop == 0], aes(x = N_total, y = leach_n2, group=interaction(z)), show.legend = FALSE) +
      ggtitle(paste('leach_n2o3 plot with leaching at eonr', mukey_n)))
  
  ggsave(plot_n, filename = "./vr_value_v2/Data/figures/leaching_curve_example_ntotal.jpg")
  }#end of PLOT ME

  return(testing_set_dt)
}

# library('foreach')

time1 <- Sys.time()

fields_seq <- 1:nrow(fields_list_dt)

# fields_seq <- sample(1:nrow(fields_list_dt), 10)
# process_field_economics(20)

big_list <- list()
for(j in fields_seq){
  big_list[[length(big_list)+1]] <- process_field_economics(j)
  }

time2 <- Sys.time()

nopar <- time2 - time1

# nopar/100*nrow(fields_list_dt)/60

perfomances_dt <- rbindlist(big_list)

perfomances_dt[,.N, .(id_10, mukey,id_field)] %>% .[,.N, .(id_10, id_field)] %>% .[,.N, .(id_10)] %>% .[,N] %>% table()

# filter_this <- perfomances_dt[,.N, .(id_10, mukey, z, id_field, model, tech)][1]
# 
# filter_dt_in_dt(x_dt = perfomances_dt, filter_dt = filter_this, return_table = T)
# 
# perfomances_dt2 <- unique(perfomances_dt)

perfomances_dt2 <- merge(perfomances_dt, 
                         perfomances_dt[model == '12', .(id_10, mukey, z, id_field, N_fert_12 = N_fert)], by = c("id_10", "mukey", "z", "id_field"))


perfomances_dt2[,overpred := ifelse(N_fert > N_fert_12, 1, 0 )]
perfomances_dt2[,subpred := ifelse(N_fert < N_fert_12, 1, 0 )]
perfomances_dt2[,angulo := ifelse(N_fert == N_fert_12, 1, 0 )]

perfomances_dt2[, .(Yld =  mean(Yld),
                    leach_n2 = mean(leach_n2),
                    N_fert = mean(N_fert),
                    N_fert_min = min(N_fert),
                    N_fert_max = max(N_fert),
                    P = mean(P),
                    # cor = cor(N_fert_12, N_fert),
                    RMSE = mlr::measureRMSE(truth = N_fert_12, response = N_fert),
                    overpred = sum(overpred)/.N,
                    subpred = sum(subpred)/.N,
                    angulo = sum(angulo)/.N), by = .( model, tech)][order(-P)]

saveRDS(perfomances_dt2, "./vr_value_v2/Data/files_rds/perfomances_dt3.rds")

perfomances_dt2 <- readRDS("./vr_value_v2/Data/files_rds/perfomances_dt2.rds")


