# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
setwd('~')
rm(list=ls())

source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')
source('./n_policy_git/Codes/parameters.R')

# library(randomForest)
# library(mlr)
# eonr_mukey_dt3 <- readRDS("./n_policy_box/Data/files_rds/eonr_mukey_dt3.rds")
  
yc_yearly_dt3 <- readRDS("./n_policy_box/Data/files_rds/yc_yearly_dt3.rds")
grid10_tiles_sf6 <- readRDS("./n_policy_box/Data/Grid/grid10_tiles_sf6.rds") 
grid10_soils_dt5 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt5.rds") %>% data.table()
grid10_fields_sf2 <- readRDS('./n_policy_box/Data/Grid/grid10_fields_sf2.rds')
reg_model_stuff <- readRDS( "./n_policy_box/Data/files_rds/reg_model_stuff.rds")
grid10_soils_sf2 <- readRDS('./n_policy_box/Data/Grid/grid10_soils_sf2.rds')

yc_yearly_dt3[,leach_n := leach_1 + leach_2] #update leaching adding corn and soy
yc_yearly_dt3[, P := Yld * Pc + Yld_soy * Ps - N_fert * Pn] #update profits adding corn and soy
#======================================================================================
# Do a plot with Yld and Leaching for a static N_fert across the state
areas_dt <- data.table(grid10_soils_sf2) %>% .[,.(area_ha = sum(area_ha)), by = .(id_10, mukey)]

state_agg_dt <- merge(yc_yearly_dt3, areas_dt, by = c('id_10', 'mukey'))

<<<<<<< HEAD
state_agg_dt2  <- aggregate_by_area(data_dt = state_agg_dt, variables = c('Yld', 'Yld_soy', 'leach_1','leach_2'), 
=======
state_agg_dt2  <- aggregate_by_area(data_dt = state_agg_dt, variables = c('Yld', 'Yld_soy','leach_n2'), 
>>>>>>> 8f2bd5e377de79a8314a5e2f5fdf8658c0c50c97
                                                weight = 'area_ha', by_c = c('N_fert'))# %>% .[,-'area_ha']
baselevel_leach <- 45.6537
baselevel_leach <- 47
baselevel_yld <- 11136.46
baselevel_nfert <- 186.5601

state_agg_dt2[,leach_prop := round((leach_n / baselevel_leach) - 1,2)*100 ]

plot_1 <- ggplot(data = state_agg_dt2[N_fert < 250]) + 
  geom_line(aes(x = N_fert, y = Yld, linetype = "Yield_corn")) +
  geom_line(aes(x = N_fert, y = leach_1*200, linetype = "N Leaching")) +
  #geom_hline(yintercept = baselevel_yld, linetype = 'dashed', color = 'grey', size = 1)+
  geom_vline(xintercept = baselevel_nfert, linetype = 'dashed', color = 'grey', size = 1)+
  labs(y = 'Yield (kg/ha)',
                x = 'N rate (kg/ha)',
                colour = "Parameter")+
           scale_y_continuous(sec.axis = sec_axis(~./200, name = "N leaching (kg/ha)", breaks = seq(30,80,5), labels = seq(30,80,5))) +
           scale_linetype_manual(values = c("dashed", "solid"))+
      theme_bw()+
      # guides(linetype = guide_legend(order=2),
      #        size = guide_legend(order=1)) +
      theme(legend.title =  element_blank(),
            legend.position = c(0.87, 0.15),
            legend.text=element_text(size=8),
            panel.grid = element_blank())

plot_1

ggsave(plot = plot_1, filename = "./n_policy_box/Data/figures/state_response_curve.jpg", width = 5, height = 3,
       units = 'in')

#======================================================================================
# GET THE FIELDS THAT CAN BE RUN
# stations_dt2 <- reg_model_stuff2$stations
full_fields_dt <- reg_model_stuff$full_fields #one row by field x soil

# Clean the fields, leaving only 2 by cell
full_fields_dt2 <- full_fields_dt[,.(.N), by = .(id_10, id_field, region)][,-'N'] #one row by field
table(full_fields_dt2[,.N, .(id_10)]$N)

full_fields_dt2[,N := .N, .(id_10)]
# full_fields_wide_dt <- dcast(full_fields_dt2, id_10 + region +  N ~ id_field, value.var = "id_field")
  # names(full_fields_wide_dt)[4:7] <- paste('field', names(full_fields_wide_dt)[4:7], sep = '_')
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
no_cost_varb <- reg_model_stuff$no_cost_var
ss_var <- reg_model_stuff$ss_var
# crop_varb <- reg_model_stuff$crop_varb

process_field_economics <- function(j){
  # j = 11
  print(j)
  field_info <- fields_list_dt[j]
  # field_info <- data.table(id_10 = 18, id_field = 1)
  
  # ic_field_dt_both <- data.table()
  # for(field_n_loop in id_10_info$id_field){ #For doing the both approach
  # for(field_n_loop in field_info$id_field){ #For doing the local approach
  # field_n <- id_10_info$id_field[1]
  
    field_soils_dt <- grid10_soils_dt5[id_10 == field_info$id_10 &
                      id_field == field_info$id_field,]
    
    
    
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
  
      one_field_sf <- grid10_soils_sf2[grid10_soils_sf2$id_10 == field_info$id_10 &
                                         grid10_soils_sf2$id_field == field_info$id_field,]
      one_field_sf2 <- one_field_sf[one_field_sf$mukey %in% field_soils_dt$mukey,]
      
      (field <- tm_shape(one_field_sf2) + tm_polygons("mukey") + 
         tm_layout(legend.text.size = 0.7,
                   main.title = paste('ONE FIELD MAP -', round(sum(one_field_sf$area_ha),1),' ha'),
                   main.title.position = "center",
                   main.title.size = 1))
      tmap_save(field, filename = "./n_policy_box/Data/figures/field.jpg", scale = 2)  
    }
    #---------------------------------------------------------------------------
    # FILTER APSIM DATA AND ADD VARIABLES NEEDED FOR PREDICTIONS
    z_odd = c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)
    z_even = z_odd+1
    z_select <- if(field_info$id_field %in% c(1,3)){z_odd} else{z_even}
    z_select <- z_select[!z_select %in% training_z] #keep only testing z
    ic_field_dt <- yc_yearly_dt3[id_10 == field_info$id_10 & 
                                   mukey %in% unique(field_soils_dt$mukey) & z %in% z_select]
    ic_field_dt[,.N, by = z]
    
    # Update area and coordinates using field level information
    ic_field_dt <- merge(ic_field_dt, field_soils_dt[,.(id_10, mukey, area_ha, lat, long)], by = c('id_10', 'mukey'))
    ic_field_dt[, P := Yld * Pc + Yld_soy * Ps - N_fert * Pn] #update profits adding corn and soy
    ic_field_dt[, n_0_60cm_v5 := n_20cm_v5 + n_40cm_v5 + n_60cm_v5]
    ic_field_dt[,leach_n := leach_1 + leach_2] #update leaching adding corn and soy
    
    ic_field_dt[,.N, by = .(z, mukey)]
    
    small_list <- list()
    #===================================================================================================================
    # CHANGE THE RATIO APPROACH
    policies_ratios <- names(reg_model_stuff)[str_detect(names(reg_model_stuff), pattern = 'ratio_')]
    
    for(policy_n in policies_ratios){
      # policy_n = policies_ratios[[2]]
      ratio_n <- as.numeric(str_extract(policy_n,pattern = '[0-9.]+'))
      Pn_tmp = ratio_n * Pc
      # print(Pn_tmp/Pc)
      ic_field_dt[, P_1 := Yld * Pc - N_fert * Pn_tmp]  #update profits
      ic_field_dt[, P_2 := Yld_soy * Ps]  #update profits
      ic_field_dt[, P := P_1 + P_2]  #update profits
      ic_field_dt[, gov := N_fert * (Pn_tmp - Pn)] #gov collection
      #===================================================================================================================
      # 1) PREDICT WITH REGIONAL MINIMUM NMS UR ----
    
      #---------------------------------------------------------------------------
      # PERFORMANCE EVALUATION
      # the NMS is trained with z1-10 and testing is evaluated with z11-25
      testing_set <- merge(ic_field_dt[!z %in% training_z],
                           reg_model_stuff[[policy_n]]$minimum, by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      .[N_fert == eonr_pred] %>%
      .[,c("mukey", "z", "id_10", "area_ha", "Yld", 'Yld_soy', 'leach_1', 'leach_2', "leach_n", "n_deep_v5","N_fert",'P', 'P_1', 'P_2', "gov")]
      
      areas <- testing_set[,.(mukey, area_ha)] %>% unique()
      
      sum(areas$area_ha)
      
      small_list[[length(small_list)+1]] <- testing_set[,NMS := '1'][,tech := 'UR'][,policy := policy_n]
      #===================================================================================================================
      # 1b) MINIMUM OK-
      
      #---------------------------------------------------------------------------
      # PERFORMANCE EVALUATION
      # the NMS is trained with z1-10 and testing is evaluated with z11-25
      testing_set <- merge(ic_field_dt[!z %in% training_z],
                           reg_model_stuff[[policy_n]]$minimum_ok, #because I forgot to set the name to eonr_pred
                           by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
        .[N_fert == eonr_pred] %>%
        .[,c("mukey", "z", "id_10", "area_ha", "Yld", 'Yld_soy', 'leach_1', 'leach_2', "leach_n", "n_deep_v5","N_fert",'P', 'P_1', 'P_2', "gov")]
      
      areas <- testing_set[,.(mukey, area_ha)] %>% unique()
      
      sum(areas$area_ha)
      
      small_list[[length(small_list)+1]] <- testing_set[,NMS := '1_ok'][,tech := 'UR'][,policy := policy_n]
      
      #===================================================================================================================
      # 2) PREDICT WITH REGIONAL RF 1 - UR 
      # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
      prediction_set <- data.table(unique(ic_field_dt[!z %in% training_z,
                            c('mukey', 'z','area_ha', no_cost_varb, ss_var), with = FALSE])) #this is unique v5 conditions, doesn't have the different N rates
      
      table(prediction_set$z)
      
      test <- copy(prediction_set) #need to be here, before the columns are updated
      
      # We need to aggregate at the field level because is UR
      do_not_aggregate = c("mukey", "z", "area_ha")
      do_aggregate = setdiff(c(no_cost_varb, ss_var), do_not_aggregate)
      
      prediction_set_aggregated  <- aggregate_by_area(data_dt = prediction_set, variables = do_aggregate, 
                                                    weight = 'area_ha', by_c = c('z'))# %>% .[,-'area_ha']
      
      prediction_set_aggregated[,eonr_pred := ceiling(predict(reg_model_stuff[[policy_n]]$rf1, 
                                                              prediction_set_aggregated)/10)*10]
      
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
      .[,c("mukey", "z", "id_10", "area_ha", "Yld", 'Yld_soy', 'leach_1', 'leach_2', "leach_n", "n_deep_v5", "N_fert", 'P', 'P_1', 'P_2', "gov" )]

      small_list[[length(small_list)+1]] <- testing_set[,NMS := '2'][,tech := 'UR'][,policy := policy_n]

      #===================================================================================================================
      # 3) PREDICT WITH REGIONAL RF 1 -VR
      
      # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY AND PERFORMANCE EVALUATION
      testing_set <- ic_field_dt[!z %in% training_z] %>% 
      .[,eonr_pred := ceiling(predict(reg_model_stuff[[policy_n]]$rf1, .)/10)*10] %>%
      .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
      .[N_fert == eonr_pred] %>% 
      .[,c("mukey", "z", "id_10", "area_ha", "Yld", 'Yld_soy', 'leach_1', 'leach_2', "leach_n", "n_deep_v5", "N_fert", 'P', 'P_1', 'P_2', "gov")] %>% .[order(mukey, z)]
      
      small_list[[length(small_list)+1]] <- testing_set[,NMS := '3'][,tech := 'VR'][,policy := policy_n]
  
      #===================================================================================================================
      # 4) PREDICT WITH REGIONAL RF 2 - UR 
      # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
      
      prediction_set_aggregated[,eonr_pred := ceiling(predict(reg_model_stuff[[policy_n]]$rf2, prediction_set_aggregated)/10)*10]
      
      #---------------------------------------------------------------------------
      # PERFORMANCE EVALUATION
      testing_set <- merge(ic_field_dt[!z %in% training_z],
                             prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
      .[N_fert == eonr_pred] %>%
      .[,c("mukey", "z", "id_10", "area_ha", "Yld", 'Yld_soy', 'leach_1', 'leach_2', "leach_n", "n_deep_v5", "N_fert", 'P', 'P_1', 'P_2', "gov")]
      
      small_list[[length(small_list)+1]] <- testing_set[,NMS := '4'][,tech := 'UR'][,policy := policy_n]
  
      #===================================================================================================================
      # 5) PREDICT WITH REGIONAL NMS 2 -VR
      
      # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY AND PERFORMANCE EVALUATION
      testing_set <- ic_field_dt[!z %in% training_z] %>% 
      .[,eonr_pred := ceiling(predict(reg_model_stuff[[policy_n]]$rf2, . )/10)*10] %>%
      .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
      .[N_fert == eonr_pred] %>% 
      .[,c("mukey", "z", "id_10", "area_ha", "Yld", 'Yld_soy', 'leach_1', 'leach_2', "leach_n", "n_deep_v5", "N_fert", 'P', 'P_1', 'P_2', "gov")] %>% .[order(mukey, z)]
      
      small_list[[length(small_list)+1]] <- testing_set[,NMS := '5'][,tech := 'VR'][,policy := policy_n]
      
      #===================================================================================================================
      # 11) EX POST UR - Best rate at the field level
      
      # Need to get EONR with aggregated data
      prediction_set_aggregated_profits <- aggregate_by_area(data_dt = ic_field_dt[!z %in% training_z], 
                                                             variables = c('P'), 
                                                             weight = 'area_ha', by_c = c('N_fert','z'))  %>%
        .[, .SD[ P == max( P)], by = .(z)] %>% .[, .SD[ N_fert == min( N_fert)], by = .(z)] %>%
        .[,-c('area_ha', 'P')] %>% setnames('N_fert', 'eonr_pred')
      
      #---------------------------------------------------------------------------
      # PERFORMANCE EVALUATION
      testing_set <- merge(ic_field_dt[!z %in% training_z],
                           prediction_set_aggregated_profits[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
        .[N_fert == eonr_pred] %>%
        .[,c("mukey", "z", "id_10", "area_ha", "Yld", 'Yld_soy', 'leach_1', 'leach_2', "leach_n", "n_deep_v5", "N_fert", 'P', 'P_1', 'P_2', "gov" )]
      
      small_list[[length(small_list)+1]] <- testing_set[,NMS := '11'][,tech := 'UR'][,policy := policy_n]
      #===================================================================================================================
      # 12) EX POST VR
      
      # PERFORMANCE EVALUATION
      testing_set <- ic_field_dt[!z %in% training_z] %>% 
        .[, .SD[ P == max( P)], by = .(id_10, mukey, z)] %>% .[, .SD[ N_fert == min( N_fert )], by = .(id_10, mukey, z)] %>%
        .[,c("mukey", "z", "id_10", "area_ha", "Yld", 'Yld_soy', 'leach_1', 'leach_2', "leach_n", "n_deep_v5", "N_fert", 'P', 'P_1', 'P_2', "gov" )]
      
      small_list[[length(small_list)+1]] <- testing_set[, NMS := '12'][,tech := 'VR'][,policy := policy_n]
      #===================================================================================================================
    }
  
    #===================================================================================================================
    # CHARGE A FEE APPROACH
    policies_fee <- names(reg_model_stuff)[str_detect(names(reg_model_stuff), pattern = 'fee_')]
    
    for(policy_n in policies_fee){
      # policy_n = policies_fee[[3]]
      fee_n <- as.numeric(str_extract(policy_n,pattern = '[0-9.]+'))
      # print(fee_n)
      ic_field_dt[, P_1 := Yld * Pc - N_fert * Pn - leach_1 * fee_n]  #update profits
      ic_field_dt[, P_2 := Yld_soy * Ps - leach_2 * fee_n]  #update profits
      ic_field_dt[, P := P_1 + P_2]  #update profits
      ic_field_dt[, gov := leach_n * fee_n] #gov collectionn
      
      
      #===================================================================================================================
      # 1) PREDICT WITH REGIONAL MINIMUM NMS UR ----
      
      #---------------------------------------------------------------------------
      # PERFORMANCE EVALUATION
      # the NMS is trained with z1-10 and testing is evaluated with z11-25
      testing_set <- merge(ic_field_dt[!z %in% training_z],
                           reg_model_stuff[[policy_n]]$minimum, by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
        .[N_fert == eonr_pred] %>%
        .[,c("mukey", "z", "id_10", "area_ha", "Yld", 'Yld_soy', 'leach_1', 'leach_2', "leach_n", "n_deep_v5","N_fert",'P', 'P_1', 'P_2', "gov")]
      
      areas <- testing_set[,.(mukey, area_ha)] %>% unique()
      
      sum(areas$area_ha)
      
      small_list[[length(small_list)+1]] <- testing_set[,NMS := '1'][,tech := 'UR'][,policy := policy_n]
      #===================================================================================================================
      # 1b) MINIMUM OK-
      
      #---------------------------------------------------------------------------
      # PERFORMANCE EVALUATION
      # the NMS is trained with z1-10 and testing is evaluated with z11-25
      testing_set <- merge(ic_field_dt[!z %in% training_z],
                           reg_model_stuff[[policy_n]]$minimum_ok, by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
        .[N_fert == eonr_pred] %>%
        .[,c("mukey", "z", "id_10", "area_ha", "Yld", 'Yld_soy', 'leach_1', 'leach_2', "leach_n", "n_deep_v5","N_fert",'P', 'P_1', 'P_2', "gov")]
      
      areas <- testing_set[,.(mukey, area_ha)] %>% unique()
      
      sum(areas$area_ha)
      
      small_list[[length(small_list)+1]] <- testing_set[,NMS := '1_ok'][,tech := 'UR'][,policy := policy_n]
      
      
      #===================================================================================================================
      # 2) PREDICT WITH REGIONAL RF 1 - UR 
      # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
      prediction_set <- data.table(unique(ic_field_dt[!z %in% training_z,
                                                      c('mukey', 'z','area_ha', no_cost_varb, ss_var), with = FALSE])) #this is unique v5 conditions, doesn't have the different N rates
      
      table(prediction_set$z)
      
      test <- copy(prediction_set) #need to be here, before the columns are updated
      
      # We need to aggregate at the field level because is UR
      do_not_aggregate = c("mukey", "z", "area_ha")
      do_aggregate = setdiff(c(no_cost_varb, ss_var), do_not_aggregate)
      
      prediction_set_aggregated  <- aggregate_by_area(data_dt = prediction_set, variables = do_aggregate, 
                                                      weight = 'area_ha', by_c = c('z'))# %>% .[,-'area_ha']
      
      prediction_set_aggregated[,eonr_pred := ceiling(predict(reg_model_stuff[[policy_n]]$rf1, 
                                                              prediction_set_aggregated)/10)*10]
      
      test[,n_0_60cm_v5 := n_0_60cm_v5 * area_ha]
      
      test2 <- merge(test[,.(n_0_60cm_v5 = sum(n_0_60cm_v5) / sum(area_ha)),by= c("z")], 
                     prediction_set_aggregated[,.(z, n_0_60cm_v5)], by = c('z'))
      
      test2[, ok := round(n_0_60cm_v5.x,1) == round(n_0_60cm_v5.y,1)]
      
      all( test2$ok )
      
      #---------------------------------------------------------------------------
      # PERFORMANCE EVALUATION
      testing_set <- merge(ic_field_dt[!z %in% training_z],
                           prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we join back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
        .[,eonr_pred := ifelse(eonr_pred < 10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
        .[N_fert == eonr_pred] %>%
        .[,c("mukey", "z", "id_10", "area_ha", "Yld", 'Yld_soy', 'leach_1', 'leach_2', "leach_n", "n_deep_v5", "N_fert", 'P', 'P_1', 'P_2', "gov")]
      
      small_list[[length(small_list)+1]] <- testing_set[,NMS := '2'][,tech := 'UR'][,policy := policy_n]
      
      #===================================================================================================================
      # 3) PREDICT WITH REGIONAL RF 1 -VR
      
      # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY AND PERFORMANCE EVALUATION
      testing_set <- ic_field_dt[!z %in% training_z] %>% 
        .[,eonr_pred := ceiling(predict(reg_model_stuff[[policy_n]]$rf1, .)/10)*10] %>%
        .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
        .[N_fert == eonr_pred] %>% 
        .[,c("mukey", "z", "id_10", "area_ha", "Yld", 'Yld_soy', 'leach_1', 'leach_2', "leach_n", "n_deep_v5", "N_fert", 'P', 'P_1', 'P_2', "gov" )] %>% .[order(mukey, z)]
      
      small_list[[length(small_list)+1]] <- testing_set[,NMS := '3'][,tech := 'VR'][,policy := policy_n]
      
      #===================================================================================================================
      # 4) PREDICT WITH REGIONAL RF 2 - UR 
      # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
      
      prediction_set_aggregated[,eonr_pred := ceiling(predict(reg_model_stuff[[policy_n]]$rf2, prediction_set_aggregated)/10)*10]
      
      #---------------------------------------------------------------------------
      # PERFORMANCE EVALUATION
      testing_set <- merge(ic_field_dt[!z %in% training_z],
                           prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
        .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
        .[N_fert == eonr_pred] %>%
        .[,c("mukey", "z", "id_10", "area_ha", "Yld", 'Yld_soy', 'leach_1', 'leach_2', "leach_n", "n_deep_v5", "N_fert", 'P', 'P_1', 'P_2', "gov")]
      
      small_list[[length(small_list)+1]] <- testing_set[,NMS := '4'][,tech := 'UR'][,policy := policy_n]
      
      #===================================================================================================================
      # 5) PREDICT WITH REGIONAL NMS 2 -VR
      
      # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY AND PERFORMANCE EVALUATION
      testing_set <- ic_field_dt[!z %in% training_z] %>% 
        .[,eonr_pred := ceiling(predict(reg_model_stuff[[policy_n]]$rf2, . )/10)*10] %>%
        .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
        .[N_fert == eonr_pred] %>% 
        .[,c("mukey", "z", "id_10", "area_ha", "Yld", 'Yld_soy', 'leach_1', 'leach_2', "leach_n", "n_deep_v5", "N_fert", 'P', 'P_1', 'P_2', "gov")] %>% .[order(mukey, z)]
      

      small_list[[length(small_list)+1]] <- testing_set[,NMS := '5'][,tech := 'VR'][,policy := policy_n]
    
      #===================================================================================================================
      # 11) EX POST UR - Best rate at the field level
      
      # Need to get EONR with aggregated data
      prediction_set_aggregated_profits <- aggregate_by_area(data_dt = ic_field_dt[!z %in% training_z], 
                                                             variables = c('P'), 
                                                             weight = 'area_ha', by_c = c('N_fert','z'))  %>%
        .[, .SD[ P == max( P)], by = .(z)] %>% .[, .SD[ N_fert == min( N_fert)], by = .(z)] %>%
        .[,-c('area_ha', 'P')] %>% setnames('N_fert', 'eonr_pred')
      
      #---------------------------------------------------------------------------
      # PERFORMANCE EVALUATION
      testing_set <- merge(ic_field_dt[!z %in% training_z],
                           prediction_set_aggregated_profits[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
        .[N_fert == eonr_pred] %>%
        .[,c("mukey", "z", "id_10", "area_ha", "Yld", 'Yld_soy', 'leach_1', 'leach_2', "leach_n", "n_deep_v5", "N_fert", 'P', 'P_1', 'P_2', "gov" )]
      
      small_list[[length(small_list)+1]] <- testing_set[,NMS := '11'][,tech := 'UR'][,policy := policy_n]
      #===================================================================================================================
      # 12) EX POST VR
      
      # PERFORMANCE EVALUATION
      testing_set <- ic_field_dt[!z %in% training_z] %>% 
        .[, .SD[ P == max( P)], by = .(id_10, mukey, z)] %>% .[, .SD[ N_fert == min( N_fert )], by = .(id_10, mukey, z)] %>%
        .[,c("mukey", "z", "id_10", "area_ha", "Yld", 'Yld_soy', 'leach_1', 'leach_2', "leach_n", "n_deep_v5", "N_fert", 'P', 'P_1', 'P_2', "gov" )]
      
      small_list[[length(small_list)+1]] <- testing_set[, NMS := '12'][,tech := 'VR'][,policy := policy_n]
      
      }
    #===================================================================================================================
    # YR APPROACH
    ic_field_dt[, P := Yld * Pc - N_fert * Pn]#update profits
    ic_field_dt[, gov := 0] #gov collection
    
    policies_yr <- names(reg_model_stuff)[str_detect(names(reg_model_stuff), pattern = 'yr_')]
    
    for(policy_n in policies_yr){
      # policy_n = policies_yr[[8]]
      yr_n <- as.numeric(str_extract(policy_n,pattern = '[0-9.]+'))
      # print(policy_n)
      #===================================================================================================================
      # 1) PREDICT WITH REGIONAL MINIMUM NMS UR ----
      
      #---------------------------------------------------------------------------
      # PERFORMANCE EVALUATION
      # the NMS is trained with z1-10 and testing is evaluated with z11-25
      testing_set <- merge(ic_field_dt[!z %in% training_z],
                           reg_model_stuff[[policy_n]]$minimum, by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
        .[N_fert == eonr_pred] %>%
        .[,c("mukey", "z", "id_10", "area_ha", "Yld", 'Yld_soy', 'leach_1', 'leach_2', "leach_n", "n_deep_v5","N_fert",'P', 'P_1', 'P_2', "gov")]
      
      areas <- testing_set[,.(mukey, area_ha)] %>% unique()
      
      sum(areas$area_ha)
      
      small_list[[length(small_list)+1]] <- testing_set[,NMS := '1'][,tech := 'UR'][,policy := policy_n]
      #===================================================================================================================
      # 1b) MINIMUM OK-
      
      #---------------------------------------------------------------------------
      # PERFORMANCE EVALUATION
      # the NMS is trained with z1-10 and testing is evaluated with z11-25
      testing_set <- merge(ic_field_dt[!z %in% training_z],
                           reg_model_stuff[[policy_n]]$minimum_ok, by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
        .[N_fert == eonr_pred] %>%
        .[,c("mukey", "z", "id_10", "area_ha", "Yld", 'Yld_soy', 'leach_1', 'leach_2', "leach_n", "n_deep_v5","N_fert",'P', 'P_1', 'P_2', "gov")]
      
      areas <- testing_set[,.(mukey, area_ha)] %>% unique()
      
      sum(areas$area_ha)
      
      small_list[[length(small_list)+1]] <- testing_set[,NMS := '1_ok'][,tech := 'UR'][,policy := policy_n]
      
      #===================================================================================================================
      # 2) PREDICT WITH REGIONAL RF 1 - UR 
      # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
      prediction_set <- data.table(unique(ic_field_dt[!z %in% training_z,
                                                      c('mukey', 'z','area_ha', no_cost_varb, ss_var), with = FALSE])) #this is unique v5 conditions, doesn't have the different N rates
      
      table(prediction_set$z)
      
      test <- copy(prediction_set) #need to be here, before the columns are updated
      
      # We need to aggregate at the field level because is UR
      do_not_aggregate = c("mukey", "z", "area_ha")
      do_aggregate = setdiff(c(no_cost_varb, ss_var), do_not_aggregate)
      
      prediction_set_aggregated  <- aggregate_by_area(data_dt = prediction_set, variables = do_aggregate, 
                                                      weight = 'area_ha', by_c = c('z'))# %>% .[,-'area_ha']
      
      prediction_set_aggregated[,eonr_pred := ceiling(predict(reg_model_stuff[[policy_n]]$rf1, 
                                                              prediction_set_aggregated)/10)*10]
      
      test[,n_0_60cm_v5 := n_0_60cm_v5 * area_ha]
      
      test2 <- merge(test[,.(n_0_60cm_v5 = sum(n_0_60cm_v5) / sum(area_ha)),by= c("z")], 
                     prediction_set_aggregated[,.(z, n_0_60cm_v5)], by = c('z'))
      
      test2[, ok := round(n_0_60cm_v5.x,1) == round(n_0_60cm_v5.y,1)]
      
      all( test2$ok )
      
      #---------------------------------------------------------------------------
      # PERFORMANCE EVALUATION
      testing_set <- merge(ic_field_dt[!z %in% training_z],
                           prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we join back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
        .[,eonr_pred := ifelse(eonr_pred < 10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
        .[N_fert == eonr_pred] %>%
        .[,c("mukey", "z", "id_10", "area_ha", "Yld", 'Yld_soy', 'leach_1', 'leach_2', "leach_n", "n_deep_v5", "N_fert", 'P', 'P_1', 'P_2', "gov")]
      
      small_list[[length(small_list)+1]] <- testing_set[,NMS := '2'][,tech := 'UR'][,policy := policy_n]
      
      #===================================================================================================================
      # 3) PREDICT WITH REGIONAL RF 1 -VR
      
      # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY AND PERFORMANCE EVALUATION
      testing_set <- ic_field_dt[!z %in% training_z] %>% 
        .[,eonr_pred := ceiling(predict(reg_model_stuff[[policy_n]]$rf1, .)/10)*10] %>%
        .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
        .[N_fert == eonr_pred] %>% 
        .[,c("mukey", "z", "id_10", "area_ha", "Yld", 'Yld_soy', 'leach_1', 'leach_2', "leach_n", "n_deep_v5", "N_fert", 'P', 'P_1', 'P_2', "gov" )] %>% .[order(mukey, z)]
      
      small_list[[length(small_list)+1]] <- testing_set[,NMS := '3'][,tech := 'VR'][,policy := policy_n]
      
      #===================================================================================================================
      # 4) PREDICT WITH REGIONAL RF 2 - UR 
      # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
      
      prediction_set_aggregated[,eonr_pred := ceiling(predict(reg_model_stuff[[policy_n]]$rf2, prediction_set_aggregated)/10)*10]
      
      #---------------------------------------------------------------------------
      # PERFORMANCE EVALUATION
      testing_set <- merge(ic_field_dt[!z %in% training_z],
                           prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
        .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
        .[N_fert == eonr_pred] %>%
        .[,c("mukey", "z", "id_10", "area_ha", "Yld", 'Yld_soy', 'leach_1', 'leach_2', "leach_n", "n_deep_v5", "N_fert", 'P', 'P_1', 'P_2', "gov")]
      
      small_list[[length(small_list)+1]] <- testing_set[,NMS := '4'][,tech := 'UR'][,policy := policy_n]
      
      #===================================================================================================================
      # 5) PREDICT WITH REGIONAL NMS 2 -VR
      
      # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY AND PERFORMANCE EVALUATION
      testing_set <- ic_field_dt[!z %in% training_z] %>% 
        .[,eonr_pred := ceiling(predict(reg_model_stuff[[policy_n]]$rf2, . )/10)*10] %>%
        .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
        .[N_fert == eonr_pred] %>% 
        .[,c("mukey", "z", "id_10", "area_ha", "Yld", 'Yld_soy', 'leach_1', 'leach_2', "leach_n", "n_deep_v5", "N_fert", 'P', 'P_1', 'P_2', "gov")] %>% .[order(mukey, z)]
      
      
      small_list[[length(small_list)+1]] <- testing_set[,NMS := '5'][,tech := 'VR'][,policy := policy_n]
      
      #===================================================================================================================
      # 11) EX POST UR - Best rate at the field level
      
      # Need to get EONR with aggregated data
      prediction_set_aggregated_profits <- aggregate_by_area(data_dt = ic_field_dt[!z %in% training_z], 
                                                             variables = c('Yld'), 
                                                             weight = 'area_ha', by_c = c('N_fert','z'))  %>%
        .[,Yld_max := max(Yld), by = .(z)] %>% .[,Yld_rel := Yld/Yld_max] %>%
        .[,Yld_rel := ifelse(Yld >0,Yld/Yld_max, 1)] %>%
        .[, .SD[ N_fert == min( N_fert)], by = .(z)] %>% #select minimum rate that allows that yr
        .[,-c('area_ha', 'Yld')] %>% setnames('N_fert', 'eonr_pred')
      
      #---------------------------------------------------------------------------
      # PERFORMANCE EVALUATION
      testing_set <- merge(ic_field_dt[!z %in% training_z],
                           prediction_set_aggregated_profits[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
        .[N_fert == eonr_pred] %>%
        .[,c("mukey", "z", "id_10", "area_ha", "Yld", 'Yld_soy', 'leach_1', 'leach_2', "leach_n", "n_deep_v5", "N_fert", 'P', 'P_1', 'P_2', "gov" )]
      
      small_list[[length(small_list)+1]] <- testing_set[,NMS := '11'][,tech := 'UR'][,policy := policy_n]
      
      #===================================================================================================================
      # 12) EX POST VR
      
      # PERFORMANCE EVALUATION
      
      testing_set <- ic_field_dt[!z %in% training_z] %>% 
        .[,Yld_max := max(Yld), by = .(mukey, z)] %>% .[,Yld_rel := ifelse(Yld >0,Yld/Yld_max, 1)] %>%
        .[Yld_rel >= yr_n] %>% 
        .[, .SD[ N_fert == min( N_fert)], by = .(mukey, z)] %>% #select minimum rate that allows that yr
        .[,c("mukey", "z", "id_10", "area_ha", "Yld", 'Yld_soy', 'leach_1', 'leach_2', "leach_n", "n_deep_v5", "N_fert", 'P', 'P_1', 'P_2', "gov" )]
      
      small_list[[length(small_list)+1]] <- testing_set[, NMS := '12'][,tech := 'VR'][,policy := policy_n]
      
    }
  #===================================================================================================================
  # N LEACHING REDUCTION MODEL: farmers due to maral understanding decide to adopt a model that reduces N leaching
    ic_field_dt[, P_1 := Yld * Pc - N_fert * Pn]  #update profits
    ic_field_dt[, P_2 := Yld_soy * Ps]  #update profits
    ic_field_dt[, P := P_1 + P_2]  #update profits
    ic_field_dt[, gov := 0] #gov collection
    
    
    #Get the ex-post data for n leaching reduction (used then in NMS 11 and 12)
    baseline_leaching <- merge(ic_field_dt[!z %in% training_z], reg_model_stuff[['fee_0']]$minimum_ok, by = 'region') %>% .[N_fert == eonr_pred] %>% .[,.(id_10, mukey, z, leach_base = leach_n)]
    ic_field_dt_nr <- merge(ic_field_dt[!z %in% training_z], baseline_leaching, by = c('id_10', 'mukey', 'z'))
    ic_field_dt_nr[,leach_rel := leach_n/leach_base]
    
    
    policies_nred <- names(reg_model_stuff)[str_detect(names(reg_model_stuff), pattern = 'nred_')]
    
    for(policy_n in policies_nred){
      # policy_n = policies_nred[[5]]
      nred_n <- as.numeric(str_extract(policy_n,pattern = '[0-9.]+'))
      # print(policy_n)
      #===================================================================================================================
      # 1) PREDICT WITH REGIONAL MINIMUM NMS UR ----
      
      #---------------------------------------------------------------------------
      # # PERFORMANCE EVALUATION
      # # the NMS is trained with z1-10 and testing is evaluated with z11-25
      # testing_set <- merge(ic_field_dt[!z %in% training_z],
      #                      reg_model_stuff[[policy_n]]$minimum, by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      #   .[N_fert == eonr_pred] %>%
      #   .[,c("mukey", "z", "id_10", "area_ha", "Yld", 'Yld_soy', 'leach_1', 'leach_2', "leach_n", "n_deep_v5","N_fert",'P', 'P_1', 'P_2', "gov")]
      # 
      # areas <- testing_set[,.(mukey, area_ha)] %>% unique()
      # 
      # sum(areas$area_ha)
      # 
      # small_list[[length(small_list)+1]] <- testing_set[,NMS := '1'][,tech := 'UR'][,policy := policy_n]
      # #===================================================================================================================
      # 1b) MINIMUM OK-
      
      #---------------------------------------------------------------------------
      # PERFORMANCE EVALUATION
      # the NMS is trained with z1-10 and testing is evaluated with z11-25
      testing_set <- merge(ic_field_dt[!z %in% training_z],
                           reg_model_stuff[[policy_n]]$minimum_ok, by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
        .[N_fert == eonr_pred] %>%
        .[,c("mukey", "z", "id_10", "area_ha", "Yld", 'Yld_soy', 'leach_1', 'leach_2', "leach_n", "n_deep_v5","N_fert",'P', 'P_1', 'P_2', "gov")]
      
      areas <- testing_set[,.(mukey, area_ha)] %>% unique()
      
      sum(areas$area_ha)
      
      small_list[[length(small_list)+1]] <- testing_set[,NMS := '1_ok'][,tech := 'UR'][,policy := policy_n]
      
      #===================================================================================================================
      # 2) PREDICT WITH REGIONAL RF 1 - UR 
      # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
      prediction_set <- data.table(unique(ic_field_dt[!z %in% training_z,
                                                      c('mukey', 'z','area_ha', no_cost_varb, ss_var), with = FALSE])) #this is unique v5 conditions, doesn't have the different N rates
      
      table(prediction_set$z)
      
      test <- copy(prediction_set) #need to be here, before the columns are updated
      
      # We need to aggregate at the field level because is UR
      do_not_aggregate = c("mukey", "z", "area_ha")
      do_aggregate = setdiff(c(no_cost_varb, ss_var), do_not_aggregate)
      
      prediction_set_aggregated  <- aggregate_by_area(data_dt = prediction_set, variables = do_aggregate, 
                                                      weight = 'area_ha', by_c = c('z'))# %>% .[,-'area_ha']
      
      prediction_set_aggregated[,eonr_pred := ceiling(predict(reg_model_stuff[[policy_n]]$rf1, 
                                                              prediction_set_aggregated)/10)*10]
      
      test[,n_0_60cm_v5 := n_0_60cm_v5 * area_ha]
      
      test2 <- merge(test[,.(n_0_60cm_v5 = sum(n_0_60cm_v5) / sum(area_ha)),by= c("z")], 
                     prediction_set_aggregated[,.(z, n_0_60cm_v5)], by = c('z'))
      
      test2[, ok := round(n_0_60cm_v5.x,1) == round(n_0_60cm_v5.y,1)]
      
      all( test2$ok )
      
      #---------------------------------------------------------------------------
      # PERFORMANCE EVALUATION
      testing_set <- merge(ic_field_dt[!z %in% training_z],
                           prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we join back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
        .[,eonr_pred := ifelse(eonr_pred < 10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
        .[N_fert == eonr_pred] %>%
        .[,c("mukey", "z", "id_10", "area_ha", "Yld", 'Yld_soy', 'leach_1', 'leach_2', "leach_n", "n_deep_v5", "N_fert", 'P', 'P_1', 'P_2', "gov")]
      
      small_list[[length(small_list)+1]] <- testing_set[,NMS := '2'][,tech := 'UR'][,policy := policy_n]
      
      #===================================================================================================================
      # 3) PREDICT WITH REGIONAL RF 1 -VR
      
      # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY AND PERFORMANCE EVALUATION
      testing_set <- ic_field_dt[!z %in% training_z] %>% 
        .[,eonr_pred := ceiling(predict(reg_model_stuff[[policy_n]]$rf1, .)/10)*10] %>%
        .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
        .[N_fert == eonr_pred] %>% 
        .[,c("mukey", "z", "id_10", "area_ha", "Yld", 'Yld_soy', 'leach_1', 'leach_2', "leach_n", "n_deep_v5", "N_fert", 'P', 'P_1', 'P_2', "gov" )] %>% .[order(mukey, z)]
      
      small_list[[length(small_list)+1]] <- testing_set[,NMS := '3'][,tech := 'VR'][,policy := policy_n]
      
      #===================================================================================================================
      # 4) PREDICT WITH REGIONAL RF 2 - UR 
      # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
      
      prediction_set_aggregated[,eonr_pred := ceiling(predict(reg_model_stuff[[policy_n]]$rf2, prediction_set_aggregated)/10)*10]
      
      #---------------------------------------------------------------------------
      # PERFORMANCE EVALUATION
      testing_set <- merge(ic_field_dt[!z %in% training_z],
                           prediction_set_aggregated[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
        .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
        .[N_fert == eonr_pred] %>%
        .[,c("mukey", "z", "id_10", "area_ha", "Yld", 'Yld_soy', 'leach_1', 'leach_2', "leach_n", "n_deep_v5", "N_fert", 'P', 'P_1', 'P_2', "gov")]
      
      small_list[[length(small_list)+1]] <- testing_set[,NMS := '4'][,tech := 'UR'][,policy := policy_n]
      
      #===================================================================================================================
      # 5) PREDICT WITH REGIONAL NMS 2 -VR
      
      # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY AND PERFORMANCE EVALUATION
      testing_set <- ic_field_dt[!z %in% training_z] %>% 
        .[,eonr_pred := ceiling(predict(reg_model_stuff[[policy_n]]$rf2, . )/10)*10] %>%
        .[,eonr_pred := ifelse(eonr_pred <10, 10, ifelse(eonr_pred > 330, 330, eonr_pred))] %>%
        .[N_fert == eonr_pred] %>% 
        .[,c("mukey", "z", "id_10", "area_ha", "Yld", 'Yld_soy', 'leach_1', 'leach_2', "leach_n", "n_deep_v5", "N_fert", 'P', 'P_1', 'P_2', "gov")] %>% .[order(mukey, z)]
      
      
      small_list[[length(small_list)+1]] <- testing_set[,NMS := '5'][,tech := 'VR'][,policy := policy_n]
      
      #===================================================================================================================
      # 11) EX POST UR - Best rate at the field level
      
      # Need to get EONR with aggregated data
      
      prediction_set_aggregated_profits <- aggregate_by_area(data_dt = ic_field_dt_nr, 
                                                             variables = c('leach_rel'), 
                                                             weight = 'area_ha', by_c = c('N_fert','z'))  %>%
        .[leach_rel >= nred_n & leach_rel <= 1] %>% 
        .[, .SD[ leach_rel == min( leach_rel)], by = .(z)] %>% #select minimum leach_rel
        .[, .SD[ N_fert == min( N_fert)], by = .(z)] %>% #select minimum rate in case one is repeated
        .[,.(z, eonr_pred = N_fert)]
      
      # ggplot(ic_field_dt_nr[z == 26]) + geom_line(aes(x = N_fert, y = leach_rel, colour = mukey))
      
      #---------------------------------------------------------------------------
      # PERFORMANCE EVALUATION
      testing_set <- merge(ic_field_dt[!z %in% training_z],
                           prediction_set_aggregated_profits[,.(z, eonr_pred)], by = c('z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
        .[N_fert == eonr_pred] %>%
        .[,c("mukey", "z", "id_10", "area_ha", "Yld", 'Yld_soy', 'leach_1', 'leach_2', "leach_n", "n_deep_v5", "N_fert", 'P', 'P_1', 'P_2', "gov" )]
      
      small_list[[length(small_list)+1]] <- testing_set[,NMS := '11'][,tech := 'UR'][,policy := policy_n]
      
      #===================================================================================================================
      # 12) EX POST VR
      
      # PERFORMANCE EVALUATION
      testing_set <- ic_field_dt_nr[leach_rel >= nred_n & leach_rel <= 1] %>% 
        .[, .SD[ leach_rel == min( leach_rel)], by = .(mukey, z)] %>% #select minimum leach_rel
        .[, .SD[ N_fert == min( N_fert)], by = .(mukey, z)] %>% #select minimum rate in case one is repeated
        .[,c("mukey", "z", "id_10", "area_ha", "Yld", 'Yld_soy', 'leach_1', 'leach_2', "leach_n", "n_deep_v5", "N_fert", 'P', 'P_1', 'P_2', "gov" )]
      
      small_list[[length(small_list)+1]] <- testing_set[, NMS := '12'][,tech := 'VR'][,policy := policy_n]
    
    }
      
    
    
    # =========================================================================================================================================================
    
    testing_set_dt <- rbindlist(small_list)
    
    testing_set_dt[,.N, .(mukey, policy, NMS)] %>% .[,N] %>% table() #number of z by mukey
    testing_set_dt[,.N, .(mukey)] #number of methods, z and policies by mukey
    
    # testing_set_dt[,NMS := as.numeric(method)]
    testing_set_dt[z == 11,.N, .(z, NMS, tech)]
    testing_set_dt[NMS == '1_ok', .(Yld =  mean(Yld),
                      leach_n = mean(leach_n),
                      N_fert = mean(N_fert),
                      P = mean(P),
                      area_ha = sum(area_ha)), by = .( policy )][order(-P)]
    
    testing_set_dt <- cbind(field_info[1,.(id_10, id_field, region)], testing_set_dt[,-'id_10'])
    testing_set_dt[,.N, by = .(NMS, tech)]
   
  #===================================================================================================================
  #PLOT ME
  
  if(FALSE){
  
  mukey_n <- field_soils_dt[area_ha == max(area_ha)]
  mukey_n <- mukey_n$mukey[1]
  testing_set_plot <- testing_set_dt[mukey == mukey_n ]
  testing_set_plot[,NMS := factor(method, levels= c('1', '2', '3','4', '5', '6', '7', '8', '9', '10', '11', '12'))]
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
  z_labels <- ic_field_plot[N_fert == max(ic_field_plot$N_fert), .(N_fert, Yld, z, leach_n)][order(-leach_n)]
  z_labels[seq(1, nrow(z_labels), by = 2), N_fert := N_fert - 20]
  
  (plot_n2 <- ggplot() +
      geom_point(data = testing_set_plot, aes(x = N_fert, y = leach_n, colour = method, size = method)) +
      geom_line(data = ic_field_plot, aes(x = N_fert, y = leach_n, group=z), show.legend = FALSE) +
      scale_size_manual(values=c(rep(2, 11), 4)) +
      scale_color_manual(values=colors_sample)+
      ylab('N Leaching (kg/ha)')+
      xlab('N rate (kg/ha)')+
      geom_text(data = z_labels, aes(x = N_fert, y = leach_n, label = z))+
      theme_bw()+
      theme(panel.grid = element_blank()))
  
  
  ggsave(grid.arrange(plot_n1, plot_n2), filename = "./n_policy_box/Data/figures/yield_curve_example.jpg")
  
  # Y plot with Yld at eonr
  (plot_n1 <- ggplot() +
      geom_point(data = testing_set_plot[prev_crop == 1 & NMS != 12], aes(x = N_fert, y = Yld, colour = method)) +
      geom_point(data = testing_set_plot[prev_crop == 1 & NMS == 12], aes(x = N_fert, y = Yld), colour = 'black', size = 3, show.legend = FALSE) +
      geom_line(data = ic_field_plot[mukey == mukey_n & prev_crop == 1], aes(x = N_fert, y = Yld, group=interaction(z)), show.legend = FALSE) +
      # ggtitle(paste('Yld plot with Yld at eonr', mukey_n)))
      ggtitle('Yield'))
  
  ic_field_dt[mukey == mukey_n & prev_crop == 0][order(P)]
  ggsave(plot_n1, filename = "./n_policy_box/Data/figures/yield_curve_example.jpg")
  
  
  # P plot with P at eonr
  (plot_n <- ggplot() +
      geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 1 & NMS != 12], aes(x = N_fert, y = P, colour = method)) +
      geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 1 & NMS == 12], aes(x = N_fert, y = P), size = 3, show.legend = FALSE) +
      geom_line(data = ic_field_plot[mukey == mukey_n & prev_crop == 1], aes(x = N_fert, y = P, group=interaction(z)), show.legend = FALSE) +
      ggtitle(paste('P plot with P at eonr', mukey_n)))
  
  ic_field_dt[mukey == mukey_n & prev_crop == 0][order(P)]
  ggsave(plot_n, filename = "./n_policy_box/Data/figures/yield_curve_example.jpg")
  
  # leach_no3 plot with leaching at eonr
  (plot_n2 <- ggplot() +
      geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 0 & NMS != 12], aes(x = N_fert, y = leach_n, colour = method)) +
      geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 0 & NMS == 12], aes(x = N_fert, y = leach_n), size = 3, show.legend = FALSE) +
      geom_line(data = ic_field_plot[mukey == mukey_n & prev_crop == 0], aes(x = N_fert, y = leach_n, group=interaction(z)), show.legend = FALSE) +
      # ggtitle(paste('leach_no3 plot with leaching at eonr', mukey_n)))
      ggtitle('N Leaching'))
  
  ggsave(plot_n2, filename = "./n_policy_box/Data/figures/leaching_curve_example.jpg")
  #---------------------------------------------------------------------------
  # ESL510
  (plot_n1 <- ggplot() +
      geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 1 & NMS != 12], aes(x = N_fert, y = Yld), size = 0.2) +
      geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 1 & NMS == 12], aes(x = N_fert, y = Yld), size = 3, show.legend = FALSE) +
      geom_line(data = ic_field_plot[mukey == mukey_n & prev_crop == 1], aes(x = N_fert, y = Yld, colour=z)) +
      # ggtitle(paste('Yld plot with Yld at eonr', mukey_n)))
      ggtitle('Yield'))
  
  (plot_n2 <- ggplot() +
      geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 1 & NMS != 12], aes(x = N_fert, y = leach_n), size = 0.2) +
      geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 1 & NMS == 12], aes(x = N_fert, y = leach_n), size = 3, show.legend = FALSE) +
      geom_line(data = ic_field_plot[mukey == mukey_n & prev_crop == 1], aes(x = N_fert, y = leach_n, colour=z)) +
      # ggtitle(paste('Yld plot with Yld at eonr', mukey_n)))
      ggtitle('N Leaching'))
  
  grid.arrange(plot_n1, plot_n2)
  
  #---------------------------------------------------------------------------
  #NOW WITH N_TOTAL (FERT + SOIL)
  ic_field_plot[,N_total := N_fert + n_deep_v5]
  testing_set_plot[,N_total := N_fert + n_deep_v5]
  
  # P plot with P at eonr
  (plot_n <- ggplot() +
      geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 0 & NMS != 12], aes(x = N_total, y = P, colour = method)) +
      geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 0 & NMS == 12], aes(x = N_total, y = P), size = 3, show.legend = FALSE) +
      geom_line(data = ic_field_plot[mukey == mukey_n & prev_crop == 0], aes(x = N_total, y = P, group=interaction(z)), show.legend = FALSE) +
      ggtitle(paste('P plot with P at eonr', mukey_n)))
  
  ic_field_dt[mukey == mukey_n & prev_crop == 0][order(P)]
  ggsave(plot_n, filename = "./n_policy_box/Data/figures/yield_curve_example_ntotal.jpg")
  
  # leach_no3 plot with leaching at eonr
  (plot_n <- ggplot() +
      geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 0 & NMS != 12], aes(x = N_total, y = leach_n, colour = method)) +
      geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 0 & NMS == 12], aes(x = N_total, y = leach_n), size = 3, show.legend = FALSE) +
      geom_line(data = ic_field_plot[mukey == mukey_n & prev_crop == 0], aes(x = N_total, y = leach_n, group=interaction(z)), show.legend = FALSE) +
      ggtitle(paste('leach_no3 plot with leaching at eonr', mukey_n)))
  
  ggsave(plot_n, filename = "./n_policy_box/Data/figures/leaching_curve_example_ntotal.jpg")
  }#end of PLOT ME

  return(testing_set_dt)
}

# library('foreach')

time1 <- Sys.time()

fields_seq <- 1:nrow(fields_list_dt)
# fields_seq <- sample(1:nrow(fields_list_dt), 10)
# test_dt <- process_field_economics(20)

big_list <- list()
for(j in fields_seq){
  big_list[[length(big_list)+1]] <- process_field_economics(j)
  }

time2 <- Sys.time()

nopar <- time2 - time1

# nopar/100*nrow(fields_list_dt)/60

perfomances_dt <- rbindlist(big_list)

perfomances_dt[,.N, .(id_10, mukey,id_field)] %>% 
  .[,.N, .(id_10, id_field)] %>% .[,.N, .(id_10)] %>% .[,N] %>% table()

perfomances_dt[,.N, .(id_10, mukey,id_field, method)]$N

# filter_this <- perfomances_dt[,.N, .(id_10, mukey, z, id_field, method, tech)][1]
# 
# filter_dt_in_dt(x_dt = perfomances_dt, filter_dt = filter_this, return_table = T)
# 
# perfomances_dt2 <- unique(perfomances_dt)

perfomances_dt2 <- merge(perfomances_dt, 
                         perfomances_dt[NMS == '12', .(id_10, mukey, z, id_field, policy, N_fert_12 = N_fert)], 
                         by = c("id_10", "mukey", "z", "id_field", "policy"))


perfomances_dt2[,overpred := ifelse(N_fert > N_fert_12, 1, 0 )]
perfomances_dt2[,subpred := ifelse(N_fert < N_fert_12, 1, 0 )]
perfomances_dt2[,angulo := ifelse(N_fert == N_fert_12, 1, 0 )]

perfomances_dt2[, .(Yld =  mean(Yld),
                    leach_n = mean(leach_n),
                    N_fert = mean(N_fert),
                    N_fert_min = min(N_fert),
                    N_fert_max = max(N_fert),
                    P = mean(P),
                    # cor = cor(N_fert_12, N_fert),
                    RMSE = mlr::measureRMSE(truth = N_fert_12, response = N_fert),
                    overpred = sum(overpred)/.N,
                    subpred = sum(subpred)/.N,
                    angulo = sum(angulo)/.N), by = .( method, tech)][order(-P)]


saveRDS(perfomances_dt2, "./n_policy_box/Data/files_rds/perfomances_dt.rds")
