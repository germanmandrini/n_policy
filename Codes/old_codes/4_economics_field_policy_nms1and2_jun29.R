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
# yc_yearly_dt <- readRDS("./n_policy_box/Data/files_rds/yc_yearly_dt.rds")  
yc_yearly_dt3 <- readRDS("./n_policy_box/Data/files_rds/yc_yearly_dt3.rds")
grid10_tiles_sf6 <- readRDS("./n_policy_box/Data/Grid/grid10_tiles_sf6.rds") 
grid10_soils_dt5 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt5.rds") %>% data.table()
grid10_fields_sf2 <- readRDS('./n_policy_box/Data/Grid/grid10_fields_sf2.rds')
reg_model_stuff <- readRDS( "./n_policy_box/Data/files_rds/reg_model_stuff.rds")
grid10_soils_sf2 <- readRDS('./n_policy_box/Data/Grid/grid10_soils_sf2.rds')

names(reg_model_stuff)
reg_model_stuff$ratio_6$minimum_ok
reg_model_stuff$fee_0$minimum_ok
# yc_yearly_dt3[,L := L1 + L2] #update leaching adding corn and soy
# yc_yearly_dt3[, P := Y_corn * Pc + Y_soy * Ps - N_fert * Pn] #update profits adding corn and soy

#Get profits relative to zero Nitrogen Rate
# data_zero_dt <- yc_yearly_dt3[N_fert == 10, .(id_10, mukey, z, Y_corn_zero = Y_corn, Y_soy_zero = Y_soy, L_zero = L, N_fert_zero = N_fert)] %>% unique()
# yc_yearly_dt3 <- merge(yc_yearly_dt3, data_zero_dt, by = c('id_10', 'mukey', 'z'))

#======================================================================================
# GET THE FIELDS THAT CAN BE RUN
# stations_dt2 <- reg_model_stuff2$stations
full_fields_dt <- reg_model_stuff$full_fields #one row by field x soil

# Clean the fields
full_fields_dt2 <- full_fields_dt[,.(.N), by = .(id_10, id_field, region)][,-'N'] #one row by field
table(full_fields_dt2[,.N, .(id_10)]$N)

full_fields_dt2[,N := .N, .(id_10)]
fields_list_dt <- full_fields_dt2
# 
# fields_list_dt <- full_fields_dt2[N != 1][order(id_10)]
# fields_list_dt[,N := .N, .(id_10)]
# table(fields_list_dt$N)
# unique(fields_list_dt)

# ----------------
training_z <- reg_model_stuff$training_z
no_cost_varb <- reg_model_stuff$no_cost_var
ss_var <- reg_model_stuff$ss_var
# crop_varb <- reg_model_stuff$crop_varb

process_field_economics <- function(j){
  # j = 2
  print(j)
  field_info <- fields_list_dt[j]
  # field_info <- data.table(id_10 = 911, id_field = 3)
  
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
    
    ic_field_dt[, n_0_60cm_v5 := n_20cm_v5 + n_40cm_v5 + n_60cm_v5]
    # ic_field_dt[,L := L1 + L2] #update leaching adding corn and soy
    
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
      ic_field_dt[, P_1 := Y_corn * Pc - N_fert * Pn_tmp]  #update profits
      ic_field_dt[, P_2 := Y_soy * Ps]  #update profits
      ic_field_dt[, P := P_1]  #update profits
      ic_field_dt[, G := N_fert * (Pn_tmp - Pn)] #gov collection
      
      #===================================================================================================================
      # 1b) MINIMUM OK-
      
      #---------------------------------------------------------------------------
      # PERFORMANCE EVALUATION
      # the NMS is trained with z1-10 and testing is evaluated with z11-25
      testing_set <- merge(ic_field_dt[!z %in% training_z],
                           reg_model_stuff[[policy_n]]$minimum_ok, #because I forgot to set the name to eonr_pred
                           by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
        .[N_fert == eonr_pred] %>%
        .[,c("mukey", "z", "id_10", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', 'P_1', 'P_2', "G")]
      
      areas <- testing_set[,.(mukey, area_ha)] %>% unique()
      
      sum(areas$area_ha)
      
      small_list[[length(small_list)+1]] <- testing_set[,NMS := '1'][,tech := 'UR'][,policy := policy_n]
      
      #===================================================================================================================
      # # 2) PREDICT WITH REGIONAL RF 1 - UR 
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
      .[,c("mukey", "z", "id_10", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5", "N_fert", 'P', 'P_1', 'P_2', "G")]
      
      small_list[[length(small_list)+1]] <- testing_set[,NMS := '2'][,tech := 'UR'][,policy := policy_n]
  
      }
  
    #===================================================================================================================
    # CHARGE A FEE APPROACH
    policies_fee <- names(reg_model_stuff)[str_detect(names(reg_model_stuff), pattern = 'fee_')]
    
    for(policy_n in policies_fee){
      # policy_n = policies_fee[[3]]
      fee_n <- as.numeric(str_extract(policy_n,pattern = '[0-9.]+'))
      # print(fee_n)
      ic_field_dt[, P_1 := Y_corn * Pc - N_fert * Pn - L1 * fee_n]  #update profits
      ic_field_dt[, P_2 := Y_soy * Ps - L2 * fee_n]  #update profits
      ic_field_dt[, P := Y_corn * Pc - N_fert * Pn - L * fee_n]  #update profits
      ic_field_dt[, G := L * fee_n] #gov collectionn
      
      
      # #===================================================================================================================
      # 1b) MINIMUM OK-
      
      #---------------------------------------------------------------------------
      # PERFORMANCE EVALUATION
      # the NMS is trained with z1-10 and testing is evaluated with z11-25
      testing_set <- merge(ic_field_dt[!z %in% training_z],
                           reg_model_stuff[[policy_n]]$minimum_ok, by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
        .[N_fert == eonr_pred] %>%
        .[,c("mukey", "z", "id_10", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', 'P_1', 'P_2', "G")]
      
      areas <- testing_set[,.(mukey, area_ha)] %>% unique()
      
      sum(areas$area_ha)
      
      small_list[[length(small_list)+1]] <- testing_set[,NMS := '1'][,tech := 'UR'][,policy := policy_n]
      
      
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

      # prediction_set_aggregated[,eonr_pred := ceiling(predict(reg_model_stuff[[policy_n]]$rf1,
      #                                                         prediction_set_aggregated)/10)*10]
      # 
      test[,n_0_60cm_v5 := n_0_60cm_v5 * area_ha]

      test2 <- merge(test[,.(n_0_60cm_v5 = sum(n_0_60cm_v5) / sum(area_ha)),by= c("z")],
                     prediction_set_aggregated[,.(z, n_0_60cm_v5)], by = c('z'))

      test2[, ok := round(n_0_60cm_v5.x,1) == round(n_0_60cm_v5.y,1)]

      all( test2$ok )
      
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
        .[,c("mukey", "z", "id_10", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5", "N_fert", 'P', 'P_1', 'P_2', "G")]
      
      small_list[[length(small_list)+1]] <- testing_set[,NMS := '2'][,tech := 'UR'][,policy := policy_n]
      
      }
    #===================================================================================================================
     # N LEACHING REDUCTION MODEL: farmers due to maral understanding decide to adopt a model that reduces N leaching
    ic_field_dt[, P_1 := Y_corn * Pc - N_fert * Pn]  #update profits
    ic_field_dt[, P_2 := Y_soy * Ps]  #update profits
    ic_field_dt[, P := P_1]  #update profits
    ic_field_dt[, G := 0] #gov collection
    
    
    #Get the ex-post data for n leaching reduction (used then in NMS 11 and 12)
    # baseline_leaching <- merge(ic_field_dt[!z %in% training_z], reg_model_stuff[['fee_0']]$minimum_ok, by = 'region') %>% .[N_fert == eonr_pred] %>% .[,.(id_10, mukey, z, leach_base = L)]
    # 
    # ic_field_dt_nr <- merge(ic_field_dt[!z %in% training_z], baseline_leaching, by = c('id_10', 'mukey', 'z'))
    # ic_field_dt_nr[,leach_rel := L/leach_base]
    # ic_field_dt_nr[L == 0 & leach_base == 0, leach_rel := 1] #avoid dividing by 0
    # ic_field_dt_nr[L > 0 & leach_base == 0, leach_rel := L/0.001] #avoid dividing by 0
    
    policies_nred <- names(reg_model_stuff)[str_detect(names(reg_model_stuff), pattern = 'nred_')]
    
    for(policy_n in policies_nred){
      # policy_n = policies_nred[[1]]
      nred_n <- as.numeric(str_extract(policy_n,pattern = '[0-9.]+'))
      # print(policy_n)
      # #===================================================================================================================
      # 1b) MINIMUM OK-
      
      #---------------------------------------------------------------------------
      # PERFORMANCE EVALUATION
      # the NMS is trained with z1-10 and testing is evaluated with z11-25
      testing_set <- merge(ic_field_dt[!z %in% training_z],
                           reg_model_stuff[[policy_n]]$minimum_ok, by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
        .[N_fert == eonr_pred] %>%
        .[,c("mukey", "z", "id_10", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', 'P_1', 'P_2', "G")]
      
      areas <- testing_set[,.(mukey, area_ha)] %>% unique()
      
      sum(areas$area_ha)
      
      small_list[[length(small_list)+1]] <- testing_set[,NMS := '1'][,tech := 'UR'][,policy := policy_n]
      
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

      # prediction_set_aggregated[,eonr_pred := ceiling(predict(reg_model_stuff[[policy_n]]$rf1,
      #                                                         prediction_set_aggregated)/10)*10]

      test[,n_0_60cm_v5 := n_0_60cm_v5 * area_ha]

      test2 <- merge(test[,.(n_0_60cm_v5 = sum(n_0_60cm_v5) / sum(area_ha)),by= c("z")],
                     prediction_set_aggregated[,.(z, n_0_60cm_v5)], by = c('z'))

      test2[, ok := round(n_0_60cm_v5.x,1) == round(n_0_60cm_v5.y,1)]

      all( test2$ok )
      
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
        .[,c("mukey", "z", "id_10", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5", "N_fert", 'P', 'P_1', 'P_2', "G")]
      
      small_list[[length(small_list)+1]] <- testing_set[,NMS := '2'][,tech := 'UR'][,policy := policy_n]
      
      }
      
    
    
    # =========================================================================================================================================================
    
    testing_set_dt <- rbindlist(small_list)
    
    testing_set_dt[,.N, .(mukey, policy, NMS)] %>% .[,N] %>% table() #number of z by mukey. SHould be all equal
    testing_set_dt[,.N, .(mukey)] #number of methods, z and policies by mukey
    
    # testing_set_dt[,NMS := as.numeric(method)]
    testing_set_dt[NMS == '1', .(Y_corn =  mean(Y_corn),
                      L = mean(L),
                      N_fert = mean(N_fert),
                      P = mean(P),
                      area_ha = sum(area_ha)), by = .( policy )][order(-P)]
    
    testing_set_dt <- cbind(field_info[1,.(id_10, id_field, region)], testing_set_dt[,-'id_10'])
    testing_set_dt[,.N, by = .(NMS, tech)]
   
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
  .[,.N, .(id_10, id_field)] %>% .[,.N, .(id_10)] %>% .[,N] %>% table() #number of fields by cell

perfomances_dt[,.N, .(id_10, mukey,id_field, policy, NMS)]$N %>% table() #number of z by all the other things

perfomances_dt[,.N, .(id_10, mukey,id_field, policy, NMS)]$N %>% table()


saveRDS(perfomances_dt, "./n_policy_box/Data/files_rds/perfomances_dt.rds")

