# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
setwd('~')
rm(list=ls())

source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')
source('C:/Users/germa/Documents/n_policy_git/Codes/parameters.R')
source('C:/Users/germanm2/Documents/n_policy_git/Codes/parameters.R')
source('./n_policy_git/Codes/parameters.R')

library("foreach")
library("doParallel")
# library(randomForest)
# library(mlr)
# eonr_mukey_dt3 <- readRDS("./n_policy_box/Data/files_rds/eonr_mukey_dt3.rds")
# yc_yearly_dt <- readRDS("./n_policy_box/Data/files_rds/yc_yearly_dt.rds")  
yc_yearly_dt3 <- readRDS("./n_policy_box/Data/files_rds/yc_yearly_dt3.rds")
grid10_tiles_sf6 <- readRDS("./n_policy_box/Data/Grid/grid10_tiles_sf6.rds") 
grid10_soils_dt5 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt5.rds") %>% data.table()
reg_model_stuff <- readRDS( "./n_policy_box/Data/files_rds/reg_model_stuff.rds")

# grid10_fields_sf2 <- readRDS('./n_policy_box/Data/Grid/grid10_fields_sf2.rds')
# grid10_soils_sf2 <- readRDS('./n_policy_box/Data/Grid/grid10_soils_sf2.rds')

names(reg_model_stuff)


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
# training_z <- reg_model_stuff$training_z
pred_vars <- readRDS("./n_policy_box/Data/files_rds/pred_vars.rds")

rm(reg_model_stuff)
# crop_varb <- reg_model_stuff$crop_varb

process_field_economics <- function(j){
  # j = 20
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
    # z_select <- z_select[!z_select %in% training_z] #keep only testing z
    ic_field_dt <- yc_yearly_dt3[id_10 == field_info$id_10 & 
                                   mukey %in% unique(field_soils_dt$mukey) & z %in% z_select]
    
    # Update area and coordinates using field level information
    ic_field_dt <- merge(ic_field_dt, field_soils_dt[,.(id_10, mukey, area_ha, lat, long)], by = c('id_10', 'mukey'))
    
    ic_field_dt[, n_0_60cm_v5 := n_20cm_v5 + n_40cm_v5 + n_60cm_v5]
    # ic_field_dt[,L := L1 + L2] #update leaching adding corn and soy
    
    ic_field_dt[,.N, by = .(z, mukey)]
    ic_field_dt$mukey %>% unique()
    #===================================================================================================================
    # CHANGE THE RATIO APPROACH
    testing_set <- ic_field_dt %>%
        .[,c("mukey", "z", "id_10", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert")]
      

    #===================================================================================================================
    # AGGREGATE THE INFORMATION AT THE FIELD LEVEL 
  
    ic_field_dt[, P := Y_corn * Pc - N_fert * Pn]
    prediction_set <- data.table(unique(ic_field_dt[, c('mukey', 'z','area_ha', pred_vars, 'N_fert','P'), with = FALSE])) #this is unique v5 conditions, doesn't have the different N rates

    table(prediction_set$z)

    # We need to aggregate at the field level because is UR
    do_aggregate = c(pred_vars, 'P')

    prediction_set_aggregated  <- aggregate_by_area(data_dt = prediction_set, variables = do_aggregate,
                                                  weight = 'area_ha', by_c = c('z', 'N_fert')) %>% 
      .[, .SD[ P == max( P)], by = .(z)] %>%
      .[, .SD[ N_fert == min(N_fert)], by = .(z)] %>% setnames('N_fert', 'eonr_12') %>% .[,-'P']
    

    output_list <- list() 
    output_list[['testing_set']] <- cbind(field_info[1,.(id_10, id_field, region)], testing_set[,-'id_10'])
    output_list[['prediction_set_aggregated']] <- cbind(field_info[1,.(id_10, id_field, region)], prediction_set_aggregated)
      
   
      
  return(output_list)
}

# library('foreach')

time1 <- Sys.time()

fields_seq <- 1:nrow(fields_list_dt)

#---------------------
#Get the two sets for each field
process_field_economics(20)

registerDoParallel(cores = detectCores()/2)
output_list = foreach(j = fields_seq, .combine = "c", .packages = c("data.table", "dplyr")) %do% {
  # j <- 1
  tmp_dt <- process_field_economics(j)
  list(tmp_dt)
}#end of dopar loop

stopImplicitCluster()
length(output_list) == nrow(fields_list_dt)

#---------------------
# Split the sets in two list
testing_set_list <- list()
prediction_set_aggregated_list <- list()

for(i in 1:length(output_list)){
  testing_set_list[[i]] <- output_list[[i]][["testing_set"]]
  prediction_set_aggregated_list[[i]] <- output_list[[i]][["prediction_set_aggregated"]]
}

length(prediction_set_aggregated_list)
prediction_set_aggregated_dt <- rbindlist(prediction_set_aggregated_list)
testing_set_dt <- rbindlist(testing_set_list)

saveRDS(prediction_set_aggregated_dt, "./n_policy_box/Data/files_rds/prediction_set_aggregated_dt.rds")
saveRDS(testing_set_dt, "./n_policy_box/Data/files_rds/testing_set_dt.rds")
