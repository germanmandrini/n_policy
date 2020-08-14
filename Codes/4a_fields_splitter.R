# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
# setwd('~')
rm(list=ls())

source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')
source('./n_policy_git/Codes/parameters.R')
source('C:/Users/germanm2/Documents/n_policy_git/Codes/parameters.R')


library("foreach")
library("doParallel")
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
# training_z <- reg_model_stuff$training_z
low_var <- reg_model_stuff$no_cost_var
high_var <- reg_model_stuff$high_var

low_var <- c("rain_30", "rain_60", "rain_90",
             "t_max_30", "t_max_60", "t_max_90", "t_min_30", "t_min_60",
             "t_min_90", "Y_prev", 'Y_corn_lt_avg', "day_sow", "day_v5", "lai_v5")#'Y_corn_lt_min', 'Y_corn_lt_max', 

high_var <- c("whc",  "oc_20cm_v5", "sw_dep_v5", "n_0_60cm_v5",  "surfaceom_wt_v5", "sand_40cm", "clay_40cm") #"root_wt_v5",, "n_deep_v5", "esw_pct_v5", 


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
    
      # the NMS is trained with z1-10 and testing is evaluated with z11-25
      testing_set <- ic_field_dt %>%
        .[,c("mukey", "z", "id_10", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert")]
      

      #===================================================================================================================
      # AGGREGATE THE INFORMATION AT THE FIELD LEVEL 
      prediction_set <- data.table(unique(ic_field_dt[, c('mukey', 'z','area_ha', low_var, high_var), with = FALSE])) #this is unique v5 conditions, doesn't have the different N rates

      table(prediction_set$z)

      test <- copy(prediction_set) #need to be here, before the columns are updated

      # We need to aggregate at the field level because is UR
      do_not_aggregate = c("mukey", "z", "area_ha")
      do_aggregate = setdiff(c(low_var, high_var), do_not_aggregate)

      prediction_set_aggregated  <- aggregate_by_area(data_dt = prediction_set, variables = do_aggregate,
                                                    weight = 'area_ha', by_c = c('z'))# %>% .[,-'area_ha']

      output_list <- list() 
      output_list[['testing_set']] <- cbind(field_info[1,.(id_10, id_field, region)], testing_set[,-'id_10'])
      output_list[['prediction_set_aggregated']] <- cbind(field_info[1,.(id_10, id_field, region)], prediction_set_aggregated)
      
   
      
  return(output_list)
}

# library('foreach')

time1 <- Sys.time()

fields_seq <- 1:nrow(fields_list_dt)

which(fields_list_dt$id_10 == 22 & fields_list_dt$id_field == 1)
fields_list_dt[20]
# fields_seq <- sample(1:nrow(fields_list_dt), 50)
# test_dt <- process_field_economics(20)
test_dt$testing_set[N_fert == 150 & z == 11]

#---------------------
#Get the two sets for each field
registerDoParallel(cores = 4)
output_list = foreach(j = fields_seq, .combine = "c", .packages = c("data.table")) %dopar% {
  # j <- 1
  tmp_dt <- process_field_economics(j)
  list(tmp_dt)
}#end of dopar loop

stopImplicitCluster()


#---------------------
# Split the sets in two list
testing_set_list <- list()
prediction_set_aggregated_list <- list()

for(i in 1:length(big_list)){
  testing_set_list[[i]] <- big_list[[i]][["testing_set"]]
  prediction_set_aggregated_list[[i]] <- big_list[[i]][["prediction_set_aggregated"]]
}


prediction_set_aggregated_dt <- rbindlist(prediction_set_aggregated_list)
testing_set_dt <- rbindlist(testing_set_list)

saveRDS(prediction_set_aggregated_dt, "./n_policy_box/Data/files_rds/prediction_set_aggregated_dt.rds")
saveRDS(testing_set_dt, "./n_policy_box/Data/files_rds/testing_set_dt.rds")
