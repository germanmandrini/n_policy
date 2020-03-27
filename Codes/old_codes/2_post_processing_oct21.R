# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
# setwd('~')

source('./Codes_useful/R.libraries.R')
# source('./Codes_useful/gm_functions.R')
source('./vr_value_v2/Codes/parameters.R')
source('./Codes_useful/gm_functions.R')

#======================================================================================
#DATA CLEANING & QUALITY CONTROL
if(FALSE){
  multiple_files <- list.files("./vr_value_v2/Data/yc_output_summary", full.names = T)
  length(multiple_files)
  
  yc_yearly_list <- list()
  for(file_n in multiple_files){
    yc_yearly_list[[length(yc_yearly_list)+1]] <- readRDS(file_n)
  }
  
  yc_yearly_dt <- rbindlist(yc_yearly_list)
  saveRDS(yc_yearly_dt, './vr_value_v2/Data/files_rds/yc_yearly_dt.rds')
  
  length(unique(yc_yearly_dt$id_10))
  
  #------------------------------------------------------------------------------------
  # QC
  # Remove low lai_v5 (farmers would not fertilize)
  hist(yc_yearly_dt$lai_v5)
  
  ggplot(yc_yearly_dt[sample(1:nrow(yc_yearly_dt), 10000, replace = F)], aes(x = lai_v5, y = Yld))+geom_point()
  yc_yearly_dt2a <- yc_yearly_dt[lai_v5 >= yc_yearly_dt[Yld > 200, min(lai_v5)]] #lai thereshold = lowest lai where yld was positive
  
  # Assumption: if the lai is too low, the farmer will not apply the rate and will plant soybean
  
  #------------------------------------------------------------------------------------
  #Remove low yielding mukeys (farmers will not plant here)
  
  yearly_ymax_dt <- copy(yc_yearly_dt2a) %>% 
    .[,yld_max_mky := max(Yld), by = .(id_10, mukey, z)] %>% 
    .[Yld == yld_max_mky] %>% 
    .[, .SD[ N_fert  == min(N_fert )], by = .(id_10, mukey, z)] #filter the rate that maximized yield
  
  yearly_ymax_dt2 <- yearly_ymax_dt[,.(Yld = mean(Yld), 
                                 n_deep_v5 = mean(n_deep_v5),
                                 n_deep_v5_max = max(n_deep_v5)), by = .(id_10, mukey)]
  summary(yearly_ymax_dt2$Yld)
  
  hist(yearly_ymax_dt2$Yld)
  hist(yearly_ymax_dt2$n_deep_v5)
  summary(yearly_ymax_dt2$n_deep_v5)
  summary(yearly_ymax_dt2$n_deep_v5_max)
  hist(yearly_ymax_dt2[n_deep_v5 > 300]$n_deep_v5)
  
  qc_control <- yearly_ymax_dt2[Yld < 3000 | n_deep_v5 > 200 | n_deep_v5_max > 900]
  
  remove_this <- filter_dt_in_dt(yc_yearly_dt2a, filter_dt = qc_control[,.(id_10, mukey)])
  yc_yearly_dt2b <- yc_yearly_dt2a[-remove_this]
  length(unique(yc_yearly_dt2b$id_10))
  
  #------------------------------------------------------------------------------------
  # REMOVE MUKEYS THAT DID NOT HAVE ALL THE RUNS (ROTATIONS OR Z ARE MISSING)
  missing_ids_dt <- yc_yearly_dt2b[,.N, by = .(id_10, mukey, z)][N > 25] #at least x rates
  missing_ids_dt2 <- missing_ids_dt[,.N, by = .(id_10, mukey)][N >= 15][,ok := 1] #at least x z
  
  # missing_ids_dt3 <- missing_ids_dt2[,.N, by = .(id_10, mukey)][N ==2][,-'N'][,ok := 1] #both prev_crop
  
  length(unique(missing_ids_dt2$id_10))
  
  yc_yearly_dt2c <- merge(yc_yearly_dt2b, missing_ids_dt2, by =c('id_10', 'mukey'), all.x = T) %>% 
    .[!is.na(ok)] %>% .[,-'ok']
  length(unique(yc_yearly_dt2c$id_10))
  
  #------------------------------------------------------------------------------------
  # CHECK FIELDS AREAS AFTER CLEANING AND REMOVE FIELDS < 35 HA
  
  grid10_soils_dt4 <- readRDS("./vr_value_v2/Data/Grid/grid10_soils_dt4.rds")
  grid10_soils_dt4[,id_10_mukey := paste(id_10, mukey, sep = '_')]
  
  good_to_go_dt <- yc_yearly_dt2c[,.(id_10, mukey)] %>% unique() %>% .[ ,id_10_mukey := paste(id_10, mukey, sep = '_')]
  
  #remove missing soils
  grid10_soils_dt5 <- grid10_soils_dt4[id_10_mukey %in% unique(good_to_go_dt$id_10_mukey),]
  
  #recalculate areas
  # full_fields_dt <- data.table(grid10_soils_dt5, st_coordinates(st_centroid(grid10_soils_sf5))) %>% .[,-'geometry']
  # setnames(full_fields_dt, c('X', 'Y'), c('long', 'lat'))
  
  full_fields_dt <- grid10_soils_dt5[, .(area_ha = sum(area_ha),
                                   long = mean(long),
                                   lat = mean(lat)),
                                   by = .(id_10, id_field, mukey)]
  
  full_fields_dt[,field_area_ha := sum(area_ha), by = .(id_10, id_field)]
  full_fields_dt[,field_soils_cnt := length(unique(mukey)), by = .(id_10, id_field)]
  length(unique(full_fields_dt$id_10))
  
  hist(full_fields_dt$field_area_ha[full_fields_dt$field_area_ha < 30])
  
  full_fields_dt2 <- full_fields_dt[field_area_ha > 30]
  length(unique(full_fields_dt2$id_10))
  full_fields_dt2[,id_10_field := paste(id_10, id_field, sep = '_')] #to clean the spatial files
  full_fields_dt2[ ,id_10_mukey := paste(id_10, mukey, sep = '_')] #to clean the yc file
  
  #------------------------------------------------------------------------------------
  #CLEAN ALL THE FILES BASED ON THE QUALITY CONTROL AND THE AREA REQUIREMENTS
  #remove fields that did not get 35 ha
  grid10_soils_sf5 <- grid10_soils_sf5 %>% dplyr::mutate(id_10_field = paste(id_10, id_field, sep = '_'))
  grid10_soils_sf5 <- grid10_soils_sf5[grid10_soils_sf5$id_10_field %in% unique(full_fields_dt2$id_10_field),]
  nrow(grid10_soils_sf5)
  grid10_soils_sf5 <- grid10_soils_sf5 %>% dplyr::select(-c('id_10_mukey', 'id_10_field'))
  saveRDS(grid10_soils_sf5, "./vr_value_v2/Data/Grid/grid10_soils_sf5.rds") 
  
  #in the yc_yearly we need to remove those mukeys corresponding to fields that did not get the 35 ha
  yc_yearly_dt2c[ ,id_10_mukey := paste(id_10, mukey, sep = '_')]
  yc_yearly_dt2d <- yc_yearly_dt2c[id_10_mukey %in% unique(full_fields_dt2$id_10_mukey),][,-'id_10_mukey']
  length(unique(yc_yearly_dt2d$id_10))
}

# saveRDS(yc_yearly_dt2d, "./vr_value_v2/Data/files_rds/yc_yearly_dt2d.rds")

#======================================================================================
# GET REGIONS - they will have the same amount of corn cells on them
if(FALSE){
  grid10_tiles_sf <- readRDS("./vr_value_v2/Data/Grid/grid10_tiles.sf5.rds") 
  
  tm_shape(grid10_tiles_sf) + tm_polygons("county_name")
  
  grid10_tiles_sf <- st_transform(grid10_tiles_sf, 4326)
  regions_dt <- data.table(grid10_tiles_sf[, c('id_10', 'corn5_cell')], st_coordinates(st_centroid(grid10_tiles_sf))) %>% .[,-'geometry']
  setnames(regions_dt, c('X', 'Y'), c('long', 'lat'))
  lat_min <- min(regions_dt$lat)
  lat_max <- max(regions_dt$lat)
  regions_dt[, region := .bincode(regions_dt$lat, breaks=c(lat_min-1, quantile(rep(regions_dt$lat, regions_dt$corn5_cell), c(0.333333, 0.66666)), lat_max +1))]
  regions_dt[,.(corn5_cell = sum(corn5_cell)), by = region]
  
  #----------
  
  # Add the region to both spatial files
  grid10_tiles_sf2 <- left_join(grid10_tiles_sf, regions_dt[,.(id_10, region)], by = 'id_10')
  saveRDS(grid10_tiles_sf2, "./vr_value_v2/Data/Grid/grid10_tiles_sf2.rds") 
  grid10_soils_sf6 <- left_join(grid10_soils_sf5, regions_dt[,.(id_10, region)], by = 'id_10')
  saveRDS(grid10_soils_sf6, "./vr_value_v2/Data/Grid/grid10_soils_sf6.rds") 
}

grid10_tiles_sf2 <- readRDS("./vr_value_v2/Data/Grid/grid10_tiles_sf2.rds") 
grid10_soils_sf6 <- readRDS("./vr_value_v2/Data/Grid/grid10_soils_sf6.rds") 

tm_shape(grid10_tiles_sf2) + tm_polygons("region")

#======================================================================================
# ADD GEOGRAPHIC INFORMATION 
grid10_soils_dt <- data.table(grid10_soils_sf6, st_coordinates(st_centroid(grid10_soils_sf6))) %>% .[,-'geometry']
setnames(grid10_soils_dt, c('X', 'Y'), c('long', 'lat'))

#For now, each mukey will have the mean lat and long in the id_10. If it is repeated in both fields, we average the location.
grid10_soils_dt2 <- grid10_soils_dt[,.(area_ha = sum(area_ha),
                   region = mean(region), 
                   long = mean(long), lat = mean(lat)), by = .(id_tile, id_10, state_name, county_name, corn5_tile, musym, mukey)] #TO DO: lat long mean by id_10

table(grid10_soils_dt2$region)

grid10_soils_dt2[,.N, by = mukey] #mukeys that are in more than one id_10

yc_yearly_dt3 <- merge(yc_yearly_dt2d, grid10_soils_dt2[,.(id_10, mukey,  area_ha, region, long, lat)], by = c('id_10', 'mukey'))

#======================================================================================
# ADD LONG TERM YIELD
long_term_yld_dt <- yc_yearly_dt3[,.(Yld = max(Yld)), by = .(id_10, mukey,z, prev_crop)] %>%
                                 .[, .(Yld_lt_avg = mean(Yld),
                                       Yld_lt_min = min(Yld),
                                       Yld_lt_max = max(Yld)), by = .(id_10, mukey)]
hist(long_term_yld_dt$Yld_lt_avg)
# eonr_mukey_dt2 <- merge(eonr_mukey_dt2, long_term_yld_dt, by = c('id_10', 'mukey'))
# saveRDS(eonr_mukey_dt2, "./vr_value_v2/Data/files_rds/eonr_mukey_dt2.rds")

yc_yearly_dt3 <- merge(yc_yearly_dt3, long_term_yld_dt, by = c('id_10', 'mukey'))

yc_yearly_dt3 <- yc_yearly_dt3[,-c('sim_name', 'type', 'day_v5', 'year') ]

saveRDS(yc_yearly_dt3, "./vr_value_v2/Data/files_rds/yc_yearly_dt3.rds")

region_link <- data.table(grid10_soils_sf6) %>% .[,.(id_10, region)] %>% unique()
full_fields_dt2 <- merge(full_fields_dt2, region_link, by = 'id_10')
saveRDS(full_fields_dt2, "./vr_value_v2/Data/files_rds/full_fields_dt2.rds")

