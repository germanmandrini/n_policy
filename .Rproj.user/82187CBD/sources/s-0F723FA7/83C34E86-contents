setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
# setwd('~')

source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')
# source('./vr_value_v2/Codes/parameters.R')
# source('./Codes_useful/gm_functions.R')

#======================================================================================
#DATA CLEANING & QUALITY CONTROL
if(FALSE){
  multiple_files <- list.files("./vr_value_v2/Data/yc_output_summary_1", full.names = T)
  length(multiple_files)
  
  yc_yearly_list <- list()
  for(file_n in multiple_files){
    yc_yearly_list[[length(yc_yearly_list)+1]] <- readRDS(file_n)
  }
  length(yc_yearly_list)
  yc_yearly_dt <- rbindlist(yc_yearly_list)
  yc_yearly_dt[,id_10 := as.integer(id_10)]
  saveRDS(yc_yearly_dt, './vr_value_v2/Data/files_rds/yc_yearly_dt.rds')
  
  yc_yearly_dt <- readRDS('./vr_value_v2/Data/files_rds/yc_yearly_dt.rds')
  
  length(unique(yc_yearly_dt$id_10))
  
  #------------------------------------------------------------------------------------
  # QC
  # Remove low lai_v5 (farmers would not fertilize)
  hist(yc_yearly_dt$lai_v5)
  
  ggplot(yc_yearly_dt[sample(1:nrow(yc_yearly_dt), 10000, replace = F)], aes(x = lai_v5, y = Yld)) + 
    geom_point()
  
  yc_yearly_dt2a <- yc_yearly_dt[lai_v5 >= yc_yearly_dt[Yld > 200, min(lai_v5)]] #lai thereshold = lowest lai where yld was positive
  
  # Assumption: if the lai is too low, the farmer will not apply the rate and will plant soybean
  
  #------------------------------------------------------------------------------------
  #Remove low yielding mukeys (farmers will not plant here)
  
  yearly_ymax_dt <- copy(yc_yearly_dt2a) %>% 
    .[,yld_max_mky := max(Yld), by = .(id_10, mukey, z)] %>% 
    .[Yld == yld_max_mky] %>% 
    .[, .SD[ N_fert  == min(N_fert )], by = .(id_10, mukey, z)] #filter the rate that maximized yield
  
  yearly_ymax_dt[Yld == 0]
  summary(yearly_ymax_dt)
  
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
  
  yc_yearly_dt3 <- merge(yc_yearly_dt2b, missing_ids_dt2, by =c('id_10', 'mukey'), all.x = T) %>% 
    .[!is.na(ok)] %>% .[,-'ok']
  length(unique(yc_yearly_dt3$id_10))
  
  #------------------------------------------------------------------------------------
  # CHECK FIELDS AREAS AFTER CLEANING 
  grid10_soils_dt4 <- readRDS("./vr_value_v2/Data/Grid/grid10_soils_dt4.rds")
  
  simulated_soils_dt <- yc_yearly_dt3[,.N, by = .(id_10, z, mukey)][,-'N']
  z_even = c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30)
  z_odd = z_even-1
  
  simulated_soils_field1 <- simulated_soils_dt[z %in% z_odd] %>% .[,id_field := 1 ]
  simulated_soils_field3 <- copy(simulated_soils_field1) %>% .[,id_field := 3 ]
  simulated_soils_field2 <- simulated_soils_dt[z %in% z_even] %>% .[,id_field := 2 ]
  simulated_soils_field4 <- copy(simulated_soils_field2) %>% .[,id_field := 4]
  simulated_soils_dt2 <- simulated_soils_field1 %>% 
    rbind(simulated_soils_field2) %>% 
    rbind(simulated_soils_field3) %>% 
    rbind(simulated_soils_field4) %>% .[,.N, by = .(id_10, mukey, id_field)] %>% .[,-'N'] %>%
    .[,run := 'ok']
  simulated_soils_dt2[,id_10:= as.integer(id_10)]
  
  
  grid10_soils_dt5 <- merge(grid10_soils_dt4, simulated_soils_dt2, by =c('id_10', 'id_field','mukey')) %>% 
    .[,-c('run', 'id10_field_mukey', 'field_area')]
  
  table(grid10_soils_dt5$id_field)
  table(grid10_soils_dt5$mukey_rank)
  
  #Recalculate areas
  grid10_soils_dt5[, field_ha := sum(area_ha), by = .(id_10, id_field)]
  grid10_soils_dt5[, prop_area := area_ha/field_ha]
  grid10_soils_dt5[, field_ha := 40]
  grid10_soils_dt5[,area_ha := field_ha * prop_area]
  
  table(grid10_soils_dt5[,.N, by = .(id_10, id_field, mukey)]$N)
  table(grid10_soils_dt5[,.N, by = .(id_10, id_field)]$N)
  saveRDS(grid10_soils_dt5, "./vr_value_v2/Data/Grid/grid10_soils_dt5.rds")
}

# saveRDS(yc_yearly_dt2d, "./vr_value_v2/Data/files_rds/yc_yearly_dt2d.rds")

#======================================================================================
# GET REGIONS - they will have the same amount of corn cells on them
if(FALSE){
  grid10_tiles_sf5 <- readRDS("./vr_value_v2/Data/Grid/grid10_tiles.sf5.rds") 
  # 
  # tm_shape(grid10_tiles_sf5) + tm_polygons("county_name")
  # 
  # grid10_tiles_sf5 <- st_transform(grid10_tiles_sf5, 4326)
  # regions_dt <- data.table(grid10_tiles_sf5[, c('id_10', 'corn5_cell')], st_coordinates(st_centroid(grid10_tiles_sf5))) %>% .[,-'geometry']
  # setnames(regions_dt, c('X', 'Y'), c('long', 'lat'))
  # lat_min <- min(regions_dt$lat)
  # lat_max <- max(regions_dt$lat)
  # regions_dt[, region := .bincode(regions_dt$lat, breaks=c(lat_min-1, quantile(rep(regions_dt$lat, regions_dt$corn5_cell), c(0.333333, 0.66666)), lat_max +1))]
  # regions_dt[,.(corn5_cell = sum(corn5_cell)), by = region]
  
  
  
  #----------
  # Fix the file by hand and load again
      # rid10_region <- grid10_tiles_sf7 %>% group_by(region) %>% summarize()
      # 
      # #install.packages('smoothr')
      # library(smoothr)
      # area_thresh <- units::set_units(10*10+10, km^2)
      # grid10_region2 <- fill_holes(grid10_region, threshold = area_thresh)
      # st_write(grid10_region, "./vr_value_v2/Data/shapefiles/grid10_region.shp", delete_dsn = TRUE)

  grid10_region_by_hand <- sf::read_sf('./vr_value_v2/Data/shapefiles/grid10_region_by_hand.shp')
  grid10_region_by_hand <- st_transform(grid10_region_by_hand, crs = st_crs(grid10_tiles_sf5))
  tm_shape(grid10_region_by_hand) + tm_polygons('region')
  
  # Add the region to both spatial files
  grid10_tiles_sf6 <- st_join(grid10_tiles_sf5, grid10_region_by_hand, join = st_intersects, largest = T)
  tm_shape(grid10_tiles_sf6) + tm_polygons('region')
  saveRDS(grid10_tiles_sf6, "./vr_value_v2/Data/Grid/grid10_tiles_sf6.rds") 
  regions_dt <- data.table(grid10_tiles_sf6) %>% .[,.N,.(id_10, region)] %>% .[,-'N']
  grid10_soils_dt5 <- left_join(grid10_soils_dt5[,-'region'], regions_dt[,.(id_10, region)], by = 'id_10')
  table(grid10_soils_dt5$region)
  saveRDS(grid10_soils_dt5, "./vr_value_v2/Data/Grid/grid10_soils_dt5.rds")
  
  yc_yearly_dt3 <- merge(yc_yearly_dt3, regions_dt, by = 'id_10')
  
}

grid10_tiles_sf6 <- readRDS("./vr_value_v2/Data/Grid/grid10_tiles_sf6.rds") 
grid10_soils_dt5 <- readRDS("./vr_value_v2/Data/Grid/grid10_soils_dt5.rds") 

tm_shape(grid10_tiles_sf6) + tm_polygons("region")

#======================================================================================
# ADD LONG TERM YIELD
long_term_yld_dt <- yc_yearly_dt3[,.(Yld = max(Yld)), by = .(id_10, mukey,z)] %>%
                                 .[, .(Yld_lt_avg = mean(Yld),
                                       Yld_lt_min = min(Yld),
                                       Yld_lt_max = max(Yld)), by = .(id_10, mukey)]
hist(long_term_yld_dt$Yld_lt_avg)
# eonr_mukey_dt2 <- merge(eonr_mukey_dt2, long_term_yld_dt, by = c('id_10', 'mukey'))
# saveRDS(eonr_mukey_dt2, "./vr_value_v2/Data/files_rds/eonr_mukey_dt2.rds")

yc_yearly_dt3 <- merge(yc_yearly_dt3, long_term_yld_dt, by = c('id_10', 'mukey'))

yc_yearly_dt3 <- yc_yearly_dt3[,-c('sim_name', 'day_v5', 'year') ]

# Add the region 

saveRDS(yc_yearly_dt3, "./vr_value_v2/Data/files_rds/yc_yearly_dt3.rds")

# region_link <- data.table(grid10_soils_sf6) %>% .[,.(id_10, region)] %>% unique()
# full_fields_dt2 <- merge(full_fields_dt2, region_link, by = 'id_10')
# saveRDS(full_fields_dt2, "./vr_value_v2/Data/files_rds/full_fields_dt2.rds")

