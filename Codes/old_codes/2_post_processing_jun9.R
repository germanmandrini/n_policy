# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
# setwd('~')

source('./Codes_useful/R.libraries.R')
# source('./Codes_useful/gm_functions.R')
source('./vr_value/Codes/parameters.R')
source('./Codes_useful/gm_functions.R')

#======================================================================================
#LOAD YEARLY DATA AND OBTAIN THE EONR DATA SET
if(FALSE){
  yc_yearly_dt <- readRDS('./vr_value/Data/files_rds/yc_yearly_dt.rds')
  yc_yearly_dt[,rotation := ifelse(rotation == 'MSM', 0, 1)]

  yc_yearly_dt[, P := Yld * Pc - N_fert * Pn]
  # yc_yearly_dt[, P := Yld * Pc - N_fert * Pn - leach_n2 * Pe]
  eonr_mukey_dt <- yc_yearly_dt[, .SD[ P == max( P)], by = .(id_10, mukey, z, rotation)]
  setnames(eonr_mukey_dt, 'N_fert', 'eonr')
  nrow(eonr_mukey_dt) * length(unique(yc_yearly_dt$N_fert)) == nrow(yc_yearly_dt) #some simulations didnt run all the rates?
  saveRDS(eonr_mukey_dt, "./vr_value/Data/files_rds/eonr_mukey_dt.rds")
}  

  #======================================================================================
if(FALSE){ #only execute with base analysis
  #------------------------------------------------------------------------------------
  # REMOVE MUKEYS THAT DID NOT HAVE ALL THE RUNS (ROTATIONS OR Z ARE MISSING)
  missing_ids_dt <- yc_yearly_dt[,.N, by = .(id_10, mukey)][N < 1640] #10 is fine to be missing
  
  #------------------------------------------------------------------------------------
  
  # QC
  #Remove low yielding mukeys (farmers will not plant here)
  #run only for the regular eonr. Clean the same mukeys accross different apporaches
  qc_control <- eonr_mukey_dt[,.(Yld = mean(Yld), 
                                 P = mean(P),
                                 n_deep_v5 = mean(n_deep_v5),
                                 n_deep_v5_max = max(n_deep_v5)), by = .(id_10, mukey)]
  summary(qc_control$Yld)
  hist(qc_control$Yld)
  hist(qc_control$n_deep_v5)
  summary(qc_control$n_deep_v5)
  summary(qc_control$n_deep_v5_max)
  hist(qc_control[n_deep_v5 > 300]$n_deep_v5)
  
  qc_control2 <- qc_control[Yld < 3000 | n_deep_v5 > 200 | n_deep_v5_max > 400]
  qc_control2 <- rbind(qc_control2[,.(id_10, mukey)], missing_ids_dt[,.(id_10, mukey)]) %>% unique()
  
  saveRDS(qc_control2, './vr_value/Data/files_rds/qc_control2.rds')
}


#CLEAN ALL THE FILES AFTER QC

qc_control2 <- readRDS('./vr_value/Data/files_rds/qc_control2.rds')

remove_this <- filter_dt_in_dt(yc_yearly_dt, filter_dt = qc_control2[,.(id_10, mukey)])
yc_yearly_dt2 <- yc_yearly_dt[-remove_this]
saveRDS(yc_yearly_dt2, "./vr_value/Data/files_rds/yc_yearly_dt2.rds")

remove_this <- filter_dt_in_dt(eonr_mukey_dt, filter_dt = qc_control2[,.(id_10, mukey)])
eonr_mukey_dt2 <- eonr_mukey_dt[-remove_this]
saveRDS(eonr_mukey_dt2, "./vr_value/Data/files_rds/eonr_mukey_dt2.rds")

grid10_soils_sf4 <- readRDS("./vr_value/Data/Grid/grid10_soils_sf4.rds")  
grid10_soils_sf4 <- grid10_soils_sf4 %>% dplyr::mutate(id_10_mukey = paste(id_10, mukey, sep = '_'))
eonr_mukey_dt2[ ,id_10_mukey := paste(id_10, mukey, sep = '_')]
grid10_soils_sf5 <- grid10_soils_sf4[grid10_soils_sf4$id_10_mukey %in% unique(eonr_mukey_dt2$id_10_mukey),]
saveRDS(grid10_soils_sf5, "./vr_value/Data/Grid/grid10_soils_sf5.rds") 


#======================================================================================
# GET REGIONS - they will have the same amount of corn cells on them
if(FALSE){
  grid10_tiles_sf <- readRDS("./vr_value/Data/Grid/grid10_tiles.sf5.rds") 
  
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
  saveRDS(grid10_tiles_sf2, "./vr_value/Data/Grid/grid10_tiles_sf2.rds") 
  grid10_soils_sf6 <- left_join(grid10_soils_sf5, regions_dt[,.(id_10, region)], by = 'id_10')
  saveRDS(grid10_soils_sf6, "./vr_value/Data/Grid/grid10_soils_sf6.rds") 
}
grid10_tiles_sf2 <- readRDS("./vr_value/Data/Grid/grid10_tiles_sf2.rds") 
grid10_soils_sf6 <- readRDS("./vr_value/Data/Grid/grid10_soils_sf6.rds") 

tm_shape(grid10_tiles_sf2) + tm_polygons("region")

#======================================================================================
# ADD GEOGRAPHIC INFORMATION 
grid10_soils_dt <- data.table(grid10_soils_sf6, st_coordinates(st_centroid(grid10_soils_sf6))) %>% .[,-'geometry']
setnames(grid10_soils_dt, c('X', 'Y'), c('long', 'lat'))
grid10_soils_dt <- grid10_soils_dt[id_10 %in% eonr_mukey_dt$id_10]

#For now, each mukey will have the mean lat and long in the id_10. If it is repeated in both fields, we average the location.
grid10_soils_dt2 <- grid10_soils_dt[,.(area_ha = sum(area_ha),
                   region = mean(region), 
                   long = mean(long), lat = mean(lat)), by = .(id_tile, id_10, state_name, county_name, corn5_tile, musym, mukey)] #TO DO: lat long mean by id_10

table(grid10_soils_dt2$region)

grid10_soils_dt2[,.N, by = mukey] #mukeys that are in more than one id_10

eonr_mukey_dt3 <- merge(eonr_mukey_dt2, grid10_soils_dt2[,.(id_10, state_name, county_name,  mukey,  area_ha, region, long, lat)], by = c('id_10', 'mukey'))

#======================================================================================
# ADD LONG TERM YIELD
long_term_yld_dt <- eonr_mukey_dt3[, .(Yld_lt_avg = mean(Yld),
                                       Yld_lt_min = min(Yld),
                                       Yld_lt_max = max(Yld)), by = .(id_10, mukey)]
hist(long_term_yld_dt$Yld_lt_avg)
eonr_mukey_dt3 <- merge(eonr_mukey_dt3, long_term_yld_dt, by = c('id_10', 'mukey'))
eonr_mukey_dt3[,rotation := ifelse(rotation == 'MSM', 0, 1)]
saveRDS(eonr_mukey_dt3, "./vr_value/Data/files_rds/eonr_mukey_dt3.rds")


