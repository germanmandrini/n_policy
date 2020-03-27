# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
# setwd('~')

source('./Codes_useful/R.libraries.R')
source('./vr_value/Codes/parameters.R')
source('./vr_value/Codes/interpolate_parallel.R')
source('./vr_value/Codes/getfield_loop.R')
# source('./Codes_useful/gm_functions.R')

grid10_soils_sf4 <- readRDS("./vr_value/Data/Grid/grid10_soils_sf4.rds")  
# grid10_horizons_v2_dt <- readRDS("./vr_value/Data/Grid/grid10_horizons_v2_dt.rds")
# all_locs_weather_dt <- readRDS('./vr_value/Data/met_files/all_locs_weather_dt.rds')

#----------------------------------------------------------------------------
# GET REGIONS - THEY WILL BE THE BATCHES
grid10_tiles_sf <- readRDS("./vr_value/Data/Grid/grid10_tiles.sf5.rds") 
tm_shape(grid10_tiles_sf) + tm_polygons("county_name")

grid10_tiles_sf <- st_transform(grid10_tiles_sf, 4326)
regions_dt <- data.table(id_10 =grid10_tiles_sf$id_10, st_coordinates(st_centroid(grid10_tiles_sf))) 
lat_min <- min(regions_dt$Y)
step = (max(regions_dt$Y) - min(regions_dt$Y))/3
lat_max <- max(regions_dt$Y)
regions_dt[, region := .bincode(regions_dt$Y, breaks=c(lat_min-1, lat_min + step, lat_min + 2 * step, lat_max +1))]

# Add the rgion to both spatial files
grid10_tiles_sf2 <- left_join(grid10_tiles_sf, regions_dt[,.(id_10, region)], by = 'id_10')
saveRDS(grid10_tiles_sf2, "./vr_value/Data/Grid/grid10_tiles_sf2.rds") 
grid10_soils_sf5 <- left_join(grid10_soils_sf4, regions_dt[,.(id_10, region)], by = 'id_10')
saveRDS(grid10_soils_sf5, "./vr_value/Data/Grid/grid10_soils_sf5.rds") 

#----------------------------------------------------------------------------
# PROCESS BY BATCH
runned <- list.files('./vr_value/Data/yc_output_yearly/', pattern = 'ic_cell', full.names = T)

# runned <- sample(runned, 100, replace = F)

for(reg_n in sort(unique(regions_dt$region))){
  # reg_n = sort(unique(regions_dt$region))[3]
  print(reg_n)
  runned_batch <- runned[str_extract(string = runned, pattern = '[0-9]+') %in% regions_dt[region == reg_n]$id_10]
  
  # Open all files
  ic_ls <- list()
  for(file_n in runned_batch){
    # file_n <- runned[1]
    ic_ls[[length(ic_ls)+1]] <- readRDS(file_n)
  }
  
  ic_dt <- rbindlist(ic_ls)
  
  #FIND the initial conditions
  ic_dt1 <- ic_dt[month == 1, .(id_10,  z,  mukey, no3_1, no3_2, no3_3, no3_4, nh4_1, nh4_2, nh4_3, nh4_4) ]
  ic_dt1 <- data.table(unique(ic_dt1))
  ic_dt1[,n_40cm := no3_1 + no3_2 + no3_3 + no3_4 + nh4_1 + nh4_2 + nh4_3 + nh4_4]
  ic_dt1 <- ic_dt1[, .(id_10,  z,  mukey, n_40cm)]
  
  unique(ic_dt1[n_40cm > 80]$id_10)
  hist(ic_dt1$n_40cm)
  
  ic_dt2 <- ic_dt[month == 12] %>% .[, Y := Y_dry / 0.85] %>% .[, .(id_10,  z,  mukey, NRate, Y, leach_no3_ss)]
  # ic_dt2 <-ic_dt2[Y > 0]
  
  #INTERPOLATE BETWEEN RATES USING LOESS TO ALLOW SOLVING EONR NUMERICALY 
  
  ic_dt2[,sets := paste(id_10, mukey, z, sep = '_')]
  ic_dt2 <- ic_dt2[order(sets, NRate)]
  
  interpolated_dt <- interpolate_paralell(data_dt = ic_dt2, column_sets = 'sets')
  
  #OBS VS INTERPOLATED
  if(FALSE){
    
    setnames(interpolated_dt, c('Y', 'leach_no3_ss'), c('Y_int', 'leach_int'))
    setnames(ic_dt2, c('Y', 'leach_no3_ss'), c('Y_obs', 'leach_obs'))
    
    ic_dt2 <- merge(ic_dt2, interpolated_dt, by = c('id_10',  'z',   'mukey', 'NRate'))
    
    ggplot(data=ic_dt2, aes(x = Y_obs, y = Y_int)) +
      geom_point() + coord_fixed() + geom_abline() + #ylim(8000, 18000)+ xlim(8000, 18000) +
      theme(aspect.ratio=1, 
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))+
      ggtitle('Interpolated vs Observed') +
      theme_bw()
    
    ggplot(data=ic_dt2, aes(x = leach_obs, y = leach_int)) +
      geom_point() + coord_fixed() + geom_abline() + #ylim(8000, 18000)+ xlim(8000, 18000) +
      theme(aspect.ratio=1, 
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))+
      ggtitle('Interpolated vs Observed') +
      theme_bw()
    
    ic_dt2[,Y_diff := abs(Y_obs - Y_int)]
    
    set_plot <- ic_dt2[order(-Y_diff)][1, ]$sets
    set_plot <- unique(ic_dt2$sets)[160]
    
    ggplot() +
      geom_point(data = ic_dt2[sets == set_plot], aes(x = NRate, y = Y_obs)) + 
      geom_path(data = interpolated_dt[ sets == set_plot], aes(x = NRate, y = Y_int))+
      ggtitle('Interpolated vs Observed') +
      theme_bw()
    
    #go back to the original names
    setnames(interpolated_dt, c('Y_int', 'leach_int'), c('Y', 'leach_no3_ss')) 
    setnames(ic_dt2, c('Y_obs', 'leach_obs'), c('Y', 'leach_no3_ss'))
  }
  
  #GET THE AREA
  areas_dt <- data.table(grid10_soils_sf5) %>% .[, .(area_ha = sum(area_ha)), by = .(id_10, mukey)]
  
  
  #----------------------------------------------------------------------------
  #MERGE EVERYTHING
  interpolated_dt <- merge(interpolated_dt, ic_dt1, by = c('id_10', 'z', 'mukey'))
  interpolated_dt <- merge(interpolated_dt, regions_dt[,.(id_10, region)],  by = c('id_10'))
  interpolated_dt <- merge(interpolated_dt, areas_dt,  by = c('id_10', 'mukey'))
  
  #Split by field
  # keep <- c('keep', 'get_field','interpolated_dt', 'grid10_soils_sf4')
  # rm(list = ls()[!ls() %in% keep])
  
  # interpolated_dt <- get_field_loop(interpolated_dt, grid10_soils_sf4)
  
  
  # VARIABLES CALCULATION
  interpolated_dt[Y < 0, Y := 0]
  interpolated_dt[, P := Y * Pc - NRate * Pn]
  interpolated_dt[, P_gross := P * area_ha]
  interpolated_dt[, Y_gross := Y * area_ha]
  interpolated_dt[, n_40cm_gross := n_40cm * area_ha]
  interpolated_dt[, leach_no3_ss_gross := leach_no3_ss * area_ha]
  
  # SAVE CLEANED DATA
  saveRDS(interpolated_dt, paste0('./vr_value/Data/files_rds/interpolated_', reg_n, '_dt.rds'))
    
}

