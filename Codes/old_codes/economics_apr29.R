# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
# setwd('~')

source('./Codes_useful/R.libraries.R')
source('./vr_value/Codes/parameters.R')
# source('./Codes_useful/gm_functions.R')

grid10_soils_sf4 <- readRDS("./vr_value/Data/Grid/grid10_soils_sf4.rds")  
# grid10_horizons_v2_dt <- readRDS("./vr_value/Data/Grid/grid10_horizons_v2_dt.rds")
# all_locs_weather_dt <- readRDS('./vr_value/Data/met_files/all_locs_weather_dt.rds')

#----------------------------------------------------------------------------
# MAKE SOME MAPS AND CHOOSE THE TILES
grid10_tiles_sf <- readRDS("./vr_value/Data/Grid/grid10_tiles.sf5.rds") 
tm_shape(grid10_tiles_sf) + tm_polygons("county_name")
length(unique(grid10_tiles_sf$id_10)) * 8 / 60 / 24

#----------------------------------------------------------------------------
# REGIONAL RATES (SIMILAR TO MRTN)
#----------------------------------------------------------------------------
runned <- list.files('./vr_value/Data/yc_output/', pattern = 'ic_cell', full.names = T)

if(FALSE){
  # Open all files
  ic_ls <- list()
  for(file_n in runned){
    # file_n <- runned[1]
    ic_ls[[length(ic_ls)+1]] <- readRDS(file_n)
  }
  ic_dt <- rbindlist(ic_ls)
  
  ic_dt <- ic_dt[month == 12, .(id_10,  z,  mukey, Y_dry, NRate)]
    
  #Add the area and region
  grid10_tiles_sf <- st_transform(grid10_tiles_sf, 4326)
  regions_dt <- data.table(id_10 =grid10_tiles_sf$id_10, st_coordinates(st_centroid(grid10_tiles_sf))) 
  
  area_dt <- data.table(grid10_soils_sf5) %>% .[,.(area_ha = sum(area_ha)), by = .(mukey, id_10)]
  
  summary(area_dt[,.(area_ha = sum(area_ha)), by = .(id_10)]$area_ha)
  
  lat_min <- min(regions_dt$Y)
  step = (max(regions_dt$Y) - min(regions_dt$Y))/3
  lat_max <- max(regions_dt$Y)
  
  regions_dt[, region := .bincode(regions_dt$Y, breaks=c(lat_min-1, lat_min + step, lat_min + 2 * step, lat_max +1))]
  
  ic_dt <- merge(ic_dt, area_dt[, .(mukey, id_10, area_ha)], by = c('mukey', 'id_10'))
  ic_dt <- merge(ic_dt, regions_dt[,.(id_10, region)],  by = c('id_10'))
  
  #Economics
  
  ic_dt[, Y := Y_dry / 0.85]
  ic_dt[, R := Y * Pc]
  ic_dt[, N_cost := NRate * Pn]
  ic_dt[, P := R - N_cost]
  
  ic_dt[, P_total := P * area_ha]
  rates_reg_dt <- ic_dt[, .(P_ha = sum(P_total)/sum(area_ha)), by = .(region, NRate)]
  
  (p <- ggplot(data=rates_reg_dt, aes(x = NRate, y = P_ha, colour = as.factor(region))) +
    geom_point(size = 3) +
    stat_smooth(se = FALSE) +
    scale_color_discrete(name = "REGION") +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14))+
    ggtitle(paste('Regional Profits'))+    
    theme_bw() +
    theme(legend.position='bottom',
          panel.grid = element_blank(),
          strip.background = element_blank(),
          legend.text.align = 0,
          #legend.title = element_blank(),
          strip.text = element_blank()))
  
  ggsave(p, filename = "./vr_value/Data/figures/regional_rates.jpg")
  
  rates_reg_dt <- rates_reg_dt[, .SD[ P_ha == max(P_ha)], by = region][,-'P_ha']
  rates_reg_dt <- merge(unique(regions_dt[,.(id_10, region)]), rates_reg_dt)
  
  saveRDS(rates_reg_dt, './vr_value/Data/files_rds/regional_rates.rds')
}
rates_reg_dt <- readRDS('./vr_value/Data/files_rds/regional_rates.rds')


#----------------------------------------------------------------------------
id_10_runned <- as.numeric(unlist(str_extract_all(runned, pattern = '[0-9]+')))
fields_seq <- data.table(grid10_soils_sf4) %>% .[id_10 %in% id_10_runned, c('id_10', 'id_field')] %>% unique()

economics_ls <- list()

for(j in 1:nrow(fields_seq)){
  # j = 50
  print(j)
  fields_seq_tmp <- fields_seq[j,]
  one_field_sf <- grid10_soils_sf4[grid10_soils_sf4$id_10 == fields_seq_tmp$id_10 &
                                     grid10_soils_sf4$id_field == fields_seq_tmp$id_field,]
  
  area_dt <- data.table(one_field_sf) %>% .[,.(area_ha = sum(area_ha)), by = mukey]
  
  #---------------------------------------------------------------------------
  #MAKE A MAP OF THE FIELD
  if(FALSE){
  field <- tm_shape(one_field_sf) + tm_polygons("mukey") + 
    tm_layout(legend.text.size = 0.7,
              main.title = paste('ONE FIELD MAP -', round(sum(one_field_sf$area_ha),1),' ha'),
              main.title.position = "center",
              main.title.size = 1)
  tmap_save(field, filename = "./vr_value/Data/figures/field.jpg", scale = 2)  
  }
  #---------------------------------------------------------------------------
  # LOAD OUTPUT
  ic_field_dt <-readRDS(paste0('./vr_value/Data/yc_output/ic_cell', fields_seq_tmp$id_10,'.rds'))
  
  ic_field_dt <- ic_field_dt[mukey %in% unique(one_field_sf$mukey)][month == 12, .(id_10,  z,  mukey, Y_dry, NRate)]
  ic_field_dt <- merge(ic_field_dt, area_dt)
  
  summary(ic_field_dt[,.(area_ha = sum(area_ha)),  by = .(id_10, z, NRate)]$area_ha)
  
  #---------------------------------------------------------------------------
  # ECONOMICS
  ic_field_dt[, Y := Y_dry / 0.85]
  ic_field_dt[, R := Y * Pc]
  ic_field_dt[, N_cost := NRate * Pn]
  ic_field_dt[, P := R - N_cost]
  # ic_field_dt[, P_total := P * area_ha]
  
  # SHOW YIELD CURVES
  if(FALSE){
  ggplot(data=ic_field_dt, aes(x = NRate, y = Y, color = z)) +
    geom_point(size = 3) +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"))+
    # theme_bw() +
    facet_grid(mukey~.) +  
    ggtitle(paste('Y Response for one mukey'))
  
  ggplot(data=ic_field_dt, aes(x = NRate, y = Y, color = mukey)) +
    #geom_point(size = 3) +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"))+
    ggtitle(paste('Summarized Profits Response by mukey'))
  }
  #---------------------------------------------------------------------------
  # PROFITS USING REGIONAL RECOMMENDATION
  
  
  
  n_regional <- rates_reg_dt[id_10 == fields_seq_tmp$id_10]$NRate
  n_regional_dt <- ic_field_dt[NRate == n_regional]
  n_regional_dt[,c_app := c_ura_ha]
  n_regional_dt[,P2 := P - c_app]
  n_regional_dt[,P2_mukey := P2 * area_ha]
  n_regional_dt2 <- n_regional_dt[,.(P_reg = sum(P2_mukey)), by = .(z)] #total field by z
  # n_regional_dt3 <- n_regional_dt2[,.(P_reg = mean(P2_field))]
 
  
  #---------------------------------------------------------------------------
  # EX ANTE UR 
  
  rates <- ic_field_dt[,.(P = sum(P)), by =  .(NRate)][, .SD[ P == max( P)]][,-'P']
  setnames(rates, 'NRate', 'eonr')
  
  eonr_ur_dt <- cbind(ic_field_dt, rates) %>% .[NRate == eonr] %>% .[ ,-'NRate']
  eonr_ur_dt[,c_app := c_ura_ha]
  eonr_ur_dt[,P2 := P - c_app]
  eonr_ur_dt[,P2_mukey := P2 * area_ha]
  eonr_ur_dt2 <- eonr_ur_dt[,.(P_ur = sum(P2_mukey)), by = .(z)] #total field by z
  # eonr_ur_dt3 <- eonr_ur_dt2[,.(P_ur = mean(P2_field))]
  
  #---------------------------------------------------------------------------
  # EX ANTE VR 
  rates <- ic_field_dt[,.(P = sum(P)), by =  .(mukey, NRate)][, .SD[ P == max( P)], by = mukey][,-'P']
  setnames(rates, 'NRate', 'eonr')
  
  eonr_vr_dt <- merge(ic_field_dt, rates) %>% .[NRate == eonr] %>% .[ ,-'NRate']
  eonr_vr_dt[,c_app := c_vra_ha]
  eonr_vr_dt[,P2 := P - c_app]
  eonr_vr_dt[,P2_mukey := P2 * area_ha] #each mukey using the respective eonr by z
  eonr_vr_dt2 <- eonr_vr_dt[,.(P_vr = sum(P2_mukey)), by = .(z)] #total field by z
  # eonr_vr_dt3 <- eonr_vr_dt2[,.(P_vr = mean(P2_field))]
  
  #---------------------------------------------------------------------------
  # MAKE A DT
  economics_field_dt <- merge(n_regional_dt2, eonr_ur_dt2) %>% merge(eonr_vr_dt2)
  economics_field_dt[,val_info := P_ur - P_reg]
  economics_field_dt[,val_tech := P_vr - P_ur]
  economics_field_dt <- cbind(fields_seq_tmp, economics_field_dt)
  economics_field_dt[,val_info_ha := val_info/sum(area_dt$area_ha)]
  economics_field_dt[,val_tech_ha := val_tech/sum(area_dt$area_ha)]
  
  #---------------------------------------------------------------------------
  # MAKE BOXPLOT
  dat.m <- melt(economics_field_dt,id.vars= c('id_10', 'id_field',  'z'), measure.vars=c('val_info_ha', 'val_tech_ha'))
  if(FALSE){
  (p <- ggplot(dat.m) +
    geom_boxplot(aes(x=variable, y=value, color=variable)) +
      # scale_color_discrete(name = "REGION") +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14))+
      ggtitle(paste('Value of information and technology - ', fields_seq_tmp$id_10))+    
      theme_bw() +
      theme(legend.position='bottom',
            panel.grid = element_blank(),
            strip.background = element_blank(),
            legend.text.align = 0,
            legend.title = element_blank(),
            strip.text = element_blank()))
  ggsave(p, filename = "./vr_value/Data/figures/value_boxplot.jpg")
  }
  
  economics_ls[[j]] <- economics_field_dt
}

economics_dt <- rbindlist(economics_ls)

val_map_dt <- economics_dt[,.(val_info_ha = max(val_info_ha),
                val_tech_ha = max(val_tech_ha)), by = id_10]

grid10_value_sf <- left_join(grid10_tiles_sf, val_map_dt, by = 'id_10')

tm_shape(grid10_value_sf) + tm_polygons(c('val_info_ha', 'val_tech_ha'))
