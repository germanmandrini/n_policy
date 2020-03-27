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
  
  ic_dt <- ic_dt[month == 12] %>% .[, Y := Y_dry / 0.85] %>% .[, .(id_10,  z,  mukey, Y, NRate)]
  ic_dt
  #INTERPOLATE RATES TO ALLOW SOLVING EONR NUMERICALY
  
  ic_dt[,id10_mukey_z := paste(id_10, mukey, z, sep = '_')]
  ic_dt <- ic_dt[order(id10_mukey_z)]
  fits_interpolate <- lmList(Y ~ NRate  + I(NRate^2)| id10_mukey_z, data=ic_dt)
  
  interpolated_dt <- data.table(expand.grid(id10_mukey_z = unique(ic_dt$id10_mukey_z), NRate = as.numeric(min(ic_dt$NRate):max(ic_dt$NRate)), stringsAsFactors = F), stringsAsFactors = F) %>% 
    .[order(id10_mukey_z)]
  interpolated_dt[, c("id_10", "mukey", "z") := tstrsplit(id10_mukey_z, "_", fixed=TRUE)]
  interpolated_dt$Y <- predict(object = fits_interpolate, newdata = interpolated_dt)

  #Get region
  
  grid10_tiles_sf <- st_transform(grid10_tiles_sf, 4326)
  regions_dt <- data.table(id_10 =grid10_tiles_sf$id_10, st_coordinates(st_centroid(grid10_tiles_sf))) 
  lat_min <- min(regions_dt$Y)
  step = (max(regions_dt$Y) - min(regions_dt$Y))/3
  lat_max <- max(regions_dt$Y)
  regions_dt[, region := .bincode(regions_dt$Y, breaks=c(lat_min-1, lat_min + step, lat_min + 2 * step, lat_max +1))]
  
  #Get areas
  area_dt <- data.table(grid10_soils_sf4) %>% .[,.(area_ha = sum(area_ha)), by = .(mukey, id_10)]
  summary(area_dt[,.(area_ha = sum(area_ha)), by = .(id_10)]$area_ha)
  
  #Merge everything
  interpolated_dt$id_10 <- as.integer(interpolated_dt$id_10)
  interpolated_dt <- merge(interpolated_dt, area_dt[, .(mukey, id_10, area_ha)], by = c('mukey', 'id_10'))
  interpolated_dt <- merge(interpolated_dt, regions_dt[,.(id_10, region)],  by = c('id_10'))
  
  #Economics
  interpolated_dt[, P := Y * Pc - NRate * Pn]
  interpolated_dt[, P_total := P * area_ha]
  rates_reg_dt <- interpolated_dt[, .(P_ha = sum(P_total)/sum(area_ha)), by = .(region, NRate)]
  
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
  
  saveRDS(rates_reg_dt, './vr_value/Data/files_rds/regional_rates.rds')
  
  
  
  saveRDS(interpolated_dt[,.(id_10, mukey, z, NRate, Y, region)], './vr_value/Data/files_rds/interpolated_dt.rds')
}
rates_reg_dt <- readRDS('./vr_value/Data/files_rds/regional_rates.rds')
interpolated_dt <- readRDS('./vr_value/Data/files_rds/interpolated_dt.rds')
#----------------------------------------------------------------------------
# GET THE EONR FOR EACH SET
interpolated_dt[, P := Y * Pc - NRate * Pn]
eonr_byset_dt <- interpolated_dt[, .SD[ P == max( P)], by = .(id_10, mukey,  z)]
saveRDS(eonr_byset_dt, './vr_value/Data/files_rds/eonr_byset_dt.rds')

#----------------------------------------------------------------------------
id_10_runned <- as.numeric(unlist(str_extract_all(runned, pattern = '[0-9]+')))
fields_seq <- data.table(grid10_soils_sf4) %>% .[id_10 %in% id_10_runned, c('id_10', 'id_field')] %>% unique()

economics_ls <- list()

for(j in 1:nrow(fields_seq)){
  # j = 45
  print(j)
  fields_seq_tmp <- fields_seq[j,]
  one_field_sf <- grid10_soils_sf4[grid10_soils_sf4$id_10 == fields_seq_tmp$id_10 &
                                     grid10_soils_sf4$id_field == fields_seq_tmp$id_field,]
  
  area_dt <- data.table(one_field_sf) %>% .[,.(area_ha = sum(area_ha)), by = mukey]
  
  #---------------------------------------------------------------------------
  #MAKE A MAP OF THE FIELD
  if(FALSE){
  (field <- tm_shape(one_field_sf) + tm_polygons("mukey") + 
    tm_layout(legend.text.size = 0.7,
              main.title = paste('ONE FIELD MAP -', round(sum(one_field_sf$area_ha),1),' ha'),
              main.title.position = "center",
              main.title.size = 1))
  tmap_save(field, filename = "./vr_value/Data/figures/field.jpg", scale = 2)  
  }
  #---------------------------------------------------------------------------
  # LOAD OUTPUT
  ic_field_dt <- interpolated_dt[id_10 == fields_seq_tmp$id_10 & mukey %in% unique(one_field_sf$mukey)]
  sapply(ic_field_dt, class)
  
  ic_field_dt <- merge(ic_field_dt, area_dt, by = 'mukey')
  
  summary(ic_field_dt[,.(area_ha = sum(area_ha)),  by = .(id_10, z, NRate)]$area_ha)
  
  #---------------------------------------------------------------------------
  # ECONOMICS
  ic_field_dt[, P := Y * Pc - NRate * Pn]
  ic_field_dt[, P_mukey := P * area_ha]
  
  # SHOW YIELD CURVES
  if(FALSE){
    mukey_n = unique(ic_field_dt$mukey)[1]  
  ggplot(data=ic_field_dt[mukey == mukey_n], aes(x = NRate, y = Y, color = z)) +
    geom_point(size = 3) +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"))+
    # theme_bw() +
    facet_grid(mukey~.) +  
    ggtitle(paste('Y Response for one mukey'))
  
  (p1 <- ggplot(data=ic_field_dt[mukey == mukey_n], aes(x = NRate, y = Y)) +
      #geom_point(aes(color = z), size = 3) +
      #stat_smooth(aes(color = z), method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE) +
      geom_line(aes(color = z))+
      stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 3, se = FALSE) +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))+
      theme_bw() +
      facet_grid(mukey~.) +  
      theme(#legend.position='bottom',
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.text.align = 0,
        #legend.title = element_blank(),
        strip.text = element_blank())+
      ggtitle(paste('Y Response for one mukey')))
  
  (p2 <- ggplot(data=ic_field_dt[mukey == mukey_n], aes(x = NRate, y = P)) +
    # geom_point(aes(color = z), size = 3) +
    # stat_smooth(aes(color = z), method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE) +
      geom_line(aes(color = z))+
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 3, se = FALSE) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"))+
    theme_bw() +
    facet_grid(mukey~.) +  
    theme(#legend.position='bottom',
          panel.grid = element_blank(),
          strip.background = element_blank(),
          legend.text.align = 0,
          #legend.title = element_blank(),
          strip.text = element_blank())+
    ggtitle(paste('Profits Response for one mukey')))
  
  
  ggsave(grid.arrange(p1, p2), filename = "./vr_value/Data/figures/response_one_mukey.jpg")
  
  
  ggplot(data=ic_field_dt, aes(x = NRate, y = Y, color = mukey)) +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE) +
    theme_bw() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"),
          panel.grid = element_blank(),
          strip.background = element_blank(),
          legend.text.align = 0,
          legend.position='bottom',
          strip.text = element_blank())+
    ggtitle(paste('Summarized Y Response by mukey'))
  
  
  p3 <- ggplot(data=ic_field_dt, aes(x = NRate, y = Y, color = mukey)) +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE) +
    theme_bw() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"),
          panel.grid = element_blank(),
          strip.background = element_blank(),
          legend.text.align = 0,
          legend.position='bottom',
          strip.text = element_blank())+
    ggtitle(paste('Summarized Y Response by mukey'))
  
  ggsave(p3, filename = "./vr_value/Data/figures/response_all_mukey_averaged_for_z.jpg")
  }
  #---------------------------------------------------------------------------
  # PROFITS USING REGIONAL RECOMMENDATION
  rates <- rates_reg_dt[region == unique(ic_field_dt$region)]$NRate
  n_regional_dt <- ic_field_dt[NRate == rates]
  n_regional_dt2 <- n_regional_dt[,.(P_reg = sum(P_mukey)), by = .(z)] #total field by z

  #---------------------------------------------------------------------------
  # EX ANTE UR 
  rates <- ic_field_dt[,.(P = sum(P_mukey)), by =  .(NRate)][, .SD[ P == max( P)]]
  setnames(rates, 'NRate', 'eonr')
  
  eonr_ur_dt <- cbind(ic_field_dt, rates[,-'P']) %>% .[NRate == eonr] %>% .[ ,-'NRate']
  sum(eonr_ur_dt$P_mukey) == rates$P #check
  eonr_ur_dt2 <- eonr_ur_dt[,.(P_ur = sum(P_mukey)), by = .(z)] #total field by z
  
  #---------------------------------------------------------------------------
  # EX ANTE VR 
  rates <- ic_field_dt[,.(P = mean(P)), by = .(mukey, NRate)][, .SD[ P == max( P)], by = mukey] %>% 
    setnames('NRate', 'eonr') 
  eonr_vr_dt <- merge(ic_field_dt, rates[,-'P']) %>% .[NRate == eonr] %>% .[ ,-'NRate']
  eonr_vr_dt[,.(P = mean(P)), by = mukey]$P == rates$P #check
  eonr_vr_dt2 <- eonr_vr_dt[,.(P_vr = sum(P_mukey)), by = .(z)] #total field by z
  
  #---------------------------------------------------------------------------
  # MAKE A DT
  economics_field_dt <- merge(n_regional_dt2, eonr_ur_dt2) %>% merge(eonr_vr_dt2)
  economics_field_dt <- cbind(fields_seq_tmp, economics_field_dt)
  economics_field_dt[,area_ha := sum(area_dt$area_ha)]
  economics_field_dt[,val_info := P_ur - P_reg]
  economics_field_dt[,val_tech := P_vr - P_ur]
  economics_field_dt[,val_info_ha := val_info/area_ha]
  economics_field_dt[,val_tech_ha := val_tech/area_ha]
  economics_field_dt[,mukey_count := nrow(area_dt)]
  
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
saveRDS(economics_dt, './vr_value/Data/files_rds/economics_dt.rds')

#---------------------------------------------------------------------------
# MAKE A MAP OF VALUE OF I AND T
val_map_dt <- economics_dt[,.(val_info_ha = mean(val_info_ha),
                val_tech_ha = mean(val_tech_ha)), by = id_10]

grid10_value_sf <- left_join(grid10_tiles_sf, val_map_dt, by = 'id_10')

(p <- tm_shape(grid10_value_sf) + tm_polygons(c('val_info_ha', 'val_tech_ha'))+
  tm_layout(legend.text.size = 0.7,
            main.title = paste('VALUE OF TECHNOLOGY AND INFORMATION'),
            main.title.position = "center",
            main.title.size = 1.2))

tmap_save(p, "./vr_value/Data/figures/value_t_i.jpg")
#---------------------------------------------------------------------------
# MAKE A MAP OF PROFITS
economics_dt
prof_map_dt <- economics_dt[,P_ur_ha := P_ur / area_ha]
prof_map_dt <- prof_map_dt[,.(P_ur_ha = mean(P_ur_ha)), by = id_10]

grid10_value_sf <- left_join(grid10_value_sf, prof_map_dt, by = 'id_10')

(p <- tm_shape(grid10_value_sf) + tm_polygons('P_ur_ha')+
    tm_layout(legend.text.size = 0.7,
              main.title = paste('AVERAGE UR PROFITS $/HA'),
              main.title.position = "center",
              main.title.size = 1.2))

tmap_save(p, "./vr_value/Data/figures/profits.jpg")


#---------------------------------------------------------------------------
# MAKE BOXPLOT
dat.m <- melt(economics_dt,id.vars= c('id_10', 'id_field',  'z'), measure.vars=c('val_info_ha', 'val_tech_ha'))
(p <- ggplot(dat.m) +
    geom_boxplot(aes(x=variable, y=value, color=variable)) +
    # scale_color_discrete(name = "REGION") +
    ggtitle('Value of information and technology')+    
    theme_bw() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          #legend.position='bottom',
          panel.grid = element_blank(),
          strip.background = element_blank(),
          legend.text.align = 0,
          legend.position = "none",
          legend.title = element_blank(),
          strip.text = element_blank()))

ggsave(p, filename = "./vr_value/Data/figures/value_boxplot.jpg")
