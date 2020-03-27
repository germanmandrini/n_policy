# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
# setwd('~')

source('./Codes_useful/R.libraries.R')
source('./vr_value/Codes/parameters.R')

grid10_tiles_sf2 <- readRDS("./vr_value/Data/Grid/grid10_tiles_sf2.rds") 

#----------------------------------------------------------------------------
# REGIONAL RATES (SIMILAR TO MRTN)
#---------------------------------------------------------------------------- 
# 1 - get the regional model w and w/o ss
# 2 - get a db with the eonr for each id_10_mukey_z
eonr_expost_dt <- data.table()

for(reg_n in 1:3){
  # reg_n = 1
  print(reg_n)
  # LOAD CLEANED DATA
  
  interpolated_dt <- readRDS(paste0('./vr_value/Data/files_rds/interpolated_', reg_n, '_dt.rds'))
  #-----------------------------------------------------------------------------------------------
  # Make a eonr_db
  eonr_cell_dt <- interpolated_dt[,.(P = sum(P_gross) / sum(area_ha),
                                     Y = sum(Y_gross) / sum(area_ha),
                                     leach_no3_ss = sum(leach_no3_ss_gross) / sum(area_ha),
                                     n_40cm = sum(n_40cm_gross) / sum(area_ha),
                                     area_ha = sum(area_ha)), by =  .(id_10, z, NRate)]
  summary(eonr_cell_dt$area_ha)
  
  eonr_cell_dt2 <- eonr_cell_dt[, .SD[ P == max( P)], by = .(id_10, z)]
  setnames(eonr_cell_dt2, 'NRate', 'eonr')
  
  eonr_expost_dt <- rbind(eonr_expost_dt, eonr_cell_dt2)
  
  #-----------------------------------------------------------------------------------------------
  # NO SS APPROACH
  
  min(interpolated_dt$Y)
  
  no_ss_rates_dt <- interpolated_dt[interpolated_dt$n_40cm < 80, .(P_ha = sum(P_gross)/sum(area_ha)), by = .(region, NRate)]
  
  (p <- ggplot(data=no_ss_rates_dt, aes(x = NRate, y = P_ha, colour = as.factor(region))) +
      geom_point(size = 3) +
      stat_smooth(se = FALSE) +
      scale_color_discrete(name = "REGION") +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14))+
      ggtitle(paste('Regional Profits - No SS aproach'))+    
      theme_bw() +
      theme(legend.position='bottom',
            panel.grid = element_blank(),
            strip.background = element_blank(),
            legend.text.align = 0,
            #legend.title = element_blank(),
            strip.text = element_blank()))
  
  ggsave(p, filename = "./vr_value/Data/figures/regional_rates.jpg")
  
  rates_no_ss_dt <- no_ss_rates_dt[, .SD[ P_ha == max(P_ha)], by = region][,-'P_ha']
  rates_no_ss_dt[, method := 'no_ss']
  setnames(rates_no_ss_dt, 'NRate', '(Intercept)')
  
  #-----------------------------------------------------------------------------------------------
  # SS APPROACH BINNING: it is better than a regression by soil, because it will weight more soils with largest area. 
  # in the regression, each soil is a rep, with the same weight. Values obtained are smaller than expected.
  
  interpolated_dt[, n_40cm_bin := .bincode(interpolated_dt$n_40cm, 
                                           breaks=seq(from = -1, to = (max(interpolated_dt$n_40cm)+10), by = 10))]

  profits_bybin_dt <- interpolated_dt[interpolated_dt$n_40cm < 100, .(P_ha = sum(P_gross)/sum(area_ha)), by = .(NRate, n_40cm_bin)]
  
  eonr_bybin_dt <- profits_bybin_dt[, .SD[ P_ha == max( P_ha)], by = .(n_40cm_bin)][,n_40cm := n_40cm_bin *10][order(n_40cm_bin)][,-'n_40cm_bin']
  
  setnames(eonr_bybin_dt, 'NRate', 'eonr')
  
  (p <- ggplot(eonr_bybin_dt, aes(x = n_40cm, y = eonr)) +
      geom_point(size = 1) +
      stat_smooth(method = "lm", formula = y ~ x, size = 1, se = FALSE)+
      theme_bw() +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"),
            legend.position='bottom')+
      # facet_grid(mukey~.) +  
      ggtitle(paste('SS model - Region ', reg_n)))
  
  ggsave(p, filename = paste0('./vr_value/Data/figures/ss_model_region', reg_n, '.jpg'))
  
  regression_ss <- lm(eonr ~ n_40cm, data = eonr_bybin_dt) 
  summary(regression_ss)
  

  rates_yes_ss_dt <- data.table(t(regression_ss$coefficients[1:2]))
  rates_yes_ss_dt[,region := reg_n]
  rates_yes_ss_dt[, method := 'ss']
  
  rates_reg_dt <- rbind(rates_yes_ss_dt, rates_no_ss_dt, fill = T)
  
  saveRDS(rates_reg_dt, paste0('./vr_value/Data/files_rds/rates_reg', reg_n,'_ls.rds'))
  }

saveRDS(eonr_expost_dt, './vr_value/Data/files_rds/eonr_expost_dt.rds')

#----------------------------------------------------------------------------
#DO A MAP OF THE VARIABILITY OF EONR

#Summarize variables of interest
eonr_map_dt <- eonr_expost_dt[, .(eonr = mean(eonr),
                  eonr_sd = sd(eonr),
                  P = mean( P),
                  Y = mean( Y),
                  leach_no3_ss = mean(leach_no3_ss),
                  n_40cm = mean(n_40cm)), by = id_10]


# MAKE A MAP OF EX POST EONR AND SD
grid10_eonr_sf <- left_join(grid10_tiles_sf2, eonr_map_dt, by = 'id_10')

(p <- tm_shape(grid10_eonr_sf) + tm_polygons(c('eonr','eonr_sd'), n = 20)+
    tm_layout(legend.text.size = 0.7,
              main.title = paste('EX POST EONR AND SD'),
              main.title.position = "center",
              main.title.size = 1.2))

tmap_save(p, "./vr_value/Data/figures/eonr_expost.jpg")

# MAKE A MAP OF EX POST P
(p <- tm_shape(grid10_eonr_sf) + tm_polygons(c('Y', 'P'))+
    #tm_text('county_name') +
    tm_layout(legend.text.size = 0.7,
              main.title = paste('Y-EONR AND P-EONR'),
              main.title.position = "center",
              main.title.size = 1.2))

tmap_save(p, "./vr_value/Data/figures/yeonr_expost.jpg")

(p <- tm_shape(grid10_eonr_sf) + tm_polygons(c('Y')) +
    tm_shape(counties_sf) + tm_borders(col = 'red', lwd = 2) + tm_text('county_name')+
    tm_layout(legend.text.size = 0.7,
              main.title = paste('Y-EONR AND P-EONR'),
              main.title.position = "center",
              main.title.size = 1.2))

tmap_save(p, "./vr_value/Data/figures/yeonr_expost.jpg")


#AGGREAGATED BY COUNTY
counties_sf <- grid10_eonr_sf %>% group_by(county_name) %>% summarise(eonr = mean(eonr, na.rm = T), 
                                                                      eonr_sd = sd(eonr, na.rm = T),
                                                                      P = mean(P, na.rm = T),
                                                                      Y = mean(Y, na.rm = T),
                                                                      leach_no3_ss = mean(leach_no3_ss, na.rm = T),
                                                                      n_40cm = mean(n_40cm, na.rm = T))
st_is_valid(counties_sf)
counties_sf <- st_cast(st_cast(counties_sf, 'MULTIPOLYGON'), 'POLYGON')

(p <- tm_shape(counties_sf) + tm_polygons(c('eonr', 'Y'), n = 20)+
    tm_text('county_name') +
    tm_layout(legend.text.size = 0.7,
              main.title = paste('EX POST EONR AND Y'),
              main.title.position = "center",
              main.title.size = 1.2))

tmap_save(p, "./vr_value/Data/figures/counties_yeonr_expost.jpg")

#---------------------------------------------------------------------------
# MAKE BOXPLOT
dat.m <- melt(economics_dt,id.vars= c('id_10', 'id_field',  'z'), measure.vars=c('val_ss_ha','val_info_ha', 'val_tech_ha'))
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
