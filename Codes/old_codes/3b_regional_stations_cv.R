# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents') #CPSC
# setwd("/home/germanm2")
setwd('~')

source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')

# library(randomForest)
# library(ranger)
# library(mlr)
source('./vr_value_v2/Codes/parameters.R')
# Pn = 0.87 * 2

# eonr_mukey_dt2 <- readRDS("./vr_value_v2/Data/files_rds/eonr_mukey_dt2.rds")
grid10_tiles_sf6 <- readRDS("./vr_value_v2/Data/Grid/grid10_tiles_sf6.rds") 
grid10_soils_dt5 <- readRDS("./vr_value_v2/Data/Grid/grid10_soils_dt5.rds") %>% data.table()
yc_yearly_dt3 <- readRDS("./vr_value_v2/Data/files_rds/yc_yearly_dt3.rds")
grid10_fields_sf2 <- readRDS('./vr_value_v2/Data/Grid/grid10_fields_sf2.rds')

#======================================================================================
# if(FALSE){
#   # SAMPLE LOCS FOR REGIONAL TESTING
#   # MRTN had 80 locs in IL
#   # Sample 80 fields. Equal number by region. Assume that each field is heavily explored and different soils are mapped and sampled every year.
#   full_fields_dt <- grid10_soils_dt5[,.(long = mean(long),
#                                     lat = mean(lat),
#                                     region = mean(region),
#                                     field_soils_cnt = length(unique(mukey))), by = .(id_10, id_field)]
#   
#   full_fields_dt[,fields_cell := length(unique(id_field)), by = id_10]
#   table(full_fields_dt$id_field)
#   
#   
#   #Choose stations from cells with 4 fields, to avoid loosing field in cells with few fields.
#   # Choose fields with more than 2 soils to get some variability
#   stations_dt <- full_fields_dt[field_soils_cnt > 1 & fields_cell == 4 & id_field %in% c(3,4) ]
#   
#   #Only if 3 and 4 meet the requirements (one could have only 1 soil)
#   stations_dt[,N := .N, by = id_10]
#   stations_dt <- stations_dt[N==2, -'N']
#   
#   # stations_by_region <- round(nrow(stations_dt)*0.05/3,0)
#   stations_by_region <- 40
#   
#   # Also, only one station by id_10
#   set.seed(123)
#   stations_sample_cells_dt <- stations_dt[, .(lat = mean(lat),
#                                               long = mean(long)),by = .(region, id_10)]
#   
#   # Equally distributed by region
#   stations_sample_cells_dt <- stations_sample_cells_dt[order(region, lat)]
#   
#   #Split the fields of each region into the number of stations we are getting by region, and then select one from each group
#   stations_sample_cells_dt[,quantile := cut(lat, quantile(lat, probs = 0:stations_by_region/stations_by_region),
#                     labels = FALSE, include.lowest = TRUE), by = region]
# 
#   stations_sample_cells_dt <- stations_sample_cells_dt[,.SD[sample(.N, 1)],by = .(region, quantile)][,-c('long', 'quantile', 'lat')]
#   
#   #Select fields for each sampled id_10
#   stations_dt2 <- grid10_soils_dt5[id_10 %in% stations_sample_cells_dt$id_10 &
#                                      id_field %in% c(3,4), .(region, id_10, id_field, mukey, lat, long)]
#   
#   stations_dt2[,.N,.(id_10, id_field)][,.N, by = .(id_10)]
#   
#   #-------------------
#   #Remove fields that are stations
#   remove_this <- filter_dt_in_dt(grid10_soils_dt5, filter_dt = unique(stations_dt2[,.(id_10, id_field)]))
#   # remove_this <- c(remove_this, filter_dt_in_dt(fields_dt, filter_dt = data.table(id_10 = 296, id_field = 3)))
#   full_fields_dt2 <- grid10_soils_dt5[-remove_this, .(region, id_10, id_field, mukey, lat, long)]
#   length(unique(full_fields_dt2$id_10))
#   
# 
# }else{
#   reg_model_stuff <- readRDS( "./vr_value_v2/Data/files_rds/reg_model_stuff.rds")
#   stations_dt2 <- reg_model_stuff[['stations']]
#   full_fields_dt2 <- reg_model_stuff[['full_fields']]
#   model1b_eonr <- reg_model_stuff[['model1b_eonr']]
#   model2b_eonr <- reg_model_stuff[['model2b_eonr']]
#   rm(reg_model_stuff)
# }
#======================================================================================
  if(FALSE){
    fields_cnt <- grid10_soils_dt5[,.N, by = .(id_10, id_field)] %>% .[,.(field_cnt = .N), by = id_10]
    
    grid10_soils_dt5 <- merge(grid10_soils_dt5, fields_cnt, by = 'id_10')
    candidates_dt <- grid10_soils_dt5[field_cnt == 4 & id_field %in% c(1,2)]
    
    set.seed(256)
    stations_dt <- data.table()
    stations_by_region <- 40
    for(region_n in c(1,2,3)){
      # region_n <- 1
      candidates_region_dt <- candidates_dt[region == region_n]
      id_10_sample <- sample(unique(candidates_region_dt$id_10), stations_by_region, replace = F)
      stations_dt <- rbind(stations_dt, candidates_region_dt[id_10 %in% id_10_sample])
    }
    
    #-------------------
    #Remove fields that are stations
    remove_this <- filter_dt_in_dt(grid10_soils_dt5, filter_dt = unique(stations_dt[,.(id_10, id_field)]))
    # remove_this <- c(remove_this, filter_dt_in_dt(fields_dt, filter_dt = data.table(id_10 = 296, id_field = 3)))
    full_fields_dt <- grid10_soils_dt5[-remove_this, .(region, id_10, id_field, mukey, lat, long)]
    length(unique(full_fields_dt$id_10))
  }else{
    reg_model_stuff <- readRDS("./vr_value_v2/Data/files_rds/reg_model_stuff.rds")
    full_fields_dt <- reg_model_stuff[['full_fields']]
    stations_dt <- reg_model_stuff[['stations']]
    rm(reg_model_stuff)
  }

# EXPLORE THE DATA-------------------
yld_explore_dt <- yc_yearly_dt3[,.(Yld = mean(Yld)), by = z]
yld_explore_dt[,z:= as.numeric(z)]
yld_explore_dt <- yld_explore_dt[order(z)]

eonr_explore_dt <- yc_yearly_dt3[, P := Yld * Pc - N_fert * Pn] %>% 
  .[, .SD[ P == max( P)], by = .(id_10, mukey, z)]

setnames(eonr_explore_dt, 'N_fert', 'eonr')
eonr_explore_dt[,z:= as.numeric(z)]
eonr_explore_dt[,.(eonr = mean(eonr),
                   leach_n2 = mean(leach_n2)), by = z][order(z)]
ggplot(eonr_explore_dt) +
  geom_boxplot(aes(x = factor(z), y = eonr))

ggplot(eonr_explore_dt) +
  geom_boxplot(aes(x = factor(z), y = n_deep_v5))

ggplot(eonr_explore_dt) +
  geom_boxplot(aes(x = factor(z), y = leach_n2 ))

# eonr_explore_dt[,set := ifelse(z %in% training_z, '1training', '2testing')]
# eonr_explore_dt[z %in% c(5,10), set := 'discard']
# eonr_explore_dt[,.(eonr = mean(eonr)), by = set]
# 
# ggplot(eonr_explore_dt) +
#   geom_boxplot(aes(x = set, y = eonr))



#======================================================================================
# EXPLORE STATIONS MAP

full_fields_dt[,rf := 1]
stations_dt[,rs := 1]

regularfields_sf <- dplyr::left_join(grid10_fields_sf2, full_fields_dt[,.(id_10, id_field, rf)], 
                                     by = c('id_10', 'id_field')) %>% dplyr::filter(!is.na(rf)) %>%
  dplyr::select(-rf)


stations_sf <- dplyr::left_join(grid10_fields_sf2, stations_dt[,.(id_10, id_field, rs)], 
                                     by = c('id_10', 'id_field')) %>% dplyr::filter(!is.na(rs)) %>%
  dplyr::select(-rs)

stations_by_region = 40
stations_sf %>% data.table() %>% .[,.N, by = .(id_10, id_field)] %>% nrow() == stations_by_region * 6

# grid10_tiles_sf2 <- grid10_tiles_sf7 %>% mutate(corn_ha_cell = corn5_cell*30*30/10000/11)
# grid10_tiles_sf2$region <- as.character(grid10_tiles_sf2$region)

grid10_region <- grid10_tiles_sf6 %>% group_by(region) %>% summarize()

# #install.packages('smoothr')
# library(smoothr)
# area_thresh <- units::set_units(10*10+10, km^2)
# grid10_region2 <- fill_holes(grid10_region, threshold = area_thresh)
grid10_region_by_hand <- sf::read_sf('./vr_value_v2/Data/shapefiles/grid10_region_by_hand.shp')
grid10_region_by_hand <- st_transform(grid10_region_by_hand, crs = st_crs(stations_sf))


(fields_map_clean <- tm_shape(grid10_tiles_sf6) + tm_borders() +
    tm_shape(grid10_region_by_hand) + tm_borders(lwd = 4) +
    tm_shape(stations_sf) + tm_dots(size = 0.3, col = 'black') +
    tm_shape(regularfields_sf) + tm_dots(size = 0.04) +
    tm_layout(legend.text.size = 0.7,
              #main.title = 'Final fields map',
              main.title.position = "center",
              main.title.size = 1))

# install.packages('tmap')
library('tmap')
tmap_save(fields_map_clean, filename = "./vr_value_v2/Data/figures/fields_map_and_stations.jpg", 
          height = 8, width = 6)  

st_write(stations_sf, "./vr_value_v2/Data/shapefiles/stations_sf.shp", delete_dsn = TRUE)
st_write(regularfields_sf, "./vr_value_v2/Data/shapefiles/regularfields_sf.shp", delete_dsn = TRUE)
# st_write(grid10_region, "./vr_value_v2/Data/shapefiles/grid10_region.shp", delete_dsn = TRUE)

# =========================================================================================================================================================
reg_model_stuff <- list()
reg_model_stuff[['full_fields']] <-  full_fields_dt[,-'rf']
reg_model_stuff[['stations']] <-  stations_dt[,-'rs']

# reg_model_stuff[['trial_rates']] <-  trial_rates

saveRDS(reg_model_stuff, "./vr_value_v2/Data/files_rds/reg_model_stuff.rds")

