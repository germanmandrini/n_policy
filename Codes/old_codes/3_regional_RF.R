# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
# setwd('~')

source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')
library(randomForest)
# source('./vr_value/Codes/parameters.R')
eonr_mukey_dt3 <- readRDS("./vr_value/Data/files_rds/eonr_mukey_dt3.rds")

grid10_soils_sf6 <- readRDS("./vr_value/Data/Grid/grid10_soils_sf6.rds")

grid10_tiles_sf2 <- readRDS("./vr_value/Data/Grid/grid10_tiles_sf2.rds") 

yc_yearly_dt2 <- readRDS("./vr_value/Data/files_rds/yc_yearly_dt2.rds")
# reg_model_stuff <- readRDS("./vr_value/Data/files_rds/reg_model_stuff.rds")

#======================================================================================
if(FALSE){
  # SAMPLE LOCS FOR REGIONAL TESTING
  # MRTN had 80 locs in IL
  # Sample 80 fields. Equal number by region. Assume that each field is heavily explored and different soils are mapped and sampled every year.
  grid10_soils_dt <- data.table(grid10_soils_sf6, st_coordinates(st_centroid(grid10_soils_sf6))) %>% .[,-'geometry']
  setnames(grid10_soils_dt, c('X', 'Y'), c('long', 'lat'))
  
  #Find complete fields (all mukeys were run and not eliminated in QC)
  full_fields_dt <- grid10_soils_dt[,.(long = mean(long),
                                       area_ha = sum(area_ha)), by = .(id_10, id_field, region)]
  
  full_fields_dt2 <- full_fields_dt[area_ha > 35]
    
  stations_dt <- full_fields_dt2[area_ha > 39.9]
  # stations_by_region <- round(nrow(stations_dt)*0.05/3,0)
  stations_by_region <- 30
  
  #Choose stations from cells with 3 fields, to avoid loosing a field in cells with few fields
  
  stations_dt <- stations_dt[, N := .N,by = .(region, id_10)][N == 3]
  
  set.seed(123)
  # Equally distributed by region
  stations_dt <- stations_dt[order(region, long)]
  
  #Split the fields of each region into the number of stations we are getting by region, and then select one from each group
  stations_dt[,quantile := cut(long, quantile(long, probs = 0:stations_by_region/stations_by_region),
                    labels = FALSE, include.lowest = TRUE), by = region]

  stations_dt <- stations_dt[,.SD[sample(.N, 1)],by = .(region, quantile)][,-'long']
  
  
  #------
  # MAKE A MAP OF THE RESEARCH STATIONS
  stations_sf <- grid10_soils_sf6 %>% dplyr::filter(id_10 %in% stations_dt$id_10 & id_field == 3)
  stations_sf <- stations_sf %>% group_by(id_tile, id_10, id_field) %>% summarize() %>% st_centroid()
  
  
  
  (stations_map <- tm_shape(grid10_tiles_sf2) + tm_polygons("region") +
      tm_shape(stations_sf) + tm_dots(size = 0.5) + 
      tm_layout(legend.text.size = 0.7,
                main.title = 'Stations map',
                main.title.position = "center",
                main.title.size = 1))
  
  tmap_save(stations_map, filename = "./vr_value/Data/figures/stations_map.jpg", scale = 2)  
  
  
  #-------------------
  #Remove fields that are stations
  remove_this <- filter_dt_in_dt(full_fields_dt2, filter_dt = stations_dt[,.(id_10, id_field)])
  # remove_this <- c(remove_this, filter_dt_in_dt(fields_dt, filter_dt = data.table(id_10 = 296, id_field = 3)))
  full_fields_dt2 <- full_fields_dt2[-remove_this]
}else{
  stations_dt <- reg_model_stuff[['stations']]
  full_fields_dt <- reg_model_stuff[['full_fields']]
}

#======================================================================================
# GET THE REGIONAL MINIMUM MODEL
yc_yearly_dt2[,rotation := ifelse(rotation == 'MSM', 0, 1)]

yc_yearly_dt2 <- merge(yc_yearly_dt2, eonr_mukey_dt3[,c("id_10", "mukey", "z", "rotation", "area_ha","region")],
                       by = c("id_10", "mukey", "z", "rotation")) #add the info we added to eonr_mukey

train_vect <- filter_dt_in_dt(yc_yearly_dt2[z %in% c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10" )],
                              filter_dt = stations_dt[,.(id_10, id_field)], return_table = T)


train_vect[,P_gross := P * area_ha]
model_minimum_regional <- train_vect[,.(P_gross = sum(P_gross), area_ha = sum(area_ha)), by = .(region, rotation, N_fert)][, .SD[ P_gross == max( P_gross)], by = .(region, rotation)][,.( region, rotation, N_fert)]
setnames(model_minimum_regional, 'N_fert', 'eonr_pred')

reg_model_stuff <- list()
# reg_model_stuff <- readRDS("./vr_value/Data/files_rds/reg_model_stuff.rds")

reg_model_stuff[['model_minimum_regional']] <-  model_minimum_regional

# saveRDS(reg_model_stuff, "./vr_value/Data/files_rds/reg_model_stuff.rds")

# =========================================================================================================================================================
#USING SIGNIFICANT VARIABLES 
no_cost_varb <- c("region", "long", "lat", "rotation", "rain_30", "rain_60", "rain_90", 
                  "t_max_30", "t_max_60", "t_max_90", "t_min_30", "t_min_60", 
                  "t_min_90", "Yld_prev", 'Yld_lt_avg', 'Yld_lt_min', 'Yld_lt_max' )

ss_varb <- c("n_20cm_v5", "n_40cm_v5", "n_60cm_v5","n_deep_v5", "esw_pct_v5", "dul_dep", "ll15_dep", "whc", "sw_dep_v5")

# crop_status_varb <- c("biomass_n_v5", "biomass_v5", "green_biomass_n_v5", "greenn_v5", "leafgreennconc_v5",  "leafgreenn_v5", "lai_v5")


train_vect <- filter_dt_in_dt(eonr_mukey_dt3 , filter_dt = stations_dt)
# Filter only the 10 training z
TrainSet <- eonr_mukey_dt3[train_vect] %>% .[z %in% c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10" )]
ValidSet <- eonr_mukey_dt3[-train_vect] %>% .[!z %in% c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10" )]

# Create a Random Forest model with default parameters
model1b <- randomForest(eonr ~ ., data = TrainSet[,c('eonr', no_cost_varb), with = FALSE], importance = TRUE)
model2b <- randomForest(eonr ~ ., data = TrainSet[,c('eonr', no_cost_varb, ss_varb), with = FALSE], importance = TRUE)


varImpPlot(model1b, type=2)
varImpPlot(model2b, type=2)

ValidSet$eonr_pred1b <- predict(model1b, ValidSet, type = "class")
ValidSet$eonr_pred2b <- predict(model2b, ValidSet, type = "class")


cor(ValidSet$eonr_pred1b, ValidSet$eonr) ^ 2 #r2
cor(ValidSet$eonr_pred2b, ValidSet$eonr) ^ 2 #r2

#RMSE
ValidSet[,res1 := abs(eonr - eonr_pred1b)]
ValidSet[,res2 := abs(eonr - eonr_pred2b)]
sd(ValidSet$res1)
sd(ValidSet$res2)

ggplot(data = ValidSet, aes(x = eonr, y = eonr_pred2b))+geom_point()+
  coord_fixed() + geom_abline() + ylim(0, 350)+ xlim(0, 350) +
  geom_smooth()+
  theme(aspect.ratio=1, 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_bw()

#Open the list with reg_model_stuff, add the things in here and save it again
reg_model_stuff <- readRDS("./vr_value/Data/files_rds/reg_model_stuff.rds")

reg_model_stuff2 <- list(model_minimum_regional = reg_model_stuff[["model_minimum_regional"]], model1b = model1b, 
                         model2b = model2b, no_cost_var = no_cost_varb, ss_var = ss_varb, full_fields = full_fields_dt, stations = stations_dt )

saveRDS(reg_model_stuff2, "./vr_value/Data/files_rds/reg_model_stuff.rds")
