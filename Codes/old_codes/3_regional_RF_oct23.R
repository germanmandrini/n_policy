# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
setwd('~')

source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')

library(randomForest)
source('./vr_value/Codes/parameters.R')
# eonr_mukey_dt2 <- readRDS("./vr_value/Data/files_rds/eonr_mukey_dt2.rds")

full_fields_dt2 <- readRDS("./vr_value/Data/files_rds/full_fields_dt2.rds")

grid10_soils_sf6 <- readRDS("./vr_value/Data/Grid/grid10_soils_sf6.rds")

grid10_tiles_sf2 <- readRDS("./vr_value/Data/Grid/grid10_tiles_sf2.rds") 

yc_yearly_dt3 <- readRDS("./vr_value/Data/files_rds/yc_yearly_dt3.rds")

reg_model_stuff <- readRDS("./vr_value/Data/files_rds/reg_model_stuff.rds")


#======================================================================================
if(FALSE){
  length(unique(full_fields_dt2$id_10))
  
  # SAMPLE LOCS FOR REGIONAL TESTING
  # MRTN had 80 locs in IL
  # Sample 80 fields. Equal number by region. Assume that each field is heavily explored and different soils are mapped and sampled every year.
  stations_dt <- full_fields_dt2[,.(long = mean(long),
                                    lat = mean(lat),
                                    region = mean(region),
                                    field_soils_cnt = length(unique(mukey))), by = .(id_10, id_field)]
  
  stations_dt[,fields_cell := length(unique(id_field)), by = id_10]
  
  #Choose stations from cells with 3 fields, to avoid loosing field in cells with few fields.
  # Choose fields with more than 2 soils to get some variability
  stations_dt <- stations_dt[ field_soils_cnt > 2 & fields_cell == 3]
  
  # stations_by_region <- round(nrow(stations_dt)*0.05/3,0)
  stations_by_region <- 40
  
  # Also, only one station by id_10
  set.seed(123)
  stations_sample_cells_dt <- stations_dt[, .(lat = mean(lat),
                                              long = mean(long)),by = .(region, id_10)]
  
  # Equally distributed by region
  stations_sample_cells_dt <- stations_sample_cells_dt[order(region, lat)]
  
  #Split the fields of each region into the number of stations we are getting by region, and then select one from each group
  stations_sample_cells_dt[,quantile := cut(lat, quantile(lat, probs = 0:stations_by_region/stations_by_region),
                    labels = FALSE, include.lowest = TRUE), by = region]

  stations_sample_cells_dt <- stations_sample_cells_dt[,.SD[sample(.N, 1)],by = .(region, quantile)][,-c('long', 'quantile', 'lat')]
  
  #Randomly select one field for each sampled id_10
  stations_dt2 <- stations_dt[id_10 %in% stations_sample_cells_dt$id_10, -c('field_soils_cnt', 'fields_cell') ]
  stations_dt2 <- stations_dt2[,.SD[sample(.N, 1)],by = .(id_10)]
  
  stations_dt3 <- filter_dt_in_dt(full_fields_dt2, filter_dt = stations_dt2[,.(id_10, id_field)], return_table = TRUE) %>% 
    .[,.(id_10, id_field, mukey)] %>% unique()
  
  #-------------------
  #Remove fields that are stations
  remove_this <- filter_dt_in_dt(full_fields_dt2, filter_dt = unique(stations_dt3[,.(id_10, id_field)]))
  # remove_this <- c(remove_this, filter_dt_in_dt(fields_dt, filter_dt = data.table(id_10 = 296, id_field = 3)))
  full_fields_dt3 <- full_fields_dt2[-remove_this]
  length(unique(full_fields_dt3$id_10))

}else{
  stations_dt3 <- reg_model_stuff[['stations']]
  full_fields_dt3 <- reg_model_stuff[['full_fields']]
  model1b_eonr <- reg_model_stuff[['model1b_eonr']]
  model2b_eonr <- reg_model_stuff[['model2b_eonr']]
  rm(reg_model_stuff)
}

#======================================================================================
# EXPLORE STATIONS MAP

grid10_fields_clean_sf <- grid10_soils_sf6 %>% group_by(id_tile, id_10, id_field) %>% summarise(area_ha = sum(area_ha))

stations_dt4 <- stations_dt3[,.(id_10, id_field)] %>% .[,station := 1]
grid10_fields_clean_sf2 <- dplyr::left_join(grid10_fields_clean_sf, stations_dt4, by = c('id_10', 'id_field'))
stations_sf <- grid10_fields_clean_sf2[!is.na(grid10_fields_clean_sf2$station),]
grid10_fields_clean_sf2 <- grid10_fields_clean_sf2[is.na(grid10_fields_clean_sf2$station),]

grid10_tiles_sf2 <- grid10_tiles_sf2 %>% mutate(corn_ha_cell = corn5_cell*30*30/10000/11)
grid10_tiles_sf2$region <- as.character(grid10_tiles_sf2$region)

grid10_region <- grid10_tiles_sf2 %>% group_by(region) %>% summarize()

#install.packages('smoothr')
library(smoothr)
area_thresh <- units::set_units(10*10+10, km^2)
grid10_region2 <- fill_holes(grid10_region, threshold = area_thresh)

(fields_map_clean <- tm_shape(grid10_tiles_sf2) + tm_borders() + 
    tm_shape(grid10_region2) + tm_borders(lwd = 4) +
    tm_shape(stations_sf) + tm_dots(size = 0.3, col = 'black') + 
    tm_shape(grid10_fields_clean_sf2) + tm_dots(size = 0.04) + 
    tm_layout(legend.text.size = 0.7,
              #main.title = 'Final fields map',
              main.title.position = "center",
              main.title.size = 1))
# install.packages('tmap')
# library('tmap')
tmap_save(fields_map_clean, filename = "./vr_value/Data/figures/fields_map_and_stations.jpg")  

st_write(stations_sf, "./vr_value/Data/shapefiles/stations_sf.shp", delete_dsn = TRUE)
st_write(grid10_fields_clean_sf2, "./vr_value/Data/shapefiles/grid10_fields_clean_sf2.shp", delete_dsn = TRUE)
st_write(grid10_tiles_sf2, "./vr_value/Data/shapefiles/grid10_tiles_sf2.shp", delete_dsn = TRUE)

#======================================================================================
# GET THE REGIONAL MINIMUM MODEL
training_z <- c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10" )

# training_z_regional <- c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10", "A11", "A12", "A13", "A14", "A15", "A16", "A17", "A18", "A19", "A20", "A21", "A22", "A23", "A24", "A25", "A26", "A27", "A28", "A29", "A30" )

# yc_yearly_dt3 <- merge(yc_yearly_dt3, eonr_mukey_dt2[,c("id_10", "mukey", "z", "prev_crop", "area_ha","long", "lat", "region", "Yld_lt_avg", "Yld_lt_min", "Yld_lt_max")],
#                        by = c("id_10", "mukey", "z", "prev_crop")) #add the info we added to eonr_mukey

train_vect <- filter_dt_in_dt(yc_yearly_dt3 , filter_dt = unique(stations_dt3[,.(id_10, mukey)]))

# Filter only the 10 training z
TrainSet <- yc_yearly_dt3[train_vect] %>% .[z %in% training_z]

all(TrainSet[,.N, by = .(id_10, mukey)]$N == 660)

TrainSet[,Yld_max_z := max(Yld), by = .(id_10, mukey, z, prev_crop)]
TrainSet[Yld_max_z < 10] # killed by frost
TrainSet[Yld_max_z < 2000] # bad data
TrainSet <- TrainSet[Yld_max_z > 2000 ] #remove bad data. Trials would be discarded

table(TrainSet[, .(soils = length(unique(mukey))), by = id_10]$soils)

TrainSet

# ValidSet <- yc_yearly_dt3[-train_vect] #%>% .[!z %in% training_z_regional]

all(order(unique(TrainSet$id_10)) == order(unique(stations_dt3$id_10)))

# =========================================================================================================================================================
# CREATE THE REGIONAL MINIMUM MODEL
TrainSet[,P := Yld * Pc - N_fert * Pn]
TrainSet[,P_gross := P * area_ha]

model_minimum_regional <- TrainSet[,.(P_gross = sum(P_gross), area_ha = sum(area_ha)), by = .(region, prev_crop, N_fert)][, .SD[ P_gross == max( P_gross)], by = .(region, prev_crop)][,.( region, prev_crop, N_fert)]
setnames(model_minimum_regional, 'N_fert', 'eonr_pred')

reg_model_stuff <- list()
# reg_model_stuff <- readRDS("./vr_value/Data/files_rds/reg_model_stuff.rds")

reg_model_stuff[['model_minimum_regional']] <-  model_minimum_regional

# saveRDS(reg_model_stuff, "./vr_value/Data/files_rds/reg_model_stuff.rds")

# =========================================================================================================================================================
# CREATE THE REGIONAL RF-YIELD
no_cost_varb <- c("long", "lat", "prev_crop", "rain_30", "rain_60", "rain_90", 
                  "t_max_30", "t_max_60", "t_max_90", "t_min_30", "t_min_60", 
                  "t_min_90", "Yld_prev", 'Yld_lt_avg', 'Yld_lt_min', 'Yld_lt_max', 'lai_v5')

ss_varb <- c("n_20cm_v5", "n_40cm_v5", "n_60cm_v5", "esw_pct_v5", "whc")



# =========================================================================================================================================================
# CREATE THE REGIONAL RF-EONR

source('./vr_value/Codes/parameters.R')
TrainSet[, P := Yld * Pc - N_fert * Pn]
eonr_mukey_dt <- TrainSet[, .SD[ P == max( P)], by = .(id_10, mukey, z, prev_crop)] %>% .[, .SD[ N_fert == min( N_fert)], by = .(id_10, mukey, z, prev_crop)]
eonr_mukey_dt[,.N, .(id_10)]
table(eonr_mukey_dt[, .(soils = length(unique(mukey))), by = id_10]$soils)
eonr_mukey_dt <- eonr_mukey_dt[,c('mukey','N_fert', no_cost_varb, ss_varb), with = FALSE]
setnames(eonr_mukey_dt, 'N_fert', 'eonr')
lapply(eonr_mukey_dt, class)
summary(eonr_mukey_dt)


# Create a Random Forest model with default parameters
model1b_eonr <- randomForest(eonr ~ ., data = eonr_mukey_dt[,c('eonr',no_cost_varb), with = FALSE],
                        importance = TRUE, ntree=500)

model2b_eonr <- randomForest(eonr ~ ., data = eonr_mukey_dt[,c('eonr', no_cost_varb, ss_varb), with = FALSE],
                        importance = TRUE, ntree=500)
model2b_eonr
plot(model2b_eonr)

# model1b_eonr <- reg_model_stuff[['model1b_eonr']]
# model2b_eonr <- reg_model_stuff[['model2b_eonr']]

jpeg("./vr_value/Data/figures/model1_eonr_variables.jpg")
varImpPlot(model1b_eonr, type=2, main = 'a) Regional RF1')
dev.off() 

jpeg("./vr_value/Data/figures/model2_eonr_variables.jpg")
varImpPlot(model2b_eonr, type=2, main = 'b) Regional RF2')
dev.off() 

# p1 <- varImpPlot(model1b_eonr, type=2)
# p2 <- varImpPlot(model2b_eonr, type=2)
# tmap_arrange(p1, p2, nrow = 1)
# class(p1)
# 
# varImpPlot(model1b_eonr, model2b_eonr, type=2)

reg_model_stuff[['model1b_eonr']] <-  model1b_eonr
reg_model_stuff[['model2b_eonr']] <-  model2b_eonr

# =========================================================================================================================================================
# CREATE THE REGIONAL REGRESSION
ggplot(data = eonr_mukey_dt, aes(x = lai_v5, y = eonr)) +
        geom_point()

# eonr_mukey_dt[, n_0_60cm_v5 := n_20cm_v5 + n_40cm_v5 +n_60cm_v5]
# 
# ggplot(eonr_mukey_dt, aes(x = n_0_60cm_v5, y = eonr, colour = factor(prev_crop))) +
#   geom_point()+ 
#   geom_smooth(method = 'lm' ,se = FALSE, formula = y~ x)
  

# lm(data = eonr_mukey_dt, eonr ~ n_0_60cm_v5 * prev_crop )
# 
# lm(data = eonr_mukey_dt, eonr ~ n_0_60cm_v5 + prev_crop + I(n_0_60cm_v5* prev_crop ) + factor(mukey))
# lm(data = eonr_mukey_dt, eonr ~ n_0_60cm_v5 * prev_crop)
# coef(lm(data = eonr_mukey_dt, eonr ~ n_0_60cm_v5 * prev_crop))
# reg_coef <- eonr_mukey_dt[,list(Intercept=coef(lm(eonr ~ n_0_60cm_v5 * prev_crop))[1], 
#           n_0_60cm_v5=coef(lm(eonr ~ n_0_60cm_v5 * prev_crop))[2],
#           prev_crop=coef(lm(eonr ~ n_0_60cm_v5 * prev_crop))[3],
#           n_0_60cm_v5_prev_crop =coef(lm(eonr ~ n_0_60cm_v5 * prev_crop))[4]), by=mukey]
# lapply(reg_coef, mean)
# reg_coef <- lm(data = eonr_mukey_dt, eonr ~ n_0_60cm_v5 * prev_crop )
# 
# reg_coef_dt <- data.table(t(matrix(coef(reg_coef))))
# names(reg_coef_dt) <- names(coef(reg_coef))
# setnames(reg_coef_dt, c('(Intercept)', 'n_0_60cm_v5:prev_crop') , c('intercept', 'n_0_60cm_v5_prev_crop'))
# 
# reg_model_stuff[['model_regression_regional']] <-  reg_coef_dt

# =========================================================================================================================================================

# #Make a small validation set
# ValidSet2 <- ValidSet[id_10 %in% sample(unique(ValidSet$id_10), 30)]
# ValidSet2[, P := Yld * Pc - N_fert * Pn]
# ValidSet2 <- ValidSet2[, .SD[ P == max( P)], by = .(id_10, mukey, z, prev_crop)]
# ValidSet2 <- ValidSet2[,c('N_fert', no_cost_varb, ss_varb), with = FALSE]
# setnames(ValidSet2, 'N_fert', 'eonr')
# ValidSet3 <- ValidSet2[sample(1:nrow(ValidSet2), 2000),]
# 
# ValidSet3[,eonr_pred := predict(model2b_eonr, ValidSet3, type = "class")]
# ValidSet3[, eonr_pred_round := round(eonr_pred/10, 0)*10]
# ValidSet3[, eonr_pred_ceil := ceiling(eonr_pred/10)*10]
# ValidSet3[, eonr_pred_floor := floor(eonr_pred/10)*10]
# 
# 
# rmse_dt <- melt(ValidSet3, id.vars = 'eonr', measure.vars = names(ValidSet3)[24:26])
# rmse_dt[,res := (value - eonr)^2]
# rmse_dt[,.(rmse = sqrt(mean(res))),
#         by = variable]


# =========================================================================================================================================================
reg_model_stuff[['no_cost_var']] <-  no_cost_varb
reg_model_stuff[['ss_var']] <-  ss_varb
reg_model_stuff[['full_fields']] <-  full_fields_dt3
reg_model_stuff[['stations']] <-  stations_dt3
reg_model_stuff[['training_z']] <-  training_z
# reg_model_stuff[['trial_rates']] <-  trial_rates

saveRDS(reg_model_stuff, "./vr_value/Data/files_rds/reg_model_stuff.rds")

