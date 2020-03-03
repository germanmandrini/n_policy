setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents') #CPSC
# setwd("/home/germanm2")
# setwd('~')

source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')

library(randomForest)
source('./vr_value_v2/Codes/parameters.R')
Pn = 0.87 * 2
# eonr_mukey_dt2 <- readRDS("./vr_value_v2/Data/files_rds/eonr_mukey_dt2.rds")
grid10_tiles_sf6 <- readRDS("./vr_value_v2/Data/Grid/grid10_tiles_sf6.rds") 
grid10_soils_dt5 <- readRDS("./vr_value_v2/Data/Grid/grid10_soils_dt5.rds") %>% data.table()
yc_yearly_dt3 <- readRDS("./vr_value_v2/Data/files_rds/yc_yearly_dt3.rds")
grid10_fields_sf2 <- readRDS('./vr_value_v2/Data/Grid/grid10_fields_sf2.rds')

#======================================================================================
if(FALSE){
  
  # SAMPLE LOCS FOR REGIONAL TESTING
  # MRTN had 80 locs in IL
  # Sample 80 fields. Equal number by region. Assume that each field is heavily explored and different soils are mapped and sampled every year.
  full_fields_dt <- grid10_soils_dt5[,.(long = mean(long),
                                    lat = mean(lat),
                                    region = mean(region),
                                    field_soils_cnt = length(unique(mukey))), by = .(id_10, id_field)]
  
  full_fields_dt[,fields_cell := length(unique(id_field)), by = id_10]
  table(full_fields_dt$id_field)
  
  
  #Choose stations from cells with 4 fields, to avoid loosing field in cells with few fields.
  # Choose fields with more than 2 soils to get some variability
  stations_dt <- full_fields_dt[field_soils_cnt > 1 & fields_cell == 4 & id_field %in% c(3,4) ]
  
  #Only if 3 and 4 meet the requirements (one could have only 1 soil)
  stations_dt[,N := .N, by = id_10]
  stations_dt <- stations_dt[N==2, -'N']
  
  # stations_by_region <- round(nrow(stations_dt)*0.05/3,0)
  stations_by_region <- 40
  
  # Also, only one station by id_10
  set.seed(999)
  stations_sample_cells_dt <- stations_dt[, .(lat = mean(lat),
                                              long = mean(long)),by = .(region, id_10)]
  
  # Equally distributed by region
  stations_sample_cells_dt <- stations_sample_cells_dt[order(region, lat)]
  
  #Split the fields of each region into the number of stations we are getting by region, and then select one from each group
  stations_sample_cells_dt[,quantile := cut(lat, quantile(lat, probs = 0:stations_by_region/stations_by_region),
                    labels = FALSE, include.lowest = TRUE), by = region]

  stations_sample_cells_dt <- stations_sample_cells_dt[,.SD[sample(.N, 1)],by = .(region, quantile)][,-c('long', 'quantile', 'lat')]
  
  #Select fields for each sampled id_10
  stations_dt2 <- grid10_soils_dt5[id_10 %in% stations_sample_cells_dt$id_10 &
                                     id_field %in% c(3,4), .(region, id_10, id_field, mukey, lat, long)]
  
  stations_dt2[,.N,.(id_10, id_field)][,.N, by = .(id_10)]
  
  #-------------------
  #Remove fields that are stations
  remove_this <- filter_dt_in_dt(grid10_soils_dt5, filter_dt = unique(stations_dt2[,.(id_10, id_field)]))
  # remove_this <- c(remove_this, filter_dt_in_dt(fields_dt, filter_dt = data.table(id_10 = 296, id_field = 3)))
  full_fields_dt2 <- grid10_soils_dt5[-remove_this, .(region, id_10, id_field, mukey, lat, long)]
  length(unique(full_fields_dt2$id_10))
  

}else{
  reg_model_stuff <- readRDS( "./vr_value_v2/Data/files_rds/reg_model_stuff.rds")
  stations_dt2 <- reg_model_stuff[['stations']]
  full_fields_dt2 <- reg_model_stuff[['full_fields']]
  model1b_eonr <- reg_model_stuff[['model1b_eonr']]
  model2b_eonr <- reg_model_stuff[['model2b_eonr']]
  rm(reg_model_stuff)
}

#======================================================================================
# EXPLORE STATIONS MAP

full_fields_dt2[,rf := 1]
stations_dt2[,rs := 1]

regularfields_sf <- dplyr::left_join(grid10_fields_sf2, full_fields_dt2[,.(id_10, id_field, rf)], 
                                     by = c('id_10', 'id_field')) %>% dplyr::filter(!is.na(rf)) %>%
  dplyr::select(-rf)


stations_sf <- dplyr::left_join(grid10_fields_sf2, stations_dt2[,.(id_10, id_field, rs)], 
                                     by = c('id_10', 'id_field')) %>% dplyr::filter(!is.na(rs)) %>%
  dplyr::select(-rs)


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
tmap_save(fields_map_clean, filename = "./vr_value_v2/Data/figures/fields_map_and_stations.jpg", height = 8, width = 6)  

st_write(stations_sf, "./vr_value_v2/Data/shapefiles/stations_sf.shp", delete_dsn = TRUE)
st_write(regularfields_sf, "./vr_value_v2/Data/shapefiles/regularfields_sf.shp", delete_dsn = TRUE)
# st_write(grid10_region, "./vr_value_v2/Data/shapefiles/grid10_region.shp", delete_dsn = TRUE)

#======================================================================================
# PREPARE THE DATA
training_z <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
z_odd = c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)
z_even = z_odd+1

indx <- filter_dt_in_dt(yc_yearly_dt3, filter_dt = unique(stations_dt2[,.(id_10, mukey)]))
StationsSet <- yc_yearly_dt3[indx]
ValidSet <- yc_yearly_dt3[-indx]

sets_rf <- unique(ValidSet[,.(id_10, mukey)])
sets_rf <- sets_rf[sample(1:nrow(sets_rf), 100)]
ValidSet2 <- filter_dt_in_dt(ValidSet, filter_dt = sets_rf, return_table = T)


# Filter the right training z, depending on odd or even fields. Remove some z that are coming from fields 1 and 2 that are not RS
stations_dt2[,z_type := ifelse(id_field == 3, 'odd', 'even')]
StationsSet[,z_type := ifelse(z %in% z_odd, 'odd', 'even')]
StationsSet <- merge(StationsSet, stations_dt2[,.(id_10, mukey, z_type)])

yld_explore_dt <- StationsSet[,.(Yld = mean(Yld)), by = z]
yld_explore_dt[,z:= as.numeric(z)]
yld_explore_dt <- yld_explore_dt[order(z)]

ggplot(yld_explore_dt, aes(x = z, y = Yld))+
  geom_bar(stat="identity")

TrainSet <- StationsSet[z %in% training_z]
# ValidSet <- StationsSet[!z %in% training_z]
TrainSet[,.(Yld = mean(Yld)), by = z]
TrainSet <- TrainSet[!z %in% c(5,10)]
ValidSet3 <- ValidSet2[!z %in% training_z]

# =========================================================================================================================================================
# CREATE THE REGIONAL MINIMUM MODEL

#Analysis included only responsive sites (sawyer 2006)
TrainSet[, Yld_response := max(Yld) - min(Yld), by = .(id_10, mukey,z)]
TrainSet2 <- TrainSet[Yld_response > 500]


#Select a few rates
#Alll this comes from https://rcompanion.org/handbook/I_11.html
# N_rates_trial <- c(10, 90,170,250, 330)
N_rates_trial <- seq(10,330,10)

quadratic_dt <- TrainSet2[,list(intercept=coef(lm(Yld~N_fert + I(N_fert^2)))[1], 
                                coef1=coef(lm(Yld ~ N_fert + I(N_fert^2)))[2],
                                coef2=coef(lm(Yld ~ N_fert + I(N_fert^2)))[3]),by=.(id_10, mukey,z, region)]

# Expand and calculate yield
N_rates_int <- seq(min(N_rates_trial),max(N_rates_trial), by = 10)
quadratic_dt2 <- quadratic_dt[rep(x = 1:nrow(quadratic_dt), each = length(N_rates_int))]



quadratic_dt2[,N_fert := rep(N_rates_int, nrow(quadratic_dt))]
quadratic_dt2[,Yld := intercept + coef1 * N_fert + coef2 * (N_fert^2)]
quadratic_dt2[,P:= Yld * Pc - N_fert * Pn]

#Average all curves
quadratic_dt3 <- quadratic_dt2[,.(P_avg = mean(P)), by = .(region, N_fert)]
ggplot(quadratic_dt3) + geom_point(aes(x = N_fert, y = P_avg, colour = interaction(region)))

#Select EONR
model_minimum_regional <- quadratic_dt3[, .SD[ P_avg == max( P_avg)], by = .(region)][,.( region, N_fert)]
setnames(model_minimum_regional, 'N_fert', 'eonr_pred')

reg_model_stuff <- list()
# reg_model_stuff <- readRDS("./vr_value_v2/Data/files_rds/reg_model_stuff.rds")

reg_model_stuff[['model_minimum_regional']] <-  model_minimum_regional

# =========================================================================================================================================================

# # CREATE THE REGIONAL MINIMUM MODEL
# TrainSet2[,.(P_avg = mean(P), area_ha = sum(area_ha)), by = .(region, prev_crop, N_fert)][, .SD[ P_avg == max( P_avg)], by = .(region, prev_crop)][,.( region, prev_crop, N_fert)]
# 
# model_minimum_regional <- TrainSet[,.(P_gross = sum(P_gross), area_ha = sum(area_ha)), by = .(region, prev_crop, N_fert)][, .SD[ P_gross == max( P_gross)], by = .(region, prev_crop)][,.( region, prev_crop, N_fert)]
# setnames(model_minimum_regional, 'N_fert', 'eonr_pred')
# 
# reg_model_stuff <- list()
# # reg_model_stuff <- readRDS("./vr_value_v2/Data/files_rds/reg_model_stuff.rds")
# 
# reg_model_stuff[['model_minimum_regional']] <-  model_minimum_regional

# saveRDS(reg_model_stuff, "./vr_value_v2/Data/files_rds/reg_model_stuff.rds")

# =========================================================================================================================================================
# CREATE THE REGIONAL RF-YIELD
no_cost_varb <- c("rain_30", "rain_60", "rain_90", 
                  "t_max_30", "t_max_60", "t_max_90", "t_min_30", "t_min_60", 
                  "t_min_90", "Yld_prev", 'Yld_lt_avg', 'Yld_lt_min', 'Yld_lt_max')

ss_varb <- c("n_20cm_v5", "n_40cm_v5", "n_60cm_v5", "esw_pct_v5", "whc")

# =========================================================================================================================================================
# CREATE THE REGIONAL RF-EONR

TrainSet[, P := Yld * Pc - N_fert * Pn]

#Add the coordinates
coordinates_dt <- grid10_soils_dt5[,.(lat = mean(lat), 
                                      long = mean(long)), by = .(id_10, mukey)]

TrainSet <- merge(TrainSet,coordinates_dt , by = c('id_10', 'mukey'))


eonr_mukey_dt <- TrainSet[, .SD[ P == max( P)], by = .(id_10, mukey, z)]
setnames(eonr_mukey_dt, 'N_fert', 'eonr')
eonr_mukey_dt[,.(Yld = mean(Yld)), by = z]
TrainSet2[Yld == 0]
summary(eonr_mukey_dt)

table(eonr_mukey_dt[, .(soils = length(unique(mukey))), by = id_10]$soils)
eonr_mukey_dt2 <- eonr_mukey_dt[,c('eonr', no_cost_varb, ss_varb), with = FALSE]


# Create a Random Forest model with default parameters
# model1b_eonr <- randomForest(eonr ~ ., data = eonr_mukey_dt2[,c('eonr',no_cost_varb), with = FALSE],
#                         importance = TRUE, ntree=500)

model2b_eonr <- randomForest(eonr ~ ., data = eonr_mukey_dt2[,c('eonr', no_cost_varb, ss_varb), with = FALSE],
                        importance = TRUE, ntree=300, nodesize = 20 )
model2b_eonr
plot(model2b_eonr)

jpeg("./vr_value_v2/Data/figures/model1_eonr_variables.jpg")
varImpPlot(model1b_eonr, type=2, main = 'a) Regional RF1')
dev.off() 

jpeg("./vr_value_v2/Data/figures/model2_eonr_variables.jpg")
varImpPlot(model2b_eonr, type=2, main = 'b) Regional RF2')
dev.off() 

reg_model_stuff[['model1b_eonr']] <-  model1b_eonr
reg_model_stuff[['model2b_eonr']] <-  model2b_eonr

#===========================================================================================
# install.packages('mlr')
# install.packages('ranger')
library(ranger)
library(mlr)

# # eonr_mukey_dt2 <- eonr_mukey_dt[,c('eonr', no_cost_varb, ss_varb), with = FALSE]
# 
# # Create the tasks encapsulate the data set and further relevant information
regr_task = makeRegrTask(data = eonr_mukey_dt2, target = "eonr") #task = data
# 
# # Create the RF learner, with some optimized hyperparameters:
# learner_rf = makeLearner("regr.ranger",
#                          num.trees = 100,
#                          min.node.size = 25) #instructions
# 
# # Train a model
# mod_rf <- train(learner_rf, task = regr_task)
# names(mod_rf)
# mod_rf$learner.model
# 
# 
# #Make preditions
# task_pred = predict(mod_rf, task = regr_task)
# 
# task_pred$data$truth
# measureRMSE(truth = task_pred$data$truth, response = task_pred$data$response)
# cor(task_pred$data$truth, task_pred$data$response)

#------ TUNE HYPERPARAMETERS

# ranger.learner <- makeLearner("classif.ranger", predict.type = "prob")
## Create combined training data
train_data <- eonr_mukey_dt2
ValidSet3[, P := Yld * Pc - N_fert * Pn]
validation_data <- ValidSet3[, .SD[ P == max( P)], by = .(id_10, mukey, z)]
setnames(validation_data, 'N_fert', 'eonr')
validation_data <- validation_data[,c('eonr', no_cost_varb, ss_varb), with = FALSE]

train_task_data <- rbind(train_data, validation_data)
size <- nrow(train_task_data)
train_ind <- seq_len(nrow(train_data))
validation_ind <- seq.int(max(train_ind) + 1, size)


# Create training task
# train_task <- makeClassifTask(data = train_task_data, target = "y", positive = 1)
train_task <- makeRegrTask(data = train_task_data, target = "eonr") #task = data

## Tune hyperparameters
ctrl <- makeTuneControlRandom(maxit = 200) 
ps <- makeParamSet(
  makeIntegerParam("num.trees", lower=50, upper=300),
  makeIntegerParam("min.node.size", lower=5, upper=50),
  makeIntegerParam("mtry", lower=2, upper=8)
)

library("parallelMap")
parallelStartSocket(3)

res <- tuneParams("regr.ranger", task = train_task, 
                  resampling = makeFixedHoldoutInstance(train_ind, validation_ind, size), 
                  par.set = ps, 
                  control = ctrl) 


parallelStop()

learner_rf = setHyperPars(makeLearner("regr.ranger"), par.vals = res$x)#instructions

# Train a model
mod_rf <- train(learner_rf, task = regr_task)
names(mod_rf)
mod_rf$learner.model


#Make preditions
task_pred = predict(mod_rf, task = regr_task)

task_pred$data$truth
measureRMSE(truth = task_pred$data$truth, response = task_pred$data$response)
cor(task_pred$data$truth, task_pred$data$response)

# =========================================================================================================================================================
# COMPARE n predictions IN THE TRAINING SET

eonr_mukey_dt2
eonr_mukey_dt2[,eonr_rf := ceiling(predict(model2b_eonr, eonr_mukey_dt2, type = "class")/10)*10] 

ggplot(eonr_mukey_dt2) +
  geom_density(aes(x = eonr), colour = 'red') +
  geom_density(aes(x = eonr_rf), colour = 'blue')

ggplot(data = eonr_mukey_dt2, aes(x = eonr, y = eonr_rf)) +
  geom_point()+
  geom_smooth(se=FALSE)+
  coord_fixed() + geom_abline() + ylim(0, 350)+ xlim(0, 350) +
  theme(aspect.ratio=1, 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_bw()

reg_2 <- lm(data =eonr_mukey_dt2, eonr ~ eonr_rf) #check a fitted line between pred and obs
summary(reg_2)


#---------------------
#Profits in the training set
TrainSet

results_list <- list()
results_list[[1]] <- TrainSet[,eonr_rf := ceiling(predict(model2b_eonr, TrainSet, type = "class")/10)*10] %>% 
  .[ N_fert == eonr_rf] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := '5']


results_list[[2]] <- TrainSet[, .SD[ P == max( P)], by = .(id_10, mukey, z)] %>%
  .[,c("mukey", "z", "id_10", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := '12']

results_list[[3]] <- merge( TrainSet[,-'eonr_rf'],model_minimum_regional, by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "Yld", "leach_n2", "n_deep_v5","N_fert",'P')] %>% .[,model := '1']

perfomances_dt <- rbindlist(results_list)

perfomances_dt[, .(Yld =  mean(Yld),
                   leach_n2 = mean(leach_n2),
                   N_fert = mean(N_fert),
                   N_fert_min = min(N_fert),
                   N_fert_max = max(N_fert),
                   P = mean(P)), by = .( model)]

#---------------------
# COmpare profits

paired <- merge(perfomances_dt[model == '5', .(id_10, mukey,z, model, N_fert,P)],
                perfomances_dt[model == '12', .(id_10, mukey,z, model, N_fert,P)],
                by = c('id_10', 'mukey', 'z'), suffixes=c("", "_12"))

ggplot(data = paired[sample(1:nrow(paired), 300)], aes(x = P_12, y = P)) +
  geom_point()+
  geom_smooth(se=FALSE)+
  coord_fixed() + geom_abline() +# ylim(0, 350)+ xlim(0, 350) +
  theme(aspect.ratio=1, 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_bw()

#----------------------------
# Check a regression

formula_lm <- paste0('eonr ~ ', paste(no_cost_varb, collapse = '+'), '+', paste(ss_varb, collapse = '+'))

reg <- lm(data = eonr_mukey_dt2, formula = as.formula(formula_lm))
summary(reg)

eonr_mukey_dt2$eonr_reg <- predict(reg, eonr_mukey_dt2)

cor(eonr_mukey_dt2$eonr, eonr_mukey_dt2$eonr_rf) #rf
cor(eonr_mukey_dt2$eonr, eonr_mukey_dt2$eonr_reg) # reg

ggplot(data = eonr_mukey_dt2, aes(x = eonr, y = eonr_reg)) +
  geom_point()+
  geom_smooth(se=FALSE)+
  coord_fixed() + geom_abline() + ylim(0, 350)+ xlim(0, 350) +
  theme(aspect.ratio=1, 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_bw()

reg_2 <- lm(data =eonr_mukey_dt2, eonr ~ eonr_reg) #check a fitted line between pred and obs
summary(reg_2)
# =========================================================================================================================================================
# =========================================================================================================================================================
# =========================================================================================================================================================
# COMPARE n predictions IN THE VALIDATION SET
ValidSet3[, P := Yld * Pc - N_fert * Pn]

ValidSet3_eonr <- ValidSet3[, .SD[ P == max( P)], by = .(id_10, mukey, z)]
setnames(ValidSet3_eonr, 'N_fert', 'eonr')

ValidSet3_eonr[,eonr_rf := ceiling(predict(model2b_eonr, ValidSet3_eonr, type = "class")/10)*10] 

ggplot(ValidSet3_eonr) +
  geom_density(aes(x = eonr), colour = 'red') +
  geom_density(aes(x = eonr_rf), colour = 'blue')

ggplot(data = ValidSet3_eonr, aes(x = eonr, y = eonr_rf)) +
  geom_point()+
  geom_smooth(se=FALSE)+
  coord_fixed() + geom_abline() + ylim(0, 350)+ xlim(0, 350) +
  theme(aspect.ratio=1, 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_bw()

reg_2 <- lm(data =ValidSet3_eonr, eonr ~ eonr_rf) #check a fitted line between pred and obs
summary(reg_2)

#----------------------------
task_pred = predict(mod_rf, newdata = ValidSet3_eonr)
ValidSet3_eonr$eonr_mlr <- task_pred$data$response
ValidSet3_eonr[,eonr_mlr := ceiling(eonr_mlr/10)*10] 

ggplot(data = ValidSet3_eonr, aes(x = eonr, y = eonr_mlr)) +
  geom_hex()+
  geom_smooth(se=FALSE)+
  coord_fixed() + geom_abline() + ylim(0, 350)+ xlim(0, 350) +
  theme(aspect.ratio=1, 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_bw()

#---------------------
#Profits in the validation set
results_list <- list()
results_list[[1]] <- ValidSet3[,eonr_rf := ceiling(predict(model2b_eonr, ValidSet3, type = "class")/10)*10] %>% 
  .[ N_fert == eonr_rf] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := '5']


results_list[[2]] <- ValidSet3[, .SD[ P == max( P)], by = .(id_10, mukey, z)] %>%
  .[,c("mukey", "z", "id_10", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := '12']

results_list[[3]] <- merge( ValidSet3[,-'eonr_rf'],model_minimum_regional, by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "Yld", "leach_n2", "n_deep_v5","N_fert",'P')] %>% .[,model := '1']

results_list[[4]] <- ValidSet3[,eonr_reg := ceiling(predict(reg, ValidSet3)/10)*10] %>% 
  .[ N_fert == eonr_reg] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := 'reg']

results_list[[5]] <- ValidSet3[,eonr_mlr := round(predict(mod_rf, newdata = ValidSet3)$data$response/10)*10] %>% 
  .[ N_fert == eonr_mlr] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := '5_mlr']

perfomances_dt <- rbindlist(results_list)

perfomances_dt[, .(Yld =  mean(Yld),
                   leach_n2 = mean(leach_n2),
                   N_fert = mean(N_fert),
                   N_fert_min = min(N_fert),
                   N_fert_max = max(N_fert),
                   P = mean(P)), by = .( model)]

#---------------------
# Compare profits

paired <- merge(perfomances_dt[model == '5', .(id_10, mukey,z, model, N_fert,P)],
                perfomances_dt[model == '12', .(id_10, mukey,z, model, N_fert,P)],
                by = c('id_10', 'mukey', 'z'), suffixes=c("", "_12"))

ggplot(data = paired[sample(1:nrow(paired), 300)], aes(x = P_12, y = P)) +
  geom_point()+
  geom_smooth(se=FALSE)+
  coord_fixed() + geom_abline() +# ylim(0, 350)+ xlim(0, 350) +
  theme(aspect.ratio=1, 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_bw()

#----------------------------
# Check a regression

ValidSet3_eonr$eonr_reg <- predict(reg, ValidSet3_eonr)

cor(ValidSet3_eonr$eonr, ValidSet3_eonr$eonr_rf) #rf
cor(ValidSet3_eonr$eonr, ValidSet3_eonr$eonr_reg) # reg

ggplot(data = ValidSet3_eonr, aes(x = eonr, y = eonr_reg)) +
  geom_point()+
  geom_smooth(se=FALSE)+
  coord_fixed() + geom_abline() + ylim(0, 350)+ xlim(0, 350) +
  theme(aspect.ratio=1, 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_bw()

reg_3 <- lm(data =ValidSet3_eonr, eonr ~ eonr_reg) #check a fitted line between pred and obs
summary(reg_3)
# =========================================================================================================================================================
# CREATE THE REGIONAL REGRESSION


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

#Make a small validation set
ValidSet[,.(Yld = mean(Yld)), by = z]
# ValidSet <- merge(ValidSet,coordinates_dt , by = c('id_10', 'mukey'))
# ValidSet2 <- ValidSet[id_10 %in% sample(unique(ValidSet$id_10), 30)]
ValidSet[, P := Yld * Pc - N_fert * Pn]

ggplot() + 
  geom_density(data = eonr_mukey_dt, aes(x = eonr), color = 'red') +
  geom_density(data = results_list[[8]], aes( x= N_fert), color = 'blue')

results_list <- list()
results_list[[1]] <- ValidSet[,eonr_pred := ceiling(predict(model2b_eonr, ValidSet, type = "class")/10)*10-20] %>% 
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := '5-20']

results_list[[2]] <- ValidSet[,eonr_pred := ceiling(predict(model2b_eonr, ValidSet, type = "class")/10)*10-10] %>% 
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := '5-10']

results_list[[3]] <- ValidSet[,eonr_pred := ceiling(predict(model2b_eonr, ValidSet, type = "class")/10)*10] %>% 
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := '5']

results_list[[4]] <- ValidSet[,eonr_pred := ceiling(predict(model2b_eonr, ValidSet, type = "class")/10)*10+10] %>% 
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := '5+10']

results_list[[5]] <- ValidSet[,eonr_pred := ceiling(predict(model2b_eonr, ValidSet, type = "class")/10)*10+20] %>% 
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := '5+20']

results_list[[6]] <- ValidSet[,eonr_pred := ceiling(predict(model2b_eonr, ValidSet, type = "class")/10)*10+30] %>% 
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := '5+30']

results_list[[7]] <- ValidSet[,eonr_pred := ceiling(predict(model2b_eonr, ValidSet, type = "class")/10)*10+40] %>% 
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := '5+40']


results_list[[8]] <- ValidSet[, .SD[ P == max( P)], by = .(id_10, mukey, z)] %>%
  .[,c("mukey", "z", "id_10", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := '12']

results_list[[9]] <- merge(ValidSet[,-'eonr_pred'],model_minimum_regional, by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10", "Yld", "leach_n2", "n_deep_v5","N_fert",'P')] %>% .[,model := '1']


perfomances_dt <- rbindlist(results_list)

perfomances_dt[, .(Yld =  mean(Yld),
                   leach_n2 = mean(leach_n2),
                   N_fert = mean(N_fert),
                   N_fert_min = min(N_fert),
                   N_fert_max = max(N_fert),
                   P = mean(P)), by = .( model)]



paired <- merge(perfomances_dt[model == '5', .(id_10, mukey,z, model, N_fert,P)],
                perfomances_dt[model == '12', .(id_10, mukey,z, model, N_fert,P)],
                by = c('id_10', 'mukey', 'z'), suffixes=c("", "_12"))

ggplot(data = paired, aes(x = N_fert_12, y = N_fert, colour = as.factor(model))) +
  geom_hex()+
  geom_smooth(se=FALSE)+
  coord_fixed() + geom_abline() + ylim(0, 350)+ xlim(0, 350) +
  theme(aspect.ratio=1, 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_bw()

geom_hex()

reg_3 <- lm(data = paired, N_fert ~ N_fert_12)
summary(reg_3)

ggplot(data = paired[sample(1:nrow(paired), 300)], aes(x = P_12, y = P)) +
  geom_point()+
  geom_smooth(se=FALSE)+
  coord_fixed() + geom_abline() +# ylim(0, 350)+ xlim(0, 350) +
  theme(aspect.ratio=1, 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_bw()

paired[,res:= N_fert - N_fert_12]

eonr_mukey_dt[,eonr_pred := ceiling(predict(model2b_eonr, eonr_mukey_dt, type = "class")/10)*10] %>% 
  .[N_fert == eonr_pred] %>%
  .[,c("mukey", "z", "id_10",  "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )] %>% .[,model := '5']


ggplot(data = eonr_mukey_dt, aes(x = eonr_pred)) +
  geom_density()


ggplot(data = eonr_mukey_dt, aes(x = eonr)) +
  geom_density()

paired[,P_diff:= P - P_12]

paired_meanP <- paired[, .(P_diff = mean(P_diff)), by = res]
sapply(paired_meanP, class)


ggplot(data = paired_meanP, aes(x = res, y = P_diff)) +
  geom_point()








ValidSet2 <- ValidSet2[, .SD[ P == max( P)], by = .(id_10, mukey, z, prev_crop)]
ValidSet2 <- ValidSet2[,c('N_fert', no_cost_varb, ss_varb), with = FALSE]
setnames(ValidSet2, 'N_fert', 'eonr')
ValidSet3 <- ValidSet2[sample(1:nrow(ValidSet2), 2000),]

ValidSet3[,eonr_pred := predict(model2b_eonr, ValidSet3, type = "class")]
ValidSet3[, eonr_pred_round := round(eonr_pred/10, 0)*10]
ValidSet3[, eonr_pred_ceil := ceiling(eonr_pred/10)*10]
ValidSet3[, eonr_pred_floor := floor(eonr_pred/10)*10]


rmse_dt <- melt(ValidSet3, id.vars = 'eonr', measure.vars = names(ValidSet3)[24:26])
rmse_dt[,res := (value - eonr)^2]
rmse_dt[,.(rmse = sqrt(mean(res))),
        by = variable]


# =========================================================================================================================================================
reg_model_stuff[['no_cost_var']] <-  no_cost_varb
reg_model_stuff[['ss_var']] <-  ss_varb
reg_model_stuff[['full_fields']] <-  full_fields_dt2[,-'rf']
reg_model_stuff[['stations']] <-  stations_dt2[,-'rs']
reg_model_stuff[['training_z']] <-  training_z
# reg_model_stuff[['trial_rates']] <-  trial_rates

saveRDS(reg_model_stuff, "./vr_value_v2/Data/files_rds/reg_model_stuff.rds")

