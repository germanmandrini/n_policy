# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
# setwd('~')

source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')

library(randomForest)
# source('./vr_value/Codes/parameters.R')
# eonr_mukey_dt2 <- readRDS("./vr_value/Data/files_rds/eonr_mukey_dt2.rds")

# full_fields_dt2 <- readRDS("./vr_value/Data/files_rds/full_fields_dt2.rds")
# 
# grid10_soils_sf6 <- readRDS("./vr_value/Data/Grid/grid10_soils_sf6.rds")
# 
# grid10_tiles_sf2 <- readRDS("./vr_value/Data/Grid/grid10_tiles_sf2.rds") 

yc_yearly_dt3 <- readRDS("./vr_value/Data/files_rds/yc_yearly_dt3.rds")

reg_model_stuff <- readRDS("./vr_value/Data/files_rds/reg_model_stuff.rds")

stations_dt3 <- reg_model_stuff$stations
no_cost_varb <- reg_model_stuff$no_cost_var
ss_varb <- reg_model_stuff$ss_var

rm(reg_model_stuff)
# full_fields_dt3 <- reg_model_stuff[['full_fields']]
# rm(full_fields_dt3)

#======================================================================================
# GET THE DATA FROM RS
training_z <- c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10" )

# training_z_regional <- c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10", "A11", "A12", "A13", "A14", "A15", "A16", "A17", "A18", "A19", "A20", "A21", "A22", "A23", "A24", "A25", "A26", "A27", "A28", "A29", "A30" )

# yc_yearly_dt3 <- merge(yc_yearly_dt3, eonr_mukey_dt2[,c("id_10", "mukey", "z", "prev_crop", "area_ha","long", "lat", "region", "Yld_lt_avg", "Yld_lt_min", "Yld_lt_max")],
#                        by = c("id_10", "mukey", "z", "prev_crop")) #add the info we added to eonr_mukey

train_vect <- filter_dt_in_dt(yc_yearly_dt3 , filter_dt = unique(stations_dt3[,.(id_10, mukey)]))

# Filter only the 10 training z
TrainSet <- yc_yearly_dt3[train_vect] %>% .[z %in% training_z]

#Clean the data

# TrainSet[,Yld_max_z := max(Yld), by = .(id_10, mukey, z, prev_crop)]

# hist(TrainSet$Yld_max_z)
# TrainSet[Yld_max_z < 10] # killed by frost
# TrainSet[Yld_max_z < 2000] # bad data
# TrainSet <- TrainSet[Yld_max_z > 2000 ] #remove bad data. No farmer will apply N 
# TrainSet <- TrainSet[,-'Yld_max_z']

TrainSet[,.N, .(id_10)]
table(TrainSet[, .(soils = length(unique(mukey))), by = id_10]$soils)

# ValidSet <- yc_yearly_dt3[-train_vect] #%>% .[!z %in% training_z_regional]

all(order(unique(TrainSet$id_10)) == order(unique(stations_dt3$id_10)))

# # =========================================================================================================================================================
# CREATE THE YR MODEL
source('./vr_value/Codes/parameters.R')
TrainSet[,Yld_max := max(Yld), by = .(id_10, mukey, z, prev_crop)]
TrainSet[,Yld_rel := Yld/Yld_max]

Y_rel_seq <- c(seq(0,0.9, by = 0.05), seq(0.91,1, by = 0.01))

reg_model_stuff_policy <- list()

for(yr_n in Y_rel_seq){
  # yr_n = 0.9
  eonr_mukey_dt <- TrainSet[Yld_rel >= yr_n]
  eonr_mukey_dt <- eonr_mukey_dt[, .SD[ N_fert == min( N_fert)], by = .(id_10, mukey, z, prev_crop)] #in case two rates had equal profits
  eonr_mukey_dt <- eonr_mukey_dt[,c('mukey','N_fert', no_cost_varb, ss_varb), with = FALSE]
  setnames(eonr_mukey_dt, 'N_fert', 'eonr')
  
  # Create a Random Forest model with default parameters
  model_yr <- randomForest(eonr ~ ., data = eonr_mukey_dt[,c('eonr', no_cost_varb, ss_varb), with = FALSE],
                            importance = TRUE, ntree=500)
  
  name_model = paste0('model_yr_', yr_n)
  name_figure <- paste0("./vr_value/Data/figures/n_policy/", name_model, "_variables.jpg")
  
  jpeg(name_figure)
  varImpPlot(model_yr, type=2, main = name_model)
  dev.off() 
  
  
  reg_model_stuff_policy[[name_model]] <-  model_yr
}

# =========================================================================================================================================================
# CREATE THE LEACHING FEE MODEL
fee_seq <- seq(0, Pe_max, by = 2)

for(fee_n in fee_seq){
  # fee_n = fee_seq[1]
  TrainSet[, P := Yld * Pc - N_fert * Pn - leach_n2 * fee_n]
  eonr_mukey_dt <- TrainSet[, .SD[ P == max( P)], by = .(id_10, mukey, z, prev_crop)]
  eonr_mukey_dt <- eonr_mukey_dt[, .SD[ N_fert == min( N_fert)], by = .(id_10, mukey, z, prev_crop)] #in case two rates had equal profits
  eonr_mukey_dt <- eonr_mukey_dt[,c('mukey','N_fert', no_cost_varb, ss_varb), with = FALSE]
  setnames(eonr_mukey_dt, 'N_fert', 'eonr')
  
  # Create a Random Forest model with default parameters
  model_fee <- randomForest(eonr ~ ., data = eonr_mukey_dt[,c('eonr', no_cost_varb, ss_varb), with = FALSE],
                            importance = TRUE, ntree=500)
  
  name_model = paste0('model_fee_', fee_n)
  name_figure <- paste0("./vr_value/Data/figures/n_policy/", name_model, "_variables.jpg")
  
  jpeg(name_figure)
  varImpPlot(model_fee, type=2, main = name_model)
  dev.off() 
  
  
  reg_model_stuff_policy[[name_model]] <-  model_fee
}

# =========================================================================================================================================================
# CREATE THE N TAX MODEL 5%
tax_seq <- seq(0, 10, by = 0.25)

for(tax_n in tax_seq){
  # tax_n = 0.05
  TrainSet[, P := Yld * Pc - N_fert * (Pn * (1+tax_n))]
  eonr_mukey_dt <- TrainSet[, .SD[ P == max( P)], by = .(id_10, mukey, z, prev_crop)]
  eonr_mukey_dt <- eonr_mukey_dt[, .SD[ N_fert == min( N_fert)], by = .(id_10, mukey, z, prev_crop)] #in case two rates had equal profits
  eonr_mukey_dt <- eonr_mukey_dt[,c('mukey','N_fert', no_cost_varb, ss_varb), with = FALSE]
  setnames(eonr_mukey_dt, 'N_fert', 'eonr')
  summary(eonr_mukey_dt)
  
  # Create a Random Forest model with default parameters
  model_tax <- randomForest(eonr ~ ., data = eonr_mukey_dt[,c('eonr', no_cost_varb, ss_varb), with = FALSE],
                                importance = TRUE, ntree=500)
  
  name_model = paste0('model_tax_', tax_n)
  name_figure <- paste0("./vr_value/Data/figures/n_policy/", name_model, "_variables.jpg")
  
  jpeg(name_figure)
  varImpPlot(model_tax, type=2, main = name_model)
  dev.off() 
  
  
  reg_model_stuff_policy[[name_model]] <-  model_tax
}


saveRDS(reg_model_stuff_policy, "./vr_value/Data/files_rds/reg_model_stuff_policy.rds")

