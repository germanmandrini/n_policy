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

stations_dt3 <- reg_model_stuff[['stations']]
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

no_cost_varb <- c("long", "lat", "prev_crop", "rain_30", "rain_60", "rain_90", 
                  "t_max_30", "t_max_60", "t_max_90", "t_min_30", "t_min_60", 
                  "t_min_90", "Yld_prev", 'Yld_lt_avg', 'Yld_lt_min', 'Yld_lt_max')

ss_varb <- c("n_20cm_v5", "n_40cm_v5", "n_60cm_v5", "esw_pct_v5", "whc")

# # =========================================================================================================================================================
# CREATE THE REGIONAL RF-95% (it is 95% of the yield at EONR)
source('./vr_value/Codes/parameters.R')
TrainSet[, P := Yld * Pc - N_fert * Pn]
#EONR data
eonr_mukey_dt <- TrainSet[, .SD[ P == max( P)], by = .(id_10, mukey, z, prev_crop)]
eonr_mukey_dt <- eonr_mukey_dt[, .SD[ N_fert == min( N_fert)], by = .(id_10, mukey, z, prev_crop)] #in case two rates had equal profits
setnames(eonr_mukey_dt, c('N_fert', 'Yld', 'leach_n2', "P"), c('N_fert_eonr', 'Yld_eonr', 'leach_eonr', "P_eonr"))

#Y max date
ymx_mukey_dt <- TrainSet[, .SD[ Yld == max(Yld)], by = .(id_10, mukey, z, prev_crop)]
ymx_mukey_dt <- ymx_mukey_dt[, .SD[ N_fert == min( N_fert)], by = .(id_10, mukey, z, prev_crop)] #select the lowest rate that gives a yield at least 95% of max
setnames(ymx_mukey_dt, c('N_fert', 'Yld', 'leach_n2', "P"), c('N_fert_mx', 'Yld_mx', 'leach_mx', "P_mx"))

# 95% relative yield
# TrainSet[,Yld_max := max(Yld), by = .(id_10, mukey, z, prev_crop)]
# TrainSet[,Yld_r := Yld/Yld_max]
# 
# n95_mukey_dt <- TrainSet[Yld_r >= 0.95]
# n95_mukey_dt <- n95_mukey_dt[, .SD[ N_fert == min( N_fert)], by = .(id_10, mukey, z, prev_crop)] #select the lowest rate that gives a yield at least 95% of max
# 
# setnames(n95_mukey_dt, c('N_fert', 'Yld', 'leach_n2', "P"), c('N_fert95', 'Yld95', 'leach_n95', "P_95"))

n95_mukey_dt <- merge(TrainSet,
      eonr_mukey_dt[,.(id_10, mukey, z, prev_crop, Yld_eonr)], 
      by = c('id_10', 'mukey', 'z', 'prev_crop'))
      
n95_mukey_dt[,Yld_r := Yld/Yld_eonr]
n95_mukey_dt <- n95_mukey_dt[Yld_r >= 0.95]
n95_mukey_dt <- n95_mukey_dt[, .SD[ N_fert == min( N_fert)], by = .(id_10, mukey, z, prev_crop)] #select the lowest rate that gives a yield at least 95% of max

setnames(n95_mukey_dt, c('N_fert', 'Yld', 'leach_n2', "P"), c('n_95', 'Yld95', 'leach_n95', "P_95"))

compare_dt <- merge(ymx_mukey_dt[,.(id_10, mukey, z, prev_crop, N_fert_mx, Yld_mx, leach_mx, P_mx)],
                    eonr_mukey_dt[,.(id_10, mukey, z, prev_crop, N_fert_eonr, Yld_eonr, leach_eonr, P_eonr)], 
                    by = c('id_10', 'mukey', 'z', 'prev_crop'))
compare_dt <- merge(compare_dt,
                    n95_mukey_dt[,.(id_10, mukey, z, prev_crop, n_95, Yld95, leach_n95, P_95)],
                    by = c('id_10', 'mukey', 'z', 'prev_crop'))


compare_dt[,.(N_fert_mx = mean(N_fert_mx),
              N_fert_eonr = mean(N_fert_eonr),
              n_95 = mean(n_95),
              Yld_mx = mean(Yld_mx),
              Yld_eonr = mean(Yld_eonr),
              Yld95 = mean(Yld95),
              leach_mx = mean(leach_mx),
              leach_eonr = mean(leach_eonr),
              leach_n95 = mean(leach_n95),
              P_mx = mean(P_mx),
              P_eonr = mean(P_eonr),
              P_95 = mean(P_95))]

# Create a Random Forest model with default parameters
model_95 <- randomForest(n_95 ~ ., data = n95_mukey_dt[,c('n_95', no_cost_varb, ss_varb), with = FALSE],
                        importance = TRUE, ntree=500)
model_95
plot(model_95)

jpeg("./vr_value/Data/figures/n_policy/model_95_variables.jpg")
varImpPlot(model_95, type=2, main = 'a) model_95')
dev.off() 

reg_model_stuff_policy <- list()
reg_model_stuff_policy[['model_95']] <-  model_95

# =========================================================================================================================================================
# CREATE THE LEACHING FEE MODEL
fee_seq <- seq(Pe_min, Pe_max, by = 1)

for(fee_n in tax_seq){
  # fee_n = 9.98
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
  varImpPlot(model_tax, type=2, main = name_model)
  dev.off() 
  
  
  reg_model_stuff_policy[[name_model]] <-  model_fee
}




# # =========================================================================================================================================================
# # CREATE THE LEACHING FEE MODEL MIN
# TrainSet[, P := Yld * Pc - N_fert * Pn - leach_n2 * Pe_min]
# eonr_mukey_dt <- TrainSet[, .SD[ P == max( P)], by = .(id_10, mukey, z, prev_crop)]
# eonr_mukey_dt <- eonr_mukey_dt[, .SD[ N_fert == min( N_fert)], by = .(id_10, mukey, z, prev_crop)] #in case two rates had equal profits
# eonr_mukey_dt <- eonr_mukey_dt[,c('mukey','N_fert', no_cost_varb, ss_varb), with = FALSE]
# setnames(eonr_mukey_dt, 'N_fert', 'eonr')
# summary(eonr_mukey_dt)
# 
# 
# # Create a Random Forest model with default parameters
# model_fee_min <- randomForest(eonr ~ ., data = eonr_mukey_dt[,c('eonr', no_cost_varb, ss_varb), with = FALSE],
#                               importance = TRUE, ntree=500)
# 
# jpeg("./vr_value/Data/figures/n_policy/model_fee_min.jpg")
# varImpPlot(model_fee_min, type=2, main = 'b) model_fee_min')
# dev.off() 
# 
# reg_model_stuff_policy[['model_fee_min']] <-  model_fee_min
# # =========================================================================================================================================================
# 
# # CREATE THE LEACHING FEE MODEL MED
# TrainSet[, P := Yld * Pc - N_fert * Pn - leach_n2 * Pe_med]
# eonr_mukey_dt <- TrainSet[, .SD[ P == max( P)], by = .(id_10, mukey, z, prev_crop)]
# eonr_mukey_dt <- eonr_mukey_dt[, .SD[ N_fert == min( N_fert)], by = .(id_10, mukey, z, prev_crop)] #in case two rates had equal profits
# eonr_mukey_dt <- eonr_mukey_dt[,c('mukey','N_fert', no_cost_varb, ss_varb), with = FALSE]
# setnames(eonr_mukey_dt, 'N_fert', 'eonr')
# summary(eonr_mukey_dt)
# 
# 
# # Create a Random Forest model with default parameters
# model_fee_med <- randomForest(eonr ~ ., data = eonr_mukey_dt[,c('eonr', no_cost_varb, ss_varb), with = FALSE],
#                               importance = TRUE, ntree=500)
# 
# jpeg("./vr_value/Data/figures/n_policy/model_fee_med.jpg")
# varImpPlot(model_fee_med, type=2, main = 'c) model_fee_med')
# dev.off() 
# 
# 
# reg_model_stuff_policy[['model_fee_med']] <-  model_fee_med
# 
# # =========================================================================================================================================================
# # CREATE THE LEACHING FEE MODEL MAX
# TrainSet[, P := Yld * Pc - N_fert * Pn - leach_n2 * Pe_max]
# eonr_mukey_dt <- TrainSet[, .SD[ P == max( P)], by = .(id_10, mukey, z, prev_crop)]
# eonr_mukey_dt <- eonr_mukey_dt[, .SD[ N_fert == min( N_fert)], by = .(id_10, mukey, z, prev_crop)] #in case two rates had equal profits
# eonr_mukey_dt <- eonr_mukey_dt[,c('mukey','N_fert', no_cost_varb, ss_varb), with = FALSE]
# setnames(eonr_mukey_dt, 'N_fert', 'eonr')
# summary(eonr_mukey_dt)
# 
# 
# # Create a Random Forest model with default parameters
# model_fee_max <- randomForest(eonr ~ ., data = eonr_mukey_dt[,c('eonr', no_cost_varb, ss_varb), with = FALSE],
#                               importance = TRUE, ntree=500)
# 
# jpeg("./vr_value/Data/figures/n_policy/model_fee_max_variables.jpg")
# varImpPlot(model_fee_max, type=2, main = 'd) model_fee_max')
# dev.off() 
# 
# reg_model_stuff_policy[['model_fee_max']] <-  model_fee_max

# =========================================================================================================================================================
# CREATE THE N TAX MODEL 5%
tax_seq <- seq(0, 2, by = 0.1)

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

