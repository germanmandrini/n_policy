# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents') #CPSC
# setwd("/home/germanm2")
# setwd('~')

# source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')

# library(randomForest)
# library(ranger)
# library(mlr)
# source('./vr_value_v2/Codes/parameters.R')
# Pn = 0.87 * 2

# eonr_mukey_dt2 <- readRDS("./vr_value_v2/Data/files_rds/eonr_mukey_dt2.rds")
# grid10_tiles_sf6 <- readRDS("./vr_value_v2/Data/Grid/grid10_tiles_sf6.rds") 
# grid10_soils_dt5 <- readRDS("./vr_value_v2/Data/Grid/grid10_soils_dt5.rds") %>% data.table()
yc_yearly_dt3 <- readRDS("./vr_value_v2/Data/files_rds/yc_yearly_dt3.rds")
# grid10_fields_sf2 <- readRDS('./vr_value_v2/Data/Grid/grid10_fields_sf2.rds')


# reg_model_stuff <- readRDS("./vr_value_v2/Data/files_rds/reg_model_stuff.rds")
# full_fields_dt <- reg_model_stuff[['full_fields']]
reg_model_stuff <- readRDS("./vr_value_v2/Data/files_rds/reg_model_stuff.rds")
stations_dt <- reg_model_stuff[['stations']]
# rm(reg_model_stuff)


#======================================================================================


# SPLIT THE DATA (TRAINING, VALIDATION)-------------------
# training_z <- 1:10
# set.seed(234)
# training_z <- sort(sample(1:30, 10, replace = F))

# training_z <- c(1:4, 16:21)
# training_z <- c(4, 6, 8, 11, 13, 15, 16, 17, 28, 25)
training_z <- c(11:20)

z_odd = c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)
z_even = z_odd+1

indx <- filter_dt_in_dt(yc_yearly_dt3, filter_dt = unique(stations_dt[,.(id_10, mukey)]))

TrainSet <- yc_yearly_dt3[indx]
TrainSet <- TrainSet[z %in% training_z]

z_dry <- TrainSet[,.(Yld = mean(Yld), .N), by = z][Yld < 5000]$z
TrainSet <- TrainSet[!z %in% z_dry]

# Filter the right training z, depending on odd or even fields. Remove some z that are coming from fields 1 and 2 that are not RS
stations_dt[,z_type := ifelse(id_field == 1, 'odd', 'even')]
TrainSet[,z_type := ifelse(z %in% z_odd, 'odd', 'even')]

right_mukey_z_combination <- unique(stations_dt[,.(id_10, mukey, z_type)])
right_mukey_z_combination[, right_comb := 1]

right_mukey_z_combination[,.N, by = id_10]
right_mukey_z_combination[id_10 == 440]
stations_dt[id_10 == 440]

TrainSet2 <- merge(TrainSet, right_mukey_z_combination, all.x = T)
TrainSet2 <- TrainSet2[!is.na(right_comb)]
TrainSet2[ ,right_comb := NULL]

# Plot a relationship
# TrainSet_eonr <- TrainSet2[, P := Yld * Pc - N_fert * Pn] %>% .[, n_0_60cm_v5 := n_20cm_v5 + n_40cm_v5 + n_60cm_v5] %>% 
#   .[, .SD[ P == max( P)], by = .(id_10, mukey, z)]
# setnames(TrainSet_eonr, 'N_fert', 'eonr')
# 
# TrainSet_eonr[id_10 == 440]
# unique(stations_dt[,.(id_10, id_field, mukey)]) %>% .[,.N, by = .(id_10, id_field)] %>% .[,.(N = mean(N))]
# 
# ggplot(data = TrainSet_eonr, aes(x = n_0_60cm_v5, y = eonr)) + 
#   geom_point() + geom_smooth(formula = 'y ~ x', method = lm)

# # Make a Validation set
# ValidSet <- yc_yearly_dt3[-indx]
# 
# sets_rf <- unique(ValidSet[,.(id_10, mukey)])
# sets_rf <- sets_rf[sample(1:nrow(sets_rf), 100)]
# ValidSet2 <- filter_dt_in_dt(ValidSet, filter_dt = sets_rf, return_table = T)
# 
# ValidSet2_eonr <- ValidSet2[, P := Yld * Pc - N_fert * Pn] %>% .[, n_0_60cm_v5 := n_20cm_v5 + n_40cm_v5 + n_60cm_v5] %>% 
#   .[, .SD[ P == max( P)], by = .(id_10, mukey, z)]
# setnames(ValidSet2_eonr, 'N_fert', 'eonr')
# 
# ggplot(data = ValidSet2_eonr, aes(x = n_0_60cm_v5, y = eonr)) + 
#   geom_point() + geom_smooth(formula = 'y ~ x', method = lm)

# Save the objects that will be needed later
# reg_model_stuff <- list()
# reg_model_stuff[['ValidSet']] <- ValidSet2

reg_model_stuff[['TrainSet']] <- TrainSet2
reg_model_stuff[['training_z']] <-  training_z
saveRDS(reg_model_stuff, "./vr_value_v2/Data/files_rds/reg_model_stuff.rds")
