# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
setwd('~')
rm(list=ls())

source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')
source('./n_policy_git/Codes/parameters.R')
# source('C:/Users/germanm2/Documents/n_policy_validation_git/Codes/parameters.R')

#---------------------------------------------------------------------------------------
# Load MRTN data
mrtn_dt1 <- read.csv('./n_policy_box/Data/validation/mrtn_yield_south.csv', header = F, col.names = c('eonr_lb_ac', 'yield_bu_ac')) %>% data.table() %>% 
  .[,region := '1']
mrtn_dt2 <- read.csv('./n_policy_box/Data/validation/mrtn_yield_central.csv', header = F, col.names = c('eonr_lb_ac', 'yield_bu_ac')) %>% data.table() %>% 
  .[,region := '2']
mrtn_dt3 <- read.csv('./n_policy_box/Data/validation/mrtn_yield_north.csv', header = F, col.names = c('eonr_lb_ac', 'yield_bu_ac')) %>% data.table() %>% 
  .[,region := '3']
mrtn_dt <- rbindlist(list(mrtn_dt1, mrtn_dt2, mrtn_dt3))
# Corn: 1 bushel/acre = 62.77 (63) kilograms/hectare
mrtn_dt[,Y_corn := 62.77*yield_bu_ac]
# Corn: 1 lb/acre = 1.12085 kilograms/hectare
mrtn_dt[,eonr := 1.12085*eonr_lb_ac]
mrtn_dt[,source := 'mrtn']

#---------------------------------------------------------------------------------------
# Load Simulated data

testing_set_dt <- readRDS("./n_policy_box/Data/files_rds/testing_set_dt.rds")
testing_set_dt[, P := Y_corn * Pc - N_fert * Pn]  #update profits

simulated_dt <- testing_set_dt[, .SD[ P == max( P)], by = .(id_10, mukey, z)] %>% 
  .[, .SD[ N_fert == min( N_fert )], by = .(region, id_10, mukey, z)]
setnames(simulated_dt, 'N_fert', 'eonr')
simulated_dt[,source := 'simulated']

#---------------------------------------------------------------------------------------
# Merge data
data_dt <- rbind(simulated_dt[,.(source, region, Y_corn)], mrtn_dt[,.(source, region, Y_corn)])

#---------------------------------------------------------------------------------------
# Region boxplots

ggplot(data = data_dt) +
  geom_boxplot(aes(y = Y_corn, x = source))+
  facet_wrap(region~.) 
