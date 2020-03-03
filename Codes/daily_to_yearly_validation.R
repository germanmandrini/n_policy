# Load libraries and functions #################################################
# GOAL: compare the intial condition simulation for 2009 and the YC simulation for the same year

setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('C:/Users/germanm2/Box Sync/My_Documents') #CPSC
source('./Codes_useful/R.libraries.R')

#Load the initial conditions
files_all <- list.files('./vr_value/Data/initial_conditions/',full.names = T, recursive = T, pattern = '_stab.rds')

initial_condtions_ls <- list()
for(file_n in files_all){
  data_tmp <- readRDS(file_n)
  initial_condtions_ls[[length(initial_condtions_ls)+1]] <- data_tmp[year > 2008]
  
}

initial_yearly_dt <- rbindlist(initial_condtions_ls) %>% .[,.(Y = max(Y), fertiliser = sum(fertiliser)), by = .(id_10, mukey, rotation, z, type, sim_name, year)]
rm(initial_condtions_ls)

#Load the YC output
files_all <- list.files('./vr_value/Data/yc_output/',full.names = T, recursive = T, pattern = 'A2_YC.rds')

files_all[str_detect(files_all, '1413387')]

yc_ls <- list()
for(file_n in files_all){
  # file_n = files_all[1]
  data_tmp <- readRDS(file_n)
  
  
  yc_ls[[length(yc_ls)+1]] <- readRDS(file_n)
}

yieldcurve_yearly_dt <- rbindlist(yc_ls) %>% .[,.(Y = max(Y, na.rm = T), fertiliser = sum(fertiliser)), by = .(id_10, mukey, rotation, z, type, sim_name, year)]

yieldcurve_yearly_dt$fertiliser

#Merge both
setnames(initial_yearly_dt, 'Y', 'Y_stab')
setnames(yieldcurve_yearly_dt, 'Y', 'Y_yc')

initial_yearly_dt[mukey == 921541]
yieldcurve_yearly_dt[mukey == 921541]

yc_yearly_dt <- readRDS('./vr_value/Data/files_rds/yc_yearly_dt.rds')

yc_yearly_dt$mukey[1000]

