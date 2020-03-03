# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('~')
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")

source('./Codes_useful/R.libraries.R')
# source('./Codes_useful/gm_functions.R')

files_all <- list.dirs('~/vr_value/Data/yc_output',full.names =F, recursive = F)
saveRDS(files_all, '~/vr_value/Data/yc_output/files_runned_svr.rds')
