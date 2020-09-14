rm(list=ls())

# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# codes_folder <-'C:/Users/germanm2/Documents'#CPSC

# setwd('~')
setwd('~/scratch')
folder_name <- './n_policy_box/Data/yc_output_summary_147_swat/'
info <- file.info(list.files(folder_name, full.names = T))
saveRDS(info, './n_policy_box/Data/time_track_147/file_info.rds')


if(FALSE){
  #on the server
  setwd('~')
  source('./Codes_useful/R.libraries.R')
  file_info_dt <-readRDS('./n_policy_box/Data/time_track_147/file_info.rds') %>% data.table()
  class(file_info_dt$ctime)
  
  
  
  
  
  
  
  
  
  
}

