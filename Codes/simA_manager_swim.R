rm(list=ls())

library(stringr)
library(data.table)

#Get the computer where this is running
server <- ifelse(Sys.info()["nodename"] == "campodonico", TRUE, FALSE)
cpsc <-ifelse(Sys.info()["nodename"] == "CPSC-P10E53323", TRUE, FALSE)
cluster <- str_detect(string = Sys.info()["nodename"], pattern = 'campuscluster')
print(Sys.info()["nodename"])

#Set the wd
if(server){
  setwd('~')
  codes_folder <- getwd()
  test_small <- T #only one soil and one z
  regional_test <- F #makes rates every 25
  regional_soils <- F #uses regional soils
}else if(cpsc){
  setwd('C:/Users/germanm2/Box Sync/My_Documents')
  codes_folder <-'C:/Users/germanm2/Documents'
  test_small <- F
  regional_test <- T #makes rates every 25
  regional_soils <- F #uses regional soils
}else{
  # setwd('/projects/aces/germanm2/')
  setwd('/home/germanm2/scratch/')
  cluster <- TRUE	
  # codes_folder <- '/projects/aces/germanm2'
  codes_folder <- '/home/germanm2/scratch'
  test_small <- TRUE   #only one soil and one z
  regional_test <- FALSE #makes rates every 25
  regional_soils <- FALSE    #uses regional soils
}

#----------------------------------------------------------------------------
# 

water_n = 'swat'

if(server|cpsc){
  id10_n = 791
  batch_n = '88'
}else{
  id10_n = as.numeric(commandArgs(trailingOnly=TRUE)[1])
  batch_n = as.numeric(commandArgs(trailingOnly=TRUE)[2])
}

# id10_seq <- c(1426, 1212, 938, 765, 29, 69)
# id10_seq <- c( 1212, 938, 69, 1426, 765, 29) 
# id10_seq <- c(1212, 938, 69, 1426, 765, 29, 1069, 513, 53, 1069, 1200, 1507, 1330, 1363, 468 ,513, 678, 473, 655, 363, 355,  45, 188 , 53)
# id10_seq <- c(1426, 938, 29)

for(batch_n in c('89_105', '89_110', '89_115')){
id10_seq <- c(1212, 765, 69)

for(id10_n in id10_seq){
  # id10_n = id10_seq[1]
  print(id10_n)
  print(batch_n)

  # CREATE ALL STAB FILES
  start1 <- Sys.time()
  "C:/Users/germanm2/Documents/n_policy_git/Codes/simB_create_stab_files_swim.R"
  "./n_policy_git/Codes/simB_create_stab_files_swim.R"
  source(paste0(codes_folder, '/n_policy_git/Codes/simB_create_stab_files_swim.R'))
  instructions1_rows <- nrow(instructions)
  
  #RUN ALL APSIM FILES
  start2 <- Sys.time()
  "C:/Users/germanm2/Documents/n_policy_git/Codes/simE_run_files.R"
  "./n_policy_git/Codes/simE_run_files.R"
  source(paste0(codes_folder, '/n_policy_git/Codes/simE_run_files.R'))
  
  #MERGE ALL THE OUTPUT
  start3 <- Sys.time()
  "C:/Users/germanm2/Documents/n_policy_git/Codes/simF_merge_results.R"
  "./n_policy_git/Codes/simF_merge_results.R"
  source(paste0(codes_folder, '/n_policy_git/Codes/simF_merge_results.R'))
  
  #CREATE ALL YC FILES
  start4 <- Sys.time()
  "C:/Users/germanm2/Documents/n_policy_git/Codes/simG_create_yc_files_swim.R"
  "./n_policy_git/Codes/simG_create_yc_files_swim.R"
  source(paste0(codes_folder, '/n_policy_git/Codes/simG_create_yc_files_swim.R'))
  instructions2_rows <- nrow(instructions)
  
  #RUN ALL APSIM FILES
  start5 <- Sys.time()
  "C:/Users/germanm2/Documents/n_policy_git/Codes/simE_run_files.R"
  "./n_policy_git/Codes/simE_run_files.R"
  source(paste0(codes_folder, '/n_policy_git/Codes/simE_run_files.R'))
  
  #MERGE ALL THE OUTPUT
  start6 <- Sys.time()
  "C:/Users/germanm2/Documents/n_policy_git/Codes/simF_merge_results.R"
  './n_policy_git/Codes/simF_merge_results.R'
  source(paste0(codes_folder, '/n_policy_git/Codes/simF_merge_results.R'))
  
  #MAKE YEARLY SUMMARY
  files_daily <- list.files(paste0('./n_policy_box/Data/yc_output_', batch_n, '_', water_n), pattern = paste0('^',id10_n, '_'), full.names = T)
  print(files_daily)
  "C:/Users/germanm2/Documents/n_policy_git/Codes/simI_daily_to_yearly_swim.R"
  './n_policy_git/Codes/simI_daily_to_yearly_swim.R'
  source(paste0(codes_folder, '/n_policy_git/Codes/simI_daily_to_yearly_swim.R'))
  
  start7 <- Sys.time()
  
  time_track_tmp <- data.table(id_10 = id10_n,
                               mukey_n = length(unique(instructions$mukey)),
                               time = start1,
                               inst1 = instructions1_rows,
                               create1 = as.numeric(difftime(start2, start1, units = "mins")),
                               run1 = as.numeric(difftime(start3, start2, units = "mins")),
                               merge_save1 = as.numeric(difftime(start4, start3, units = "mins")),
                               inst2 = instructions2_rows,
                               create2 = as.numeric(difftime(start5, start4, units = "mins")),
                               run2 = as.numeric(difftime(start6, start5, units = "mins")),
                               merge_save2 = as.numeric(difftime(start7, start6, units = "mins")),
                               cell = as.numeric(difftime(start7, start1, units = "mins")))
  print(time_track_tmp)
  
  folder_name <- paste0('./n_policy_box/Data/time_track_', batch_n)
  if(!file.exists(folder_name)){dir.create(folder_name, recursive = TRUE)}
  saveRDS(time_track_tmp, paste0(folder_name,'/time_track_',id10_n,'.rds'))
  
  unlink(directory, recursive = TRUE)
}# end id10_n loop
}#end batch_n loop

