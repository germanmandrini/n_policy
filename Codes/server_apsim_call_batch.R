rm(list=ls())

library(stringr)
library(data.table)
# install.packages('XML','~/Rlibs')"

#Get the computer where this is running

server <- ifelse(Sys.info()["nodename"] == "campodonico", TRUE, FALSE)
cpsc <-ifelse(Sys.info()["nodename"] == "CPSC-P10E53323", TRUE, FALSE)
cluster <- str_detect(string = Sys.info()["nodename"], pattern = 'campuscluster')
print(Sys.info()["nodename"])

#Set the wd
if(server){
  setwd('~')
  codes_folder <- getwd()
  test_small <- T
}else if(cpsc){
  setwd('C:/Users/germanm2/Box Sync/My_Documents')
  codes_folder <-'C:/Users/germanm2/Documents'
  test_small <- TRUE 
}else{
  setwd('/projects/aces/germanm2/')
  cluster <- TRUE	
  codes_folder <- '/projects/aces/germanm2'
  test_small <- FALSE #makes a small run 
}

#----------------------------------------------------------------------------
id10_n = as.numeric(commandArgs(trailingOnly=TRUE)[1])
# id10_n = 172
# id10_n = 1024
# id10_n = 736
# id10_n = 1504
# id10_n = 74
# id10_n = 987
# id10_n = 1078
id10_n = 29
batch_n = 43
print(id10_n)
# id10_seq <- c(1426, 1212, 180, 29, 652, 765)
# id10_seq <- c(1322,  984, 699, 403, 152,  69)
# id10_seq <- c(1181, 1063, 938, 496, 258, 301)
# for(batch_n in c(43)){
  # id10_seq <- c(180, 29)
  # for(id10_n in id10_seq){
# CREATE ALL STAB FILES
start1 <- Sys.time()
"C:/Users/germanm2/Documents/n_policy_git/Codes/server1_create_stab_files.R"
"./n_policy_git/Codes/server1_create_stab_files.R"
source(paste0(codes_folder, '/n_policy_git/Codes/server1_create_stab_files.R'))
instructions1_rows <- nrow(instructions)

#RUN ALL APSIM FILES
start2 <- Sys.time()
"C:/Users/germanm2/Documents/n_policy_git/Codes/server_run_files_nov20.R"
"./n_policy_git/Codes/server_run_files_nov20.R"
source(paste0(codes_folder, '/n_policy_git/Codes/server_run_files_nov20.R'))

#MERGE ALL THE OUTPUT
start3 <- Sys.time()
"C:/Users/germanm2/Documents/n_policy_git/Codes/server_merge_results.R"
"./n_policy_git/Codes/server_merge_results.R"
source(paste0(codes_folder, '/n_policy_git/Codes/server_merge_results.R'))

#CREATE ALL YC FILES
start4 <- Sys.time()
"C:/Users/germanm2/Documents/n_policy_git/Codes/server2_create_yc_files.R"
"./n_policy_git/Codes/server2_create_yc_files.R"
source(paste0(codes_folder, '/n_policy_git/Codes/server2_create_yc_files.R'))
instructions2_rows <- nrow(instructions)

#RUN ALL APSIM FILES
start5 <- Sys.time()
"C:/Users/germanm2/Documents/n_policy_git/Codes/server_run_files_nov20.R"
"./n_policy_git/Codes/server_run_files_nov20.R"
source(paste0(codes_folder, '/n_policy_git/Codes/server_run_files_nov20.R'))

#MERGE ALL THE OUTPUT
start6 <- Sys.time()
"C:/Users/germanm2/Documents/n_policy_git/Codes/server_merge_results.R"
'./n_policy_git/Codes/server_merge_results.R'
source(paste0(codes_folder, '/n_policy_git/Codes/server_merge_results.R'))
 
#MAKE YEARLY SUMMARY
files_daily <- list.files(paste0('./n_policy_box/Data/yc_output_', batch_n), pattern = paste0('^',id10_n, '_'), full.names = T)
print(files_daily)
'./n_policy_git/Codes/1_daily_to_yearly_jul15.R'
"C:/Users/germanm2/Documents/n_policy_git/Codes/1_daily_to_yearly_jul15.R"
source(paste0(codes_folder, '/n_policy_git/Codes/1_daily_to_yearly_jul15.R'))

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

# time_track <- rbind(time_track, time_track_tmp)
folder_name <- paste0('./n_policy_box/Data/time_track_', batch_n)
if(!file.exists(folder_name)){dir.create(folder_name, recursive = TRUE)}
saveRDS(time_track_tmp, paste0(folder_name,'/time_track_',id10_n,'.rds'))

unlink(directory, recursive = TRUE)
#   }# end id10_n loop
# }#end batch_n loop
