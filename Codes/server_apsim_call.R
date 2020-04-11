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
}else if(cpsc){
  setwd('C:/Users/germanm2/Box Sync/My_Documents')
  codes_folder <-'C:/Users/germanm2/Documents'
}else{
  setwd('/projects/aces/germanm2/')
  cluster <- TRUE	
  codes_folder <- '/projects/aces/germanm2'
}

test_small <- TRUE #makes a small run 

# id_10_seq <- unique(grid10_soils_dt4$id_10)

# write.csv(id_10_seq, './n_policy_git/Codes/id_10_out.csv')

# grid10_soils_dt4[,field_area := sum(area_ha), by = .(id_10, id_field)]

#----------------------------------------------------------------------------
# LIST RUNNED FILES

# id_10_seq <- readRDS("./n_policy_box/Data/files_rds/id_10_walltime_dt.rds")$id_10

if(FALSE){
	runned <- list.files('./n_policy_box/Data/yc_output/')
	runned <- unique(sapply(strsplit(as.character(runned), split="_"), "[", 1))
	runned <- unlist(str_extract_all(runned, pattern = '[0-9]+'))
     
	# id_10_seq <- sort(unique(grid10_soils_sf4$id_10))
	# id_10_seq <- id_10_seq[!id_10_seq %in% id_10_runned] %>% .[!id_10_seq %in% problems]
	id_10_seq <- id_10_seq[!id_10_seq %in% id_10_runned]
}  
# id_10_seq <- sample(id_10_seq)
# time_track <- data.table()

#time_track <- readRDS('./n_policy_box/Data/yc_output/time_track.rds')

# for(id10_n in id_10_seq){
#id10_n = 5
id10_n = as.numeric(commandArgs(trailingOnly=TRUE)[1])

 print(id10_n)

  #CREATE ALL STAB FILES
  start1 <- Sys.time()
  "C:/Users/germanm2/Documents/n_policy_git/Codes/server1_create_stab_files.R"
  source(paste0(codes_folder, '/n_policy_git/Codes/server1_create_stab_files.R'))
  instructions1_rows <- nrow(instructions)

  #RUN ALL APSIM FILES
  start2 <- Sys.time()
  "C:/Users/germanm2/Documents/n_policy_git/Codes/server_run_files_nov20.R"
  source(paste0(codes_folder, '/n_policy_git/Codes/server_run_files_nov20.R'))
  
  #MERGE ALL THE OUTPUT
  start3 <- Sys.time()
  "C:/Users/germanm2/Documents/n_policy_git/Codes/server_merge_results.R"
  source(paste0(codes_folder, '/n_policy_git/Codes/server_merge_results.R'))

  #CREATE ALL YC FILES
  start4 <- Sys.time()
  "C:/Users/germanm2/Documents/n_policy_git/Codes/server2_create_yc_files.R"
  source(paste0(codes_folder, '/n_policy_git/Codes/server2_create_yc_files.R'))
  instructions2_rows <- nrow(instructions)
  
  #RUN ALL APSIM FILES
  start5 <- Sys.time()
  "C:/Users/germanm2/Documents/n_policy_git/Codes/server_run_files_nov20.R"
  source(paste0(codes_folder, '/n_policy_git/Codes/server_run_files_nov20.R'))

  #MERGE ALL THE OUTPUT
  start6 <- Sys.time()
  "C:/Users/germanm2/Documents/n_policy_git/Codes/server_merge_results.R"
  source(paste0(codes_folder, '/n_policy_git/Codes/server_merge_results.R'))
  
  #MAKE YEARLY SUMMARY
  files_daily <- list.files('./n_policy_box/Data/yc_output', pattern = paste0(id10_n, '_'), full.names = T)
  "C:/Users/germanm2/Documents/n_policy_git/Codes/1_daily_to_yearly_nov26.R"
  source(paste0(codes_folder, '/n_policy_git/Codes/1_daily_to_yearly_nov26.R'))
  
  start7 <- Sys.time()
  
  time_track_tmp <- data.table(id_10 = id10_n,
                               mukey_n = length(unique(instructions2$mukey)),
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
  saveRDS(time_track_tmp, paste0('./n_policy_box/Data/time_track/time_track_',id10_n,'.rds'))
  
  unlink(directory, recursive = TRUE)
