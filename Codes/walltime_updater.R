rm(list=ls())

library(data.table)
library(dplyr)
# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
#setwd('/projects/aces/germanm2/')
setwd('C:/Users/germanm2/Box Sync/My_Documents') #CPSC
codes_folder <-'C:/Users/germanm2/Documents'


#------------------------
#Create the walltime
grid10_soils_dt4 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt4.rds")
grid10_soils_dt4[, z_type := ifelse(id_field %in% c(1,3), 'odd', 'even')]


id_10_walltime_dt <- grid10_soils_dt4[,.N, by = .(id_10, mukey, z_type)] #number of id_10 x mukey x 15z combinations
id_10_walltime_dt2 <- id_10_walltime_dt[,.N, by = .(id_10)][order(N)] 
id_10_walltime_dt2[,dur := N*6]

id_failed <- c(866, 27, 1061, 1251, 939, 949, 242, 748, 1124, 516, 815, 918, 18, 1505, 359)
id_10_walltime_dt2[id_10 %in% id_failed, dur := N*10]

id_10_walltime_dt2 <- id_10_walltime_dt2[id_10 %in% id_failed]



write.table(id_10_walltime_dt2[,.(id_10, dur)], paste0(codes_folder,'/n_policy_git/id_10_walltime.txt'), row.names = F, col.names = F)


# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# codes_folder <-'C:/Users/germa/Documents'

#------------------------
# Load stations
# reg_model_stuff <- readRDS("./n_policy_box/Data/files_rds/reg_model_stuff.rds")
# stations_dt <- reg_model_stuff[['stations']] 
# rm(reg_model_stuff)

#------------------------

# files_all <- list.files('./n_policy_box/Data/yc_output_summary/', recursive = T, pattern = '.rds', full.names = F)
# files_all <- list.files('S:/Bioinformatics Lab/germanm2/n_policy_cluster/yc_output/', recursive = T, pattern = '.rds')
# files_all <- list.files('S:/Bioinformatics Lab/germanm2/n_policy/yc_output_summary/', recursive = T, pattern = '.rds')

# id_10_runned <- unique(sapply(strsplit(as.character(files_all), split="_"), "[", 1) )

id_10_walltime_dt <- readRDS("./n_policy_box/Data/files_rds/id_10_walltime_dt.rds")
# id_10_walltime_dt <- readRDS("./n_policy_box/Data/files_rds/time_track_walltime_dt.rds")

#Removed runned
# id_10_walltime_dt <- id_10_walltime_dt[!id_10 %in% id_10_runned]


#Pick only the failed ones
id_10_walltime_dt[,dur := N *  6]
id_10_walltime_dt[N >2, dur := N *  5]
id_10_walltime_dt[N >4, dur := N *  4]
id_failed <- c(442, 1124,618,953,319,627,924,126,406,825,98,473,1042,260,825, 1116, 
               1035, 241, 264, 273, 402, 476,  49, 576,  62, 639 ,722, 599, 72,407)
id_10_walltime_dt <- id_10_walltime_dt[id_10 %in% id_failed, dur := N *  6]
id_10_walltime_dt <- id_10_walltime_dt[order(dur)]
# id_failed <- c(1006, 102, 112, 126, 127, 152, 158, 160, 161, 167, 172, 179, 265, 268, 274, 277, 278, 279, 284, 285, 286, 287, 288, 292, 301, 327, 337, 354, 363, 374, 383, 384, 390, 621, 631, 632, 633, 641, 719, 720, 730, 738, 741, 745, 749, 751, 755, 756, 759, 762, 769, 771, 774, 790, 805, 806, 93, 988, 991, 992)
# id_10_walltime_dt <- id_10_walltime_dt[id_10 %in% id_failed]
# id_failed2 <- c(1006, 102, 112, 126, 127, 152, 158, 160, 161, 167, 172, 179, 265, 268, 274, 277, 278, 279, 284, 285, 286, 287, 288, 292, 301, 327, 337, 354, 363, 374, 383, 384, 390, 621, 631, 632, 632, 633, 641, 67, 719, 720, 730, 730, 738, 741, 745, 749, 751, 755, 756, 759, 762, 769, 771, 774, 790, 805, 806, 93, 988, 991, 992)
# setdiff(id_failed2, id_failed)


# id_10_walltime_dt <- id_10_walltime_dt[id_10 %in% id_failed]
#------------------------
# Filter stations random
if(FALSE){
  set.seed(123)
  stations_dt2 <- stations_dt[,.N, by = .(region, id_10)]
  id_10_stations <- stations_dt2[,.SD[sample(.N, 5)],by = region]$id_10
  id_10_walltime_dt <- id_10_walltime_dt[id_10 %in% id_10_stations]
}
#------------------------
# Order stations first all stations
id_10_stations <- stations_dt[,.N, by = .(region, id_10)]$id_10
id_10_walltime_dt <- rbind(id_10_walltime_dt[id_10 %in% id_10_stations], 
                           id_10_walltime_dt[!id_10 %in% id_10_stations])
#------------------------
# id_failed_timetrack <- c(935,1095,1184,123,263,29,496,500,842,978,221,1189,1246,406,421,363)
# id_failed_timetrack <- c(879,291, 1437, )
# id_10_walltime_dt <- id_10_walltime_dt[id_10 %in% id_failed_timetrack]
# id_10_walltime_dt[,dur := N *  6]

write.table(id_10_walltime_dt[,.(id_10, dur)], paste0(codes_folder,'/n_policy_git/id_10_walltime.txt'), row.names = F, col.names = F)

if(FALSE){ #send again files while still running
  
  id_10_walltime_dt_sent <- fread('./n_policy/id_10_walltime.txt', col.names = c('id_10', 'dur'))
  id_10_walltime_dt <- id_10_walltime_dt[!id_10 %in% id_10_walltime_dt_sent$id_10]
  id_10_walltime_dt[,dur := N *  4]
  files_all <- list.files('S:/Bioinformatics Lab/germanm2/n_policy_cluster/yc_output/', recursive = T, pattern = '.rds')
  id_10_runned <- unique(sapply(strsplit(as.character(files_all), split="_"), "[", 1) )
  id_10_walltime_dt <- id_10_walltime_dt[!id_10 %in% id_10_runned]
  id_10_walltime_dt <- id_10_walltime_dt[order(dur)]
  write.table(id_10_walltime_dt[,.(id_10, dur)], './n_policy/id_10_walltime2.txt', row.names = F, col.names = F)
  
}


if(FALSE){ #create the walltime file
  grid10_soils_dt4 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt4.rds")
  id_10_walltime_dt <- grid10_soils_dt4[,.N, by = .(id_10, id_field, mukey)][,-'N']
  id_10_walltime_dt[id_field == 3 ,id_field := 1 ]
  id_10_walltime_dt[id_field == 4 ,id_field := 2 ]
  id_10_walltime_dt <- unique(id_10_walltime_dt )
  id_10_walltime_dt <- id_10_walltime_dt[,.N, by =id_10][order(N)]
  id_10_walltime_dt[,dur := N *  2]
  write.table(id_10_walltime_dt[,.(id_10, dur)], './n_policy/id_10_walltime.txt', row.names = F, col.names = F)
  saveRDS(id_10_walltime_dt, "./n_policy_box/Data/files_rds/id_10_walltime_dt.rds")
}


if(FALSE){ #re run id10 that didn't pass the daily to yearly
  id_10_rerun <- unique(sapply(strsplit(as.character(basename(files_daily2)), split="_"), "[", 1) )
  id_10_walltime_dt <- readRDS("./n_policy_box/Data/files_rds/id_10_walltime_dt.rds")
  id_10_walltime_dt <- id_10_walltime_dt[id_10 %in% id_10_rerun]
  write.table(id_10_walltime_dt[,.(id_10, dur)], './n_policy/id_10_walltime.txt', row.names = F, col.names = F)
}

if(FALSE){ #re run id10 that didn't pass the daily to yearly
  grid10_soils_dt4 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt4.rds")
  files_yearly <- list.files("./n_policy_box/Data/yc_output_summary", recursive = T)
  library(data.table)
  already_run_dt <- data.table(id_10 = as.integer(sapply(strsplit(as.character(files_yearly), split="_"), "[", 1)),
                            mukey = gsub(sapply(strsplit(as.character(files_yearly), split="_"), "[", 2),pattern = '.rds', replacement = '')) 
  already_run_dt[,run := 'ok']
  grid10_soils_dt4_difference <- merge(grid10_soils_dt4, already_run_dt, by = c('id_10', 'mukey'))
  id_10_walltime_dt <- grid10_soils_dt4_difference[,.N, by = .(id_10, id_field, mukey)][,-'N']
  id_10_walltime_dt[id_field == 3 ,id_field := 1 ]
  id_10_walltime_dt[id_field == 4 ,id_field := 2 ]
  id_10_walltime_dt <- unique(id_10_walltime_dt )
  id_10_walltime_dt <- id_10_walltime_dt[,.N, by =id_10][order(N)]
  id_10_walltime_dt[,dur := N *  4]
  write.table(id_10_walltime_dt[,.(id_10, dur)], './n_policy/id_10_walltime.txt', row.names = F, col.names = F)
  saveRDS(id_10_walltime_dt, "./n_policy_box/Data/files_rds/id_10_walltime_dt.rds")
  write.table(id_10_walltime_dt[,.(id_10, dur)], './n_policy/id_10_walltime.txt', row.names = F, col.names = F)
}  
  
if(FALSE){ #re run id10 that didn't run in the sensor running
  normal_files <- list.files("./n_policy_box/Data/yc_output_summary_1", full.names = F)
  length(normal_files)
  sensor_files <- list.files("./n_policy_box/Data/yc_output_summary_2", full.names = F)
  length(sensor_files)
  missing <- sensor_files[!sensor_files %in% normal_files]
  id_10_missing <- unique(sapply(strsplit(as.character(missing), split="_"), "[", 1) )
  id_10_missing <- c(5, id_10_missing)
  #Create a new walltime file
  grid10_soils_dt4 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt4.rds")
  id_10_walltime_dt <- grid10_soils_dt4[,.N, by = .(id_10, id_field, mukey)][,-'N']
  id_10_walltime_dt[id_field == 3 ,id_field := 1 ]
  id_10_walltime_dt[id_field == 4 ,id_field := 2 ]
  id_10_walltime_dt <- unique(id_10_walltime_dt ) #each row is a mukey with a set of z. Each row is 15 runs
  id_10_walltime_dt <- id_10_walltime_dt[,.N, by =id_10][order(N)] #Cound of 15 runs by id_10 
  id_10_walltime_dt[,dur := N *  2]
  id_10_walltime_dt <- id_10_walltime_dt[id_10 %in% id_10_missing]
  id_10_walltime_dt[id_10 == 5, dur := dur + 10]
  write.table(id_10_walltime_dt[,.(id_10, dur)], './n_policy/id_10_walltime2.txt', row.names = F, col.names = F)
}

if(FALSE){ #find missing mukeys
  grid10_soils_dt4 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt4.rds")
  files_yearly <- list.files("./n_policy_box/Data/yc_output_summary_88_swat", recursive = T)
  length(files_yearly)
  
  already_run_dt <- data.table(id_10 = as.integer(sapply(strsplit(as.character(files_yearly), split="_"), "[", 1)),
                               mukey = gsub(sapply(strsplit(as.character(files_yearly), split="_"), "[", 2),pattern = '.rds', replacement = '')) 
  
  already_run_dt[,run := 'ok']
  
  grid10_soils_dt4 <- grid10_soils_dt4[id_10 %in% unique(already_run_dt$id_10)]
  grid10_soils_dt4_difference <- merge(grid10_soils_dt4, already_run_dt, by = c('id_10', 'mukey'), all.x = T)
  
  incomplete_id10 <- grid10_soils_dt4_difference[is.na(run)]$id_10 %>% unique()

  id_10_walltime_dt2 <- id_10_walltime_dt2[id_10 %in% incomplete_id10,]
  write.table(id_10_walltime_dt2[,.(id_10, dur)], paste0(codes_folder,'/n_policy_git/id_10_walltime.txt'), row.names = F, col.names = F)
  
}  