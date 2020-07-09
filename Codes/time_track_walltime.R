# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
# setwd('~')
rm(list=ls())

source('./Codes_useful/R.libraries.R')

files_time <- list.files('S:/Bioinformatics Lab/germanm2/n_policy_cameron/time_track',full.names = T, recursive = T)

results_list <- list()
for(file_n in files_time){
  # file_n <- files_daily[2]
  # print(file_n)
  results_list[[basename(file_n)]] <- readRDS(file_n)
}

time_track_dt <- rbindlist(results_list)
time_track_dt[mukey_n ==0]

time_track_walltime_dt <- time_track_dt[,.(id_10, mukey_n, dur = cell)]
ggplot(time_track_walltime_dt) +
  geom_point(aes(x = mukey_n, y = dur))

time_track_walltime_dt[,dur := round(dur,0)+2]


saveRDS(time_track_walltime_dt, "./n_policy_box/Data/files_rds/time_track_walltime_dt.rds")
id_10_walltime_dt <- readRDS("./n_policy_box/Data/files_rds/id_10_walltime_dt.rds")
comp_dt <- merge(id_10_walltime_dt, time_track_walltime_dt, by = 'id_10')
ggplot(comp_dt) +
  geom_point(aes(x = dur.x, y = dur.y))
