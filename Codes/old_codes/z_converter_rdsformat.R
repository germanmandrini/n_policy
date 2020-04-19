files_list <- list.files('C:/Users/germanm2/Box Sync/My_Documents/n_policy_box/Data/met_files', full.names = T)

for(file_n in files_list){
  # file_n <- files_list[121]
  weather_tmp <- readRDS(file_n)
  print(paste(file_n, unique(weather_tmp$id_10)[1]))
  saveRDS(weather_tmp, file_n)
  
}
# saveRDS(weather_tmp, file_n, version=2)