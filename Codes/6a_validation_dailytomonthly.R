######################################
# Parallelized Simulations
####################################### 
# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
# setwd('~')

library(stringr)
library(data.table)
# install.packages('XML','~/Rlibs')"
reg_model_stuff <- readRDS( "./n_policy_box/Data/files_rds/reg_model_stuff.rds")
minimum_ok <- reg_model_stuff$ratio_6$minimum_ok
rm(reg_model_stuff)
grid10_tiles_dt <- readRDS("./n_policy_box/Data/Grid/grid10_tiles_sf6.rds") %>% data.table()


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

#===================================
# prepare clusters
#===================================

# no_cores <- ifelse(detectCores() == 8, 6, 32)
# 
# cl <- makeCluster(no_cores,type='SOCK')

#===================================
# parallelized  
#===================================


make_yearly_summary <- function(file_n){
  # file_n =  "S:/Bioinformatics Lab/germanm2/n_policy_cluster/yc_output/324_680866.rds"
  # file_n <- files_daily[3]
  # file_n <- "S:/Bioinformatics Lab/germanm2/vr_value_v2_cluster/yc_output_1/1367_159876.rds"
  
  # file_n <- "S:/Bioinformatics Lab/germanm2/n_policy_cluster/yc_output/1060_173466.rds"
  #--------------------------
  # preparation
  #--------------------------
  #--- load libraries ---#
  library(data.table)
  library(stringr)
  #ERROR HANDLING
  possibleError <- tryCatch({
  
    daily_yc_dt <- readRDS(file_n) 
    daily_yc_dt <- daily_yc_dt[!str_detect(daily_yc_dt$sim_name, pattern = 'n_rich|n_minus' )] #remove sensor's treatments
    
    daily_yc_dt$N_fert <- as.numeric(lapply(strsplit(as.character(daily_yc_dt$sim_name), split="_"), "[", 5))+10
    baselevel_n <- minimum_ok[region == grid10_tiles_dt[id_10 == unique(daily_yc_dt$id_10)]$region]$eonr_pred
    
    #filter the rows with baselevel n
    monthly_dt <- daily_yc_dt[N_fert == baselevel_n,.(leach_no3 = sum(leach_no3)), by = .(sim_name, id_10,  mukey,  z, year, month, N_fert)]

    
  }, error = function(e) e)
  
  #REAL WORK:if there is no error
  if(!inherits(possibleError, "error")){
    # return(rbindlist(all_summaries, fill = TRUE))
    if(!dir.exists("./n_policy_box/Data/yc_output_summary_calendar")){dir.create("./n_policy_box/Data/yc_output_summary_calendar")}
    saveRDS(monthly_dt, paste0("./n_policy_box/Data/yc_output_summary_calendar/", basename(file_n)))
    return(paste0("./n_policy_box/Data/yc_output_summary_calendar/", basename(file_n)))
  } else {
    return(c(file_n, possibleError))
  }
  
  
}

# grid10_horizons_v2_dt <- readRDS("./n_policy_box/Data/Grid/grid10_horizons_v2_dt.rds")

files_daily <- list.files('S:/Bioinformatics Lab/germanm2/n_policy/yc_output',full.names = T, recursive = T)
# files_daily <- list.files('./n_policy_box/Data/yc_output',full.names = T, recursive = T)
files_daily <- files_daily[!basename(files_daily) %in% files_run]
test <- make_yearly_summary(files_daily[1])
readRDS(test)
# start <- Sys.time()
results_list <- list()
for(file_n in files_daily){
  # file_n <- files_daily[2]
  # print(file_n)
  results_list[[basename(file_n)]] <- make_yearly_summary(file_n)
}
# time_lasted <- Sys.time() - start
# print(time_lasted)
unlist(results_list)
files_run <- list.files('./n_policy_box/Data/yc_output_summary_calendar',full.names = F, recursive = T)
files_daily[!basename(files_daily) %in% files_run]
