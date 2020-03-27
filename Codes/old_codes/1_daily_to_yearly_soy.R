######################################
# Parallelized Simulations
####################################### setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd('C:/Users/germa/Box Sync/My_Documents')#Dell
source('./Codes_useful/R.libraries.R')

#===================================
# prepare clusters
#===================================

no_cores <- detectCores() * 7/8

cl <- makeCluster(no_cores,type='SOCK')

#===================================
# parallelized  
#===================================


make_yearly_summary <- function(file_n){
  # file_n = files_all[25528]
  # file_n = 'S:/Bioinformatics Lab/germanm2/vr_value/yc_output/cell_20/20_426337_MSM_A7_YC.rds'
  #--------------------------
  # preparation
  #--------------------------
  #--- load libraries ---#
  library(data.table)
  library(stringr)
  #ERROR HANDLING
  possibleError <- tryCatch({
  
    daily_yc_dt <- readRDS(file_n)
    daily_yc_dt <- daily_yc_dt[sim_name == unique(daily_yc_dt$sim_name)[15]]
    daily_yc_dt[,prev_crop := 'MS']
    
    #=====================================================================================================#
    # Yearly data
    yearly_data <- daily_yc_dt[year == 2009,.(Yld = max(Y, na.rm = T)/0.85
                   #leach_no3 = sum(leach_no3),
                   # N_fert = sum(fertiliser),
                   # dul_dep = max(dul_dep),
                   # ll15_dep = max(ll15_dep),
                   #whc = max(dul_dep) - max(ll15_dep)
                   ), by = .(id_10, mukey, rotation,  z, type, year)]
    
    # prev_yield <- yearly_data[year == 2009] %>% .[,.(sim_name, Yld_prev = Yld)]
    # yearly_data <- merge(yearly_data[year == 2010], prev_yield)
    
    #=====================================================================================================#
    # LEACHING April to April
    daily_yc_dt[,leach_period := ifelse(year == 2009 & day > 100, 1, 
                                        ifelse(year == 2010 & day <= 100, 1, 0))]

    leach_data <- daily_yc_dt[leach_period == 1, .(leach_n = sum(leach_no3)), by = .(id_10, mukey, rotation,  z, type)]
    
    yearly_data <- merge(yearly_data, leach_data, by = c('id_10', 'mukey',  'rotation', 'z', 'type'))
    
    #=====================================================================================================#
    # #Delta N (define leaching as all N going below 150)
    # n_data1 <- daily_yc_dt[year == 2010 & day == 101, .(id_10, mukey, rotation, z, type, sim_name, n_deep, nh4_10, nh4_11, no3_10, no3_11)]
    # n_data2 <- daily_yc_dt[year == 2011 & day == 100, .(id_10, mukey, rotation, z, type, sim_name, n_deep, nh4_10, nh4_11, no3_10, no3_11)]
    # 
    # n_delta <- merge(n_data1 , n_data2, by = c('id_10', 'mukey', 'rotation', 'z', 'type', 'sim_name'))
    # 
    # n_delta[,n_delta := n_deep.y - n_deep.x]
    # 
    # yearly_data <- merge(yearly_data, n_delta[, .(id_10, mukey, rotation, z, type, sim_name,n_delta) ], by = c('id_10', 'mukey',  'rotation', 'z', 'type', 'sim_name'))
    # yearly_data[, leach_n2 := ifelse(n_delta > 0, leach_n + n_delta, leach_n)]  #assuming that any increase in N in the soil solution will be eventually leached
    
    
    #=====================================================================================================#
    #Delta N (define leaching as all N going below 150)
    n_delta <- daily_yc_dt[year == 2010 & day == 100, .(id_10, mukey, rotation, z, type, sim_name, n_deep, nh4_10, nh4_11, no3_10, no3_11)]

    n_delta[,n_low_150cm := nh4_10 + nh4_11 + no3_10 + no3_11]

    
    
    yearly_data <- merge(yearly_data, n_delta[, .(id_10, mukey, rotation, z, type, n_low_150cm) ], by = c('id_10', 'mukey',  'rotation', 'z', 'type'))
    
    
    yearly_data[, leach_n2 := leach_n + n_low_150cm]  #assuming that any increase in N in the soil solution will be eventually leached
    
  }, error = function(e) e)
  
  #REAL WORK:if there is no error
  if(!inherits(possibleError, "error")){
    return(yearly_data[,-'n_low_150cm'])
  } else {
    return(c(file_n, possibleError))
  }
  
  
}

files_all <- list.files('S:/Bioinformatics Lab/germanm2/vr_value/yc_output',full.names = T, recursive = T, pattern = '_YC.rds')


# files_all <- list.files('~/vr_value/Data/yc_output/',full.names = T, recursive = T, pattern = '_YC.rds')

# files_all <- list.files('//aces-dfs-03.ad.uillinois.edu/cpsc-share/Bioinformatics Lab/germanm2/vr_value/yc_output',full.names = T, recursive = T, pattern = '_YC.rds')

# re_runned <- c(1000, 1080, 1138, 280, 283, 435, 52, 813, 960)
# pattern_find <- paste0('cell_', re_runned, collapse = '|')
# files_all2 <- files_all[str_detect(string = files_all, pattern = pattern_find)]
files_all2 <- files_all[str_detect(string = files_all, pattern = 'MSM')]
# files_all2 <- files_all2[str_detect(string = files_all2, pattern = '1862661')]



keep <- c('keep', 'make_yearly_summary')
# if(unique(instructions$type) == 'YC'){ keep <- append(keep, 'initial_conditions' )}
# # #rm(list = ls()[!ls() %in% keep])

clusterExport(cl, varlist = keep, envir=environment())

start <- Sys.time()
results_list <- parallel::parLapply(cl,files_all2, function(x) make_yearly_summary(x))
saveRDS(results_list, './vr_value/Data/files_rds/results_list_soy.rds')
time_lasted <- Sys.time() - start
print(time_lasted)

# no_file <- which(!sapply(results_list, is.data.table))
# no_file2 <- files_all[no_file]
# saveRDS(no_file2, './vr_value/Data/files_rds/no_file2.rds')

# start <- Sys.time()
# results_list <- parallel::parLapply(cl,no_file2, function(x) make_yearly_summary(x))
# saveRDS(results_list, './vr_value/Data/files_rds/results_list.rds')
# time_lasted <- Sys.time() - start
# print(time_lasted)

stopCluster(cl)
results_list2 <- results_list[sapply(results_list, is.data.table)]
length(results_list2)
yc_yearly_dt <- rbindlist(results_list2, fill = TRUE)
yc_yearly_dt[,prev_crop := 2)]
yc_yearly_dt <- yc_yearly_dt[,-'rotation']
saveRDS(yc_yearly_dt, './vr_value/Data/files_rds/yc_yearly_soy_dt.rds')

#-----------------------------------------------------------------------
#Update re-runs
# results_list2 <- results_list[sapply(results_list, is.data.table)]
# length(results_list2)
# update_dt1 <- rbindlist(results_list2, fill = TRUE)
# update_dt1[,prev_crop := ifelse(rotation == 'MSM', 0, 1)]
# update_dt1 <- update_dt1[,-'rotation']
# 
# 
# yc_yearly_dt <- readRDS('./vr_value/Data/files_rds/yc_yearly_dt.rds')
# update_dt2 <- yc_yearly_dt[id_10 %in% re_runned]
# yc_yearly_dt <- yc_yearly_dt[!id_10 %in% re_runned]
# 
# update_dt3 <- rbind(update_dt1, update_dt2)
# update_dt3 <- unique(update_dt3)
# 
# table(update_dt3[,.N, by = .(id_10, mukey, prev_crop, z)]$N)
# 
# yc_yearly_dt <- rbind(yc_yearly_dt, update_dt3)
# check <- yc_yearly_dt[,.N, by = .(id_10, mukey, prev_crop, z)][N != 33]
# 
# source('./Codes_useful/gm_functions.R')
# update_dt1 <- yc_yearly_dt[id_10 %in% unique(check$id_10)]
# yc_yearly_dt <- yc_yearly_dt[!id_10 %in% unique(check$id_10)]
# update_dt1 <- unique(update_dt1)
# yc_yearly_dt <- rbind(yc_yearly_dt, update_dt1)
# 
# saveRDS(yc_yearly_dt, './vr_value/Data/files_rds/yc_yearly_dt.rds')
