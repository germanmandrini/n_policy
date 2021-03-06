######################################
# Parallelized Simulations
####################################### setwd('C:/Users/germa/Box Sync/My_Documents') #dell
library(stringr)
library(data.table)
# library(parallel)
# install.packages('XML','~/Rlibs')"

#Get the computer where this is running

server <- ifelse(Sys.info()["nodename"] == "campodonico", TRUE, FALSE)
cpsc <-ifelse(Sys.info()["nodename"] == "CPSC-P10E53323", TRUE, FALSE)
cluster <- str_detect(string = Sys.info()["nodename"], pattern = 'campuscluster')
print(Sys.info()["nodename"])

#Set the wd
if(server){
  setwd('~')
  no_cores <- 32
}else if(cpsc){
  setwd('C:/Users/germanm2/Box Sync/My_Documents')
  no_cores <- 6
}else{
  setwd('/projects/aces/germanm2/')
  cluster <- TRUE
  # no_cores <- detectCores()-2
  # print(no_cores)
}

#===================================
# prepare clusters
#===================================

# cl <- makeCluster(no_cores,type='SOCK')

#===================================
# parallelized  
#===================================


make_yearly_summary <- function(file_n){
  # file_n =  "S:/Bioinformatics Lab/germanm2/vr_value_v2_cluster/yc_output/324_680866.rds"
  # file_n <- files_daily[1]
  # file_n <- "S:/Bioinformatics Lab/germanm2/vr_value_v2_cluster/yc_output/1060_173466.rds"
  #--------------------------
  # preparation
  #--------------------------
  #--- load libraries ---#
  library(data.table)
  library(stringr)
  #ERROR HANDLING
  possibleError <- tryCatch({
  
    daily_yc_dt <- readRDS(file_n) 
    
    weather_cell_dt <- readRDS(paste('./vr_value_v2/Data/met_files/weather_z_cell', unique(daily_yc_dt$id_10), '_dt.rds', sep = '')) %>%
      .[year == 2010]
    
    initial_conditions_dt <- readRDS(str_replace(file_n, pattern = 'yc_output_1', replacement = 'initial_conditions')) %>%
      .[year == 2009]
    
    all_summaries <- list()
    
    for(z_n in unique(daily_yc_dt$z)){
      # z_n = unique(daily_yc_dt$z)[1]
      daily_yc_dt2 <- daily_yc_dt[z == z_n]
      
      weather_cell_dt2 <- weather_cell_dt[z == z_n]
      
      #=====================================================================================================#
      # Frozen
      make_it_dt <- daily_yc_dt2[,.(id_10, mukey, z, year,day, sim_name, stage_name, 
                                   paddock.maize.lai,  LeafNo, paddock.maize.swdef_photo, paddock.maize.swdef_expan)][year== 2010]
      
      make_it_dt[,id_10:= as.integer(id_10)]
      make_it_dt[,z:= as.integer(z)]
      
      sapply(make_it_dt[,.(id_10, year, day, z )], class)
      sapply(weather_cell_dt2[,.(id_10, year, day, z )], class)
      
      make_it_dt <- merge(make_it_dt, 
                          weather_cell_dt2[,.(id_10, year, day, maxt,  mint, z )], 
                          by = c('id_10', 'z', 'year', 'day'))
      
      # make_it_dt <- make_it_dt[sim_name == '259_942156_SMM_A2_YC_110']
      # make_it_dt[stage_name != 'nocrop']
      
      
      #max lai, stages
      make_it_dt2 <- make_it_dt[, .(LAI_max = max(paddock.maize.lai),
                     stages_cnt = length(unique(stage_name)),
                     end_crop = any(stage_name == 'end_crop')),
                 by = sim_name]
      #frozen
      # make_it_dt2 <- merge(make_it_dt2, make_it_dt[LeafNo > 5, .(frost_temp = min(mint)), by = sim_name], by = 'sim_name')
      
      #stress
      # unique(make_it_dt$stage_name)
      # stress_dt <- make_it_dt[stage_name != 'nocrop',.(stress_expan = round(mean(paddock.maize.swdef_expan)),2), by = .(sim_name, stage_name)]
      # stress_dt <- dcast(stress_dt, sim_name  ~ stage_name, value.var = 'stress_expan')
      # names(stress_dt)[-1]  <- paste0('str_', names(stress_dt)[-1])
      # 
      # make_it_dt2 <- merge(make_it_dt2, stress_dt, by = 'sim_name')
      
      #=====================================================================================================#
      # Yearly data
      yearly_data <- daily_yc_dt2[year < 2011,.(Yld = max(Y, na.rm = T)/0.85,
                     #leach_no3 = sum(leach_no3),
                     N_fert = sum(fertiliser),
                     dul_dep = max(dul_dep),
                     ll15_dep = max(ll15_dep),
                     whc = max(dul_dep) - max(ll15_dep)), by = .(id_10, mukey, z, sim_name, year)]
      #Add previous yield
      prev_yield <- initial_conditions_dt[z == z_n] %>% .[,.(Yld_prev = max(Y, na.rm = T)/0.85), by = .(id_10, z)]
      yearly_data <- merge(yearly_data[year == 2010], prev_yield, by = c('id_10', 'z'))
      
      #Add sowing date
      sowing_day <- daily_yc_dt2[stage_name == 'sowing', .(sim_name, day)]
      setnames(sowing_day, 'day', 'day_sow')
      yearly_data <- merge(yearly_data, sowing_day, by = c('sim_name'))
      #=====================================================================================================#
      # LEACHING April to April
      daily_yc_dt2[,leach_period := ifelse(year == 2010 & day > 100, 1, 
                                          ifelse(year == 2011 & day <= 100, 1, 0))]
  
      leach_data <- daily_yc_dt2[leach_period == 1, .(leach_n = sum(leach_no3)), by = .(id_10, mukey, z, sim_name)]
      
      yearly_data <- merge(yearly_data, leach_data, by = c('id_10', 'mukey',  'z', 'sim_name'))
      
      #=====================================================================================================#
      #Delta N (define leaching as all N going below 150)
      n_below150cm <- daily_yc_dt2[year == 2011 & day == 100, .(id_10, mukey, z, sim_name, n_deep, nh4_10, nh4_11, no3_10, no3_11)]
  
      n_below150cm[,n_low_150cm := nh4_10 + nh4_11 + no3_10 + no3_11]
  
      yearly_data <- merge(yearly_data, n_below150cm[, .(id_10, mukey, z, sim_name,n_low_150cm) ], by = c('id_10', 'mukey', 'z', 'sim_name'))
      
      
      yearly_data[, leach_n2 := leach_n + n_low_150cm]  #assuming that any increase in N in the soil solution will be eventually leached
      yearly_data <- yearly_data[,-'n_low_150cm']
      
      #=====================================================================================================#
      # Increase in N on top 150 cm
      n_top_cols <- c(paste0('no3_', 1:9), paste0('nh4_', 1:9))
      n_data1 <- daily_yc_dt2[year == 2010 & day == 101, c('id_10', 'mukey',  'z',  'sim_name', n_top_cols), with = F] %>%
        .[,n_top15_1 := no3_1+no3_2+no3_3+no3_4+no3_5+no3_6+no3_7+no3_8+no3_9+nh4_1+nh4_2+nh4_3+nh4_4+nh4_5+nh4_6+nh4_7+nh4_8+nh4_9] %>% 
        .[,-n_top_cols, with = F]
      
      n_data2 <- daily_yc_dt2[year == 2011 & day == 100, c('id_10', 'mukey',  'z',  'sim_name', n_top_cols), with = F] %>%
        .[,n_top15_2 := no3_1+no3_2+no3_3+no3_4+no3_5+no3_6+no3_7+no3_8+no3_9+nh4_1+nh4_2+nh4_3+nh4_4+nh4_5+nh4_6+nh4_7+nh4_8+nh4_9] %>% 
        .[,-n_top_cols, with = F]
      
      n_delta <- merge(n_data1 , n_data2, by = c('id_10', 'mukey',  'z',  'sim_name'))
      # 
      n_delta[,n_top15_delta := n_top15_2 - n_top15_1 ]
      # 
      yearly_data <- merge(yearly_data, n_delta[, .(id_10, mukey, z, sim_name,n_top15_delta) ], by = c('id_10', 'mukey',   'z',  'sim_name'))
      # yearly_data[, leach_n2 := ifelse(n_delta > 0, leach_n + n_delta, leach_n)]  #assuming that any increase in N in the soil solution will be eventually leached
      
      
      
      #=====================================================================================================#
      # SOIL FILES
      
      horizons_cell_dt <- grid10_horizons_v2_dt[mukey == unique(daily_yc_dt2$mukey)]
      horizons_cell_dt2 <- horizons_cell_dt[bottom <= 40, .(sand_40cm = mean(sand),
                                                           clay_40cm = mean(clay))]
      
      #=====================================================================================================#
      # V5 data
      
      v5_data <- daily_yc_dt2[LeafNo > 0 & LeafNo < 5 & year == 2010, .SD[LeafNo == max(LeafNo)], by = sim_name]
      
      v5_data[,oc_20cm := (oc_1*5 + oc_2*5 + oc_3*5 + oc_4*5)/20] #this should be done using the Bulk Density of each layer
      v5_data[,oc_40cm := (oc_20cm*20 + oc_5*20)/40]
      v5_data[,n_20cm :=  no3_1 + no3_2 + no3_3 + no3_4 + nh4_1 + nh4_2 + nh4_3 + nh4_4]
      v5_data[,n_40cm := n_20cm + no3_5 + nh4_5]
      v5_data[,n_60cm := n_40cm + no3_6 + nh4_6]
      # v5_data[,n_deep := no3_1 +no3_2+no3_3+no3_4 + no3_5 + no3_6 + no3_7 + no3_8 + no3_9 + no3_10 + no3_11 + 
      #           nh4_1 + nh4_2 + nh4_3 + nh4_4 + nh4_5 + nh4_6 + nh4_7 + nh4_8 + nh4_9 + nh4_10 + nh4_11]
      
      v5_data[,esw_pct := (sw_dep - ll15_dep)/(dul_dep- ll15_dep)]
      v5_data[,esw_pct := ifelse(esw_pct>1, 1, esw_pct)]
      # cols_v5 <- c("sim_name","day", "sw_dep", "paddock.maize.biomass_n", "paddock.maize.biomass","paddock.maize.green_biomass_n", "paddock.maize.greenn",
      #  "paddock.maize.leafgreennconc", "paddock.maize.leafgreenn", "paddock.maize.swdef_expan",
      #  "paddock.maize.swdef_pheno", "paddock.maize.swdef_photo", "paddock.maize.lai", "oc_20cm", "oc_40cm", "n_20cm", "n_40cm", "n_60cm","n_deep","esw_pct")
      # cols_v5 <- c("sim_name","day", "sw_dep", "paddock.maize.biomass_n", "paddock.maize.biomass",
      #              "paddock.maize.green_biomass_n", "paddock.maize.greenn",
      #              "paddock.maize.leafgreennconc",
      #             "paddock.maize.lai", "oc_20cm", "oc_40cm", "n_20cm", "n_40cm", "n_60cm","n_deep","esw_pct")
      
      cols_v5 <- unique(c("sim_name","day", "sw_dep", "paddock.maize.biomass",
                   "paddock.maize.lai", "oc_20cm", "oc_40cm", "n_20cm", "n_40cm", "n_60cm","n_deep","esw_pct",
                   "paddock.maize.biomass_n", "paddock.maize.green_biomass_n", "paddock.maize.greenn",
                   "paddock.maize.leafgreennconc", "paddock.maize.leafgreenn"))
      
      v5_data <- v5_data[,cols_v5, with = F]
      
      names(v5_data) <- paste0(str_replace(names(v5_data), pattern = 'paddock.maize.', replacement = ''), '_v5')
      setnames(v5_data, 'sim_name_v5', 'sim_name')
      summary_tmp1 <- merge(yearly_data, v5_data, by = 'sim_name') %>% 
        merge(make_it_dt2, by = 'sim_name') %>% cbind(horizons_cell_dt2)
      
      #=====================================================================================================#
      # WEATHER FILES
        #SUmmarize weather 30, 60, 90 days before v5
      day_v5_n <- unique(v5_data$day_v5)
      weather_v5_dt <- weather_cell_dt2[day <= day_v5_n] %>% .[order(-day)]
      weather_v5_dt[,w_period := (rep(1:12, each = 30)*30)[1:nrow(weather_v5_dt)]]
      keep <- names(table(weather_v5_dt$w_period)[table(weather_v5_dt$w_period)==30]) #keep those whose count is 30 (period complete)
      keep <- keep[!keep %in% c('120','150')] #150 is too far
      weather_v5_dt <- weather_v5_dt[w_period %in% keep]
      
      weather_v5_sum_dt <- weather_v5_dt[,.(rain = sum(rain),
                                            t_max = mean(maxt),
                                            t_min = mean(mint)), w_period]
      
      weather_v5_sum_dt <- dcast(weather_v5_sum_dt, . ~ w_period, value.var = c("rain", 't_max', 't_min'))[,-1]
      summary_tmp2 <- cbind(summary_tmp1, weather_v5_sum_dt)
      all_summaries[[length(all_summaries)+1]] <- summary_tmp2
    }#end of z_n loop
    
  }, error = function(e) e)
  
  #REAL WORK:if there is no error
  if(!inherits(possibleError, "error")){
    # return(rbindlist(all_summaries, fill = TRUE))
    if(!dir.exists("./vr_value_v2/Data/yc_output_summary_3")){dir.create("./vr_value_v2/Data/yc_output_summary_3")}
    saveRDS(rbindlist(all_summaries, fill = TRUE), paste0("./vr_value_v2/Data/yc_output_summary_3/", basename(file_n)))
  } else {
    return(c(file_n, possibleError))
  }
}#end make_yearly_summary

grid10_horizons_v2_dt <- readRDS("./vr_value_v2/Data/Grid/grid10_horizons_v2_dt.rds")

# keep <- c('keep', 'make_yearly_summary', 'grid10_horizons_v2_dt')

# clusterExport(cl, varlist = keep, envir=environment())

if(FALSE){ #re run id10 that didn't run in the sensor running
  if(!dir.exists("./vr_value_v2/Data/yc_output_summary_3")){
    files_daily <- list.files('./vr_value_v2/Data/yc_output_1', full.names = T)
    files_daily <- sort(files_daily)
    saveRDS(files_daily, './vr_value_v2/Data/files_rds/files_daily.rds')
    files_daily <- files_daily[1:3500]
  }else{
    files_daily <- readRDS('./vr_value_v2/Data/files_rds/files_daily.rds')
    files_daily <- files_daily[3501:length(files_daily)]
  }
}

if(TRUE){ #re run missing files
  normal_files <- list.files("./vr_value_v2/Data/yc_output_summary_1", full.names = F)
  length(normal_files)
  sensor_files <- list.files("./vr_value_v2/Data/yc_output_summary_2", full.names = F)
  length(sensor_files)
  missing_basename <- sensor_files[!sensor_files %in% normal_files]
  files_daily <- readRDS('./vr_value_v2/Data/files_rds/files_daily.rds')
  files_daily <- files_daily[basename(files_daily) %in% missing_basename]
}

print(files_daily)

if(TRUE){#loop
start <- Sys.time()
results_list <- list()
for(file_n in files_daily){
  results_list <- make_yearly_summary(file_n)
}
time_lasted <- Sys.time() - start
print(time_lasted)
}


if(FALSE){#paralel
start <- Sys.time()
results_list <- parallel::parLapply(cl,files_daily, function(x) make_yearly_summary(x))
time_lasted <- Sys.time() - start
print(time_lasted)
stopCluster(cl)
}

# time_lasted <- Sys.time() - start
# print(time_lasted)

# files_all <- list.files('~/vr_value_v2/Data/yc_output/',full.names = T, recursive = T, pattern = '_YC.rds')

# files_all <- list.files('//aces-dfs-03.ad.uillinois.edu/cpsc-share/Bioinformatics Lab/germanm2/vr_value_v2/yc_output',full.names = T, recursive = T, pattern = '_YC.rds')

# re_runned <- c(1000, 1080, 1138, 280, 283, 435, 52, 813, 960)
# pattern_find <- paste0('cell_', re_runned, collapse = '|')
# files_all2 <- files_all[str_detect(string = files_all, pattern = pattern_find)]
# files_all2 <- files_all[str_detect(string = files_all, pattern = 'cell_280/')]
# files_all2 <- files_all2[str_detect(string = files_all2, pattern = '1862661')]
# files_all2 <- files_all[str_detect(string = files_all, pattern = '259_942156_SMM_A2_')]




# if(unique(instructions$type) == 'YC'){ keep <- append(keep, 'initial_conditions' )}
# # #rm(list = ls()[!ls() %in% keep])



# no_file <- which(!sapply(results_list, is.data.table))
# no_file2 <- files_all[no_file]
# saveRDS(no_file2, './vr_value_v2/Data/files_rds/no_file2.rds')

# start <- Sys.time()
# results_list <- parallel::parLapply(cl,no_file2, function(x) make_yearly_summary(x))
# saveRDS(results_list, './vr_value_v2/Data/files_rds/results_list.rds')
# time_lasted <- Sys.time() - start
# print(time_lasted)

#
#results_list2 <- results_list[sapply(results_list, is.data.table)]
#length(results_list2)
#yc_yearly_dt <- rbindlist(results_list2, fill = TRUE)

#saveRDS(yc_yearly_dt, './vr_value_v2/Data/files_rds/yc_yearly_dt.rds')

#-----------------------------------------------------------------------
#Update re-runs
# results_list2 <- results_list[sapply(results_list, is.data.table)]
# length(results_list2)
# update_dt1 <- rbindlist(results_list2, fill = TRUE)
# update_dt1[,prev_crop := ifelse(prev_crop == 'MSM', 0, 1)]
# update_dt1 <- update_dt1[,-'prev_crop']
# 
# 
# yc_yearly_dt <- readRDS('./vr_value_v2/Data/files_rds/yc_yearly_dt.rds')
# update_dt2 <- yc_yearly_dt[id_10 %in% re_runned]
# yc_yearly_dt <- yc_yearly_dt[!id_10 %in% re_runned]
# 
# update_dt3 <- rbind(update_dt1, update_dt2)
# update_dt3 <- unique(update_dt3)
# 
# table(update_dt3[,.N, by = .(id_10, mukey, z)]$N)
# 
# yc_yearly_dt <- rbind(yc_yearly_dt, update_dt3)
# check <- yc_yearly_dt[,.N, by = .(id_10, mukey, z)][N != 33]
# 
# source('./Codes_useful/gm_functions.R')
# update_dt1 <- yc_yearly_dt[id_10 %in% unique(check$id_10)]
# yc_yearly_dt <- yc_yearly_dt[!id_10 %in% unique(check$id_10)]
# update_dt1 <- unique(update_dt1)
# yc_yearly_dt <- rbind(yc_yearly_dt, update_dt1)
# 
# saveRDS(yc_yearly_dt, './vr_value_v2/Data/files_rds/yc_yearly_dt.rds')
