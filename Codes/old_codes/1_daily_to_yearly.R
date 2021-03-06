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
#============================???=======


make_yearly_summary <- function(file_n){
  # file_n = files_all[2221]
  #--------------------------
  # preparation
  #--------------------------
  #--- load libraries ---#
  library(data.table)
  library(stringr)
  #ERROR HANDLING
  possibleError <- tryCatch({
  
    daily_yc_dt <- readRDS(file_n)
    
    #=====================================================================================================#
    # Yearly data
    yearly_data <- daily_yc_dt[year < 2011,.(Yld = max(Y, na.rm = T)/0.85,
                   #leach_no3 = sum(leach_no3),
                   N_fert = sum(fertiliser),
                   dul_dep = max(dul_dep),
                   ll15_dep = max(ll15_dep),
                   whc = max(dul_dep) - max(ll15_dep)), by = .(id_10, mukey, rotation,  z, type, sim_name, year)]
    
    prev_yield <- yearly_data[year == 2009] %>% .[,.(sim_name, Yld_prev = Yld)]
    yearly_data <- merge(yearly_data[year == 2010], prev_yield)
    
    #=====================================================================================================#
    # LEACHING April to April
    daily_yc_dt[,leach_period := ifelse(year == 2010 & day > 100, 1, 
                                        ifelse(year == 2011 & day <= 100, 1, 0))]

    leach_data <- daily_yc_dt[leach_period == 1, .(leach_n = sum(leach_no3)), by = .(id_10, mukey, rotation,  z, type, sim_name)]
    
    yearly_data <- merge(yearly_data, leach_data, by = c('id_10', 'mukey',  'rotation', 'z', 'type', 'sim_name'))
    
    #=====================================================================================================#
    #Delta N
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
    # SOIL FILES
    
    #horizons_cell_dt <- grid10_horizons_v2_dt[mukey == unique(daily_yc_dt$mukey)]
  
    #horizons_cell2_dt <- calc_apsim_variables(horizons_cell_dt)
    #=====================================================================================================#
    # V5 data
    
    v5_data <- daily_yc_dt[LeafNo >0 & LeafNo < 5 & year == 2010, .SD[LeafNo == max(LeafNo)], by = sim_name]
    
    v5_data[,oc_20cm := (oc_1*5 + oc_2*5 + oc_3*5 + oc_4*5)/20] #this should be done using the Bulk Density of each layer
    v5_data[,oc_40cm := (oc_20cm*20 + oc_5*20)/40]
    v5_data[,n_20cm :=  no3_1 + no3_2 + no3_3 + no3_4 + nh4_1 + nh4_2 + nh4_3 + nh4_4]
    v5_data[,n_40cm := n_20cm + no3_5 + nh4_5]
    v5_data[,n_60cm := n_40cm + no3_6 + nh4_6]
    v5_data[,n_deep := no3_1 +no3_2+no3_3+no3_4 + no3_5 + no3_6 + no3_7 + no3_8 + no3_9 + no3_10 + no3_11 + 
              nh4_1 + nh4_2 + nh4_3 + nh4_4 + nh4_5 + nh4_6 + nh4_7 + nh4_8 + nh4_9 + nh4_10 + nh4_11]
    
    v5_data[,esw_pct := (sw_dep - ll15_dep)/(dul_dep- ll15_dep)]
    v5_data[,esw_pct := ifelse(esw_pct>1, 1, esw_pct)]
    # cols_v5 <- c("sim_name","day", "sw_dep", "paddock.maize.biomass_n", "paddock.maize.biomass","paddock.maize.green_biomass_n", "paddock.maize.greenn",
    #  "paddock.maize.leafgreennconc", "paddock.maize.leafgreenn", "paddock.maize.swdef_expan",
    #  "paddock.maize.swdef_pheno", "paddock.maize.swdef_photo", "paddock.maize.lai", "oc_20cm", "oc_40cm", "n_20cm", "n_40cm", "n_60cm","n_deep","esw_pct")
    # cols_v5 <- c("sim_name","day", "sw_dep", "paddock.maize.biomass_n", "paddock.maize.biomass",
    #              "paddock.maize.green_biomass_n", "paddock.maize.greenn",
    #              "paddock.maize.leafgreennconc",
    #             "paddock.maize.lai", "oc_20cm", "oc_40cm", "n_20cm", "n_40cm", "n_60cm","n_deep","esw_pct")
    
    cols_v5 <- c("sim_name","day", "sw_dep", "paddock.maize.biomass",
                 "paddock.maize.lai", "oc_20cm", "oc_40cm", "n_20cm", "n_40cm", "n_60cm","n_deep","esw_pct")
    
    v5_data <- v5_data[,cols_v5, with = F]
    
    names(v5_data) <- paste0(str_replace(names(v5_data), pattern = 'paddock.maize.', replacement = ''), '_v5')
    setnames(v5_data, 'sim_name_v5', 'sim_name')
    
    #=====================================================================================================#
    # WEATHER FILES
      #SUmmarize weather 30, 60, 90 days before v5
    weather_cell_dt <- readRDS(paste('./vr_value/Data/met_files/weather_z_cell', unique(daily_yc_dt$id_10), '_dt.rds', sep = ''))
    weather_cell_dt <- weather_cell_dt[z == unique(daily_yc_dt$z) & year == 2010]
    summary_tmp1 <- merge(yearly_data, v5_data, by = 'sim_name')
    
    summary_tmp2 <- data.table()                     
    for(day_v5_n in unique(v5_data$day_v5)){
      # day_v5_n = 141 
      weather_v5_dt <- weather_cell_dt[day <= day_v5_n] %>% .[order(-day)]
      weather_v5_dt[,w_period := (rep(1:12, each = 30)*30)[1:nrow(weather_v5_dt)]]
      keep <- names(table(weather_v5_dt$w_period)[table(weather_v5_dt$w_period)==30]) #keep those whose count is 30 (period complete)
      keep <- keep[keep != '150'] #150 is too far
      weather_v5_dt <- weather_v5_dt[w_period %in% keep]
      
      weather_v5_sum_dt <- weather_v5_dt[,.(rain = sum(rain),
                       t_max = mean(maxt),
                       t_min = mean(mint)), w_period]
      
      weather_v5_sum_dt <- dcast(weather_v5_sum_dt, . ~ w_period, value.var = c("rain", 't_max', 't_min'))[,-1]
      weather_v5_sum_dt[,day_v5 := day_v5_n]
      summary_tmp2 <- rbind(summary_tmp2, merge(summary_tmp1[day_v5 == day_v5_n], weather_v5_sum_dt, by = 'day_v5'))
    }
  
  }, error=function(e) e)
  
  #REAL WORK:if there is no error
  if(!inherits(possibleError, "error")){
    return(summary_tmp2)
  }else{
    return(c(file_n, possibleError))
  }
  
  
}

files_all <- list.files('S:/Bioinformatics Lab/germanm2/vr_value/yc_output',full.names = T, recursive = T, pattern = '_YC.rds')

# files_all <- files_all[str_detect(string = files_all, pattern = 'cell_297')]
# files_all <- files_all[str_detect(string = files_all, pattern = '1862699')]

keep <- c('keep', 'make_yearly_summary')
# if(unique(instructions$type) == 'YC'){ keep <- append(keep, 'initial_conditions' )}
# # #rm(list = ls()[!ls() %in% keep])

clusterExport(cl, varlist = keep, envir=environment())

start <- Sys.time()
results_list <- parallel::parLapply(cl,files_all, function(x) make_yearly_summary(x))
time_lasted <- Sys.time() - start
print(time_lasted)

stopCluster(cl)
yc_yearly_dt <- rbindlist(results_list[sapply(results_list, is.data.table)], fill = TRUE)
saveRDS(yc_yearly_dt, './vr_value/Data/files_rds/yc_yearly_dt.rds')

#TO DO: add residual N delta
