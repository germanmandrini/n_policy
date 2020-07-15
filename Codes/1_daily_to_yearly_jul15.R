######################################
# Parallelized Simulations
####################################### 
# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
# setwd('~')

library(stringr)
library(data.table)
# install.packages('XML','~/Rlibs')"

#Get the computer where this is running

if(FALSE){

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
  # file_n <- files_daily[1]
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
    
    weather_cell_dt <- readRDS(paste('./n_policy_box/Data/met_files/weather_z_cell', unique(daily_yc_dt$id_10), '_dt.rds', sep = '')) %>%
      .[year == 2010]
    
    initial_conditions_dt <- readRDS(str_replace(file_n, pattern = 'yc_output', replacement = 'initial_conditions')) %>%
      .[year == 2009]
    
    all_summaries <- list()
    
    for(z_n in unique(daily_yc_dt$z)){
       # z_n = unique(daily_yc_dt$z)[1]
      daily_yc_dt2 <- daily_yc_dt[z == z_n]
      
      weather_cell_dt2 <- weather_cell_dt[z == z_n]
      
      #=====================================================================================================#
      # Frozen
      names(daily_yc_dt2)
      make_it_dt <- daily_yc_dt2[,.(id_10, mukey, z, year,day, sim_name, stage_name, 
                                   paddock.maize.lai,  LeafNo, paddock.maize.swdef_photo, paddock.maize.swdef_expan, paddock.maize.swdef_pheno)][year== 2010]
      
      make_it_dt[,id_10:= as.integer(id_10)]
      make_it_dt[,z:= as.integer(z)]
      
      # make_it_dt <- merge(make_it_dt, 
      #                     weather_cell_dt2[,.(id_10, year, day, maxt,  mint, z )], 
      #                     by = c('id_10', 'z', 'year', 'day'))
      
      # make_it_dt <- make_it_dt[sim_name == '259_942156_SMM_A2_YC_110']
      # make_it_dt[stage_name != 'nocrop']
      
      
      #max lai, stages
      make_it_dt2 <- make_it_dt[, .(LAI_max = max(paddock.maize.lai),
                     stages_cnt = length(unique(stage_name)),
                     swdef_photo = mean(paddock.maize.swdef_photo),
                     swdef_expan = mean(paddock.maize.swdef_expan),
                     swdef_pheno = mean(paddock.maize.swdef_pheno),
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
      yearly_data <- daily_yc_dt2[year == 2010,.(Yld = max(Y, na.rm = T)/0.85,
                     #leach_no3 = sum(leach_no3),
                     N_fert = sum(fertiliser),
                     dul_dep = max(dul_dep),
                     ll15_dep = max(ll15_dep),
                     whc = max(dul_dep) - max(ll15_dep)), by = .(id_10, mukey, z, sim_name, year)]
      
      #Add previous yield
      prev_yield <- initial_conditions_dt[z == z_n] %>% .[,.(Yld_prev = max(Y, na.rm = T)/0.87), by = .(id_10, mukey, z)]
      yearly_data <- merge(yearly_data[year == 2010], prev_yield, by = c('id_10', 'mukey','z'))
      
      #Add soy yield
      soy_yield <- daily_yc_dt2[year == 2011,.(Yld_soy = max(Y, na.rm = T)/0.87), by = .(id_10, mukey, z, sim_name)]
      yearly_data <- merge(yearly_data, soy_yield, by = c('id_10', 'mukey','z', 'sim_name'))
      
      #Add sowing date and 10 days mean temperature
      sowing_day <- daily_yc_dt2[year == 2010 & stage_name == 'sowing', .(sim_name, day)]
      setnames(sowing_day, 'day', 'day_sow')
      sowing_day[, soilt10d := mean(daily_yc_dt2[year == 2010 & (day <= sowing_day$day_sow) & (day >= (sowing_day$day_sow-9))]$st_2)]
      yearly_data <- merge(yearly_data, sowing_day, by = c('sim_name'))
      
      #=====================================================================================================#
      # LEACHING April to April
      daily_yc_dt2[year == 2010 & day > 100 ,leach_period := 1]
      daily_yc_dt2[year == 2011 & day <= 100 ,leach_period := 1]
      daily_yc_dt2[year == 2011 & day > 100 ,leach_period := 2]
      daily_yc_dt2[year == 2012 & day <= 100 ,leach_period := 2]
      
      leach_data <- daily_yc_dt2[leach_period %in% c(1,2), .(leach_n = sum(leach_no3)), by = .(id_10, mukey, z, sim_name, leach_period)]
      
      leach_data[,leach_period  := paste0('leach_', leach_period)]
      leach_data <- dcast(leach_data, id_10 + mukey + z + sim_name ~ leach_period  , value.var = "leach_n")
      
      yearly_data <- merge(yearly_data, leach_data, by = c('id_10', 'mukey',  'z', 'sim_name'))
      
      #=====================================================================================================#
      #Delta N (define leaching as all N going below 150)
      # n_below150cm_0 <- daily_yc_dt2[year == 2010 & day == 1 & Y == 0] %>% .[,n_low_150cm := nh4_10 + nh4_11 + no3_10 + no3_11] %>% 
      #   .[,period := 0] %>% .[, .(id_10, mukey, z, sim_name, period, n_low_150cm)]#N below 150 cm. Photo in time 0
      # 
      # n_below150cm_1 <- daily_yc_dt2[year == 2011 & day == 1] %>% .[,n_low_150cm := nh4_10 + nh4_11 + no3_10 + no3_11] %>% 
      #   .[,period := 1] %>% .[, .(id_10, mukey, z, sim_name, period, n_low_150cm)] #N below 150 cm. Photo in time 1
      # 
      # n_below150cm_delta1 <- merge(n_below150cm_0, n_below150cm_1, by = c('id_10', 'mukey',  'z', 'sim_name')) %>% 
      #   .[,n_150_dlt_1 := n_low_150cm.y - n_low_150cm.x]#Change in N below 150 cm during corn (from 1 back to 0)
      # 
      # 
      # n_below150cm_2 <- daily_yc_dt2[year == 2011 & month == 12 & day == 365] %>% .[,n_low_150cm := nh4_10 + nh4_11 + no3_10 + no3_11] %>% 
      #   .[,period := 2] %>% .[, .(id_10, mukey, z, sim_name, period, n_low_150cm)]#N below 150 cm. Photo in time 2
      # n_below150cm_delta2 <- merge(n_below150cm_1, n_below150cm_2, by = c('id_10', 'mukey',  'z', 'sim_name')) %>% 
      #   .[,n_150_dlt_2 := n_low_150cm.y - n_low_150cm.x]#Change in N below 150 cm during soy (from 2 back to 1)
      # 
      # n_below150cm_delta <- merge(n_below150cm_delta1[, .(id_10, mukey, z, sim_name,n_150_dlt_1) ],
      #       n_below150cm_delta2[, .(id_10, mukey, z, sim_name,n_150_dlt_2) ], by = c('id_10', 'mukey', 'z', 'sim_name'))
      # 
      # leach_data2 <- merge(leach_data, n_below150cm_delta)
      # 
      # leach_data2[,leach_1_150cm := leach_1 + n_150_dlt_1  ]
      # leach_data2[,leach_2_150cm := leach_2 + n_150_dlt_2  ]
      # 
      # test <- nrow(leach_data2[leach_1_150cm < 0 | leach_2_150cm < 0]) >0  #test leaching should never be negative
      # if(test){stop()}
      # 
      # yearly_data <- merge(yearly_data, leach_data2[, .(id_10, mukey, z, sim_name,leach_1, leach_2, leach_1_150cm, leach_2_150cm) ], 
      #                      by = c('id_10', 'mukey', 'z', 'sim_name'))
      
      #=====================================================================================================#
      # Increase in N on top 150 cm
      # n_top_cols <- c(paste0('no3_', 1:9), paste0('nh4_', 1:9))
      # n_data1 <- daily_yc_dt2[year == 2010 & day == 101, c('id_10', 'mukey',  'z',  'sim_name', n_top_cols), with = F] %>%
      #   .[,n_top15_1 := no3_1+no3_2+no3_3+no3_4+no3_5+no3_6+no3_7+no3_8+no3_9+nh4_1+nh4_2+nh4_3+nh4_4+nh4_5+nh4_6+nh4_7+nh4_8+nh4_9] %>% 
      #   .[,-n_top_cols, with = F]
      # 
      # n_data2 <- daily_yc_dt2[year == 2011 & day == 100, c('id_10', 'mukey',  'z',  'sim_name', n_top_cols), with = F] %>%
      #   .[,n_top15_2 := no3_1+no3_2+no3_3+no3_4+no3_5+no3_6+no3_7+no3_8+no3_9+nh4_1+nh4_2+nh4_3+nh4_4+nh4_5+nh4_6+nh4_7+nh4_8+nh4_9] %>% 
      #   .[,-n_top_cols, with = F]
      # 
      # n_delta <- merge(n_data1 , n_data2, by = c('id_10', 'mukey',  'z',  'sim_name'))
      # # 
      # n_delta[,n_top15_delta := n_top15_2 - n_top15_1 ]
      # # 
      # yearly_data <- merge(yearly_data, n_delta[, .(id_10, mukey, z, sim_name,n_top15_delta) ], by = c('id_10', 'mukey',   'z',  'sim_name'))
      # # yearly_data[, leach_n2 := ifelse(n_delta > 0, leach_n + n_delta, leach_n)]  #assuming that any increase in N in the soil solution will be eventually leached
      # 
      # 
      
      #=====================================================================================================#
      # SOIL FILES
      
      horizons_cell_dt <- grid10_horizons_v2_dt[mukey == unique(daily_yc_dt2$mukey)]
      horizons_cell_dt2 <- horizons_cell_dt[bottom <= 40, .(sand_40cm = mean(sand),
                                                            om_40cm = mean(om),
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
                   "paddock.maize.lai", "oc_20cm", "oc_40cm", "n_20cm", "n_40cm", "n_60cm","n_deep","esw_pct"))
      
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
    save_dir <- paste0("./n_policy_box/Data/yc_output_summary_", batch_n)
    if(!dir.exists(save_dir)){dir.create(save_dir)}
    saveRDS(rbindlist(all_summaries, fill = TRUE), paste0(save_dir,'/', basename(file_n)))
    return(rbindlist(all_summaries, fill = TRUE))
  } else {
    return(c(file_n, possibleError))
  }
  
  
}

grid10_horizons_v2_dt <- readRDS("./n_policy_box/Data/Grid/grid10_horizons_v2_dt.rds")

# files_daily <- list.files('S:/Bioinformatics Lab/germanm2/n_policy/yc_output',full.names = T, recursive = T)
# files_daily <- list.files('./n_policy_box/Data/yc_output',full.names = T, recursive = T)

# test <- make_yearly_summary(file_n)

# start <- Sys.time()
results_list <- list()
for(file_n in files_daily){
  # file_n <- files_daily[2]
  # print(file_n)
  results_list[[basename(file_n)]] <- make_yearly_summary(file_n)
}
# time_lasted <- Sys.time() - start
# print(time_lasted)

