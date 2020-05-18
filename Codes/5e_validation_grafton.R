# setwd('C:/Users/germa/Box Sync/My_Documents') #dell

# setwd("/home/germanm2")

rm(list=ls())

# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# codes_folder <-'C:/Users/germanm2/Documents'#CPSC

setwd('~')#Server
codes_folder <-'~' #Server


source('./Codes_useful/R.libraries.R')
# library(scales)
source('./Codes_useful/gm_functions.R')

perfomances_dt3 <- readRDS("./n_policy_box/Data/files_rds/perfomances_dt3.rds") #for 5e_validation.R
validation_dt <- perfomances_dt3[policy == 'ratio_6' & NMS ==1]
grid10_tiles_sf7 <- readRDS("./n_policy_box/Data/Grid/grid10_tiles_sf7.rds") 

#---------------------------------------------------------------------------
# AGGREGATE AGAIN CONSIDERING THE CORN PRODUCTION OF THE CELL
grid10_tiles_dt <- data.table(grid10_tiles_sf7)[,.N, .(id_tile,id_10, corn_avg_ha,corn5_tile )][,-'N']

summary(grid10_tiles_dt$corn_avg_ha)
validation_dt[,id_10 := as.integer(id_10)]
validation_dt <- merge(validation_dt, grid10_tiles_dt, by = 'id_10')

validation_dt2 <- aggregate_by_area(data_dt = validation_dt, variables = c("Y_corn", 'L1', 'L2', "L", "N_fert","P", "G"), 
                                     weight = 'corn_avg_ha', by_c = c('z')) #state level, weighted by corn_ha

validation_dt2[,year := as.numeric(z)+1988]
validation_dt2[,L2 := data.table::shift(L2, n=1, fill=NA, type="lag")]
validation_dt2[,L := L1 + L2]
validation_dt2 <- validation_dt2[-1]

#---------------------------------------------------------------------------
# Add the weather
weather_historic_dt <- readRDS('./n_policy_box/Data/met_files/weather_historic_dt2019.rds')
rain_dt <- weather_historic_dt[,.(rain = sum(rain)), by = .(year, id_10)][,.(rain = mean(rain)), by = .(year)]
validation_dt2 <- merge(validation_dt2, rain_dt, by = 'year')

ggplot(validation_dt2, aes(x = rain, y = Y_corn))+
  geom_point()+geom_smooth()

ggplot(validation_dt2, aes(x = rain, y = L))+
  geom_point()+geom_smooth()

ggplot(validation_dt2)+
  geom_line(aes(x = year, y = L1*50))+
  geom_bar(stat="identity", aes(x = year, y = rain))


#---------------------------------------------------------------------------





data.table::shift(validation_dt2$L2, n=1, fill=NA, type="lag")
shift(validation_dt2$L2, n=1, fill=NA, type="lag")

class(validation_dt2$L2)
x = 1:5
# lag with n=1 and pad with NA (returns vector)
data.table::shift(x, n=1, fill=NA, type="lag")



perfomances_dt5[order(-P)]

perfomances_dt5[,policy_val := as.numeric(str_extract(policy,pattern = '[0-9.]+'))]
perfomances_dt5[,policy_name := lapply(perfomances_dt5$policy, function(x) str_split(x, pattern = '_')[[1]][1])]

perfomances_dt5[,E := L * 0.4 * Pe_med]
perfomances_dt5[,W := P + G - E]

saveRDS(perfomances_dt5, "./n_policy_box/Data/files_rds/perfomances_dt5.rds")
  
}  