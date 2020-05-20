# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# codes_folder <-'C:/Users/germanm2/Documents'#CPSC
# setwd("/home/germanm2")

setwd('~')#Server
codes_folder <-'~' #Server
rm(list=ls())

source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')

perfomances_dt3 <- readRDS("./n_policy_box/Data/files_rds/perfomances_dt3.rds") #for 5e_validation.R
validation_dt <- perfomances_dt3[policy == 'ratio_6' & NMS ==1]
grid10_tiles_sf7 <- readRDS("./n_policy_box/Data/Grid/grid10_tiles_sf7.rds") 

# if(FALSE){
#   multiple_files <- list.files("./n_policy_box/Data/yc_output_summary_calendar", full.names = T)
#   length(multiple_files)
#   
#   yc_yearly_list <- list()
#   for(file_n in multiple_files){
#     yc_yearly_list[[length(yc_yearly_list)+1]] <- readRDS(file_n)
#   }
#   # yc_yearly_list[[4207]] <- yc_yearly_list[[4207]][,-'NA'] #for some reason this came with one column called NA
#   validation_dt <- rbindlist(yc_yearly_list)
#   validation_dt[,id_10 := as.integer(id_10)]
#   ggplot(validation_dt) + geom_boxplot(aes(y = Yld, x = z))
#   
#   validation_dt2 <- validation_dt[,.(L1 = mean(leach_1),
#                    L2 = mean(leach_2),
#                    Y_corn = mean(Yld),
#                    N_fert = mean(N_fert)), by = z]
# }



#---------------------------------------------------------------------------
# AGGREGATE AGAIN CONSIDERING THE CORN PRODUCTION OF THE CELL
grid10_tiles_dt <- data.table(grid10_tiles_sf7)[,.N, .(id_tile,id_10, corn_avg_ha,corn5_tile )][,-'N']

summary(grid10_tiles_dt$corn_avg_ha)
validation_dt[,id_10 := as.integer(id_10)]
validation_dt <- merge(validation_dt, grid10_tiles_dt, by = 'id_10')

validation_dt2 <- aggregate_by_area(data_dt = validation_dt, variables = c("Y_corn", 'L1', 'L2', "L", "N_fert","P", "G"),
                                     weight = 'corn_avg_ha', by_c = c('z')) #state level, weighted by corn_ha
#---------------------------------------------------------------------------

validation_dt2[,year := as.numeric(z)+1988]
validation_dt2 <- validation_dt2[order(year)]
validation_dt2[,L2 := data.table::shift(L2, n=1, fill=NA, type="lag")]
validation_dt2[,L := L1 + L2]
validation_dt2 <- validation_dt2[-1]

#---------------------------------------------------------------------------
# Add the weather
weather_historic_dt <- readRDS('./n_policy_box/Data/met_files/weather_historic_dt2019.rds')
rain_dt1 <- weather_historic_dt[day > 100] #after april correspond to the same year
rain_dt2 <- weather_historic_dt[day < 100] #before april correspond to the year before
rain_dt2[,year := year - 1]
rain_dt <- rbind(rain_dt1, rain_dt2)
rain_dt <- rain_dt[,.(rain = sum(rain)), by = .(year, id_10)][,.(rain = mean(rain)), by = .(year)]
rain_pc_dt <- weather_historic_dt[day > 100 & day < 200,.(rain = sum(rain)), by = .(year, id_10)][,.(rain_pc = mean(rain)), by = .(year)]
validation_dt2 <- merge(validation_dt2, rain_dt, by = 'year')
validation_dt2 <- merge(validation_dt2, rain_pc_dt, by = 'year')

ggplot(validation_dt2, aes(x = rain_pc, y = Y_corn))+
  geom_point()+geom_smooth()

ggplot(validation_dt2, aes(x = rain, y = L))+
  geom_point()+geom_smooth()

ggplot(validation_dt2)+
  geom_line(aes(x = year, y = L*50))+
  geom_line(aes(x = year, y = Y_corn/5), color = 'green')+
  geom_bar(stat="identity", aes(x = year, y = rain), colour = "blue", fill=NA)


12000/(50*50)
#---------------------------------------------------------------------------

library(gdata)
#Year is corrected from april to april
graffton_waterquality <- read.xls('./n_policy_box/Data/validation/graffton_waterquality.xlsx', sheet = 'import', header = TRUE, stringsAsFactors=FALSE)
setnames(graffton_waterquality, 'Year', 'year')
validation_dt2 <- merge(validation_dt2, graffton_waterquality, by = 'year')

lm(data = validation_dt2, formula = 'L~NO3.kg_sec')

(plot_1 <- ggplot(validation_dt2, aes(x = NO3.kg_sec, y = L, color = year))+ ylim(0, 80)+ xlim(0, 25)+
  geom_point()+
  geom_smooth(formula = 'y~x', method = 'lm', se = FALSE, color = 'darkgrey') +
  # xlab('Grafton N-NO3 (kg sec-1)')+
  # ylab('L (kg ha-1)')+
  labs(x=expression(paste("Grafton N-", NO[3],"(kg sec"^"-1",")",sep="")),
      y = expression(paste("L (kg ha"^"-1", "year"^"-1",")")))+
 #geom_text(x = 20, y = 30, label = 'equation', parse = TRUE)+
    annotate("text", x=17, y=30, label= "y=1.773x + 35.597", size = 4)+
  theme_bw()+
  theme(panel.grid = element_blank()))

ggsave(plot = plot_1, 
       filename = "./n_policy_box/Data/figures/validation_grafton.jpg", width = 493/300*3, height = 390/300*3,
       units = 'in')

#---------------------------------------------------------------------------
# VALIDATION WITH NASS DATA

