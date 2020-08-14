# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
codes_folder <-'C:/Users/germanm2/Documents'#CPSC
# setwd("/home/germanm2")

setwd('~')#Server
codes_folder <-'~' #Server
rm(list=ls())

source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')

# perfomances_dt3 <- readRDS("./n_policy_box/Data/files_rds/perfomances_dt3.rds") #for 5e_validation.R
# validation_dt <- perfomances_dt3[policy == 'ratio_6' & NMS ==1]
grid10_tiles_sf7 <- readRDS("./n_policy_box/Data/Grid/grid10_tiles_sf7.rds") 
grid10_soils_dt5 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt5.rds") %>% data.table()



#-----------------------------------------------------------------------------------------------------------
# Open the files and save the base-level rates
if(FALSE){
  reg_model_stuff <- readRDS( "./n_policy_box/Data/files_rds/reg_model_stuff.rds")
  minimum_ok <- reg_model_stuff$minimum_ok
  rm(reg_model_stuff)
  
  library("foreach")
  library("doParallel")
 
  multiple_files <- list.files('S:/Bioinformatics Lab/germanm2/n_policy_box_58/Data/yc_output_58', full.names = T)
  
  length(multiple_files)
  regions_dt <- data.table(grid10_tiles_sf7) %>% .[,.N,.(id_10, region)] %>% .[,-'N']
  
  registerDoParallel(4) # register the cluster
  # registerDoParallel(cores = 10)
  output_list = foreach(file_n = multiple_files, .combine = "c", .packages = c("data.table")) %dopar% {
    # file_n <- multiple_files[43]
    tmp_dt <- readRDS(file_n)
    N_fert_baselevel <- minimum_ok[region == regions_dt[id_10 == unique(tmp_dt$id_10)]$region]$eonr_pred
    tmp_dt[,N_fert := sapply(strsplit(as.character(sim_name), split="_"), "[", 5) ]
    tmp_dt <- tmp_dt[N_fert == N_fert_baselevel & year < 2012]
    tmp_dt <- tmp_dt[, .(leach_no3 = sum(leach_no3)), by = .(id_10, mukey, z, year, month)]
    
    list(tmp_dt)
  }#end of dopar loop
  
  stopImplicitCluster()
  validation_dt <- rbindlist(output_list)
  validation_dt[,id_10 := as.integer(id_10)]
  
  saveRDS(validation_dt, './n_policy_box/Data/files_rds/validation_monthly_leaching.rds')
}
validation_dt <- readRDS('./n_policy_box/Data/files_rds/validation_monthly_leaching.rds')
validation_dt[,.N, by = z]
#Sepparate soy and corn
corn <- validation_dt[year == 2010, .(id_10, mukey, z, year, month, leach_no3)] %>% setnames('leach_no3', 'L1')
corn[,year := as.numeric(z)+1988]

soy <- validation_dt[year == 2011, .(id_10, mukey, z, year, month, leach_no3)] %>% setnames('leach_no3', 'L2')
soy[,year := as.numeric(z)+1989]

validation_dt2 <- merge(corn[,-'z'], soy[,-'z'], by = c('id_10',  'mukey',  'year', 'month'))

#---------------------------------------------------------------------------
# AGGREGATE BY CELL CONSIDERING THE AREA OF EACH SOIL
area_dt <- grid10_soils_dt5[,.(area_ha = sum(area_ha)), by = .(id_10, mukey)]
area_dt[,.(area_ha = sum(area_ha)), by = .(id_10)]$area_ha %>% table()

validation_dt2 <- merge(validation_dt2, area_dt,  by = c('id_10',  'mukey'))
validation_dt2[,.(area_ha = sum(area_ha)), by = .(id_10, year, month)]

validation_dt3 <- aggregate_by_area(data_dt = validation_dt2, variables = c('L1', 'L2'),
                                    weight = 'area_ha', by_c = c('id_10', 'year', 'month')) #cell x year x month level, weighted by area_ha

#---------------------------------------------------------------------------
# AGGREGATE AGAIN CONSIDERING THE CORN PRODUCTION OF THE CELL
grid10_tiles_dt <- data.table(grid10_tiles_sf7)[,.N, .(id_tile,id_10, corn_avg_ha )][,-'N']

summary(grid10_tiles_dt$corn_avg_ha)
validation_dt3 <- merge(validation_dt3, grid10_tiles_dt, by = 'id_10')

validation_dt4 <- aggregate_by_area(data_dt = validation_dt3, variables = c('L1', 'L2'),
                                     weight = 'corn_avg_ha', by_c = c('year', 'month')) #state x year x month level, weighted by corn_ha
validation_dt4[,L := L1 + L2]
validation_dt4
ggplot(data = validation_dt4)+ 
  geom_boxplot(aes(x = factor(month), y = L))

# #---------------------------------------------------------------------------
# # Add the weather
# weather_historic_dt <- readRDS('./n_policy_box/Data/met_files/weather_historic_dt2019.rds')
# 
# month_seq <- c(rep(1,31), rep(2, 28), rep(3,31), rep(4,30), rep(5,31), rep(6,30), rep(7,31), rep(8,31), rep(9, 30), rep(10,31), rep(11,30), rep(12,31))
# weather_historic_dt[,month := rep(month_seq, nrow(weather_historic_dt)/365)]
# 
# test <- weather_historic_dt[,.(day_min = min(day), 
#                                day_max = max(day)), by = .(month, year)]
# test[,.(day_min = mean(day_min),
#         day_max = mean(day_max)), by = .(month)]
# 
# rain_dt <- weather_historic_dt[,.(rain = sum(rain)), by = .(year, month, id_10)][,.(rain = mean(rain)), by = .(year, month)]
# 
# validation_dt4 <- merge(validation_dt4, rain_dt, by = c('year', 'month'))
# 
# ggplot(data = validation_dt4)+ 
#   geom_boxplot(aes(x = factor(month), y = rain))+
#   geom_smooth(aes(x = month, y = L*10))

#---------------------------------------------------------------------------

library(gdata)
#Year is corrected from april to april
graffton_waterquality <- read.xls('./n_policy_box/Data/validation/graffton_waterquality.xlsx', 
                                  sheet = 'qual_vol_3', header = TRUE, stringsAsFactors=FALSE) %>% data.table()

graffton_waterquality <- read.csv('./n_policy_box/Data/validation/graffton_waterquality.csv', header = T)%>% data.table()

setnames(graffton_waterquality, c('year_nu', 'month_nu'), c('year', 'month'))

validation_dt4 <- merge(validation_dt4, graffton_waterquality[,.(year, month, NO3.kg.sec)], by = c('year', 'month'))

#Date plot
validation_dt4[,date := as.Date(paste(year, month, 15), format='%Y %m %d')]
ggplot(data = validation_dt4[year > 2014 & year < 2019]) + 
  geom_line(aes(x = date, y = L), col = 'blue')+
  geom_line(aes(x = date, y = NO3.kg.sec), col = 'green')

#Monthly average plot
validation_dt4_monthly <- validation_dt4[, .(L = mean(L),
                   NO3.kg.sec = mean(NO3.kg.sec)), by = month]

(plot_1 <- ggplot(data = validation_dt4_monthly) + 
  geom_line(aes(x = month, y = L, col = 'Simulated N leaching'), size = 1)+
  geom_line(aes(x = month, y = NO3.kg.sec, col = 'Mississippi River N flow'), size = 1)+
  labs(y=expression(paste("N-leaching (kg ha"^"-1", "year"^"-1",") or N-flow (N-", NO[3],"kg sec"^"-1",")",sep="")))+
  scale_x_continuous(breaks = 1:12, labels = 1:12) +
  theme_bw()+
  theme(legend.title =  element_blank(),
        legend.text.align = 0,
        axis.text.x = element_text(size = 14),
      legend.position = c(0.80, 0.85),
      legend.text=element_text(size=12),
      panel.grid = element_blank()))#+   annotate("text", x=1, y=22, label= "a)", size = 8) )

ggsave(plot = plot_1,
       filename = "./n_policy_box/Data/figures/validation_grafton_time.pdf", width = 758/300*3, height= 586/300*3, units = 'in')
       #units = 'in') #, width = 758/300*3, height = 586/300*3

#---------------------------------------------------------------------------
#Monthly obs vs predicted
lm(data = validation_dt4, formula = 'L~NO3.kg.sec')

(plot_2 <- ggplot(validation_dt4, aes(y = NO3.kg.sec, x = L, color = year))+# ylim(0, 40)+ xlim(0, 50)+
  geom_point()+
  geom_smooth(formula = 'y~x', method = 'lm', se = FALSE, color = 'darkgrey') +
  # xlab('Grafton N-NO3 (kg sec-1)')+
  # ylab('L (kg ha-1)')+
  labs(y=expression(paste("Grafton N-", NO[3],"(kg sec"^"-1",")",sep="")),
      x = expression(paste("L (kg ha"^"-1", "year"^"-1",")")))+
 #geom_text(x = 20, y = 30, label = 'equation', parse = TRUE)+
    annotate("text", x=17, y=30, label= "y=1.773x + 35.597", size = 4)+
  theme_bw()+
  theme(panel.grid = element_blank()))


#---------------------------------------------------------------------------
#Season obs vs predicted
validation_dt4[month %in% c(1,2, 3), season := '1']
validation_dt4[month %in% c(4, 5, 6), season := '2']
validation_dt4[month %in% c(7, 8,9), season := '3']
validation_dt4[month %in% c(10, 11,12), season := '4']

validation_dt4[month %in% c(2, 3, 4, 5, 6, 7), season := '1']
validation_dt4[month %in% c(1, 8,9, 10, 11,12), season := '2']

validation_dt4_season <- validation_dt4[, .(L = mean(L),
                                            NO3.kg.sec = mean(NO3.kg.sec)), by = .(year, season)]

lm(data = validation_dt4_season, formula = 'L~NO3.kg.sec')

(plot_3 <- ggplot(validation_dt4_season, aes(x = L, y = NO3.kg.sec, color = year))+ #ylim(0, 40)+ xlim(0, 50)+
    geom_point()+
    geom_smooth(formula = 'y~x', method = 'lm', se = FALSE, color = 'darkgrey') +
    # xlab('Grafton N-NO3 (kg sec-1)')+
    # ylab('L (kg ha-1)')+
    labs(y=expression(paste("Grafton N-", NO[3],"(kg sec"^"-1",")",sep="")),
         x = expression(paste("L (kg ha"^"-1", "year"^"-1",")")))+
    #geom_text(x = 20, y = 30, label = 'equation', parse = TRUE)+
    annotate("text", x=17, y=30, label= "y=1.773x + 35.597", size = 4)+
    theme_bw()+
    theme(panel.grid = element_blank()))



#---------------------------------------------------------------------------
# Shifted
validation_dt4_shifted <- copy(validation_dt4)[order(year, month)]
# lag with n=1 and pad with NA (returns vector)
data.table::shift(1:5, n=2, fill=NA, type="lag")
validation_dt4_shifted[,L := data.table::shift(L, n=1, fill=NA, type="lag")]
validation_dt4_shifted <- validation_dt4_shifted[-c(1)]

#Monthly average plot
validation_dt4_monthly <- validation_dt4_shifted[, .(L = mean(L),
                                             NO3.kg.sec = mean(NO3.kg.sec)), by = month]

ggplot(data = validation_dt4_monthly) + 
  geom_line(aes(x = month, y = L), col = 'blue')+
  geom_line(aes(x = month, y = NO3.kg.sec), col = 'green')

validation_dt4_shifted[month %in% c(1,2, 3), season := '1']
validation_dt4_shifted[month %in% c(4, 5, 6), season := '1']
validation_dt4_shifted[month %in% c(7, 8,9), season := '2']
validation_dt4_shifted[month %in% c(10, 11,12), season := '2']

validation_dt4_season <- validation_dt4_shifted[, .(L = mean(L),
                                            NO3.kg.sec = mean(NO3.kg.sec)), by = .(year, season)]

reg <- lm(data = validation_dt4_season, formula = 'NO3.kg.sec~L')
reg_sum <- summary(reg)
r_sq_num <- round(reg_sum$r.squared,2)
corr_num <- round(cor(validation_dt4_season$L, validation_dt4_season$NO3.kg.sec),2)

(plot_4 <- ggplot(validation_dt4_season, aes(x = L, y = NO3.kg.sec, color = year))+ #ylim(0, 40)+ xlim(0, 50)+
    geom_point()+
    geom_smooth(formula = 'y~x', method = 'lm', se = FALSE, color = 'darkgrey') +
    # xlab('Grafton N-NO3 (kg sec-1)')+
    # ylab('L (kg ha-1)')+
    labs(y=expression(paste("Grafton N-", NO[3],"(kg sec"^"-1",")",sep="")),
         x = expression(paste("State L (kg ha"^"-1", "year"^"-1",")")))+
    #geom_text(x = 20, y = 30, label = 'equation', parse = TRUE)+
    annotate("text", x=8, y=4, label= "y=0.93x + 8.12", size = 4, hjust = 0)+
    annotate("text", x=8, y=3, label= expression(paste("R"^"2", "= 0.20")), size = 4, hjust = 0)+
    annotate("text", x=8, y=2, label= paste("correlation =",  corr_num), size = 4, hjust = 0)+
    theme_bw()+
    theme(panel.grid = element_blank())+
    annotate("text", x=0.3, y=25, label= "b)", size = 8) )

plot_grid <- grid.arrange(plot_1, plot_4, ncol=2, nrow = 2, 
                          layout_matrix = rbind(c(1,2), c(NA,2)),
                          widths = c(5, 5), heights = c(2.5, 0.05))
ggsave(plot = plot_grid, 
       filename = "./n_policy_box/Data/figures/validation_grafton.jpg",  width = 1263/300*3, height = 502/300*3,
       units = 'in')

