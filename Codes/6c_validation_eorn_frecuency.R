# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
setwd('~')
rm(list=ls())

source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')
source('./n_policy_git/Codes/parameters.R')
source('C:/Users/germanm2/Documents/n_policy_validation_git/Codes/parameters.R')

# grid10_tiles_dt6 <- readRDS("./n_policy_box/Data/Grid/grid10_tiles_sf6.rds") %>% data.table()
yc_yearly_dt3 <- readRDS("./n_policy_box/Data/files_rds/yc_yearly_dt3.rds")
# Pn_tmp = Pn * 0.9
yc_yearly_dt3[, P := Y_corn * Pc - N_fert * Pn]  #update profits

# yc_yearly_dt3 <- merge(yc_yearly_dt3, unique(grid10_tiles_dt6[,.(id_10, region)]), by = 'id_10')
yc_region_dt <- yc_yearly_dt3[region == 2]
yc_region_dt <- yc_region_dt[mukey %in% sample(unique(yc_region_dt$mukey), 500)]
yc_region_eonr_dt <- yc_region_dt[, .SD[ P == max( P)], by = .(id_10, mukey, z)]
yc_region_eonr_dt <- yc_region_eonr_dt[, .SD[ N_fert == min( N_fert )], by = .(id_10, mukey, z)]
setnames(yc_region_eonr_dt, 'N_fert', 'eonr')
brks <- c(0,25,50,75,101,125,150,175,201,231,330)
yc_region_eonr_dt[,bin:=findInterval(eonr, brks)]
ggplot(yc_region_eonr_dt, aes(bin     )) + geom_bar()

bins_table <- data.table(bin = 1:(length(brks)-1),
           eonr_bin = paste(brks[-11], '-' , brks[-1], sep = ''))
bins_table[bin ==10, eonr_bin := '231+']
yc_region_eonr_dt2 <- merge(yc_region_eonr_dt, bins_table, by = 'bin')

str(yc_region_eonr_dt)

yc_region_eonr_dt2$eonr_bin <- factor(yc_region_eonr_dt2$eonr_bin, levels = bins_table$eonr_bin)
ggplot(yc_region_eonr_dt2, aes(eonr_bin     )) + geom_bar()+
  labs(x= 'N fert (kg/ha)')+
  theme_bw()+
  theme(legend.title =  element_blank(),
        axis.text=element_text(size=14))

#============================================
#Relative yield plot

yc_yearly_dt3 <- readRDS("./n_policy_box/Data/files_rds/yc_yearly_dt3.rds")
# grid10_tiles_sf6 <- readRDS("./n_policy_box/Data/Grid/grid10_tiles_sf6.rds") 
grid10_soils_dt5 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt5.rds") %>% data.table()
# grid10_fields_sf2 <- readRDS('./n_policy_box/Data/Grid/grid10_fields_sf2.rds')
# reg_model_stuff <- readRDS( "./n_policy_box/Data/files_rds/reg_model_stuff.rds")
# grid10_soils_sf2 <- readRDS('./n_policy_box/Data/Grid/grid10_soils_sf2.rds')

setnames(yc_yearly_dt3, c('Yld', 'Yld_soy', 'leach_1', 'leach_2'),
         c('Y_corn', 'Y_soy', 'L1', 'L2'))
# setnames(perfomances_dt, c('Yld', 'Yld_soy', 'leach_1', 'leach_2', 'leach_n', 'gov' ),
#          c('Y_corn', 'Y_soy', 'L1', 'L2', "L",'G'))
# yc_yearly_dt3[,L := L1 + L2] #update leaching adding corn and soy
# yc_yearly_dt3[,P := Y_corn * Pc + Y_soy * Ps - N_fert * Pn] #update profits adding corn and soy

#======================================================================================
# Do two plots with Y_corn and Leaching for a static N_fert across the state for soy and corn
areas_dt <- grid10_soils_dt5[,.(area_ha = sum(area_ha)), by = .(id_10, mukey)]

state_agg_dt <- merge(yc_yearly_dt3, areas_dt, by = c('id_10', 'mukey'))

state_agg_dt2  <- aggregate_by_area(data_dt = state_agg_dt, variables = c('Y_corn', 'Y_soy', 'L1','L2'), 
                                    weight = 'area_ha', by_c = c('N_fert', 'region'))# %>% .[,-'area_ha']

state_agg_dt2[,Y_max := max(Y_corn), by = region]
state_agg_dt2[,Y_rel := Y_corn/Y_max]
state_agg_dt2[,region := factor(region)]
baselevel_L1 <- 30.29278
baselevel_L2 <- 21.92814
baselevel_L <- 52.22092  
baselevel_Y_corn <- 11125.63 
baselevel_nfert <- 175.3900

# ----------------------------------------------------
plot_1 <- ggplot(data = state_agg_dt2[N_fert < 250]) + 
  geom_line(aes(x = N_fert, y = Y_rel, linetype = "Yield", color = region)) +
  geom_line(aes(x = N_fert, y = L1*200, linetype = "N Leaching")) +
  #geom_hline(yintercept = baselevel_yld, linetype = 'dashed', color = 'grey', size = 1)+
  geom_vline(xintercept = baselevel_nfert, linetype = 'dashed', color = 'grey', size = 1)+
  labs(y = 'Corn Yield (kg/ha)',
       x = 'Corn N rate (kg/ha)',
       colour = "Parameter") +
  scale_y_continuous(sec.axis = sec_axis(~./200, name = "Corn N leaching (kg/ha)", breaks = seq(30,80,5), labels = seq(30,80,5))) +
  scale_linetype_manual(values = c("dashed", "solid"))+
  theme_bw()+
  # guides(linetype = guide_legend(order=2),
  #        size = guide_legend(order=1)) +
  theme(legend.title =  element_blank(),
        legend.position = c(0.87, 0.15),
        legend.text=element_text(size=8),
        panel.grid = element_blank())+
  annotate("text", x=220, y=12500, label= "a)", size = 10) 

