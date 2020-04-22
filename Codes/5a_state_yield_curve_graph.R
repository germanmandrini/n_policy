# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
setwd('~')
rm(list=ls())

source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')
source('./n_policy_git/Codes/parameters.R')


yc_yearly_dt3 <- readRDS("./n_policy_box/Data/files_rds/yc_yearly_dt3.rds")
# grid10_tiles_sf6 <- readRDS("./n_policy_box/Data/Grid/grid10_tiles_sf6.rds") 
grid10_soils_dt5 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt5.rds") %>% data.table()
# grid10_fields_sf2 <- readRDS('./n_policy_box/Data/Grid/grid10_fields_sf2.rds')
# reg_model_stuff <- readRDS( "./n_policy_box/Data/files_rds/reg_model_stuff.rds")
# grid10_soils_sf2 <- readRDS('./n_policy_box/Data/Grid/grid10_soils_sf2.rds')

yc_yearly_dt3[,leach_n := leach_1 + leach_2] #update leaching adding corn and soy
yc_yearly_dt3[, P := Yld * Pc + Yld_soy * Ps - N_fert * Pn] #update profits adding corn and soy
#======================================================================================
# Do two plots with Yld and Leaching for a static N_fert across the state for soy and corn
areas_dt <- grid10_soils_dt5[,.(area_ha = sum(area_ha)), by = .(id_10, mukey)]

state_agg_dt <- merge(yc_yearly_dt3, areas_dt, by = c('id_10', 'mukey'))

state_agg_dt2  <- aggregate_by_area(data_dt = state_agg_dt, variables = c('Yld', 'Yld_soy', 'leach_1','leach_2'), 
                                    weight = 'area_ha', by_c = c('N_fert'))# %>% .[,-'area_ha']
baselevel_leach <- 45.6537
baselevel_leach <- 47
baselevel_yld <- 11136.46
baselevel_nfert <- 186.5601

# state_agg_dt2[,leach_prop := round((leach_n / baselevel_leach) - 1,2)*100 ]

plot_1 <- ggplot(data = state_agg_dt2[N_fert < 250]) + 
  geom_line(aes(x = N_fert, y = Yld, linetype = "Yield")) +
  geom_line(aes(x = N_fert, y = leach_1*200, linetype = "N Leaching")) +
  #geom_hline(yintercept = baselevel_yld, linetype = 'dashed', color = 'grey', size = 1)+
  geom_vline(xintercept = baselevel_nfert, linetype = 'dashed', color = 'grey', size = 1)+
  labs(y = 'Corn Yield (kg/ha)',
       x = 'Corn N rate (kg/ha)',
       colour = "Parameter")+
  scale_y_continuous(sec.axis = sec_axis(~./200, name = "Corn N leaching (kg/ha)", breaks = seq(30,80,5), labels = seq(30,80,5))) +
  scale_linetype_manual(values = c("dashed", "solid"))+
  theme_bw()+
  # guides(linetype = guide_legend(order=2),
  #        size = guide_legend(order=1)) +
  theme(legend.title =  element_blank(),
        legend.position = c(0.87, 0.25),
        legend.text=element_text(size=8),
        panel.grid = element_blank())

plot_1

ggsave(plot = plot_1, filename = "./n_policy_box/Data/figures/state_response_curve_corn.jpg", width = 5, height = 3,
       units = 'in')

plot_2 <- ggplot(data = state_agg_dt2[N_fert < 250]) + 
  geom_line(aes(x = N_fert, y = Yld_soy, linetype = "Yield")) +
  geom_line(aes(x = N_fert, y = leach_2*120, linetype = "N Leaching")) +
  #geom_hline(yintercept = baselevel_yld, linetype = 'dashed', color = 'grey', size = 1)+
  geom_vline(xintercept = baselevel_nfert, linetype = 'dashed', color = 'grey', size = 1)+
  labs(y = 'Soy Yield (kg/ha)',
       x = 'Corn N rate (kg/ha)',
       colour = "Parameter")+
  scale_y_continuous(sec.axis = sec_axis(~./200, name = "Soy N leaching (kg/ha)", breaks = seq(0,30,2), labels = seq(0,30,2))) +
  scale_linetype_manual(values = c("dashed", "solid"))+
  theme_bw()+
  # guides(linetype = guide_legend(order=2),
  #        size = guide_legend(order=1)) +
  theme(legend.title =  element_blank(),
        legend.position = c(0.87, 0.25),
        legend.text=element_text(size=8),
        panel.grid = element_blank())

plot_2



ggsave(plot = plot_1, filename = "./n_policy_box/Data/figures/state_response_curve_soy.jpg", width = 5, height = 3,
       units = 'in')

ggsave(plot = grid.arrange(plot_1,plot_2), filename = "./n_policy_box/Data/figures/state_response_curve_both.jpg", width = 5, height = 5,
       units = 'in')
