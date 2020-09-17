rm(list=ls())

setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
codes_folder <-'C:/Users/germanm2/Documents'#CPSC

setwd('~')#Server
codes_folder <-'~' #Server

source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')
source(paste0(codes_folder, '/n_policy_git/Codes/parameters.R'))

yc_yearly_dt3 <- readRDS("./n_policy_box/Data/files_rds/yc_yearly_dt3.rds")
# grid10_tiles_sf6 <- readRDS("./n_policy_box/Data/Grid/grid10_tiles_sf6.rds") 
grid10_soils_dt5 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt5.rds") %>% data.table()
# grid10_fields_sf2 <- readRDS('./n_policy_box/Data/Grid/grid10_fields_sf2.rds')
# reg_model_stuff <- readRDS( "./n_policy_box/Data/files_rds/reg_model_stuff.rds")
# grid10_soils_sf2 <- readRDS('./n_policy_box/Data/Grid/grid10_soils_sf2.rds')

# setnames(yc_yearly_dt3, c('Yld', 'Yld_soy', 'leach_1', 'leach_2'),
#                         c('Y_corn', 'Y_soy', 'L1', 'L2'))
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
state_agg_dt2[,region := factor(region)]

baselevel_L1 <- 24.03
baselevel_L2 <- 18.48
baselevel_L <- 42.51
baselevel_Y_corn <- 12831 
baselevel_nfert <- 191

# ----------------------------------------------------
#Max possible reduction:
max_red_dt  <- state_agg_dt2[N_fert == 0 | N_fert == round(baselevel_nfert/10)*10][order(N_fert)]
max_red_dt[,L := L1 + L2]
(max_red_dt[2, L] - max_red_dt[N_fert == 0, L])/ max_red_dt[2, L]

# ----------------------------------------------------
# state_agg_dt2[,leach_prop := round((leach_n / baselevel_leach) - 1,2)*100 ]

(plot_1 <- ggplot(data = state_agg_dt2[N_fert < 250]) + 
  geom_line(aes(x = N_fert, y = Y_corn, linetype = "Yield", color = region)) +
  geom_line(aes(x = N_fert, y = L1*200, linetype = "N Leaching", color = region)) +
  #geom_hline(yintercept = baselevel_yld, linetype = 'dashed', color = 'grey', size = 1)+
  geom_vline(xintercept = baselevel_nfert, linetype = 'dashed', color = 'grey', size = 1) +
  labs(y = 'Corn Yield (kg/ha)',
       x = 'Corn N rate (kg/ha)',
       colour = "Region",
       linetype = 'Variable') +
  scale_y_continuous(sec.axis = sec_axis(~./200, name = "Corn N leaching (kg/ha)", breaks = seq(30,80,5), labels = seq(30,80,5))) +
  scale_linetype_manual(values = c("dashed", "solid"))+
  theme_bw()+
  # guides(linetype = guide_legend(order=2),
  #        size = guide_legend(order=1)) +
  theme(# legend.title =  element_blank(),
        # legend.position = c(0.87, 0.15),
        legend.position = 'bottom',
        legend.text=element_text(size=8),
        panel.grid = element_blank())+
annotate("text", x=20, y=12500, label= "a)", size = 10) )

plot_1

# ggsave(plot = plot_1, filename = "./n_policy_box/Data/figures/state_response_curve_corn.jpg", width = 5, height = 3,
#        units = 'in')

(plot_2 <- ggplot(data = state_agg_dt2[N_fert < 250]) + 
  geom_line(aes(x = N_fert, y = Y_soy, linetype = "Yield", color = region)) +
  geom_line(aes(x = N_fert, y = L2*120, linetype = "N Leaching", color = region)) +
  #geom_hline(yintercept = baselevel_yld, linetype = 'dashed', color = 'grey', size = 1)+
  geom_vline(xintercept = baselevel_nfert, linetype = 'dashed', color = 'grey', size = 1)+
  labs(y = 'Soy Yield (kg/ha)',
       x = 'Corn N rate (kg/ha)',
       colour = "Parameter") +
  scale_y_continuous(sec.axis = sec_axis(~./200, name = "Soy N leaching (kg/ha)", breaks = seq(0,30,2), labels = seq(0,30,2))) +
  scale_linetype_manual(values = c("dashed", "solid"))+
  theme_bw()+
  # guides(linetype = guide_legend(order=2),
  #        size = guide_legend(order=1)) +
   theme(#legend.title =  element_blank(),
        # legend.position = c(0.87, 0.15),
        legend.text=element_text(size=8),
        panel.grid = element_blank())+
  annotate("text", x=20, y=4000, label= "b)", size = 10) )

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(plot_1)

p3 <- grid.arrange(arrangeGrob(plot_1 + theme(legend.position="none"),
                               plot_2 + theme(legend.position="none"),
                               nrow=1),
                   mylegend, nrow=2,heights=c(10, 1))
print(p3)


ggsave(p3, filename = "./n_policy_box/Data/figures/state_response_curve_both.jpg", width = 10, height =  4, units = 'in')
