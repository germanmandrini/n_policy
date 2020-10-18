rm(list=ls())

setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
codes_folder <-'C:/Users/germanm2/Documents'#CPSC

# setwd('~')#Server
# codes_folder <-'~' #Server

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

#==========================AGGREGATED FOR THE STATE============================================================
# Do two plots with Y_corn and Leaching for a static N_fert across the state for soy and corn
areas_dt <- grid10_soils_dt5[,.(area_ha = sum(area_ha)), by = .(id_10, mukey)]

state_agg_dt <- merge(yc_yearly_dt3, areas_dt, by = c('id_10', 'mukey'))

state_agg_dt2  <- aggregate_by_area(data_dt = state_agg_dt, variables = c('Y_corn', 'Y_soy', 'L1','L2'), 
                                    weight = 'area_ha', by_c = c('region','N_fert'))# %>% .[,-'area_ha']

#Choose one region
region_n = '2'
state_agg_dt3 <- state_agg_dt2[region == region_n] #filter one region


#get thresholds
# reg_model_stuff <- readRDS( "./n_policy_box/Data/files_rds/reg_model_stuff.rds")
# leach_threshold <- reg_model_stuff$leach_threshold[as.numeric(region_n)]
# bal_threshold <- reg_model_stuff$bal_threshold[as.numeric(region_n)]
# rm(reg_model_stuff)

leach_threshold <- 29
bal_threshold <- 0


#Calculate veriables for the plot
state_agg_dt3[, L := L1 + L2]
state_agg_dt3[, cost_base := N_fert * Pn]  #update profits
state_agg_dt3[, cost_ratio := N_fert * (Pn/5)*20]  #update profits

state_agg_dt3[,L_extra := L - leach_threshold]
state_agg_dt3[L_extra <= 0, L_extra := 0]
state_agg_dt3[, cost_leach := 10 * L_extra + cost_base] 

state_agg_dt3[, N_balance := N_fert - Y_corn * 11/1000]
state_agg_dt3[,N_extra := N_balance - bal_threshold]
state_agg_dt3[N_extra <= 0, N_extra := 0]
state_agg_dt3[,cost_bal := 3 * N_extra + cost_base]  

state_agg_dt3[,P_base := Y_corn * Pc - cost_base]
state_agg_dt3[,P_ratio := Y_corn * Pc - cost_ratio]
state_agg_dt3[,P_leach := Y_corn * Pc - cost_leach]
state_agg_dt3[,P_bal := Y_corn * Pc - cost_bal]

#Get the levels that get to the same N rates for each policy
perfomances_dt4 <- readRDS("./n_policy_box/Data/files_rds/perfomances_dt4.rds")
levels_dt <- perfomances_dt4[region == region_n & NMS == 'static' & N_fert > 149 & N_fert < 151]
levels_dt[, .SD[policy_val== min(policy_val )], by = policy_name]

# ----------------------------------------------------
# CONCEPTUAL FIGURES

plot_1_dt <- state_agg_dt3[N_fert < 250] %>% .[N_extra <= bal_threshold, N_balance := NA]

vert_line_leach_threshold <- plot_1_dt[L >= leach_threshold] %>% .[L == min(L), N_fert]
vert_line_bal_threshold <- plot_1_dt[N_balance >= bal_threshold] %>% .[N_balance == min(N_balance), N_fert]


(plot_1 <- ggplot(data = plot_1_dt) + 
   geom_line(aes(x = N_fert, y = Y_corn, linetype = "Yield"), size = 1) +
   geom_line(aes(x = N_fert, y = L*200, linetype = "N Leaching"), size = 1) +
   geom_line(aes(x = N_fert, y = N_balance*150, linetype = "N Balance"), size = 1) +
   #geom_hline(yintercept = baselevel_yld, linetype = 'dashed', color = 'grey', size = 1)+
   geom_vline(xintercept = vert_line_bal_threshold, linetype = 'dashed', color = 'grey', size = 1)+
   labs(y = 'Corn Yield (kg/ha)',
        x = 'Corn N rate (kg/ha)',
        colour = "Region",
        linetype = 'Variable') +
   scale_y_continuous(sec.axis = sec_axis(~./200, name = "Corn N leaching (kg/ha)", 
                                          breaks = seq(0,80,5), labels = seq(0,80,5))) +
   scale_linetype_manual(values = c("dashed", "dotted", "solid"))+
   theme_bw()+
   # guides(linetype = guide_legend(order=2),
   #        size = guide_legend(order=1)) +
   theme(# legend.title =  element_blank(),
     # legend.position = c(0.87, 0.15),
     legend.position = 'bottom',
     legend.text=element_text(size=8),
     panel.grid = element_blank())+
   geom_text(aes(x=vert_line_bal_threshold, y=0,label='leaching and balance threshold'),hjust=0,vjust=0,angle=90,size=4)+
   annotate("text", x=10, y=14000, label= "a)", size = 10) )

plot_2_long_dt <- melt(state_agg_dt3[N_fert < 250],
                       id.vars = c('N_fert'),
                       measure.vars = c( 'cost_base', 'cost_ratio', 'cost_leach', 'cost_bal','P_base', 'P_ratio', 'P_leach', 'P_bal'))
plot_2_long_dt[,policy_name := as.character(lapply(variable, function(x) str_split(x, pattern = '_')[[1]][2]))]
plot_2_long_dt[,variable_group := as.character(lapply(variable, function(x) str_split(x, pattern = '_')[[1]][1]))]


(plot_2 <- ggplot() + 
    geom_line(data = plot_2_long_dt, aes(x = N_fert, y = value, color = policy_name, linetype = variable_group), size = 1) + 
    geom_point(data = plot_2_long_dt[,.SD[value == max(value)], by = policy_name], 
               aes(x = N_fert, y = value, color = policy_name), size = 3) +
    labs(y = 'Cost ($/ha)',
         x = 'Corn N rate (kg/ha)') +
    theme_bw()+
    # guides(linetype = guide_legend(order=2),
    #        size = guide_legend(order=1)) +
    theme(# legend.title =  element_blank(),
      # legend.position = c(0.87, 0.15),
      legend.position = 'bottom',
      legend.text=element_text(size=8),
      panel.grid = element_blank())+
    annotate("text", x=10, y=1500, label= "b)", size = 10) )

p3 <- grid.arrange(plot_1, plot_2, nrow=2)
print(p3)


ggsave(p3, filename = "./n_policy_box/Data/figures/state_response_curve_both.pdf", width = 10, height =  4, units = 'in')




# ----------------------------------------------------
#Max possible reduction:
max_red_dt  <- state_agg_dt2[N_fert == 0 | N_fert == round(baselevel_nfert/10)*10][order(N_fert)]
max_red_dt[,L := L1 + L2]
(sum(max_red_dt[N_fert != 0, L]) - sum(max_red_dt[N_fert == 0, L]))/ sum(max_red_dt[N_fert != 0, L])


# ------------------By Region---------------------------------
(plot_1 <- ggplot(data = state_agg_dt2[N_fert < 250]) + 
   geom_line(aes(x = N_fert, y = Y_corn, linetype = "Yield")) +
   geom_line(aes(x = N_fert, y = L1*200, linetype = "N Leaching")) +
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
    geom_line(aes(x = N_fert, y = Y_soy, linetype = "Yield")) +
    geom_line(aes(x = N_fert, y = L2*120, linetype = "N Leaching")) +
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


ggsave(p3, filename = "./n_policy_box/Data/figures/state_response_curve_both.pdf", width = 10, height =  4, units = 'in')


#================================By REGION======================================================
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
(sum(max_red_dt[N_fert != 0, L]) - sum(max_red_dt[N_fert == 0, L]))/ sum(max_red_dt[N_fert != 0, L])

# ---------------------------------------------------
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


ggsave(p3, filename = "./n_policy_box/Data/figures/state_response_curve_both.pdf", width = 10, height =  4, units = 'in')
