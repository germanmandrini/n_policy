# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
setwd('~')
rm(list=ls())

source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')
source('./n_policy_git/Codes/parameters.R')
# source('C:/Users/germanm2/Documents/n_policy_validation_git/Codes/parameters.R')

#======================================================================================
# 1) ABSOLUTE YIELD DISTRIBUTION
# Load MRTN data
if(FALSE){
  mrtn_dt1 <- read.csv('./n_policy_box/Data/validation/mrtn_yield_south.csv', header = F, col.names = c('eonr_lb_ac', 'yield_bu_ac')) %>% data.table() %>% 
    .[,region := '1-South']
  mrtn_dt2 <- read.csv('./n_policy_box/Data/validation/mrtn_yield_central.csv', header = F, col.names = c('eonr_lb_ac', 'yield_bu_ac')) %>% data.table() %>% 
    .[,region := '2-Central']
  mrtn_dt3 <- read.csv('./n_policy_box/Data/validation/mrtn_yield_north.csv', header = F, col.names = c('eonr_lb_ac', 'yield_bu_ac')) %>% data.table() %>% 
    .[,region := '3-North']
  mrtn_dt <- rbindlist(list(mrtn_dt1, mrtn_dt2, mrtn_dt3))
  # Corn: 1 bushel/acre = 62.77 (63) kilograms/hectare
  mrtn_dt[,Y_corn := 62.77*yield_bu_ac]
  # Fertilizer: 1 lb/acre = 1.12085 kilograms/hectare
  mrtn_dt[,eonr := 1.12085*eonr_lb_ac]
  mrtn_dt[,source := 'mrtn']
  
  saveRDS(mrtn_dt, "./n_policy_box/Data/files_rds/mrtn_yields_from_graphs.rds")
}
mrtn_dt <- readRDS("./n_policy_box/Data/files_rds/mrtn_yields_from_graphs.rds")
#---------------------------------------------------------------------------------------
# Load Simulated data
yc_yearly_dt3 <- readRDS("./n_policy_box/Data/files_rds/yc_yearly_dt3.rds")
#Add regions and areas
grid10_soils_dt5 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt5.rds")
areas_dt <- data.table(grid10_soils_dt5) %>% .[,.(area_ha = sum(area_ha)), by = .(id_10, mukey, region)]

simulated_dt <- merge(yc_yearly_dt3[,-c('region', 'area_ha')], areas_dt, by = c('id_10', 'mukey'))

#---------------------------------------------------------------------------------------
# testing_set_dt <- readRDS(paste0("./n_policy_box/Data/files_rds/yc_yearly_dt_batch", batch_n, ".rds"))

# testing_set_dt <- readRDS("./n_policy_box/Data/files_rds/testing_set_dt.rds")
simulated_dt[, P := Y_corn * Pc - N_fert * Pn]  #update profits

simulated_eonr_dt <- simulated_dt[, .SD[ P == max( P)], by = .(id_10, mukey, z)] %>% 
  .[, .SD[ N_fert == min( N_fert )], by = .(region, id_10, mukey, z)]
setnames(simulated_eonr_dt, 'N_fert', 'eonr')

simulated_eonr_dt[region == 1,region_lab := '1-South']
simulated_eonr_dt[region == 2,region_lab := '2-Central']
simulated_eonr_dt[region == 3,region_lab := '3-North']

simulated_eonr_dt[,region := factor(region_lab)]
simulated_eonr_dt[,region_lab := NULL]

simulated_eonr_dt[,source := 'simulated']

#---------------------------------------------------------------------------------------
# Merge data
data_dt <- rbind(simulated_eonr_dt[,.(source, region, Y_corn)], mrtn_dt[,.(source, region, Y_corn)])

data_dt[, .(Y_corn = mean(Y_corn)), by = .(source, region)]
#---------------------------------------------------------------------------------------
# Yield Boxplots

# New facet label names for supp variable
supp.labs <- c("MRTN data", "Simulated data")
names(supp.labs) <- c("mrtn", "simulated")


(plot_1 <- ggplot(data = data_dt) + 
    geom_boxplot(aes(x = region, y = Y_corn, color = region)) +
    # geom_line(aes(x = N_fert, y = L1_rel, linetype = "N Leaching", color = region)) +
    #geom_hline(yintercept = baselevel_yld, linetype = 'dashed', color = 'grey', size = 1)+
    # geom_vline(xintercept = baselevel_nfert, linetype = 'dashed', color = 'grey', size = 1)+
    labs(y = 'Optimum yield (kg/ha)',
         x = 'Region',
         colour = "Region") +
    # scale_y_continuous(sec.axis = sec_axis(~./200, name = "Corn N leaching (kg/ha)", breaks = seq(30,80,5), labels = seq(30,80,5))) +
    # scale_linetype_manual(values = c("dashed", "solid"))+
    theme_bw()+
    facet_wrap(source~., labeller = labeller(source = supp.labs))+
    theme(strip.background =element_rect(fill="white"),
          axis.text=element_text(size=14),
          legend.position = "none",
          strip.text = element_text(colour = 'black')))

ggsave(plot = plot_1, 
       filename = "./n_policy_box/Data/figures/validation_yield.pdf",  width = 703/300*3, height = 502/300*3,
       units = 'in')
ggsave(plot = plot_1, 
       filename = "./n_policy_box/Data/figures/validation_yield.png",  width = 703/300*3, height = 502/300*3,
       units = 'in')

#======================================================================================
# 2) RELATIVE YIELD SHAPE PLOTS
# Load MRTN data
mrtn_dt1 <- read.csv('./n_policy_box/Data/validation/mrtn_shape_south.csv', header = F, col.names = c('N_fert', 'Y_rel')) %>% data.table() %>% 
  .[,region := '1-South']
mrtn_dt2 <- read.csv('./n_policy_box/Data/validation/mrtn_shape_central.csv', header = F, col.names = c('N_fert', 'Y_rel')) %>% data.table() %>% 
  .[,region := '2-Central']
mrtn_dt3 <- read.csv('./n_policy_box/Data/validation/mrtn_shape_north.csv', header = F, col.names = c('N_fert', 'Y_rel')) %>% data.table() %>% 
  .[,region := '3-North']
mrtn_dt <- rbindlist(list(mrtn_dt1, mrtn_dt2, mrtn_dt3))
# Fertilizer: 1 lb/acre = 1.12085 kilograms/hectare
mrtn_dt[,N_fert := 1.12085*N_fert]

mrtn_dt[N_fert < 0, N_fert := 0]
mrtn_dt[,source := 'mrtn']

#---------------------------------------------------------------------------------------
# Prepare simulated data

(plot_1 <- ggplot(simulated_dt[n_deep_v5 < 100]) +
  geom_density(aes(x = n_deep_v5, color = factor(region))))


ggsave(plot = plot_1, 
       filename = "./n_policy_box/Data/figures/validation_pdf.pdf")

state_agg_dt  <- aggregate_by_area(data_dt = simulated_dt, variables = c('Y_corn', 'Y_soy', 'L1','L2'), 
                                   weight = 'area_ha', by_c = c('N_fert', 'region'))# %>% .[,-'area_ha']


state_agg_dt[region == 1,region_lab := '1-South']
state_agg_dt[region == 2,region_lab := '2-Central']
state_agg_dt[region == 3,region_lab := '3-North']

state_agg_dt[,region := factor(region_lab)]
state_agg_dt[,region_lab := NULL]
# yc_yearly_dt <- yc_yearly_batches_dt[batch %in% c(13,17)]
# state_agg_dt  <- testing_set_dt[,.(Y_corn = mean(Y_corn),
#                                  L = mean(L)), by = .(N_fert, region, batch)][order(region)]

# state_agg_dt <- state_agg_dt[N_fert < 251]
state_agg_dt[,Y_max := max(Y_corn), by = .(region)]
state_agg_dt[,Y_rel := Y_corn/Y_max*100]
state_agg_dt[,source := 'simulated']

state_agg_dt[N_fert == 0]
mrtn_dt[N_fert == 0]

#---------------------------------------------------------------------------------------
# Merge data
data_dt <- rbind(state_agg_dt[,.(source, N_fert, Y_rel, region)], mrtn_dt[,.(source, N_fert, Y_rel, region)])
data_dt[,region := factor(region)]

# New facet label names for supp variable
supp.labs <- c("MRTN data", "Simulated data")
names(supp.labs) <- c("mrtn", "simulated")


(plot_1 <- ggplot(data = data_dt) + 
    geom_line(aes(x = N_fert, y = Y_rel, color = region), size = 1) +
    # geom_line(aes(x = N_fert, y = L1_rel, linetype = "N Leaching", color = region)) +
    #geom_hline(yintercept = baselevel_yld, linetype = 'dashed', color = 'grey', size = 1)+
    # geom_vline(xintercept = baselevel_nfert, linetype = 'dashed', color = 'grey', size = 1)+
    labs(y = 'Percent of maximum yield',
         x = 'N rate (kg/ha)',
         colour = "Region") +
    # scale_y_continuous(sec.axis = sec_axis(~./200, name = "Corn N leaching (kg/ha)", breaks = seq(30,80,5), labels = seq(30,80,5))) +
    # scale_linetype_manual(values = c("dashed", "solid"))+
    xlim(0, 250) +
    theme_bw()+
    facet_wrap(source~., labeller = labeller(source = supp.labs))+
    theme(strip.background =element_rect(fill="white"),
          axis.text=element_text(size=12),
          legend.position = "none",
          strip.text = element_text(colour = 'black')))

ggsave(plot = plot_1, 
       filename = "./n_policy_box/Data/figures/validation_shape.pdf",  width = 706/300*3, height = 415/300*3,
       units = 'in')

ggsave(plot = plot_1, 
       filename = "./n_policy_box/Data/figures/validation_shape.png",  width = 706/300*3, height = 415/300*3,
       units = 'in')


#======================================================================================
# 3) EONR FREQUENCY PLOTS
# Load MRTN data
mrtn_dt1 <- read.csv('./n_policy_box/Data/validation/mrtn_eonr_south.csv', header = F, col.names = c('bin', 'prop')) %>% data.table() %>% 
  .[,region := '1-South']
mrtn_dt2 <- read.csv('./n_policy_box/Data/validation/mrtn_eonr_central.csv', header = F, col.names = c('bin', 'prop')) %>% data.table() %>% 
  .[,region := '2-Central']
mrtn_dt3 <- read.csv('./n_policy_box/Data/validation/mrtn_eonr_north.csv', header = F, col.names = c('bin', 'prop')) %>% data.table() %>% 
  .[,region := '3-North']
mrtn_dt <- rbindlist(list(mrtn_dt1, mrtn_dt2, mrtn_dt3))

mrtn_dt[prop < 0, prop := 0]
mrtn_dt[,source := 'mrtn']
mrtn_dt[,bin := gsub(pattern = 'Bar', replacement = '', x = bin)]
mrtn_dt[,bin := as.numeric(bin) +1]

#---------------------------------------------------------------------------------------
# Load simulated data
simulated_dt[, P := Y_corn * Pc - N_fert * Pn]  #update profits
simulated_eonr_dt <- simulated_dt[, .SD[ P == max( P)], by = .( id_10, mukey, z)] %>% .[, .SD[ N_fert == min( N_fert )], by = .(id_10, mukey, z)]
setnames(simulated_eonr_dt, 'N_fert', 'eonr')
#Add 5 to half of the rates to avoid being right on edge
simulated_eonr_dt[, eonr2 := as.numeric(lapply(eonr, function(eonr) eonr+sample(-5:5,1)))]

# brks <- c(0,25,50,75,101,125,150,175,201,231,330)
brks_lb_ac <- c(0,25,50,75,100,125,150,175,200,225, 250,330)
# Fertilizer: 1 lb/acre = 1.12085 kilograms/hectare
brks_kg_ha <- round(1.12085* brks_lb_ac, 0)

simulated_eonr_dt[,bin:=findInterval(eonr2, brks_kg_ha)]
simulated_eonr_dt[eonr==170]$eonr2 %>% table()
# ggplot(yc_region_eonr_dt, aes(bin     )) + geom_bar()
simulated_eonr_dt2 <- simulated_eonr_dt[,.N, by = .(region, bin)]
simulated_eonr_dt2[,prop := N/sum(N)*100, by = region]
simulated_eonr_dt2[,source := 'simulated']
simulated_eonr_dt2[region == 1,region_lab := '1-South']
simulated_eonr_dt2[region == 2,region_lab := '2-Central']
simulated_eonr_dt2[region == 3,region_lab := '3-North']

simulated_eonr_dt2[,region := factor(region_lab)]
simulated_eonr_dt2[,region_lab := NULL]
#---------------------------------------------------------------------------------------
# Merge data
data_dt <- rbind(simulated_eonr_dt2[,.(source, bin, prop, region)], mrtn_dt[,.(source, bin, prop, region)])

bins_table <- data.table(bin = 1:(length(brks_kg_ha)-1),
                         eonr_bin = paste(brks_kg_ha[-12], '-' , brks_kg_ha[-1], sep = ''))
bins_table[bin ==11, eonr_bin := '250+']
data_dt2 <- merge(data_dt, bins_table, by = 'bin')

data_dt2[, eonr_bin:= factor(eonr_bin, levels = bins_table$eonr_bin)]
data_dt2[,region := factor(region)]


supp.labs <- c("MRTN data", "Simulated data")
names(supp.labs) <- c("mrtn", "simulated")

(plot_1 <- ggplot(data_dt2, aes(eonr_bin , fill = region    )) + #geom_bar(aes(y = (..count..)/sum(..count..)))+
    geom_col(aes( y=prop))+
    labs(x= 'Economic Optimum N Rate (kg/ha)', y = "% of sites")+
    theme_bw()+
    facet_grid(source~region, labeller = labeller(source = supp.labs))+
    theme(strip.background =element_rect(fill="white"),
        legend.position = "none",
        axis.text.x=element_text(angle=45, hjust=1),
        strip.text = element_text(colour = 'black')))

ggsave(plot = plot_1, 
       filename = "./n_policy_box/Data/figures/validation_eonr.pdf",  width = 706/300*3, height = 415/300*3,
       units = 'in')

ggsave(plot = plot_1, 
       filename = "./n_policy_box/Data/figures/validation_eonr.png",  width = 706/300*3, height = 415/300*3,
       units = 'in')
       
# =========================================================================================================================================================
# CREATE THE REGIONAL MINIMUM MODEL - OK
Yld_response_threshold <- 100  
simulated_dt[, Yld_response := max(Y_corn) - min(Y_corn), by = .( id_10, mukey,z)]
simulated_dt[, P := Y_corn * Pc - N_fert * Pn]  #update profits
TrainSet_RMM <- simulated_dt[Yld_response > Yld_response_threshold] #Needs to be here, to use updated profits 

model_minimum_ok  <- aggregate_by_area(data_dt = simulated_dt, variables = c('P'), 
                                         weight = 'area_ha', by_c = c('region', 'N_fert')) %>% 
    .[, .SD[ P == max( P)], by = .( region)] %>% .[,.(region, eonr_pred = N_fert)]

model_minimum_ok[]


