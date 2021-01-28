rm(list=ls())

# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# codes_folder <-'C:/Users/germa/Documents'#Dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# codes_folder <-'C:/Users/germanm2/Documents'#CPSC
setwd('~')#Server
codes_folder <-'~' #Server


source('./Codes_useful/R.libraries.R')
# library(scales)
source('./Codes_useful/gm_functions.R')
source(paste0(codes_folder, '/n_policy_git/Codes/parameters.R'))
"~/n_policy_git/Codes/parameters.R"

perfomances_dt5 <- readRDS("./n_policy_box/Data/files_rds/perfomances_dt5.rds")
perfomances_dt4 <- readRDS("./n_policy_box/Data/files_rds/perfomances_dt4.rds")

#=============================================================================================================================================
# UNIFORM SUBLEVEL
perfomances_dt5[,region_eq := 'state']

plot_dt <- rbind(perfomances_dt4, perfomances_dt5, fill = T)%>% 
  .[policy_name %in% policies_paper & NRT %in% c('dynamic')]

#=============================================================================================================================================
# LEACHING VS COST PLOT

ggplot(data = plot_dt[region_eq == 'state']) +
  theme_bw()+
  geom_line(aes(x = -L_change, y =  policy_cost , color = policy_name), size = 1)+
  xlab('N Leaching reduction (%)')+
  ylab('Policy Cost ($/ha)')

#In relative L reduction
(p1 <- ggplot(data = plot_dt) +
  geom_line(aes(x = -L_change, y =  policy_cost , color = policy_name), size = 1)+
  xlab('N Leaching reduction (%)')+
  ylab('Policy Cost ($/ha)')+
  theme_bw()+
  theme(legend.position = 'bottom')+
  facet_free(~region_eq))


#abatement cost
(p2 <- ggplot(data = plot_dt) +
  geom_line(aes(x = -L_change, y =  abatement_cost, color = policy_name), size = 1)+
  xlab('N Leaching reduction (%)')+
  ylab('Abatement Cost ($/kg ha)')+
  theme_bw()+
  theme(legend.position = 'bottom')+
  facet_free(~region_eq))


#In absolute L reduction
plot_dt[,L_diff := L - L_base]


ggplot(data = plot_dt) +
  geom_line(aes(x = -L_diff, y =  policy_cost , color = policy_name), size = 1)+
  xlab('N Leaching reduction (kg/ha)')+
  ylab('Policy Cost ($/ha)')+
  theme_bw()+
  theme(legend.position = 'bottom')+
  facet_free(~region_eq)




#=============================================================================================================================================
# BAR CHARTS 20% REDUCTION
opt_list <- list()
#=============================================================================================================================================
# MG ABATEMENT COST OPTIMIZATION
# Plot: leaching reduction vs abatement cost, using the sublevel of the policy by region_eq that leads to the same abatement cost across region_eqs.
# and using the same sublevel for the whole state



optimization_dt <- perfomances_dt4[policy_name %in% c('ratio') & NRT %in% c('dynamic')] #, 'leach', 'bal', 'red'

optimization_dt[, L_next := data.table::shift(L, n=-1, fill=NA, type="lag"), by = region_eq]
optimization_dt[, P_next := data.table::shift(P, n=-1, fill=NA, type="lag"), by = region_eq]
optimization_dt[, L_next := L-L_next]
optimization_dt[, P_next := P-P_next]
optimization_dt[, abat_mg := P_next/L_next]

optimization_list <- list()
keep <- TRUE

while(keep){
  
  # abatement_dt[,region_rows := .N, by = region_eq]
  remove_this <- optimization_dt[, .SD[ policy_val == min( policy_val)], by = .(region_eq, NRT, policy_name)] %>%
    .[abat_mg == min(abat_mg, na.rm = T) ]
  
  optimization_dt <- optimization_dt[!(region_eq == remove_this$region_eq & policy_val <= remove_this$policy_val)] #policy_val lower than selected
  
  optimization_selected_dt <- optimization_dt[, .SD[ policy_cost  == max(policy_cost )], by = .(region_eq, NRT, policy_name)] %>%
    .[, .SD[ policy_val == min( policy_val)], by = .(region_eq, NRT, policy_name)]
  
  keep = nrow(optimization_dt) > 3#length(unique(abatement_dt$region_eq))
  
  optimization_list[[length(optimization_list)+1]] <- optimization_selected_dt[,loop := length(optimization_list)+1]
}

optimization_output_dt <- rbindlist(optimization_list)
optimization_output_dt[,.N, by = .(loop)]

# State agregation
optimization_state_dt <- aggregate_by_area(data_dt = optimization_output_dt, #use perfomances_dt3 to avoid the 95% rule by region_eq
                                           variables = c("Y_corn", 'L1', 'L2', "L", "N_fert","P", "G", 'policy_cost'), 
                                           weight = 'corn_avg_ha', by_c = c('policy_name', 'loop')) #state level, weighted by corn_ha

optimization_state_dt[,region_eq := 'state']

optimization_output_dt <- rbind(optimization_output_dt, optimization_state_dt, fill = T)

opt_list[[1]] <- optimization_output_dt[,type := 'abat_mg']

#=============================================================================================================================================
# EQUAL POLICY COST PER HA
# Plot: leaching reduction vs abatement cost, using the sublevel of the policy by region_eq that leads to the same abatement cost across region_eqs.
# and using the same sublevel for the whole state

perfomances_dt4 <- readRDS("./n_policy_box/Data/files_rds/perfomances_dt4.rds")

optimization_dt <- perfomances_dt4[policy_name %in% c('ratio', 'leach', 'bal', 'red') & NRT %in% c('dynamic')] #, 'leach', 'bal', 'red'



optimization_list <- list()
keep <- TRUE

while(keep){
  # abatement_dt[,region_rows := .N, by = region_eq]
  remove_this <- optimization_dt[, .SD[ policy_val == min( policy_val)], by = .(region_eq, NRT, policy_name)] %>%
    .[,.SD[policy_cost  == max(policy_cost , na.rm = T)], by = .(NRT, policy_name) ] %>% 
    .[,.SD[L  == max(L , na.rm = T)], by = .(NRT, policy_name) ] #in case there is mroe than one
  
  optimization_dt <- optimization_dt[!(region_eq %in% remove_this$region_eq & policy_val <= remove_this$policy_val)] #policy_val lower than selected
  
  optimization_selected_dt <- optimization_dt[, .SD[ policy_cost  == max(policy_cost )], by = .(region_eq, NRT, policy_name)] %>%
    .[, .SD[ policy_val == min( policy_val)], by = .(region_eq, NRT, policy_name)]
  
  keep = nrow(optimization_dt) > 3#length(unique(abatement_dt$region_eq))
  
  optimization_list[[length(optimization_list)+1]] <- optimization_selected_dt[,loop := length(optimization_list)+1]
}

optimization_output_dt <- rbindlist(optimization_list)
optimization_output_dt[,.N, by = .(loop)]

# State agregation
optimization_state_dt <- aggregate_by_area(data_dt = optimization_output_dt, #use perfomances_dt3 to avoid the 95% rule by region_eq
                                           variables = c("Y_corn", 'L1', 'L2', "L", "N_fert","P", "G", 'policy_cost'), 
                                           weight = 'corn_avg_ha', by_c = c('policy_name', 'loop')) #state level, weighted by corn_ha

optimization_state_dt[,region_eq := 'state']

optimization_output_dt <- rbind(optimization_output_dt, optimization_state_dt, fill = T)

opt_list[[2]] <- optimization_output_dt[,type := 'pol_cost']

reduction_strategies_dt <- rbindlist(opt_list, fill = T)

# Add the baselevel info
perfomances_dt4 <- readRDS("./n_policy_box/Data/files_rds/perfomances_dt4.rds") %>% 
  .[policy_name %in% c('ratio', 'leach', 'bal', 'red') & NRT %in% c('dynamic')]
perfomances_dt4[, c("policy_name", "policy_val") := tstrsplit(policy, "_", fixed=TRUE)]
perfomances_dt4[,policy_val := as.numeric(policy_val)]


baselevel_dt <- perfomances_dt4[NRT == 'dynamic',.SD[policy_val == min(policy_val)], by = .(policy_name, region_eq)] %>%
  .[,.(policy_name, region_eq, L_base = L, Y_base = Y_corn, P_base = P)]
reduction_strategies_dt1 <- merge(reduction_strategies_dt, baselevel_dt, by = c('policy_name','region_eq'))


baselevel_dt <- perfomances_dt5[NRT == 'dynamic',.SD[policy_val == min(policy_val)], by = .(policy_name)] %>%
  .[,region_eq := 'state'] %>%
  .[,.(policy_name, region_eq, L_base = L, Y_base = Y_corn, P_base = P)]
reduction_strategies_dt2 <- merge(reduction_strategies_dt, baselevel_dt, by = c('policy_name','region_eq'))


reduction_strategies_dt <- rbind(reduction_strategies_dt1, reduction_strategies_dt2)

reduction_strategies_dt[,L_change := round((L / L_base) - 1,3)*100 ]

#---------
#Calculate policy_cost
reduction_strategies_dt[,policy_cost := P  + G - P_base]
reduction_strategies_dt[,abatement_cost := policy_cost/(L_base - L)]

#---------
#remove yields modifications of more that 5%
reduction_strategies_dt[,Y_corn_change := Y_corn/Y_base]
reduction_strategies_dt <- reduction_strategies_dt[Y_corn_change >=0.95 & Y_corn_change <= 1.05] #remove yields modifications of more that 5%

#---------------------------------------------------------------------------
# Some cleaning
colsToDelete <- c('L1', 'L2', 'corn_avg_ha', 'L_base', 'Y_base', 'P_base','Y_corn_change')
set(reduction_strategies_dt,, colsToDelete, NULL)
