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
baselevel_dt <- perfomances_dt5[policy == 'ratio_5' & NRT == 'static', .( L_base = L, Y_base = Y_corn, P_base = P)]

opt_list <- list()
#=============================================================================================================================================
# ABATEMENT COST OPTIMIZATION
# Plot: leaching reduction vs abatement cost, using the sublevel of the policy by region_eq that leads to the same abatement cost across region_eqs.
# and using the same sublevel for the whole state

perfomances_dt4 <- readRDS("./n_policy_box/Data/files_rds/perfomances_dt4.rds")

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



0.8*42.16
# ---------
# Make leaching relative to baselevel
optimization_state_dt <- cbind(optimization_state_dt, baselevel_dt)
optimization_state_dt[,L_change := round((L / L_base) - 1,3)*100 ]

#---------
#Calculate policy_cost
optimization_state_dt[,policy_cost := P  + G - P_base]
optimization_state_dt[,abatement_cost := policy_cost/(L_base-  L)]

#---------
#remove yields modifications of more that 5%
optimization_state_dt[,Y_corn_change := Y_corn/Y_base]
optimization_state_dt <- optimization_state_dt[Y_corn_change >=0.95 & Y_corn_change <= 1.05] #remove yields modifications of more that 5%

# Some cleaning
colsToDelete <- c('L1', 'L2', 'corn_avg_ha', 'L_base', 'Y_base', 'P_base','Y_corn_change')
set(abatement_mg_state_dt,, colsToDelete, NULL)

#---------
# L reduction vs policy cost with optimization
abatement_mg_state_dt[,sublevels := 'abatement_mg']
uniform_state_dt <- perfomances_dt5[,sublevels := 'uniform'][NRT == 'dynamic' & policy_name == 'ratio']


uniform_vs_opt_dt <- rbind(abatement_mg_state_dt, uniform_state_dt, fill= T)

ggplot(data = uniform_vs_opt_dt) +
  geom_line(aes(x = -L_change, y =  policy_cost , color = policy_name, linetype = sublevels), size = 1)

#20% reduction cost
abatement_mg_regions_dt[loop ==  15]
perfomances_dt4[policy == 'ratio_10' & NRT == 'dynamic']



ggplot(data = abatement_state_dt) +
  geom_line(aes(x = L_change, y =  policy_cost , linetype = NRT), size = 1)+
  # scale_linetype_manual(values = c("dashed", "solid"))+
  facet_free(y_labels~x_labels,
             labeller = label_parsed,
             scales="free",
             switch = 'both') +
  theme_bw()+
  theme(# panel.grid = element_blank(), 
    strip.background.x = element_blank(),
    strip.placement.x = "outside",
    strip.background.y = element_blank(),
    strip.placement.y = "outside",
    # panel.spacing = unit(1.5, "lines"),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position = "bottom",
    plot.margin =  unit(c(1,1,1,1), "lines")
  )




#=============================================================================================================================================
#=============================================================================================================================================
# MAKE A MAP OF ABATEMENT COST
perfomances_dt3 <- readRDS("./n_policy_box/Data/files_rds/perfomances_dt3.rds") #for 5e_validation.R

# ---------
# Make leaching relative to baselevel
baselevel_dt <- perfomances_dt3[policy == 'ratio_5' & NRT == 'static', .(id_10, L_base = L, Y_base = Y_corn, P_base = P)]

map_dt <- merge(perfomances_dt3[NRT == 'dynamic'], baselevel_dt, by = 'id_10')
map_dt[,L_change := round((L / L_base) - 1,3)*100 ]

#---------
#Calculate policy_cost
map_dt[,policy_cost := P  + G - P_base]
map_dt[,abat_cost := policy_cost/(L_base-  L)]

#---------------------------------------------------------------------------
# Some cleaning
map_dt[, c("policy_name", "policy_val") := tstrsplit(policy, "_", fixed=TRUE)]
map_dt[,policy_val := as.numeric(policy_val)]

map_dt2 <- map_dt[abat_cost > -1 & policy_name == 'ratio'] %>% 
  .[, .SD[ L == min( L)], by = .(id_10, NRT, policy_name)] %>%
  .[, .SD[ policy_val == min( policy_val)], by = .(id_10, NRT, policy_name)]

colsToDelete <- c('L_base', 'Y_base', 'P_base','Y_corn_change')
set(map_dt,, colsToDelete, NULL)

hist(map_dt2$abat_cost)

grid10_tiles_sf7 <- readRDS("./n_policy_box/Data/Grid/grid10_tiles_sf7.rds") 
value_sf <- merge(grid10_tiles_sf7, map_dt2[,.(id_10, policy_val)], by = 'id_10', all.x = T)

tm_shape(value_sf) + tm_polygons(c('policy_val', 'region_eq'), n=10)

(p1 <- tm_shape(value_sf) + tm_polygons(c('corn_avg_ha'), 
                                        n =10, 
                                        title = c("Corn area (ha/cell)"),
                                        style ="cont", 
                                        # border.col = 'black',
                                        palette = "Greys")+
    tm_layout(panel.labels = 'a)',
              main.title.position = c(0,0),
              legend.text.size = 0.7,
              main.title.size = 1.2,
              title.snap.to.legend =F,
              legend.width = 1,
              legend.position = c('left', 'bottom')))