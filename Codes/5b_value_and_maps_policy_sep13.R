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

# source('./Codes_useful/gm_functions.R')
if(T){
  grid10_tiles_sf7 <- readRDS("./n_policy_box/Data/Grid/grid10_tiles_sf7.rds") 
  grid10_soils_dt5 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt5.rds") %>% data.table()
  grid10_fields_sf2 <- readRDS('./n_policy_box/Data/Grid/grid10_fields_sf2.rds')
  
  perfomances_dt <- readRDS("./n_policy_box/Data/files_rds/perfomances_dt.rds")
  
  perfomances_dt[,.(area_ha = sum(area_ha)), by = .(policy, NMS, id_10, id_field, z)]$area_ha %>% summary()
  
  perfomances_dt[NMS == 'dynamic1', NMS := 'dynamic']
  
  perfomances_dt[,.N, .(id_10, id_field)] %>% .[,.N, id_10] %>% .[,N] %>% table() #number of fields by cell
  perfomances_dt[,.N, .(id_10, id_field, mukey, policy, NMS)] %>% .[,N] %>% table() #number of z by mukey. SHould be all equal
  perfomances_dt[,.N, .(policy, NMS)]%>% .[,N] %>% table() #number of treatments (policy sublevels x NMS). SHould be all equal
  table(perfomances_dt$NMS) #obs by NMS. SHould be all equal
  
  summary(perfomances_dt[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, policy, NMS, z)]$area_ha)
  
  #-------------------------------------------------------------------------
  #Make profits relative to the zero rate
  if(FALSE){
    yc_yearly_dt3 <- readRDS("./n_policy_box/Data/files_rds/yc_yearly_dt3.rds")
    zero_dt <- yc_yearly_dt3[N_fert == 10, .(id_10, mukey, z, N_fert_zero = N_fert, L_zero = L, Y_corn_zero = Y_corn)]
    zero_dt[,.N, .(id_10, mukey, z)]$N %>% table()
    perfomances_dt[,policy_name := as.character(lapply(policy, function(x) str_split(x, pattern = '_')[[1]][1]))]
    perfomances_dt[,policy_val := as.numeric(str_extract(policy,pattern = '[0-9.]+'))]
    perfomances_dt <- merge(perfomances_dt, zero_dt, by = c('id_10', 'mukey', 'z'))
    
    perfomances_dt[policy_name == 'ratio', P_zero := Y_corn_zero * Pc - N_fert_zero * policy_val * Pc]
    perfomances_dt[policy_name == 'fee', P_zero := Y_corn_zero * Pc - N_fert_zero * Pn - L_zero * policy_val]
    perfomances_dt[policy_name == 'nred', P_zero := Y_corn_zero * Pc - N_fert_zero * Pn]
    
    perfomances_dt[,P := P - P_zero]
  }
  #-------------------------------------------------------------------------
  # AGGREGATE THE DATA TO FIELD X Z LEVEL CONSIDERING THE AREA
  names(perfomances_dt)
  do_not_aggregate = c("policy",'region','id_10', 'NMS', 'z', 'id_field')
  do_aggregate =  c("Y_corn", 'Y_soy', 'L1', 'L2', "L", "N_fert","P", "G")
  
  if(FALSE){
    
    perfomances_dt2 <- aggregate_by_area(data_dt = perfomances_dt, variables = do_aggregate, 
                                         weight = 'area_ha', by_c = do_not_aggregate) #field x z level (mukey is out)
  }else{
    split_list <- split(perfomances_dt,perfomances_dt$z)
    split_list_output <- list()
    for(split_list_n in split_list){
      split_list_output[[unique(split_list_n$z)]] <- aggregate_by_area(data_dt = split_list_n, variables = do_aggregate, 
                                                                       weight = 'area_ha', by_c = do_not_aggregate) #field x z level (mukey is out)
    }
    
    perfomances_dt2 <- rbindlist(split_list_output)
  }
  
  str(perfomances_dt2)
  
  perfomances_dt2 <- perfomances_dt2[order(id_10, z,id_field, NMS)]
  
  saveRDS(perfomances_dt2, "./n_policy_box/Data/files_rds/perfomances_dt2.rds") #for 5d_pdf.R
  # perfomances_dt2 <- readRDS("./n_policy_box/Data/files_rds/perfomances_dt2.rds") 
  
  #-------------------------------------------------------------------------
  # AGGREGATE THE DATA TO CELL X Z LEVEL CONSIDERING THE AREA
  names(perfomances_dt)
  do_not_aggregate = c('policy','id_10', 'region','NMS', 'z')
  do_aggregate =  c("Y_corn", 'Y_soy', 'L1', 'L2', "L", "N_fert","P", 'G')
  
  if(FALSE){
    #First aggregate without z so then we can get the leach_extreme
    perfomances_dt3 <- aggregate_by_area(data_dt = perfomances_dt2, variables = do_aggregate, 
                                         weight = 'area_ha', by_c = do_not_aggregate) #cell x z level (field is out)
  }else{
    split_list <- split(perfomances_dt2,perfomances_dt2$region)
    split_list_output <- list()
    for(split_list_n in split_list){
      split_list_output[[unique(split_list_n$region)]] <- aggregate_by_area(data_dt = split_list_n, variables = do_aggregate, 
                                                                            weight = 'area_ha', by_c = do_not_aggregate) #cell x z level (field is out)
    }
    
    perfomances_dt3 <- rbindlist(split_list_output)
  }
  
  saveRDS(perfomances_dt3, "./n_policy_box/Data/files_rds/perfomances_dt3.rds") #for 5e_validation.R
  perfomances_dt3 <- readRDS("./n_policy_box/Data/files_rds/perfomances_dt3.rds") #for 5e_validation.R
  #---------------------------------------------------------------------------
  # AGGREGATE AGAIN BY REGION CONSIDERING THE CORN PRODUCTION OF THE CELL
  grid10_tiles_dt <- data.table(grid10_tiles_sf7)[,.N, .(id_tile,id_10, corn_avg_ha,corn5_tile )][,-'N']
  
  summary(grid10_tiles_dt$corn_avg_ha)
  perfomances_dt3[,id_10 := as.integer(id_10)]
  perfomances_dt3 <- merge(perfomances_dt3, grid10_tiles_dt, by = 'id_10')
  
  
  perfomances_dt4 <- aggregate_by_area(data_dt = perfomances_dt3, variables = c("Y_corn", 'L1', 'L2', "L", "N_fert","P", "G"), 
                                       weight = 'corn_avg_ha', by_c = c('policy','NMS', 'region')) #region level, weighted by corn_ha
  
  # ---------
  # Make leaching relative to baselevel
  baselevel_dt <- perfomances_dt4[policy == 'ratio_5' & NMS == 'static', .(region, L_base = L, Y_base = Y_corn, P_base = P)]
  
  perfomances_dt4 <- merge(perfomances_dt4, baselevel_dt, by = 'region')
  perfomances_dt4[,L_change := round((L / L_base) - 1,3)*100 ]
  
  #---------
  #Calculate net_balance
  perfomances_dt4[,net_balance := P - P_base + G]
  
  #---------
  #remove yields modifications of more that 5%
  perfomances_dt4[,Y_corn_change := Y_corn/Y_base]
  
  #---------------------------------------------------------------------------
  # Some cleaning
  perfomances_dt4[,policy_val := as.numeric(str_extract(policy,pattern = '[0-9.]+'))]
  perfomances_dt4[,policy_name := as.character(lapply(policy, function(x) str_split(x, pattern = '_')[[1]][1]))]
  
  colsToDelete <- c('L_base', 'Y_base', 'P_base','Y_corn_change')
  set(perfomances_dt4,, colsToDelete, NULL)
  
  saveRDS(perfomances_dt4, "./n_policy_box/Data/files_rds/perfomances_dt4.rds")
  #---------------------------------------------------------------------------
  # AGGREGATE AGAIN AT THE STATE LEVEL, CONSIDERING THE CORN PRODUCTION OF THE CELL
  perfomances_dt5 <- aggregate_by_area(data_dt = perfomances_dt4, variables = c("Y_corn", 'L1', 'L2', "L", "N_fert","P", "G"), 
                                       weight = 'corn_avg_ha', by_c = c('policy', 'NMS')) #state level, weighted by corn_ha
  # ---------
  # Make leaching relative to baselevel
  baselevel_dt <- perfomances_dt5[policy == 'ratio_5' & NMS == 'static', .( L_base = L, Y_base = Y_corn, P_base = P)]

  perfomances_dt5 <- cbind(perfomances_dt5, baselevel_dt)
  perfomances_dt5[,L_change := round((L / L_base) - 1,3)*100 ]
  
  #---------
  #Calculate net_balance + 
  perfomances_dt5[,net_balance := P + G]
  perfomances_dt5[,policy_cost := P_base - P -  + G]
  
  #---------
  #remove yields modifications of more that 5%
  perfomances_dt5[,Y_corn_change := Y_corn/Y_base]
  perfomances_dt5 <- perfomances_dt5[Y_corn_change >=0.95 & Y_corn_change <= 1.05] #remove yields modifications of more that 5%
  
  #---------------------------------------------------------------------------
  # Some cleaning
  perfomances_dt5[,policy_val := as.numeric(str_extract(policy,pattern = '[0-9.]+'))]
  perfomances_dt5[,policy_name := as.character(lapply(policy, function(x) str_split(x, pattern = '_')[[1]][1]))]
  
  colsToDelete <- c('L1', 'L2', 'corn_avg_ha', 'L_base', 'Y_base', 'P_base','Y_corn_change')
  set(perfomances_dt5,, colsToDelete, NULL)
  
  saveRDS(perfomances_dt5, "./n_policy_box/Data/files_rds/perfomances_dt5.rds")
  
}  

#--------------------------------------------------
# STATE LEVEL PLOT 
perfomances_dt5 <- readRDS("./n_policy_box/Data/files_rds/perfomances_dt5.rds")

perfomances_dt5[policy %in% c('ratio_5', 'leach_0', 'bal_0', 'red_1') & NMS == 'static']
perfomances_dt5[policy %in% c('ratio_5', 'leach_0', 'bal_0', 'red_1') & NMS == 'dynamic']


plot_dt <- perfomances_dt5[policy_name %in% c('ratio', 'leach', 'bal', 'red') & NMS %in% c('static', 'dynamic')] 

# plot_dt[policy_name%in% c('red'), policy_val  := (1-policy_val )*100]

# plot_dt[policy_name%in% c('nred') & NMS == 'dynamic' & policy_val > 15, policy_val  := -round(L_change) ]
# plot_dt[policy_name%in% c('nred') & NMS == 'dynamic' & policy_val > 15]

baselevel_L <- plot_dt[policy == 'ratio_5' & NMS == 'static', L]
baselevel_Y_corn <- plot_dt[policy == 'ratio_5' & NMS == 'static', Y_corn ]


plot_dt_long <- melt(plot_dt, id.vars = c('policy_name','policy_val', 'NMS'), measure.vars = c('Y_corn', 'L_change', 'N_fert', 
                                                                                               'P', 'G', 'net_balance'))

plot_dt_long[,y_labels := factor(variable, levels = c('N_fert', 'L_change', 'Y_corn', 'P', 'G', 'net_balance'),
                                 labels = c(expression("N Fertilizer \n (N kg " * ha^"-1" *yr^"-1"* ")"), 
                                            expression("N Leaching\n ("*'%'*" change)"),
                                            expression("Corn Yield \n (kg N " * ha^"-1" *yr^"-1"* ")"), 
                                            expression("Farm profits \n ($ " * ha^"-1" * yr^"-1"* ")"),
                                            expression("Gov. collections \n ($ " * ha^"-1" * yr^"-1"* ")"),
                                            expression("Net income \n ($ " * ha^"-1" * yr^"-1"* ")")))]



plot_dt_long[,x_labels := factor(policy_name, levels = c('ratio', 'leach', 'bal', 'red'),
                                 labels = c(expression("N:Corn price"*" ratio"),
                                            expression("Leaching fee ($ " * kg^"-1" * ha^"-1"*")"),
                                            expression("N balance fee($ " * kg^"-1" * ha^"-1"*")"),
                                            expression("N reduction (%"*")")))]


# plot_dt_long[variable == 'N_fert', plot_name := 'a) N Rate kg/ha']
# plot_dt_long[variable == 'L', plot_name := 'b) L (% change)']
# plot_dt_long[variable == 'Y_corn', plot_name := 'c) Yield kg/ha']
# plot_dt_long[variable == 'P', plot_name := 'd) Profits $/ha']
# plot_dt_long[variable == 'G', plot_name := 'e) G $/ha']
# plot_dt_long[variable == 'E', plot_name := 'f) E $/ha']
# plot_dt_long[variable == 'W', plot_name := 'g) W $/ha']
# plot_dt_long[order(variable)]

#use https://ggplot2.tidyverse.org/reference/labellers.html
hline_dt <- data.table(unique(plot_dt_long[,.(policy_name, variable, y_labels, x_labels)]))
hline_dt[variable == 'Y_corn', y_line := baselevel_Y_corn*0.95]
hline_dt[policy_name == 'ratio' & variable == 'Y_corn', y_label := '95% base-level']


(p <- ggplot() +
   # geom_line(data = plot_dt_long1, aes(x = policy_val, y =  value, colour = NMS)) +
   # scale_colour_manual(values = c("black", "brown"))+
   geom_line(data = plot_dt_long, aes(x = policy_val, y =  value, color = NMS), size = 1) +
   # scale_linetype_manual(values = c("dashed", "solid"))+
   geom_hline(data = hline_dt, aes(yintercept = y_line), linetype = 'dashed', color = 'grey', size = 1)+
   # geom_text(data = hline_dt, aes(x = 5, y = y_line+50, label =y_label ), hjust = 'left', vjust = 'center') +
   # scale_color_manual(values=c("royalblue2", "tomato3"))+   
   # geom_text(data = ann_text[variable %in% unique(plot_dt_long$variable)], aes(y = value, x = x, label = lab), 
   #           hjust = 0, size = 8) +
   #   coord_cartesian(xlim = c(min(plot_dt_long$policy_val), max(plot_dt_long$policy_val)), # This focuses the x-axis on the range of interest
   #                   clip = 'off') +   # This keeps the labels from disappearing
   facet_free(y_labels~x_labels,
              labeller = label_parsed,
              scales="free",
              switch = 'x') +
   theme_bw()+
   theme(# panel.grid = element_blank(), 
     strip.background.x = element_blank(),
     strip.placement.x = "outside",
     # panel.spacing = unit(1.5, "lines"),
     axis.title.x=element_blank(),
     axis.title.y=element_blank(),
     legend.position = "bottom",
     plot.margin =  unit(c(1,1,1,1), "lines")
   ))

ggsave(plot = p, 
       filename = "./n_policy_box/Data/figures/policies_multiplot.pdf", width = 831/300*3, height = 963/300*3,
       units = 'in')

# Elasticity of Demand Point-Slope Formula: https://pressbooks.bccampus.ca/uvicecon103/chapter/4-2-elasticity/
if(FALSE){
  
  elasticity_dt <- perfomances_dt5[policy_name == 'ratio' & NMS == 'static' & policy_val %in% c(5,6)]
  
  d_quantity <- (elasticity_dt[policy_val == 6, N_fert]  - elasticity_dt[policy_val == 5, N_fert])/
    elasticity_dt[policy_val == 5, N_fert]
  
  d_price <- (Pc*6 -  Pc*5)/ (Pc*5)
  
  d_quantity/d_price
  
  elasticity_dt <- perfomances_dt5[policy_name == 'ratio' & NMS == 'dynamic' & policy_val %in% c(5,6)]
  
  d_quantity <- (elasticity_dt[policy_val == 6, N_fert]  - elasticity_dt[policy_val == 5, N_fert])/
    elasticity_dt[policy_val == 5, N_fert]
  
  d_price <- (Pc*6 -  Pc*5)/ (Pc*5)
  
  d_quantity/d_price
}

# Total G collections in IL for 20% reduction
if(FALSE){
  IL_corn_area_ha = 5179976
  percent20_dt <- perfomances_dt5[ L_change < -19]
  percent20_dt <- percent20_dt[, .SD[ policy_val == min(policy_val)], by = .(NMS, policy_name)]
  saveRDS(percent20_dt, "./n_policy_box/Data/files_rds/percent20_dt.rds")
  percent20_dt[policy_name == 'ratio' & NMS == 'static', G] * IL_corn_area_ha / 1000000 #million in IL
  percent20_dt[policy_name == 'leach' & NMS == 'static', G] * IL_corn_area_ha / 1000000 #million in IL
  percent20_dt[policy_name == 'bal' & NMS == 'static', G] * IL_corn_area_ha / 1000000 #million in IL
}

#---------------------------------------------------------------------------
# REGION LEVEL PLOT 

perfomances_dt4 <- readRDS("./n_policy_box/Data/files_rds/perfomances_dt4.rds")

perfomances_dt4[policy %in% c('ratio_5', 'leach_0', 'nred_1', 'target_1', 'red_1', 'bal_0') & NMS == 'static']
perfomances_dt4[policy %in% c('ratio_5', 'leach_0', 'nred_1', 'target_1', 'red_1', 'bal_0') & NMS == 'dynamic']


plot_dt <- perfomances_dt4[policy_name %in% c('ratio', 'leach', 'red', 'bal') & NMS %in% c('static', 'dynamic')] 


baselevel_L <- plot_dt[policy == 'ratio_5' & NMS == 'static', L]
baselevel_Y_corn <- plot_dt[policy == 'ratio_5' & NMS == 'static', Y_corn ]


plot_dt_long <- melt(plot_dt, id.vars = c('policy_name','policy_val',  'region', 'NMS'), measure.vars = c('Y_corn', 'L_change', 'N_fert', 
                                                                                               'P', 'G', 'net_balance'))

plot_dt_long[,y_labels := factor(variable, levels = c('N_fert', 'L_change', 'Y_corn', 'P', 'G', 'net_balance'),
                                 labels = c(expression("N Fertilizer \n (N kg " * ha^"-1" *yr^"-1"* ")"), 
                                            expression("N Leaching\n ("*'%'*" change)"),
                                            expression("Corn Yield \n (kg N " * ha^"-1" *yr^"-1"* ")"), 
                                            expression("Farm profits \n ($ " * ha^"-1" * yr^"-1"* ")"),
                                            expression("Gov. collections \n ($ " * ha^"-1" * yr^"-1"* ")"),
                                            expression("Net balance \n ($ " * ha^"-1" * yr^"-1"* ")")))]



plot_dt_long[,x_labels := factor(policy_name, levels = c('ratio', 'leach', 'bal', 'red'),
                                 labels = c(expression("N:Corn price"*" ratio"),
                                            expression("Leaching fee ($ " * kg^"-1" * ha^"-1"*")"),
                                            expression("N balance fee($ " * kg^"-1" * ha^"-1"*")"),
                                            expression("N reduction (%"*")")))]


#use https://ggplot2.tidyverse.org/reference/labellers.html
# hline_dt <- data.table(unique(plot_dt_long[,.(policy_name, variable, y_labels, x_labels)]))
# hline_dt[variable == 'Y_corn', y_line := baselevel_Y_corn*0.95]
# hline_dt[policy_name == 'ratio' & variable == 'Y_corn', y_label := '95% base-level']


(p <- ggplot() +
    # geom_line(data = plot_dt_long1, aes(x = policy_val, y =  value, colour = NMS)) +
    # scale_colour_manual(values = c("black", "brown"))+
    geom_line(data = plot_dt_long, aes(x = policy_val, y =  value, color = NMS, linetype = region), size = 1) +
    # scale_linetype_manual(values = c("dashed", "solid"))+
    # geom_hline(data = hline_dt, aes(yintercept = y_line), linetype = 'dashed', color = 'grey', size = 1)+
    # geom_text(data = hline_dt, aes(x = 5, y = y_line+50, label =y_label ), hjust = 'left', vjust = 'center') +
    # scale_color_manual(values=c("royalblue2", "tomato3"))+   
    # geom_text(data = ann_text[variable %in% unique(plot_dt_long$variable)], aes(y = value, x = x, label = lab), 
    #           hjust = 0, size = 8) +
    #   coord_cartesian(xlim = c(min(plot_dt_long$policy_val), max(plot_dt_long$policy_val)), # This focuses the x-axis on the range of interest
    #                   clip = 'off') +   # This keeps the labels from disappearing
    facet_free(y_labels~x_labels,
               labeller = label_parsed,
               scales="free",
               switch = 'x') +
    theme_bw()+
    theme(# panel.grid = element_blank(), 
      strip.background.x = element_blank(),
      strip.placement.x = "outside",
      # panel.spacing = unit(1.5, "lines"),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.position = "bottom",
      plot.margin =  unit(c(1,1,1,1), "lines")
    ))

ggsave(plot = p, 
       filename = "./n_policy_box/Data/figures/policies_multiplot_region.pdf", width = 831/300*3, height = 963/300*3,
       units = 'in')
#--------------------------------------------------------------------------------
# Regional table
perfomances_dt4 <- readRDS("./n_policy_box/Data/files_rds/perfomances_dt4.rds")
percent20_dt <- readRDS("./n_policy_box/Data/files_rds/percent20_dt.rds")
percent20_dt <- percent20_dt[NMS == 'static',.(policy, NMS)]
percent20_dt <- rbind(data.table(policy = c('ratio_5'), NMS = c('static')), 
                      percent20_dt)

region_dt <- filter_dt_in_dt(perfomances_dt4, filter_dt = percent20_dt, return_table = T)
region_dt[region == 1,region_lab := '1-South']
region_dt[region == 2,region_lab := '2-Central']
region_dt[region == 3,region_lab := '3-North']

region_dt_base <- region_dt[policy == 'ratio_5', .(region_lab, Y_corn, L, N_fert, P)]
region_dt_20down <- region_dt[policy != 'ratio_5', .(region_lab, policy_name, NMS, Y_corn, L, N_fert, P, G, net_balance)]

region_dt2 <- merge(region_dt_20down, region_dt_base, by = 'region_lab', suffixes = c("", "_base"))
region_dt2[, Y_corn_diff := Y_corn - Y_corn_base]
region_dt2[, L_diff := L - L_base]
region_dt2[, N_fert_diff := N_fert - N_fert_base]
region_dt2[, P_diff := P - P_base]
region_dt2[, net_balance := P_diff + G]
region_dt2 <- region_dt2[,.(region_lab, policy_name, NMS, Y_corn, L, N_fert, P, G,Y_corn_diff, L_diff, N_fert_diff, P_diff, net_balance)]
region_dt2[order(-region_lab)]
region_dt2[,.(L_diff = mean(L_diff),
              N_fert_diff = mean(N_fert_diff),
              net_balance = mean(net_balance)), by = .(NMS, region_lab) ]

#--------------------------------------------------------------------------------
# N Balance

perfomances_dt2 <- readRDS("./n_policy_box/Data/files_rds/perfomances_dt2.rds") 

perfomances_dt2[,N_balance := N_fert - Y_corn * 11/1000]

perfomances_dt2[,L_ton_grain := L/(Y_corn / 1000)]
balance_dt <- perfomances_dt2[policy == 'ratio_5']

balance_dt[region == 1,region_lab := '1-South']
balance_dt[region == 2,region_lab := '2-Central']
balance_dt[region == 3,region_lab := '3-North']


(p1 <- ggplot(data = balance_dt)+ geom_density(aes(x = N_balance, colour = NMS), size =1)+
  facet_free(region_lab~.)+
  ggtitle('N Balance'))

ggsave(plot = p1, 
       filename = "./n_policy_box/Data/figures/balance1.png")

(p2 <- ggplot(data = balance_dt)+ geom_density(aes(x = L2, colour = NMS), size =1)+
  facet_free(region_lab~.)+
  ggtitle('Leaching'))

ggsave(plot = p2, 
       filename = "./n_policy_box/Data/figures/balance2.png")

(p3 <- ggplot(data = balance_dt[sample(1:nrow(balance_dt), 5000)], aes(x = N_balance, y = L)) + 
  geom_point()+
  geom_smooth()+
  facet_free(region_lab~.))

ggsave(plot = p3, 
       filename = "./n_policy_box/Data/figures/balance3.png")

(p4 <- ggplot(data = balance_dt[sample(1:nrow(balance_dt), 5000)], aes(x = N_balance, y = L_ton_grain)) + 
  geom_point()+
  geom_smooth()+
  facet_free(region_lab~.))

ggsave(plot = p4, 
       filename = "./n_policy_box/Data/figures/balance4.png")

#-------------------------------------------------------------------------
# YC graph
# Load Simulated data
yc_yearly_dt3 <- readRDS("./n_policy_box/Data/files_rds/yc_yearly_dt3.rds")


#Add regions and areas
grid10_soils_dt5 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt5.rds")
areas_dt <- data.table(grid10_soils_dt5) %>% .[,.(area_ha = sum(area_ha)), by = .(id_10, mukey)]

areas_dt <- grid10_soils_dt5[,.(area_ha = sum(area_ha)), by = .(id_10, mukey)]

state_agg_dt <- merge(yc_yearly_dt3[,-c('area_ha')], areas_dt, by = c('id_10', 'mukey'))

state_agg_dt2  <- aggregate_by_area(data_dt = state_agg_dt, variables = c('Y_corn', 'Y_soy', 'L1','L2'), 
                                    weight = 'area_ha', by_c = c('N_fert', 'region'))# %>% .[,-'area_ha']

state_agg_dt2[,N_balance := N_fert - Y_corn * 11/1000]
state_agg_dt2[,L := L1 + L2]
state_agg_dt2[,L_ton_grain := L/(Y_corn / 1000)]

state_agg_dt2[region == 1,region_lab := '1-South']
state_agg_dt2[region == 2,region_lab := '2-Central']
state_agg_dt2[region == 3,region_lab := '3-North']

ggplot() + 
  geom_line(data = state_agg_dt2, aes(x = N_fert, y = L_ton_grain))+
  geom_point(data = state_agg_dt2[,.SD[L_ton_grain == min(L_ton_grain)], by = region], aes(x = N_fert, y = L_ton_grain))+
  facet_free(region_lab~.)

ggplot(data = state_agg_dt2) + 
  geom_point( aes(x = N_balance, y = L))+
  facet_free(region_lab~.)

ggplot(data = state_agg_dt2) + 
  geom_point( aes(x = N_balance, y = L_ton_grain))+
  geom_point( aes(x = N_balance, y =  Y_corn/1000))+
  facet_free(region_lab~.)

#-----------------------------------------------------------------------------------------------------#
#POSTER
perfomances_dt4 <- readRDS("./n_policy_box/Data/files_rds/perfomances_dt4.rds")

plot_dt <- perfomances_dt4[policy_name %in% c('ratio', 'fee', 'nred', 'red') & NMS %in% c('static', 'dynamic') ] 

plot_dt[policy_name%in% c('nred', 'target'), policy_val  := (1-policy_val )*100]
# plot_dt[policy_name%in% c('nred') & NMS == 'dynamic' & policy_val > 15, policy_val  := -round(L_change) ]
# plot_dt[policy_name%in% c('nred') & NMS == 'dynamic' & policy_val > 15]

baselevel_L <- perfomances_dt4[policy == 'ratio_5' & NMS == 'static', L]
baselevel_Y_corn <- perfomances_dt4[policy == 'ratio_5' & NMS == 'static', Y_corn ]


plot_dt_long <- melt(plot_dt, id.vars = c('policy_name','policy_val', 'NMS'), measure.vars = c('L_change',  
                                                                                               'P', 'G', 'net_balance'))#'N_fert' 'Y_corn', 

plot_dt_long[,y_labels := factor(variable, levels = c('N_fert', 'L_change', 'Y_corn', 'P', 'G', 'net_balance'),
                                 labels = c(expression("N"*" fertilizer"), 
                                            expression("N"*" leaching"),
                                            expression("Corn"*" yield"), 
                                            expression("Farm"*" profits"),
                                            expression("Gov"*" collections"),
                                            expression("Net"*" balance")))]



plot_dt_long[,x_labels := factor(policy_name, levels = c('ratio', 'fee', 'red'),
                                 labels = c(expression("N:Corn price"*" ratio"),
                                            expression("Fee on L ($ " * kg^"-1" * ha^"-1"*")"),
                                            expression("N reduction (%"*")")))]


# plot_dt_long[variable == 'N_fert', plot_name := 'a) N Rate kg/ha']
# plot_dt_long[variable == 'L', plot_name := 'b) L (% change)']
# plot_dt_long[variable == 'Y_corn', plot_name := 'c) Yield kg/ha']
# plot_dt_long[variable == 'P', plot_name := 'd) Profits $/ha']
# plot_dt_long[variable == 'G', plot_name := 'e) G $/ha']
# plot_dt_long[variable == 'E', plot_name := 'f) E $/ha']
# plot_dt_long[variable == 'W', plot_name := 'g) W $/ha']
# plot_dt_long[order(variable)]

#use https://ggplot2.tidyverse.org/reference/labellers.html
hline_dt <- data.table(unique(plot_dt_long[,.(policy_name, variable, y_labels, x_labels)]))
hline_dt[variable == 'Y_corn', y_line := baselevel_Y_corn*0.95]
hline_dt[policy_name == 'ratio' & variable == 'Y_corn', y_label := '95% base-level']

#----
# ADD letters outside plot (go down) https://stackoverflow.com/questions/12409960/ggplot2-annotate-outside-of-plot
# ann_text <- plot_dt_long[,.(x = min(policy_val)-5,
#                 value = max(value)), by = .(policy_name, variable, variable_labels)]
# #Sort in the right order
# ann_text <- ann_text[match(c('N_fert', 'L_change', 'Y_corn', 'P', 'G', 'net_balance', 'E','W'), ann_text$variable),]
# ann_text[,lab := c("a)", "b)", "c)", "d)", "e)", "f)", "g)", "h)")]
#----

(p <- ggplot() +
   # geom_line(data = plot_dt_long1, aes(x = policy_val, y =  value, colour = NMS)) +
   # scale_colour_manual(values = c("black", "brown"))+
   geom_line(data = plot_dt_long, aes(x = policy_val, y =  value, color = NMS), size = 1) +
   # scale_linetype_manual(values = c("dashed", "solid"))+
   geom_hline(data = hline_dt, aes(yintercept = y_line), linetype = 'dashed', color = 'grey', size = 1)+
   # geom_text(data = hline_dt, aes(x = 5, y = y_line+50, label =y_label ), hjust = 'left', vjust = 'center') +
   # scale_color_manual(values=c("royalblue2", "tomato3"))+   
   # geom_text(data = ann_text[variable %in% unique(plot_dt_long$variable)], aes(y = value, x = x, label = lab), 
   #           hjust = 0, size = 8) +
   #   coord_cartesian(xlim = c(min(plot_dt_long$policy_val), max(plot_dt_long$policy_val)), # This focuses the x-axis on the range of interest
   #                   clip = 'off') +   # This keeps the labels from disappearing
   facet_free(y_labels~x_labels,
              labeller = label_parsed,
              scales="free",
              switch = 'x') +
   theme_bw()+
   theme(# panel.grid = element_blank(), 
     strip.background.x = element_blank(),
     strip.placement.x = "outside",
     # panel.spacing = unit(1.5, "lines"),
     axis.title.x=element_blank(),
     axis.title.y=element_blank(),
     legend.position = "bottom",
     plot.margin =  unit(c(1,1,1,1), "lines")
   ))

ggsave(plot = p, 
       filename = "./n_policy_box/Data/figures/policies_multiplot.jpg", width = 831/300*3, height = 650/300*3,
       units = 'in')


#--------------------------------------------------------------------------------------








# examine ggplot object: alignment is off
p

# convert to grob object: alignment is unchanged (i.e. still off)
gp <- ggplotGrob(p)
dev.off(); grid::grid.draw(gp)


# change viewport parameters for left axis grobs
for(i in which(grepl("axis-r", gp$layout$name))){
  i=48
  gp$grobs[[i]]$vp$x <- unit(0, "npc")     # originally 1npc
  gp$grobs[[i]]$vp$valid.just <- c(0, 0.5) # originally c(1, 0.5)
}

# re-examine grob object: alignment has been corrected
dev.off() 
grid::grid.draw(gp)



gp = ggplotGrob(p)
grid.ls(grid.force(gp))

# Edit the grob
gp = editGrob(grid.force(gp), gPath("GRID.stripGrob", "GRID.text"), grep = TRUE, global = TRUE,
              just = "left", x = unit(0, "npc"))

# Draw it
grid.newpage()
grid.draw(gp)






plot_dt_long2 <- plot_dt_long[!variable %in% c('N_fert', 'L_change', 'Y_corn')] 

(plot_2 <- ggplot() +
    # geom_line(data = plot_dt_long1, aes(x = policy_val, y =  value, colour = NMS)) +
    # scale_colour_manual(values = c("black", "brown"))+
    geom_line(data = plot_dt_long2, aes(x = policy_val, y =  value, color = NMS)) +
    # scale_linetype_manual(values = c("dashed", "solid"))+
    # geom_hline(data = hline_dt, aes(yintercept = y_line), linetype = 'dashed', color = 'grey', size = 1)+
    # geom_text(data = hline_dt, aes(x = 18, y = y_line, label =y_label ))+
    scale_color_manual(values=c("royalblue2", "tomato3"))+   
    geom_text(data = ann_text[variable %in% unique(plot_dt_long2$variable)], aes(y = value, x = x, label = lab), 
              hjust = 0, size = 8) +
    coord_cartesian(xlim = c(min(plot_dt_long$policy_val), max(plot_dt_long$policy_val)), # This focuses the x-axis on the range of interest
                    clip = 'off') +   # This keeps the labels from disappearing
    facet_wrap(variable_labels~., 
               ncol = 1,
               labeller = label_parsed,
               scales="free",
               strip.position = "left") +
    scale_x_continuous(breaks = seq(5,20,1), labels = seq(5,20,1)) + 
    xlab('N:Corn price ratio')+
    # geom_vline(xintercept = Pn/Pc, linetype = 'dashed', color = 'grey', size = 1)+
    theme_bw()+
    theme(panel.grid = element_blank(), 
          strip.background = element_blank(),
          strip.placement = "outside",
          panel.spacing = unit(1.5, "lines"),
          # axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.margin =  unit(c(2,1,1,1), "lines"))
)

grid.arrange(plot_1, plot_2, nrow = 1)
ggsave(plot = grid.arrange(plot_1, plot_2, nrow = 1), 
       filename = "./n_policy_box/Data/figures/ratio_all_vars.pdf", width = 979/300*3, height = 1042/300*3,
       units = 'in')
#==========================================================================
# LRED CHART
plot_dt <- perfomances_dt4[policy_name == 'nred' & NMS %in% c('static','dynamic')][order(NMS, -policy_val)]

# plot_dt[,L_red := round(1-(L / baselevel_L),2)*100 ]
# plot_dt[,L := round((L / baselevel_L) - 1,2)*100 ]
plot_dt[,policy_val  := (1-policy_val )*100]
ggplot(plot_dt) + geom_line(aes(x = policy_val, y = L_change, color = NMS))

plot_dt_long <- melt(plot_dt, id.vars = c('policy_val', 'NMS'), measure.vars = c('Y_corn', 'L_change', 'N_fert', 
                                                                                 'P', 'G', 'net_balance')) %>% data.table()

plot_dt_long[,variable_labels := factor(variable, levels = c('N_fert', 'L_change', 'Y_corn', 'P', 'G', 'net_balance'),
                                        labels = c(expression("Fertilizer (N kg " * ha^"-1" *yr^"-1"* ")"), 
                                                   expression("L ("*'%'*" change)"),
                                                   expression("Corn Yield (kg N " * ha^"-1" *yr^"-1"* ")"), 
                                                   expression("Farm profits ($ " * ha^"-1" * yr^"-1"* ")"),
                                                   expression("Gov. collections ($ " * ha^"-1" * yr^"-1"* ")"),
                                                   expression("Net balance ($ " * ha^"-1" * yr^"-1"* ")")))]

plot_dt_long1 <- plot_dt_long[variable %in% c('N_fert', 'L_change', 'Y_corn')] 


hline_dt <- data.table(unique(plot_dt_long1[,.(variable, variable_labels)]))
hline_dt[variable == 'Y_corn', y_line := baselevel_Y_corn*0.95]
hline_dt[variable == 'Y_corn', y_label := '95% baselevel']
# hline_dt[variable == 'L_change', y_line  := 0]

#----
# ADD letters outside plot (go down) https://stackoverflow.com/questions/12409960/ggplot2-annotate-outside-of-plot
ann_text <- plot_dt_long[,.(x = min(policy_val)-5,
                            value = max(value)), by = .(variable, variable_labels)]
#Sort in the right order
ann_text <- ann_text[match(c('N_fert', 'L_change', 'Y_corn', 'P', 'net_balance','W'), ann_text$variable),]
ann_text[,lab := c("a)", "b)", "c)", "d)", "e)", "f)")]
ann_text
#----

(plot_1 <- ggplot() +
   # geom_line(data = plot_dt_long1, aes(x = policy_val, y =  value, colour = NMS)) +
   # scale_colour_manual(values = c("black", "brown"))+
   geom_line(data = plot_dt_long1, aes(x = policy_val, y =  value, color = NMS)) +
   # scale_linetype_manual(values = c("dashed", "solid"))+
   geom_hline(data = hline_dt, aes(yintercept = y_line), linetype = 'dashed', color = 'grey', size = 1)+
   geom_text(data = hline_dt, aes(x = 10, y = y_line, label =y_label ))+
   scale_color_manual(values=c("royalblue2", "tomato3"))+   
   geom_text(data = ann_text[variable %in% unique(plot_dt_long1$variable)], aes(y = value, x = x, label = lab),              
             hjust = 0, size = 8) +     
   coord_cartesian(xlim = c(min(plot_dt_long$policy_val), max(plot_dt_long$policy_val)), # This focuses the x-axis on the range of interest                     
                   clip = 'off') +   # This keeps the labels from disappearing   
   facet_wrap(variable_labels~., 
              ncol = 1,
              labeller = label_parsed,
              scales="free",
              strip.position = "left") +
   # scale_x_continuous(breaks = sort(unique(plot_dt$policy_val)), labels = sort(unique(plot_dt$policy_val))) + 
   xlab('L reduction target (%)')+
   theme_bw()+
   theme(panel.grid = element_blank(), 
         strip.background = element_blank(),
         strip.placement = "outside",
         panel.spacing = unit(1.5, "lines"),
         # axis.title.x=element_blank(),
         axis.title.y=element_blank(),
         legend.position = "none",
         plot.margin =  unit(c(2,1,1,1), "lines"))
)

plot_dt_long2 <- plot_dt_long[!variable %in% c('N_fert', 'L_change', 'Y_corn', 'G')] 
(plot_2 <- ggplot() +
    # geom_line(data = plot_dt_long1, aes(x = policy_val, y =  value, colour = NMS)) +
    # scale_colour_manual(values = c("black", "brown"))+
    geom_line(data = plot_dt_long2, aes(x = policy_val, y =  value, color = NMS)) +
    # scale_linetype_manual(values = c("dashed", "solid"))+
    # geom_hline(data = hline_dt, aes(yintercept = y_line), linetype = 'dashed', color = 'grey', size = 1)+
    # geom_text(data = hline_dt, aes(x = 18, y = y_line, label =y_label ))+
    scale_color_manual(values=c("royalblue2", "tomato3"))+   
    geom_text(data = ann_text[variable %in% unique(plot_dt_long2$variable)], aes(y = value, x = x, label = lab),              
              hjust = 0, size = 8) +     
    coord_cartesian(xlim = c(min(plot_dt_long$policy_val), max(plot_dt_long$policy_val)), # This focuses the x-axis on the range of interest                     
                    clip = 'off') +   # This keeps the labels from disappearing   
    facet_wrap(variable_labels~., 
               ncol = 1,
               labeller = label_parsed,
               scales="free",
               strip.position = "left") +
    # scale_x_continuous(breaks = sort(unique(plot_dt$policy_val)), labels = sort(unique(plot_dt$policy_val))) + 
    xlab('L reduction target (%)')+
    theme_bw()+
    theme(panel.grid = element_blank(), 
          strip.background = element_blank(),
          strip.placement = "outside",
          panel.spacing = unit(1.5, "lines"),
          # axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.margin =  unit(c(2,1,1,1), "lines"))
)
grid.arrange(plot_1, plot_2, nrow = 1)

ggsave(plot = grid.arrange(plot_1, plot_2, nrow = 1), 
       filename = "./n_policy_box/Data/figures/Lred_all_vars.jpg", width = 979/300*3, height = 1042/300*3,
       units = 'in')

#==========================================================================
# FEE CHART

plot_dt <- perfomances_dt4[policy_name == 'fee' & NMS %in% c('static','dynamic') ] 

plot_dt[,L := round((L / baselevel_L) - 1,2)*100 ]

ggplot(plot_dt) + geom_line(aes(x = policy_val, y = L, color = NMS))

plot_dt_long <- melt(plot_dt, id.vars = c('policy_val', 'NMS'), measure.vars = c('Y_corn', 'L_change', 'N_fert', 
                                                                                 'P', 'G', 'net_balance'))

plot_dt_long[,variable_labels := factor(variable, levels = c('N_fert', 'L_change', 'Y_corn', 'P', 'G', 'net_balance'),
                                        labels = c(expression("Fertilizer (N kg " * ha^"-1" *yr^"-1"* ")"), 
                                                   expression("L ("*'%'*" change)"),
                                                   expression("Corn Yield (kg N " * ha^"-1" *yr^"-1"* ")"), 
                                                   expression("Farm profits ($ " * ha^"-1" * yr^"-1"* ")"),
                                                   expression("Gov. collections ($ " * ha^"-1" * yr^"-1"* ")"),
                                                   expression("Net balance ($ " * ha^"-1" * yr^"-1"* ")")))]


plot_dt_long1 <- plot_dt_long[variable %in% c('N_fert', 'L_change', 'Y_corn')] 


hline_dt <- data.table(unique(plot_dt_long1[,.(variable, variable_labels)]))
hline_dt[variable == 'Y_corn', y_line := baselevel_Y_corn*0.95]
hline_dt[variable == 'Y_corn', y_label := '95% baselevel']
# hline_dt[variable == 'L_change', y_line  := 0]

#----
# ADD letters outside plot (go down) https://stackoverflow.com/questions/12409960/ggplot2-annotate-outside-of-plot
ann_text <- plot_dt_long[,.(x = min(policy_val)-4,
                            value = max(value)), by = .(variable, variable_labels)]
#Sort in the right order
ann_text <- ann_text[match(c('N_fert', 'L_change', 'Y_corn', 'P', 'G', 'E','W'), ann_text$variable),]
ann_text[,lab := c("a)", "b)", "c)", "d)", "e)", "f)", "g)")]
#----

(plot_1 <- ggplot() +
   # geom_line(data = plot_dt_long1, aes(x = policy_val, y =  value, colour = NMS)) +
   # scale_colour_manual(values = c("black", "brown"))+
   geom_line(data = plot_dt_long1, aes(x = policy_val, y =  value, color = NMS)) +
   # scale_linetype_manual(values = c("dashed", "solid"))+
   geom_hline(data = hline_dt, aes(yintercept = y_line), linetype = 'dashed', color = 'grey', size = 1)+
   geom_text(data = hline_dt, aes(x = 16, y = y_line, label =y_label ))+
   scale_color_manual(values=c("royalblue2", "tomato3"))+   
   geom_text(data = ann_text[variable %in% unique(plot_dt_long1$variable)], aes(y = value, x = x, label = lab),              
             hjust = 0, size = 8) +     
   coord_cartesian(xlim = c(min(unique(plot_dt$policy_val)), max(unique(plot_dt$policy_val))), # This focuses the x-axis on the range of interest                     
                   clip = 'off') +   # This keeps the labels from disappearing
   facet_wrap(variable_labels~., 
              ncol = 1,
              labeller = label_parsed,
              scales="free",
              strip.position = "left") +
   # scale_x_continuous(breaks = sort(unique(plot_dt$policy_val)), labels = sort(unique(plot_dt$policy_val))) + 
   xlab('Fee on L ($/kg)')+
   theme_bw()+
   theme(panel.grid = element_blank(), 
         strip.background = element_blank(),
         strip.placement = "outside",
         panel.spacing = unit(1.5, "lines"),
         # axis.title.x=element_blank(),
         axis.title.y=element_blank(),
         legend.position = "none",
         plot.margin =  unit(c(2,1,1,1), "lines"))
)

plot_dt_long2 <- plot_dt_long[!variable %in% c('N_fert', 'L_change', 'Y_corn')] 

(plot_2 <- ggplot() +
    # geom_line(data = plot_dt_long1, aes(x = policy_val, y =  value, colour = NMS)) +
    # scale_colour_manual(values = c("black", "brown"))+
    geom_line(data = plot_dt_long2, aes(x = policy_val, y =  value, color = NMS)) +
    # scale_linetype_manual(values = c("dashed", "solid"))+
    # geom_hline(data = hline_dt, aes(yintercept = y_line), linetype = 'dashed', color = 'grey', size = 1)+
    # geom_text(data = hline_dt, aes(x = 18, y = y_line, label =y_label ))+
    scale_color_manual(values=c("royalblue2", "tomato3"))+   
    geom_text(data = ann_text[variable %in% unique(plot_dt_long2$variable)], aes(y = value, x = x, label = lab),              
              hjust = 0, size = 8) +     
    coord_cartesian(xlim = c(min(unique(plot_dt$policy_val)), max(unique(plot_dt$policy_val))), # This focuses the x-axis on the range of interest                     
                    clip = 'off') +   # This keeps the labels from disappearing
    facet_wrap(variable_labels~., 
               ncol = 1,
               labeller = label_parsed,
               scales="free",
               strip.position = "left") +
    # scale_x_continuous(breaks = sort(unique(plot_dt$policy_val)), labels = sort(unique(plot_dt$policy_val))) + 
    xlab('Fee on L ($/kg)')+
    theme_bw()+
    theme(panel.grid = element_blank(), 
          strip.background = element_blank(),
          strip.placement = "outside",
          panel.spacing = unit(1.5, "lines"),
          # axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.margin =  unit(c(2,1,1,1), "lines"))
)

grid.arrange(plot_1, plot_2, nrow = 1)

ggsave(plot = grid.arrange(plot_1, plot_2, nrow = 1), 
       filename = "./n_policy_box/Data/figures/fee_all_vars.jpg", width = 979/300*3, height = 1042/300*3,
       units = 'in')


#==========================================================================
# W MAXIMIZATION
w_dt <- perfomances_dt4[policy_name != 'subs',.SD[W == max(W)], by = .(policy_name, NMS)][order(-W)] #peak in W
w_dt[,policy_val := as.numeric(policy_val )]
w_dt[policy_name == 'nred', policy_val := (1-as.numeric(policy_val ))*100]
w_dt[policy_name == 'nred', policy_name := 'target']
baselevel_dt <- perfomances_dt4[policy  == 'ratio_6' & NMS == 1]
baselevel_dt[,policy_name := 'base-level']
baselevel_dt[,policy_val := '-']
baselevel_dt

w_dt <- rbind(w_dt ,baselevel_dt) %>% .[order(-W)]

latex_table_dt <- w_dt[,c('policy_name', 'policy_val','NMS',  'N_fert', 'Y_corn', 'L_change', 'P', 'G',  'E', 'W')]

cols_1 <- c('N_fert', 'Y_corn', 'L_change', 'P', 'G',  'E', 'W')
latex_table_dt[,(cols_1) := round(.SD,1), .SDcols=cols_1]
latex_table_dt[,Y_corn := round(Y_corn, 0)]

setnames(latex_table_dt, c('policy_name', 'policy_val', 'N_fert', 'Y_corn', 'L_change', 'P', 'G',  'E', 'W'),
         c('policy', 'level', 'N rate (kg/ha)', 'Y_corn (kg/ha)', 'L change (%)', 'Profits ($/ha)', 'G ($/ha)',  'E ($/ha)', 'W ($/ha)'))

latex_table_xt <- xtable(latex_table_dt, type = "latex", auto = TRUE, label = 'tab:w_maximization', 
                         caption = 'Indicators for the level that maximized W for each policy and NMS combination, ordered by their W. 
                         The base-level system is also shown as a benchmark')

#make L in %. Change N red name to L reduction target

library('xtable')
print(latex_table_xt, file = "./n_policy_box/Data/figures/w_maximization.tex", include.rownames=FALSE)





#---------------------------------------------------------------------------
# COMPARE A SAMPLE OF THE DIFFERENT MODELS THAT WOULD GET US TO THE 15% 
# REDUCTION TARGET FROM CURRENT SITUATION (-15% by 2025, -45% by 2035)

unique(perfomances_dt4$policy)

policies_f <- c("fee_0", "ratio_9", "fee_8", 'nred_0.85', 'nred_0.7')
NMSs_f <- c('static', 'dynamic')

table_dt <- perfomances_dt4[policy %in% policies_f & NMS %in% NMSs_f]
table_dt[policy == 'fee_0' & NMS == 'static', order := 1]
table_dt[policy == 'fee_0' & NMS == 'dynamic', order := 2] #science
table_dt[policy == 'yr_0.9' & NMS == 'static', order := 3] #ecological model
table_dt[policy == 'ratio_9' & NMS == 'static', order := 4] #tax
table_dt[policy == 'fee_8' & NMS == 'static', order := 5] #fee
table_dt[policy == 'nred_0.85' & NMS == 'dynamic', order := 6] #ecological model + science
table_dt[policy == 'ratio_9' & NMS == 'dynamic', order := 7] #tax+science
table_dt[policy == 'fee_8' & NMS == 'dynamic', order := 8] #fee+science
table_dt[policy == 'nred_0.7' & NMS == 'dynamic', order := 9] #ecological model strong  + science
table_dt <- table_dt[order(order)]

#---------------------------------------------------------------------------
# BEST OPTION 1: FARMERS FOCUSED. Given a yield restriction of 90%, what is the maximum we could reduce N leaching hurting the less the farmers 
# and not caring about the externality cost (we are already sending less N, that's it my friends)

table_dt <- perfomances_dt4[ NMS %in% NMSs_f]
baselevel_Lleach <- table_dt[policy == 'fee_0' & NMS == 1, L] 
baselevel_Y_corn <- table_dt[policy == 'fee_0' & NMS == 1, Y_corn ]
baselevel_nfert <- table_dt[policy == 'fee_0' & NMS == 1, N_fert ]

table_dt[,Y_corn_red := round((Y_corn / baselevel_Y_corn),2)]

table_dt <- table_dt[Y_corn_red > 0.9] #remove those that decrease yield too much

table_dt[,abatement := baselevel_n - L]
table_dt[,abat_prop := round((abatement)/ baselevel_n,2)]
target_abat_prop <- table_dt[order(-abat_prop)] %>% .[1:10, abat_prop] %>% mean()

table_dt <- table_dt[abat_prop > (target_abat_prop - 0.01)]
table_dt[,soc_benefits := P + G] #only farmers
table_dt <- table_dt[order(-soc_benefits)]
table_dt

#---------------------------------------------------------------------------
# BEST OPTION 2: considering a cost of the externality, what NMS would maximize the welfare of the society
NMSs_f <- c('static','dynamic','4')
table_dt <- perfomances_dt4[ NMS %in% NMSs_f]
baselevel_n <- table_dt[policy == 'fee_0' & NMS == 1, L] 
table_dt[,abatement := baselevel_n - L]
table_dt[,abat_prop := round((abatement)/ baselevel_n,2)]
baselevel_Y_corn <- table_dt[policy == 'fee_0' & NMS == 1, Y_corn ]
table_dt[,Y_corn_red := round((Y_corn / baselevel_Y_corn),2)]

table_dt[,soc_benefits := P + G]
target_n <- baselevel_n * (1-0.45) #to accomplish the 45% reduction goal
table_dt[,externatility := ifelse((L - target_n) > 0, (L -target_n)*Pe_total,0)] #externalities are pay for each kg above the 45% target
table_dt[,soc_welfare := soc_benefits - externatility ]
table_dt <- table_dt[order(-soc_welfare)][1:40]
table_dt

baselevel_benefits <- table_dt[policy == 'fee_0' & NMS == 1, soc_benefits] 
table_dt[,abat_cost := (soc_benefits - baselevel_benefits)/abatement]


# DT[order(match(y, as.numeric(k)))]

10800000 * 0.404686 * 42 / 1e6 #millons to expend in RS and recovering
+1-(33/45)

#---------------------------------------------------------------------------
# Get RMSE
# install.packages('mlr')
library(mlr) 

rmse_dt <- perfomances_dt[stringr::str_detect(string = perfomances_dt$policy, pattern = 'ratio'),
                          .(Y_corn =  mean(Y_corn),
                            L = mean(L),
                            N_fert = mean(N_fert),
                            N_fert_min = min(N_fert),
                            N_fert_max = max(N_fert),
                            P = mean(P),
                            # cor = cor(N_fert_12, N_fert),
                            RMSE = mlr::measureRMSE(truth = N_fert_12, response = N_fert),
                            overpred = sum(overpred)/.N,
                            subpred = sum(subpred)/.N,
                            angulo = sum(angulo)/.N), by = .( NMS, policy)][order(-P)]

rmse_dt[,policy_val := as.numeric(str_extract(policy,pattern = '[0-9.]+'))]
rmse_dt[,policy_name := lapply(policy, function(x) str_split(x, pattern = '_')[[1]][1])]
rmse_dt <- rmse_dt[ NMS %in% c('static','dynamic','3','4','5')]

plot_1 <- ggplot(rmse_dt)+
  geom_line(aes(x = policy_val, y =  RMSE, colour = NMS)) +
  scale_x_continuous(breaks = seq(1,20,1), labels = seq(1,20,1)) + 
  xlab('N:Corn price ratio')+
  geom_vline(xintercept = Pn/Pc, linetype = 'dashed', color = 'grey', size = 1)+
  # ylab('Yield (kg/ha)')+
  theme_bw()+
  theme(panel.grid = element_blank())
plot_1

ggsave(plot = plot_1, filename = "./n_policy_box/Data/figures/rmse.jpg", width = 5, height = 5,
       units = 'in')

plot_1 <- ggplot(rmse_dt)+
  geom_line(aes(x = policy_val, y =  overpred, colour = NMS))+
  scale_x_continuous(breaks = seq(1,20,1), labels = seq(1,20,1)) + 
  xlab('N:Corn price ratio')+
  geom_vline(xintercept = Pn/Pc, linetype = 'dashed', color = 'grey', size = 1)+
  # ylab('Yield (kg/ha)')+
  theme_bw()+
  theme(panel.grid = element_blank())

plot_2 <- ggplot(rmse_dt)+
  geom_line(aes(x = policy_val, y =  subpred, colour = NMS))+
  scale_x_continuous(breaks = seq(1,20,1), labels = seq(1,20,1)) + 
  xlab('N:Corn price ratio')+
  geom_vline(xintercept = Pn/Pc, linetype = 'dashed', color = 'grey', size = 1)+
  # ylab('Yield (kg/ha)')+
  theme_bw()+
  theme(panel.grid = element_blank())

grid.arrange(plot_1 , plot_2)

#---------------------------------------------------------------------------
# BOXPLOT OF EXPOST RATES
boxplot_dt <- perfomances_dt[stringr::str_detect(string = perfomances_dt$policy, pattern = 'ratio') & NMS ==12]
boxplot_dt[,policy_val := as.numeric(str_extract(policy,pattern = '[0-9.]+'))]
boxplot_dt[,policy_name := lapply(policy, function(x) str_split(x, pattern = '_')[[1]][1])]
class(boxplot_dt$policy_val)
boxplot_dt[,policy_val := factor(policy_val, levels = 1:15)]

plot_1 <- ggplot(boxplot_dt)+
  geom_boxplot(aes(x = policy_val, y =  N_fert)) +
  #scale_x_continuous(breaks = seq(1,15,1), labels = seq(1,15,1)) + 
  xlab('N:Corn price ratio')+
  # ylab('Yield (kg/ha)')+
  theme_bw()+
  theme(panel.grid = element_blank())
plot_1

ggsave(plot = plot_1, filename = "./n_policy_box/Data/figures/boxplot_NMS12.jpg", width = 5, height = 5,
       units = 'in')

plot_1 <- ggplot(boxplot_dt[policy_val %in% c(2,8,15)])+
  geom_density(aes(x =  N_fert, colour = policy_val)) +
  #scale_x_continuous(breaks = seq(1,15,1), labels = seq(1,15,1)) + 
  xlab('N:Corn price ratio')+
  # ylab('Yield (kg/ha)')+
  theme_bw()+
  theme(panel.grid = element_blank())

plot_1

ggsave(plot = plot_1, filename = "./n_policy_box/Data/figures/boxplot_NMS12.jpg", width = 5, height = 5,
       units = 'in')


#---------------------------------------------------------------------------
#Value of information 
# MAKE A MAP OF ECONOMIC VALUE OF INFORMATION SS
profits_dt <- perfomances_dt4[policy_name == 'ratio'  & NMS %in% c(1,2,4,5)]
ggplot(profits_dt)+
  geom_line(aes(x = policy_val, y =  P, colour = NMS))

value_long_dt <- data.table()
value_dt <- perfomances_dt4[policy_name == 'ratio'  & NMS %in% c(1,2)]
value_dt[NMS == 1, P := -P]
value_dt <- value_dt[, .(P = sum(P)), by = .(policy_val)]
value_dt[, variable := 'val_info']
value_long_dt <- rbind(value_long_dt, value_dt)

value_dt <- perfomances_dt4[policy_name == 'ratio'  & NMS %in% c(2,4)]
value_dt[NMS == 2, P := -P]
value_dt <- value_dt[, .(P = sum(P)), by = .(policy_val)]
value_dt[, variable := 'val_ss']
value_long_dt <- rbind(value_long_dt, value_dt)

value_dt <- perfomances_dt4[policy_name == 'ratio'  & NMS %in% c(2,3)]
value_dt[NMS == 2, P := -P]
value_dt <- value_dt[, .(P = sum(P)), by = .(policy_val)]
value_dt[, variable := 'val_tech']
value_long_dt <- rbind(value_long_dt, value_dt)

value_dt <- perfomances_dt4[policy_name == 'ratio'  & NMS %in% c(4,5)]
value_dt[NMS == 4, P := -P]
value_dt <- value_dt[, .(P = sum(P)), by = .(policy_val)]
value_dt[, variable := 'val_tech_ss']
value_long_dt <- rbind(value_long_dt, value_dt)

plot_1 <- ggplot(value_long_dt)+
  geom_line(aes(x = policy_val, y =  P, colour = variable), show.legend = F) +
  # scale_y_continuous(breaks = seq(1,10,1), labels = seq(1,10,1)) + 
  facet_grid(variable~., scales = "free" ) +
  scale_x_continuous(breaks = seq(1,20,1), labels = seq(1,20,1)) + 
  xlab('N:Corn price ratio')+
  geom_vline(xintercept = Pn/Pc, linetype = 'dashed', color = 'grey', size = 1)+
  ylab('Value ($/ha)')+
  theme_bw()+
  theme(panel.grid = element_blank())
plot_1

ggsave(plot = plot_1, filename = "./n_policy_box/Data/figures/valueISST_by_ratio.jpg", width = 10, height = 10,
       units = 'in')

#==========================================================================
# nred CHART #===============================================================
#==========================================================================
unique(perfomances_dt4$policy_name)
plot_dt <- perfomances_dt4[policy_name == 'nred' & NMS %in% c('1_ok','dynamic','3','4','5')  ] 

plot_dt[,soc_benefits := P + G]
target_n <- baselevel_n * (1-0.45) #to accomplish the 45% reduction goal
plot_dt[,externatility := ifelse((L - target_n) > 0, (L -target_n)*Pe_total,0)]
plot_dt[,soc_welfare := soc_benefits - externatility ]
plot_dt[order(-soc_welfare)][1:40]


plot_dt_long <- melt(plot_dt, id.vars = c('policy_val', 'NMS'), measure.vars = c('Y_corn', 'L', 'N_fert', 
                                                                                 'P', 'externatility','soc_welfare'))
plot_dt_long[variable == 'N_fert', plot_name := 'a) N Rate']
plot_dt_long[variable == 'L', plot_name := 'b) N Leaching']
plot_dt_long[variable == 'Y_corn', plot_name := 'c) Yield']
plot_dt_long[variable == 'P', plot_name := 'd) Profits']
# plot_dt_long[variable == 'G', plot_name := 'e) Tax revenue']
plot_dt_long[variable == 'externatility', plot_name := 'f) externatility']
plot_dt_long[variable == 'soc_welfare', plot_name := 'g) soc_welfare']
plot_dt_long[order(variable)]

plot_dt_long1 <- plot_dt_long[variable %in% c('N_fert', 'L', 'Y_corn')] 

hline_dt <- data.table(plot_name = unique(plot_dt_long1$plot_name))
hline_dt[plot_name == 'c) Yield', y_line := baselevel_Y_corn*0.95]
hline_dt[plot_name == 'c) Yield', y_label := '95% baselevel']

plot_1 <- ggplot()+
  geom_line(data = plot_dt_long1, aes(x = policy_val, y =  value, colour = NMS)) +
  geom_hline(data = hline_dt, aes(yintercept = y_line), linetype = 'dashed', color = 'grey', size = 1)+
  geom_text(data = hline_dt, aes(x = .95, y = y_line, label =y_label ))+
  facet_grid(plot_name~., scales = "free") +
  # scale_x_continuous(breaks = seq(1,20,1), labels = seq(1,20,1)) + 
  xlab('N reduction level')+
  geom_vline(xintercept = 1, linetype = 'dashed', color = 'grey', size = 1)+
  # ylab('Yield (kg/ha)')+
  theme_bw()+
  theme(panel.grid = element_blank())

plot_1

ggsave(plot = plot_1, filename = "./n_policy_box/Data/figures/nred_all_vars_part1.jpg", width = 10, height = 10,
       units = 'in')

plot_dt_long2 <- plot_dt_long[!variable %in% c('N_fert', 'L', 'Y_corn')] 

plot_1 <- ggplot(plot_dt_long2)+
  geom_line(aes(x = policy_val, y =  value, colour = NMS)) +
  facet_grid(plot_name~., scales = "free") +
  # scale_x_continuous(breaks = seq(1,20,1), labels = seq(1,20,1)) + 
  xlab('N reduction level')+
  geom_vline(xintercept = 1, linetype = 'dashed', color = 'grey', size = 1)+
  # ylab('Yield (kg/ha)')+
  theme_bw()+
  theme(panel.grid = element_blank())

plot_1

ggsave(plot = plot_1, filename = "./n_policy_box/Data/figures/nred_all_vars_part2.jpg", 
       width = 10, height = 10,
       units = 'in')

#==========================================================================
# FEE CHART

plot_dt <- perfomances_dt4[policy_name == 'fee' & NMS == 'static'] 

plot_dt1 <- melt(plot_dt, id.vars = 'policy_val', measure.vars = c('Y_corn', 'L', 'N_fert', 'P', 'G'))
plot_dt2 <- melt(plot_dt[policy_val == 6], id.vars = 'policy_val', measure.vars = c('Y_corn', 'L', 'N_fert', 'P', 'G'))

plot_dt3 <- merge(plot_dt1, plot_dt2[,.(variable, value_max = value)], by = c('variable'))
plot_dt3[, value_rel := value/value_max]

ggplot(plot_dt3) +
  geom_line(aes(x = policy_val, y =  value_rel, colour = variable))

#---------------------------------------------------------------------------


# cols <- c('cor', 'overpred', 'RMSE_MAE')
# rmse_dt[, (cols) := lapply(.SD, function(x) round(x, 2)), .SDcols = cols]
# 
# cols <- c( 'RMSE', 'MAE', 'RMSE_MAE')
# rmse_dt[, (cols) := lapply(.SD, function(x) round(x,1)), .SDcols = cols]

# rmse_dt[,NMS := factor(NMS, levels= c('static', 'dynamic', '3','4', '5', '6', '7', '8', '9', '10', '11', '12'))]

# (p1 <- ggplot(rmse_dt, aes(x = NMS, y = RMSE))+
#     geom_bar(stat="identity") )
# rmse_dt[,NMS := as.integer(NMS)]
# rmse_dt <- rmse_dt[order(NMS)]

# perfomances_dt4[,NMS := as.integer(NMS)]
latex_table_dt <- perfomances_dt4[,-'corn_avg_ha']

cols_1 <- c('Y_corn', 'L', 'leach_ext', 'N_fert', 'P')
latex_table_dt[,(cols_1) := round(.SD,1), .SDcols=cols_1]
latex_table_dt[,Y_corn := round(Y_corn, 0)]


setnames(latex_table_dt, c('Y_corn', 'L', 'leach_ext', 'N_fert', 'P'),
         c('Yield', 'N leaching', 'N leach ext', 'N rate', 'Profits'))

library('xtable')
print(xtable(latex_table_dt, type = "latex", auto = TRUE, label = 'tab:state_output', 
             caption = 'Results for the State of Illinois. 
             Aggregated considering the area planted to corn for each cell, using eq. \ref{eq_I_state}'), 
      file = "./n_policy_box/Data/figures/state_output.tex", include.rownames=FALSE)



?print.xtable
?xtable

latex_table_dt[NMS==2, ]$Profits - latex_table_dt[NMS==1, ]$Profits 
latex_table_dt[NMS==4, ]$Profits - latex_table_dt[NMS==1, ]$Profits #Value of infomation
latex_table_dt[NMS==5, ]$Profits - latex_table_dt[NMS==4, ]$Profits #Ex-ante Value of T
latex_table_dt[NMS==12, ]$Profits - latex_table_dt[NMS==11, ]$Profits #Ex-ost Value of T


latex_table_dt[NMS==2, ]$'N leaching' - latex_table_dt[NMS==1, ]$'N leaching'
latex_table_dt[NMS==4, ]$'N leaching' - latex_table_dt[NMS==1, ]$'N leaching' #EB of infomation
latex_table_dt[NMS==5, ]$'N leaching' - latex_table_dt[NMS==4, ]$'N leaching' #EB of T

-(latex_table_dt[NMS==4, 'N leaching'] - latex_table_dt[NMS==1, 'N leaching'])/latex_table_dt[NMS==1, 'N leaching'] #% Decrease in N leaching
-(latex_table_dt[NMS==4, 'N leach ext'] - latex_table_dt[NMS==1, 'N leach ext'])/latex_table_dt[NMS==1, 'N leach ext'] #% Decrease in N leaching extreme
-(latex_table_dt[NMS==4, 'N rate'] - latex_table_dt[NMS==1, 'N rate'])/latex_table_dt[NMS==1, 'N rate'] #% Decrease in N use

#=====================================================================================================================
# MRTN vs Minimum NMS
reg_NMS_stuff <- readRDS( "./n_policy_box/Data/files_rds/reg_NMS_stuff.rds")
NMS_minimum_regional <- reg_NMS_stuff$NMS_minimum_regional
rm(reg_NMS_stuff)

mrtn_dt <- data.table(region = c(3,3,2,2,1,1), 
                      prev_crop = c(0,1,0,1,0,1),
                      MRTN_Rate_lbN_ac = c(161, 200, 175, 193,187, 192))
mrtn_dt <- mrtn_dt[prev_crop == 0]
mrtn_dt[,MRTN_rate := round(MRTN_Rate_lbN_ac * 1.12,0)] #1 pound per acre = 1.12 kilograms per hectare

NMS_minimum_regional2 <- merge(NMS_minimum_regional, mrtn_dt[,-c('prev_crop','MRTN_Rate_lbN_ac')], by = c('region'))
# NMS_minimum_regional2[,prev_crop := ifelse(prev_crop == 0, 'Soybean', 'Corn')]
NMS_minimum_regional2[,region := ifelse(region == 1, '1_South', ifelse(region == 2, '2_Central', '3_North'))]
setnames(NMS_minimum_regional2, 'eonr_pred', 'NMS1_rate')
NMS_minimum_regional2[order(-region)]

print.xtable(xtable(NMS_minimum_regional2, type = "latex", auto = TRUE, 
                    label = 'tab:NMS1', 
                    caption = 'NMS 1 predictions paired with MRTN recommendations for the same region'),
             file = "./n_policy_box/Data/figures/NMS1.tex", include.rownames=FALSE)

#=====================================================================================================================
#-----------------------------------------VALUE OF INFORMATION--------------------------------------------------------
#=====================================================================================================================
# MAKE A MAP OF TOTAL LEACHING WITH NMS 1 AND REDUCTION WITH NMS 4
value_dt <- perfomances_dt4[NMS %in% c(1, 4), .(id_10, NMS, L, corn_avg_ha, leach_ext)]

value_dt[, L_cell := L * corn_avg_ha]
value_dt[, leach_ext_cell := leach_ext * corn_avg_ha]

value_dt <- dcast(value_dt, id_10 ~ NMS, value.var = c('L_cell', 'leach_ext_cell', 'L'))
value_dt[,L_4 := NULL]

# setnames(value_dt, c('static', '4'), c('L_m1', 'L_m2'))
#make one negative
value_dt[, eb_cell := L_cell_4-L_cell_1] #Enviromental Benefit


value_sf <- merge(grid10_tiles_sf7, value_dt[,.(id_10, L_cell_1,eb_cell, leach_ext_cell_1, L_1)], by = 'id_10', all.x = T)


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



tm_shape(value_sf) + tm_polygons(c('L_1')) #Show me: leaching by ha


breaks_n <- c(50000,100000,200000,300000,400000)

(p2 <- tm_shape(value_sf) + tm_polygons(c('L_cell_1'), 
                                        breaks = breaks_n, 
                                        title = c("N Leaching (kg/cell)"),
                                        style ="cont", 
                                        colorNA = 'white',
                                        palette = "Greys")+
    tm_layout(panel.labels = 'b)',
              legend.text.size = 0.7,
              main.title.size = 1.2,
              legend.position = c('left', 'bottom'))) #Leaching with MRTN (baseline_characterization_map)
#---------------------------------------------------------------------------------------------------
# MAKE A MAP OF RMSE (In what areas are the NMSs more off?) -----
rmse_map_dt <- perfomances_dt[NMS %in% c(1,4) ,.(RMSE = mlr::measureRMSE(truth = N_fert_12, response = N_fert),
                                                 MAE = mlr::measureMAE(truth = N_fert_12, response = N_fert),
                                                 subpred = sum(subpred)/.N,
                                                 overpred = sum(overpred)/.N), by = .(id_10, NMS)]

rmse_map_dt2 <- dcast(rmse_map_dt, id_10 ~ NMS, value.var = c('RMSE', 'MAE', 'subpred', 'overpred'))


rmse_map_sf <- merge(grid10_tiles_sf7, rmse_map_dt2, by = 'id_10', all.x = T)

tm_shape(rmse_map_sf) + tm_polygons(c('RMSE_1', 'RMSE_4', 'overpred_4', 'subpred_4'))

#---------------------------------------------------------------------------------------------------
# MAKE A MAP OF EONR for NMS 1, 4, 12 -----
rates_map_dt <-  perfomances_dt3[NMS %in% c(1,4, 12)] 

rates_map_dt2 <- dcast(rates_map_dt, id_10 ~ NMS, value.var = c('L','N_fert', 'P', 'Y_corn'))

rates_map_sf <- merge(grid10_tiles_sf7, rates_map_dt2, by = 'id_10', all.x = T)

rates_map_sf <- merge(grid10_tiles_sf7, rates_map_dt[,.(id_10, NMS, Y_corn, L, N_fert, P)], by = 'id_10', all = T)

empty_cells_sf <- rates_map_sf[is.na(rates_map_sf$NMS),]
rates_map_sf2  <- rates_map_sf[!is.na(rates_map_sf$NMS),]


for(NMS_n in c(1,4,12)){
  rates_map_sf2 <- rbind(rates_map_sf2, empty_cells_sf %>% mutate(NMS = NMS_n))
  
}

rates_map_sf3 <- melt(rates_map_sf2, id.vars = c("id_10", "geometry", 'NMS'), measure.vars = c("Y_corn", "L", "N_fert", "P"))
rates_map_sf3 <- st_sf(rates_map_sf3)

nrow(grid10_tiles_sf7)*3

rates_map_sf3$NMS <- factor(rates_map_sf3$NMS, levels = c(1,4,12))

tm_shape(rates_map_sf3) + tm_polygons('value')+
  tm_facets(c("NMS", "variable"), ncol = 3, free.scales= T, as.layers = T)

(p1 <- tm_shape(rates_map_sf3[rates_map_sf3$variable == 'Y_corn',]) + 
    tm_polygons('value',  
                title = c("Y_corn (kg/ha)"),
                palette = "Greys", 
                colorNA = 'white')+
    tm_facets(c("NMS"), free.scales = F, as.layers = T) +
    tm_layout(legend.outside = F))

(p2 <- tm_shape(rates_map_sf3[rates_map_sf3$variable == 'L',]) + 
    tm_polygons('value',  
                n= 6,
                title = c("N Leaching (kg/ha)"),
                palette = "Greys", 
                colorNA = 'white')+
    tm_facets(c("NMS"), free.scales = F, as.layers = T) +
    tm_layout(legend.outside = F, 
              panel.label.height = 0
    ))


rates_map_sf4 <- rates_map_sf3
rates_map_sf4[  rates_map_sf4$variable == 'N_fert' & rates_map_sf4$NMS ==  1 &   rates_map_sf4$value == 180 & 
                  !(is.na(rates_map_sf4$value)),]$value <- 0

rates_map_sf4[  rates_map_sf4$variable == 'N_fert' & rates_map_sf4$NMS ==  1 & !(is.na(rates_map_sf4$value)),]$value %>% table()

(p3 <- tm_shape(rates_map_sf3[rates_map_sf3$variable == 'N_fert',]) + 
    tm_polygons('value',
                # n=10,
                palette = "Greys", 
                title = c("N Fert (kg/ha)"),
                colorNA = 'white') +
    tm_facets(c("NMS"), free.scales = T) +
    tm_layout(legend.outside = F, 
              panel.label.height = 0,
              legend.position = c('left', 'bottom')
    ))

(p4 <- tm_shape(rates_map_sf3[rates_map_sf3$variable == 'P',]) + 
    tm_polygons('value',  
                #n = 10,
                title = c("P ($/ha)"),
                palette = "Greys", 
                colorNA = 'white')+
    tm_facets(c("NMS"), free.scales = F, as.layers = T) +
    tm_layout(legend.outside = F, 
              panel.label.height = 0
    ))


tmap_save(tmap_arrange(p1,p2,p3,p4, ncol = 1), "./n_policy_box/Data/figures/appendix1_map.jpg", 
          width = 10, height = 15,
          units = 'in')
#---------------------------------------------------------------------------------------------------
# MAKE A MAP OF ECONOMIC VALUE OF INFORMATION SS
value_dt <- perfomances_dt4[NMS %in% c(1,4)]

#make one negative
value_dt[NMS == 1, Y_corn := -Y_corn]
value_dt[NMS == 1, L := -L]
value_dt[NMS == 1, leach_ext := -leach_ext]
value_dt[NMS == 1, N_fert := -N_fert]
value_dt[NMS == 1, P := -P]

# Add values by group
value_dt <- value_dt[, .(Y_corn =  sum(Y_corn),
                         L = sum(L),
                         leach_ext = sum(leach_ext), 
                         N_fert = sum(N_fert),
                         P = sum(P)), by = .(id_10)]

# baseline_leaching_dt <- perfomances_dt3[NMS == 1, .(id_10, baseline_leach = L)]
# value_dt <- merge(value_dt, baseline_leaching_dt, by = 'id_10')
# 
# ggplot(data = value_dt, aes(x = baseline_leach, y = P)) +
#   geom_point() + geom_smooth()

value_sf <- merge(value_sf, value_dt[,.(id_10, L,P)], by = 'id_10', all.x = T)
hist(value_sf$eb_cell)

breaks_n <- c(-60000,-40000,-10000,0)

(p3 <- tm_shape(value_sf) + tm_polygons(c('eb_cell'), 
                                        # textNA="Not VR area", 
                                        title = expression(paste('EB'^'I-SS-ex ante', '(kg/cell)')),
                                        breaks = breaks_n, 
                                        border.col = 'black',
                                        #style ="cont", 
                                        palette = "-Greys", 
                                        colorNA = 'white', midpoint = -10000)+
    tm_layout(panel.labels = expression(paste('c)')),
              # main.title = 'f',
              main.title.position = c(0,0),
              legend.text.size = 0.7,
              main.title.size = 1.2,
              title.snap.to.legend = F,
              legend.width = 1,
              legend.position = c('left', 'bottom'))) #Enviromental benefit of NMS 4


value_sf <- dplyr::mutate(value_sf, P_r = round(P, 0))

breaks_n <- c(-20,0,10,20, 30,40)

# (p4 <- tm_shape(value_sf, bbox = st_bbox(value_sf)) + tm_polygons(c('P'), 
#                                                                   title = expression(paste('V'^'I-SS-ex ante', '($/ha)')), 
#                                         breaks = breaks_n, 
#                                         style ="cont", palette = "Greys", colorNA = 'white', midpoint = 0)+
#     tm_layout(panel.labels = 'd)',
#               legend.text.size = 0.7,
#               main.title.size = 1.2,
#               legend.position = c('left', 'bottom'))) #Economic Value

(p4 <- tm_shape(value_sf) + tm_polygons(c('P'), 
                                        # textNA="Not VR area", 
                                        title = expression(paste('V'^'I-SS-ex ante', '($/ha)')),
                                        breaks = breaks_n, 
                                        border.col = 'black',
                                        # style ="cont", 
                                        palette = "Greys", colorNA = 'white', midpoint = 0)+
    tm_layout(panel.labels = expression(paste('c)')),
              # main.title = 'f',
              main.title.position = c(0,0),
              legend.text.size = 0.7,
              main.title.size = 1.2,
              title.snap.to.legend = F,
              legend.width = 1,
              legend.position = c('left', 'bottom'))) #Economic V of NMS 4

tmap_save(tmap_arrange(p1, p2, p3, p4, ncol = 2) , "./n_policy_box/Data/figures/information_characterization_map.jpg", 
          width = 10, height = 10,
          units = 'in')

#=====================================================================================================================
#-----------------------------------------VALUE OF TECHNOLOGY--------------------------------------------------------
#=====================================================================================================================
#1) MAKE A MAP OF VALUE TECHNOLOGY (EX POST VALUE)
#Select the two NMSs of interest
value_dt <- perfomances_dt3[NMS %in% c(11,12)]
#make one negative
value_dt[NMS == 11, Y_corn := -Y_corn]
value_dt[NMS == 11, L := -L]
value_dt[NMS == 11, leach_ext := -leach_ext]
value_dt[NMS == 11, N_fert := -N_fert]
value_dt[NMS == 11, P := -P]

# Add values by group
value_post_dt <- value_dt[, .(Y_corn =  sum(Y_corn),
                              L = sum(L),
                              leach_ext = sum(leach_ext), 
                              N_fert = sum(N_fert),
                              P = sum(P)), by = .(id_10)]
value_post_dt[order(-P)]

value_sf <- merge(grid10_tiles_sf7, value_post_dt, by = 'id_10', all.x = T)


breaks_n <- c(min(value_post_dt$P), 5,10,15, 20, max(value_post_dt$P))
(p1 <- tm_shape(value_sf) + tm_polygons(c('P'), 
                                        # textNA="Not VR area", 
                                        #title = expression(paste('VR market', '($/cell)')), 
                                        title = expression(paste('V'^'T-ex post', '($/ha)')),
                                        breaks = breaks_n, 
                                        border.col = 'black',
                                        #style ="cont", 
                                        palette = "Greys", colorNA = 'white', midpoint = 10)+
    tm_layout(panel.labels = expression(paste('a)')),
              # main.title = 'f',
              main.title.position = c(0,0),
              legend.text.size = 0.7,
              main.title.size = 1.2,
              title.snap.to.legend = F,
              legend.width = 1,
              legend.position = c('left', 'bottom')))

#---------------------------------------------------------------------------
#2) MAKE A MAP OF VALUE TECHNOLOGY (EX ANTE VALUE)
value_dt <- perfomances_dt3[NMS %in% c(4,5)]
#make one negative
value_dt[NMS == 4, Y_corn := -Y_corn]
value_dt[NMS == 4, L := -L]
value_dt[NMS == 4, leach_ext := -leach_ext]
value_dt[NMS == 4, N_fert := -N_fert]
value_dt[NMS == 4, P := -P]

# Add values by group
value_ante_dt <- value_dt[, .(Y_corn =  sum(Y_corn),
                              L = sum(L),
                              leach_ext = sum(leach_ext), 
                              N_fert = sum(N_fert),
                              P = sum(P)), by = .(id_10)]

value_sf <- merge(grid10_tiles_sf7, value_ante_dt, by = 'id_10', all.x = T) 

# value_sf$P[is.na(value_sf$P)] <- 0 


# (p2 <- tm_shape(value_sf) + tm_polygons(c('P'), n =10)+
#     tm_layout(legend.text.size = 0.7,
#               main.title = paste('Ex-ante'),
#               main.title.position = "center",
#               main.title.size = 1.2))

breaks_n <- c(floor(min(value_sf$P, na.rm = T)),0, 5,ceiling(max(value_sf$P, na.rm = T)))

(p2 <- tm_shape(value_sf) + tm_polygons(c('P'), 
                                        # textNA="Not VR area", 
                                        #title = expression(paste('VR market', '($/cell)')), 
                                        title = expression(paste('V'^'T-ex ante', '($/ha)')),
                                        breaks = breaks_n, 
                                        border.col = 'black',
                                        #style ="cont", 
                                        palette = "Greys", colorNA = 'white', midpoint = 5) +
    tm_layout(panel.labels = expression(paste('b)')),
              # main.title = 'f',
              main.title.position = c(0,0),
              legend.text.size = 0.7,
              main.title.size = 1.2,
              title.snap.to.legend = F,
              legend.width = 1,
              legend.position = c('left', 'bottom')))

value_comp_dt <- merge(value_ante_dt[,.(id_10, P_ante = P)], value_post_dt[,.(id_10, P_post = P)], by = 'id_10')

ggplot(data = value_comp_dt, aes(x= P_post, y = P_ante)) +
  geom_point() +
  geom_smooth()

# tmap_mode("view")
# 
# tm_basemap("OpenStreetMap.DE") +
#   tm_shape(value_sf) + tm_polygons(c('P'), n =10)+
#   tm_layout(legend.text.size = 0.7,
#             main.title = paste('EX-ANTE'),
#             main.title.position = "center",
#             main.title.size = 1.2)
#   
# ---------------------------------------------------------------------------
# 3) MAKE A MAP OF TECHNOLOGY MARKET CAP BY CELL

# MAKE A MAP OF VALUE TECHNOLOGY (EX ANTE VALUE)
value_dt <- perfomances_dt3[NMS %in% c(4,5)]
#make one negative
value_dt[NMS == 4, Y_corn := -Y_corn]
value_dt[NMS == 4, L := -L]
value_dt[NMS == 4, leach_ext := -leach_ext]
value_dt[NMS == 4, N_fert := -N_fert]
value_dt[NMS == 4, P := -P]

# Add values by group
value_dt <- value_dt[, .(Y_corn =  sum(Y_corn),
                         L = sum(L),
                         leach_ext = sum(leach_ext), 
                         N_fert = sum(N_fert),
                         P = sum(P),
                         corn_avg_ha = mean(corn_avg_ha)), by = .(id_10)]

value_dt2 <- value_dt[P > 2] #
value_dt2[,mkt_value := P * corn_avg_ha]
value_dt2[,.(sum(mkt_value))]

value_sf <- merge(grid10_tiles_sf7, value_dt2[,.(id_10, mkt_value)], by = 'id_10', all.x = T) %>%
  dplyr::mutate(NMS = 'ex_ante')


sum(value_sf$mkt_value, na.rm = TRUE)

# (p3 <- tm_shape(value_sf) + 
#     tm_polygons("mkt_value", textNA="Not VR area", title="VR Value (USD/Cell)", n = 10) +
#     tm_layout(legend.text.size = 0.7,
#               main.title = paste('VR Market Value'),
#               main.title.position = "center",
#               main.title.size = 1.2))

breaks_n <- c(0, 5000,10000,15000,20000, 30000,40000,50000)

(p3 <- tm_shape(value_sf) + tm_polygons(c('mkt_value'), textNA="Not VR area", 
                                        title = expression(paste('Mkt value ($/cell)')), 
                                        # title = "",
                                        breaks = breaks_n, 
                                        border.col = 'black',
                                        #style ="cont", 
                                        palette = "Greys", colorNA = 'white', midpoint = 20000)+
    tm_layout(panel.labels = expression(paste('b)')),
              # main.title = 'f',
              main.title.position = c(0,0),
              legend.text.size = 0.7,
              main.title.size = 1.2,
              title.snap.to.legend = F,
              legend.width = 1,
              legend.position = c('left', 'bottom')))  

tmap_arrange(p1, p2, p3, nrow = 1)
tmap_save(tmap_arrange(p1, p2, p3, nrow = 1), "./n_policy_box/Data/figures/techonology_characterization_map.jpg", 
          width = 10, height = 5, units = 'in')

st_write(value_sf, "./n_policy_box/Data/shapefiles/vr_cell_value_sf.shp", delete_dsn = TRUE)

#---------------------------------------------------------------------------
# 4) MAKE A MAP OF TECHNOLOGY MARKET CAP BY FIELD (for QGIS)
# AGGREGATE THE DATA TO CELL X Z LEVEL CONSIDERING THE AREA
names(perfomances_dt)
do_not_aggregate = c('id_10', 'id_field','region','NMS', 'tech')
do_aggregate =  c("Y_corn", "L", "N_fert","P")

perfomances_field_dt <- aggregate_by_area(data_dt = perfomances_dt, variables = do_aggregate, 
                                          weight = 'area_ha', by_c = do_not_aggregate) #cell x z level (mukey and field are out)

# MAKE A MAP OF VALUE TECHNOLOGY (EX ANTE VALUE)
value_dt <- perfomances_field_dt[NMS %in% c(4,5)]
#make one negative
value_dt[NMS == 4, Y_corn := -Y_corn]
value_dt[NMS == 4, L := -L]
value_dt[NMS == 4, N_fert := -N_fert]
value_dt[NMS == 4, P := -P]

# Add values by group
value_dt <- value_dt[, .(Y_corn =  sum(Y_corn),
                         L = sum(L),
                         N_fert = sum(N_fert),
                         n_policy = sum(P)), by = .(id_10, id_field)]

# value_dt <- value_dt[P > 3] #considering a cost of VR Cost of 3 usd
# value_dt[,mkt_value := P * corn_avg_ha]
# value_dt[,.(sum(mkt_value))]

value_sf <- merge(grid10_fields_sf2, value_dt[,.(id_10, id_field, n_policy)], by = c('id_10', 'id_field'), all.x = T) %>%
  dplyr::mutate(NMS = 'ex_ante')

value_sf <- value_sf[!is.na(value_sf$n_policy),]

(p <- tm_shape(value_sf) + 
    tm_polygons("n_policy", textNA="Not VR area", title="VR Value (USD/Cell)", n = 10) +
    tm_layout(legend.text.size = 0.7,
              main.title = paste('VR Market Value'),
              main.title.position = "center",
              main.title.size = 1.2))


st_write(value_sf, "./n_policy_box/Data/shapefiles/vr_field_value_sf.shp", delete_dsn = TRUE)


#---------------------------------------------------------------------------
# MAKE A MAP OF THE BEST NMS
#Select the two NMSs of interest
best_NMS_dt <- perfomances_dt3[NMS %in% 1:10]
# best_NMS_dt[NMS == 5, P := P-3]
best_NMS_dt <- best_NMS_dt[,.SD[P==max(P)], by = id_10]

best_NMS_dt[,.N, by = .(NMS)][order(-N)]

value_sf <- merge(grid10_tiles_sf7, best_NMS_dt[,.(id_10, NMS)], 
                  by = 'id_10', all = T)

value_sf <- dplyr::mutate(value_sf, NMS = ifelse(NMS <6, NA, NMS))


(p <- tm_shape(value_sf) + tm_polygons(c('NMS'), n =10)+
    tm_text('NMS')+
    tm_layout(legend.text.size = 0.7,
              main.title = paste('Best NMS by cell'),
              main.title.position = "center",
              main.title.size = 1.2))

tmap_save(p, "./n_policy_box/Data/figures/best_NMS_map.jpg")

#==============================================================================================================
#==============================================================================================================
#=============================                  YIELD CURVE EXAMPLE              ==============================   
#==============================================================================================================
#==============================================================================================================


yc_yearly_dt3 <- readRDS("./n_policy_box/Data/files_rds/yc_yearly_dt3.rds")
reg_NMS_stuff <- readRDS( "./n_policy_box/Data/files_rds/reg_NMS_stuff.rds")
training_z <- reg_NMS_stuff$training_z
rm(reg_NMS_stuff)

# tile_n = 10
cell_n = 765#755#763#765
mukey_n = 242997


testing_set_dt <- perfomances_dt[id_10 == cell_n]

testing_set_dt[,mean(Y_corn), by = mukey]

testing_set_plot <- testing_set_dt[mukey == mukey_n]
testing_set_plot[,NMS := factor(NMS, levels= c('static', 'dynamic', '3','4', '5', '6', '7', '8', '9', '10', '11', '12'))]
ic_field_plot <- yc_yearly_dt3[mukey == mukey_n & id_10 == cell_n ] %>% .[!z %in% training_z ]

# testing_set_plot[,z := gsub(pattern = 'A', replacement = 'z', x = z)]
# ic_field_plot[,z := gsub(pattern = 'A', replacement = 'z', x = z)]

# library(RColorBrewer)
# n <- 12
# qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
# col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# colors_sample =sample(col_vector, n)
# pie(rep(1,n), colors_sample)

colors_sample=c( "#7570B3", "#FFED6F", "#666666", "#7FC97F", "#386CB0", "#B3B3B3", "#FFFFCC", "#A65628", "#F4CAE4", "#E41A1C", "#E6AB02", "black")

# Y plot with Y_corn at eonr
z_labels <- ic_field_plot[N_fert == max(ic_field_plot$N_fert), .(N_fert, Y_corn, z)][order(-Y_corn)]
z_labels[seq(1, nrow(z_labels), by = 2), N_fert := N_fert - 50]

ggplot() +
  geom_point(data = testing_set_plot, aes(x = N_fert, y = Y_corn, colour = NMS, size = NMS)) +
  geom_line(data = ic_field_plot, aes(x = N_fert, y = Y_corn, group=z), show.legend = FALSE) +
  scale_size_manual(values=c(rep(2, 11), 4)) +
  scale_color_manual(values=colors_sample)+
  ylab('Yield (kg/ha)')+
  xlab('N rate (kg/ha)')+
  geom_text(data = z_labels, aes(x = N_fert, y = Y_corn, label = z))+
  theme_bw()+
  theme(panel.grid = element_blank())


# (plot_n1 <- ggplot() +
#     geom_point(data = testing_set_plot[z == z_n & NMS == '12' & prev_crop == 1], 
#                aes(x = N_fert, y = Y_corn ,  size = NMS)) +
#     geom_line(data = ic_field_plot[z == z_n & prev_crop == 1], aes(x = N_fert, y = Y_corn, linetype = "Yield")) +
#     geom_point(data = testing_set_plot[z == z_n & NMS == '12' & prev_crop == 1], 
#                aes(x = N_fert, y = L*150,  size = NMS)) +
#     geom_line(data = ic_field_plot[z == z_n & prev_crop == 1], aes(x = N_fert, y = L*150, linetype = "N Leaching")) +
#     # scale_size_manual(values=c(rep(2, 11), 4)) +
#     ## scale_color_manual(values=colors_sample)+
#     labs(y = 'Yield (kg/ha)',
#          x = 'N rate (kg/ha)',
#          colour = "Parameter")+
#     scale_y_continuous(sec.axis = sec_axis(~./150, name = "N leaching (kg/ha)"))+
#     scale_linetype_manual(values = c("dashed", "solid"))+
#     scale_size_manual(values = 4,
#                       labels = expression(paste('EONR'^'ex post')))+
#     #geom_text(data = z_labels, aes(x = N_fert, y = Y_corn, label = z))+
#     theme_bw()+
#     guides(linetype = guide_legend(order=2),
#            size = guide_legend(order=1)) +
#     theme(legend.title =  element_blank(),
#           legend.position = c(0.85, 0.15),
#           panel.grid = element_blank())+
#     annotate("text", x=300, y=11500, label= "a)", size = 10) )
ic_field_plot$z
ggplot(ic_field_plot, aes(x= N_fert, y = L, color = z)) +
  geom_line()

z_n = 23



ic_field_plot2 <- melt(ic_field_plot[z == z_n ], id.vars = 'N_fert', measure.vars = c('Y_corn', 'L'))
ic_field_plot2[variable == 'L', value := value * 150]

testing_set_plot2 <- melt(testing_set_plot[z == z_n & NMS == '12'], id.vars = 'N_fert', measure.vars = c('Y_corn', 'L'))
testing_set_plot2[variable == 'L', value := value * 150]


(plot_n1 <- ggplot() +
    geom_line(data = ic_field_plot2, aes(x = N_fert, y = value, linetype = variable, colour = variable))+
    scale_color_manual(values=c('black', 'black', 'black'),
                       labels = c(bquote (paste('EONR'^'ex post')), 'Yield', 'N leaching'))+
    geom_point(data = testing_set_plot2, aes(x = N_fert, y = value, colour = 'EONR')) +
    guides( linetype = FALSE,
            colour = guide_legend(override.aes = list(shape = c(16, NA, NA),
                                                      linetype = c("blank", "solid", "dotted"))))+
    labs(y = 'Yield (kg/ha)',
         x = 'N rate (kg/ha)',
         colour = "Variable") +
    scale_y_continuous(sec.axis = sec_axis(~./150, name = "N leaching (kg/ha)"))+
    theme_bw() +
    theme(legend.title =  element_blank(),
          legend.position = c(0.85, 0.15),
          panel.grid = element_blank())+
    annotate("text", x=300, y=15000, label= "a)", size = 10) )

summary(testing_set_plot$Y_corn)

exclude_z = testing_set_plot[Y_corn == min(Y_corn)]$z[1]

(plot_n2 <- ggplot() +
    
    geom_line(data = ic_field_plot[ !(z == exclude_z) ], aes(x = N_fert, y = Y_corn, group = z), show.legend = F) +
    geom_point(data = testing_set_plot[NMS == '12' & !(z == exclude_z) ], aes(x = N_fert, y = Y_corn , shape = 'EONR'), size = 2) +
    scale_shape_manual( values = 16,
                        labels = c(bquote (paste('EONR'^'ex post')))) +
    # scale_color_manual(values=colors_sample)+
    # scale_size_manual(values=c(rep(2, 11), 4)) +
    ## scale_color_manual(values=colors_sample)+
    labs(y = 'Yield (kg/ha)',
         x = 'N rate (kg/ha)')+
    #scale_y_continuous(sec.axis = sec_axis(~./150, name = "N Leaching (kg/ha)"))+
    #scale_linetype_manual(values = c("solid", "dashed"))+
    #geom_text(data = z_labels, aes(x = N_fert, y = Y_corn, label = z))+
    theme_bw()+
    theme(legend.title =  element_blank(),
          legend.position = c(0.85, 0.15),
          panel.grid = element_blank())+
    guides(color = FALSE) + #remove legend for color
    annotate("text", x=300, y=15000, label= "b)", size = 10)) 

summary(testing_set_plot[NMS == '12']$N_fert)

perfomances_champaign_dt <- perfomances_dt[id_10 %in% unique(dplyr::filter(grid10_tiles_sf7, county_name == 'Champaign')$id_10) & NMS == 12]

(plot_n3 <- ggplot() + 
    geom_density(data = perfomances_champaign_dt, aes( x= N_fert,  y = ..density..), alpha = 0.4)+
    labs(x = expression(paste('EONR'^'ex post', '(kg/ha)')))+
    theme_bw()+  
    theme(panel.grid = element_blank(),
          legend.position = c(0.85, .7))+
    scale_fill_manual(name = "Previous crop", labels = c("Soybean", "Corn"), values = c('#696969', '#D3D3D3'))+
    scale_linetype_manual(values=c("twodash", "dotted"))+
    guides(linetype = FALSE)+
    annotate("text", x=300, y=0.009, label= "c)", size = 10) )

(plot_n4 <- ggplot() + 
    geom_density(data = perfomances_champaign_dt, aes( x= L,  y = ..density..), alpha = 0.4)+
    labs(x = 'N leaching (kg/ha)')+
    theme_bw()+ 
    xlim(c(0, 150))+
    theme(panel.grid = element_blank(),
          legend.position = c(0.85, 0.7))+
    scale_fill_manual(name = "Previous crop", labels = c("Soybean", "Corn"), values = c('#696969', '#D3D3D3'))+
    scale_linetype_manual(values=c("twodash", "dotted"))+
    guides(linetype = FALSE)+
    annotate("text", x=140, y=0.025, label= "d)", size = 10) )

grid.arrange(plot_n1, plot_n2, nrow = 1)

grid.arrange(plot_n1, plot_n2, plot_n3, plot_n4, nrow = 2)

ggsave(grid.arrange(plot_n1, plot_n2, nrow = 1), 
       filename = "./n_policy_box/Data/figures/yield_curve_example.jpg", width = 10, height =  4, units = 'in')


grid.arrange(grid.arrange(plot_n1, plot_n2, nrow=1), plot_n3, nrow = 2)























#---------------------------------------------------------------------------
# MAKE A MAP OF THE BEST NMS
#Select the two NMSs of interest
best_NMS_dt <- perfomances_dt3[NMS %in% 1:10]
best_NMS_dt <- best_NMS_dt[,.SD[P==max(P)], by = id_10]
best_NMS_dt <- merge(best_NMS_dt, perfomances_dt3[NMS == 1, .(id_10, P_1 = P)], by = 'id_10')
best_NMS_dt[,P_improve := P-P_1]

best_NMS_dt[,.N, by = .(region, NMS)]

best_NMS_sf <- merge(grid10_tiles_sf7, best_NMS_dt, by = 'id_10', all.x = T)

(p <- tm_shape(best_NMS_sf) + tm_polygons(c('NMS','P_improve'), n =10)+
    tm_text('NMS')+
    tm_layout(legend.text.size = 0.7,
              main.title = paste('VALUE OF TECHNOLOGY AND INFORMATION'),
              main.title.position = "center",
              main.title.size = 1.2))

tmap_save(p, "./n_policy_box/Data/figures/value_t_i.jpg")

#---------------------------------------------------------------------------
# PLOT WEIRD CASE
yc_yearly_dt <- readRDS('./n_policy_box/Data/files_rds/yc_yearly_dt.rds')
length(unique(yc_yearly_dt$id_10))

yc_yearly_dt3 <- readRDS('./n_policy_box/Data/files_rds/yc_yearly_dt3.rds')
length(unique(yc_yearly_dt3$id_10))

length(unique(full_fields_dt2$id_10))


id_10_n <- n_policy_expost_dt[order(-L2)][1,]$id_10
yc_yearly_dt <- readRDS('./n_policy_box/Data/files_rds/yc_yearly_dt.rds')
one_field_dt <- data.table(grid10_soils_sf6[grid10_soils_sf6$id_10 == id_10_n,])
mukey_n <- one_field_dt[area_ha == max(area_ha)][1,] %>% .[,.(id_10, mukey)]

ic_field_dt <- filter_dt_in_dt(yc_yearly_dt , filter_dt = mukey_n, return_table = TRUE)
ic_field_dt[,prev_crop := ifelse(prev_crop == 'MSM', 0, 1)]
ic_field_dt[, P := Y_corn * Pc - N_fert * Pn]

performance_set_dt <- filter_dt_in_dt(perfomances_dt , filter_dt = mukey_n, return_table = TRUE)

performance_set_dt[,NMS := as.character(NMS)]

performance_set_dt[prev_crop == 0 & NMS != 11, .N, by = .(NMS, z)]

# P plot with P at eonr
(plot_n <- ggplot() +
    geom_point(data = performance_set_dt[prev_crop == 0 & NMS != 11], aes(x = N_fert, y = P, colour = NMS)) +
    geom_point(data = performance_set_dt[prev_crop == 0 & NMS == 11], aes(x = N_fert, y = P), size = 3, show.legend = FALSE) +
    geom_line(data = ic_field_dt[prev_crop == 0], aes(x = N_fert, y = P, group=interaction(z)), show.legend = FALSE) +
    ggtitle(paste('P plot with P at eonr', mukey_n$mukey)))

ggsave(plot_n, filename = "./n_policy_box/Data/figures/yield_curve_example.jpg")

# Lo3 plot with leaching at eonr
(plot_n <- ggplot() +
    geom_point(data = performance_set_dt[prev_crop == 0 & NMS != 11], aes(x = N_fert, y = L2, colour = NMS)) +
    geom_point(data = performance_set_dt[prev_crop == 0 & NMS == 11], aes(x = N_fert, y = L2), size = 3, show.legend = FALSE) +
    geom_line(data = ic_field_dt[prev_crop == 0], aes(x = N_fert, y = L2, group=interaction(z)), show.legend = FALSE) +
    ggtitle(paste('Lo3 plot with leaching at eonr', mukey_n$mukey)))

ggsave(plot_n, filename = "./n_policy_box/Data/figures/leaching_curve_example.jpg")

#---------------------------------------------------------------------------
# CALCULATE STATE TOTAL VARIABLES
perfomances_dt4 <- copy(perfomances_dt4)
do_aggregate =  c("Y_corn", "L2", "leach_ext", "N_fert","P")
perfomances_dt4[,(do_aggregate) := (.SD * corn_avg_ha/1000), .SDcols=do_aggregate]

state_total_production_dt <- perfomances_dt4[, lapply(.SD, function(x) sum(x)), .SDcols= do_aggregate] 
2.2 * 10^9 * 25.4 /1000 #IL production in tons https://www.nass.usda.G/Statistics_by_State/Illinois/Publications/Current_News_Release/2018/20180112-IL_Annual_Crop_Production.pdf
10.95 * 10^6 *0.4046#IL harvested area in ha
201 * 25.4/0.4046 #IL Yield

#---------------------------------------------------------------------------
# SELECT THE WORST YEAR FOR LEACHING AND SEE THE BENEFIT THERE


all_perfomances_dt2 <- aggregate_by_area(data_dt = all_perfomances_dt, variables = do_aggregate, 
                                         weight = 'area_ha', by_c = c('id_10', 'z'))

extreme_year_dt <- all_perfomances_dt2[, .SD[ nL1 == max( nL1)], by = .(id_10)]
extreme_year_dt[, lapply(.SD, function(x) mean(x)), .SDcols= do_aggregate] 



eonr_mukey_dt <- yc_yearly_dt[, .SD[ P == max( P)], by = .(id_10, mukey, z)]

1804.744/1814.46

all_perfomances_dt[id_10 == 5]
all_perfomances_dt[id_10 == 5, .(Y_corn_1 = sum(Y_corn_1), area_ha = sum(area_ha))]


# MAKE A DT
economics_field_dt <- merge(n_regional_noss_dt2, n_regional_ss_dt2) %>% merge(eonr_ur_dt2) %>% merge(eonr_vr_dt2)
economics_field_dt[,area_ha := sum(area_dt$area_ha)]
economics_field_dt[,val_ss_ha := (P_reg_ss - P_reg_no_ss)/area_ha]
economics_field_dt[,val_info_ha := (P_ur - P_reg_no_ss)/area_ha]
economics_field_dt[,val_tech_ha := (P_vr - P_ur)/area_ha]

economics_field_dt[,nval_ss_ha := (nleach_reg_ss - nleach_reg_no_ss)/area_ha]
economics_field_dt[,nval_info_ha := (nleach_ur - nleach_reg_no_ss)/area_ha]
economics_field_dt[,nval_tech_ha := (nleach_vr - nleach_ur)/area_ha]

cols <- names(economics_field_dt)[sapply(economics_field_dt,is.numeric)]
economics_field_dt[,(cols) := round(.SD,3), .SDcols=cols]


economics_field_dt <- cbind(fields_seq_tmp, economics_field_dt)
economics_field_dt[,mukey_count := nrow(area_dt)]

economics_ls[[j]] <- economics_field_dt
}

economics_dt <- rbindlist(economics_ls)
saveRDS(economics_dt, './n_policy_box/Data/files_rds/economics_dt.rds')

#---------------------------------------------------------------------------
# MAKE A MAP OF VALUE OF I AND T
val_map_dt <- economics_dt[,.(val_ss_ha = mean(val_ss_ha), 
                              val_info_ha = mean(val_info_ha),
                              val_tech_ha = mean(val_tech_ha)), by = id_10]

grid10_value_sf <- left_join(grid10_tiles_sf, val_map_dt, by = 'id_10')

(p <- tm_shape(grid10_value_sf) + tm_polygons(c('val_ss_ha','val_info_ha', 'val_tech_ha'))+
    tm_layout(legend.text.size = 0.7,
              main.title = paste('VALUE OF TECHNOLOGY AND INFORMATION'),
              main.title.position = "center",
              main.title.size = 1.2))

tmap_save(p, "./n_policy_box/Data/figures/value_t_i.jpg")
#---------------------------------------------------------------------------
# MAKE A MAP OF VALUE OF I AND T FOR LEACHING
val_map_dt <- economics_dt[,.(nval_ss_ha = mean(nval_ss_ha), 
                              nval_info_ha = mean(nval_info_ha),
                              nval_tech_ha = mean(nval_tech_ha)), by = id_10]

grid10_value_sf <- left_join(grid10_tiles_sf, val_map_dt, by = 'id_10')

(p <- tm_shape(grid10_value_sf) + tm_polygons(c('nval_ss_ha','nval_info_ha', 'nval_tech_ha'))+
    tm_layout(legend.text.size = 0.7,
              main.title = paste('VALUE OF TECHNOLOGY AND INFORMATION'),
              main.title.position = "center",
              main.title.size = 1.2))

tmap_save(p, "./n_policy_box/Data/figures/nvalue_t_i.jpg")
#---------------------------------------------------------------------------
# MAKE A MAP OF PROFITS
economics_dt
prof_map_dt <- economics_dt[,P_ur_ha := P_ur / area_ha]
prof_map_dt <- prof_map_dt[,.(P_ur_ha = mean(P_ur_ha)), by = id_10]

grid10_value_sf <- left_join(grid10_value_sf, prof_map_dt, by = 'id_10')

(p <- tm_shape(grid10_value_sf) + tm_polygons('P_ur_ha')+
    tm_layout(legend.text.size = 0.7,
              main.title = paste('AVERAGE UR PROFITS $/HA'),
              main.title.position = "center",
              main.title.size = 1.2))

tmap_save(p, "./n_policy_box/Data/figures/profits.jpg")


#---------------------------------------------------------------------------
# MAKE BOXPLOT
dat.m <- melt(economics_dt,id.vars= c('id_10', 'id_field',  'z'), measure.vars=c('val_ss_ha','val_info_ha', 'val_tech_ha'))
(p <- ggplot(dat.m) +
    geom_boxplot(aes(x=variable, y=value, color=variable)) +
    # scale_color_discrete(name = "REGION") +
    ggtitle('Value of information and technology')+    
    theme_bw() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          #legend.position='bottom',
          panel.grid = element_blank(),
          strip.background = element_blank(),
          legend.text.align = 0,
          legend.position = "none",
          legend.title = element_blank(),
          strip.text = element_blank()))

ggsave(p, filename = "./n_policy_box/Data/figures/value_boxplot.jpg")

