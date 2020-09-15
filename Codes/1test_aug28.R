rm(list=ls())

setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
codes_folder <-'C:/Users/germanm2/Documents'#CPSC

setwd('~')#Server
codes_folder <-'~' #Server

library("foreach")
library("doParallel")

source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')
source(paste0(codes_folder, '/n_policy_git/Codes/parameters.R'))

regional_test <- T
#======================================================================================
#Merge all files
# batch_n = 19Y
folders <- list.dirs("./n_policy_box/Data", full.names = F )[str_detect(pattern = 'yc_output_summary', string = list.dirs("./n_policy_box/Data" ))]
batches <- sort(as.numeric(sapply(strsplit(folders, split="_"), "[", 4) ), decreasing = T)[1]
print(batches)
two_batches_yc_dt  <- data.table()
for(batch_n in sort(batches)){
  # batch_n = 150
  # print(batch_n)
  multiple_files <- list.files(paste0("./n_policy_box/Data/yc_output_summary_", batch_n, "_swat"), full.names = T)
  print(length(multiple_files))
 
  registerDoParallel(cores = 6)
  output_list = foreach(file_n = multiple_files, .combine = "c", .packages = c("data.table")) %dopar% {
    # file_n <- multiple_files[1]
    tmp_dt <- readRDS(file_n)
    list(tmp_dt)
  }#end of dopar loop
  
  stopImplicitCluster()
  
  one_batch_dt <- rbindlist(output_list)
  one_batch_dt[,id_10 := as.integer(id_10)]
  
  one_batch_dt[,.N, by = .(id_10, mukey, z)]$N %>% table() #of rates by mukey z, has to be 33
  table(one_batch_dt$N_fert)
  one_batch_dt[,.N, by = .(id_10, mukey, z)][,.N, by = .(id_10, mukey)]$N %>% table()#of z by mukey, has to be 30 or 15
  
  one_batch_dt[id_10 < 300]
  # setnames(yc_yearly_dt, 
  #          c('Yld', 'Yld_soy', 'leach_1', 'leach_2' ),
  #          c('Y_corn', 'Y_soy', 'L1', 'L2'))
  
  one_batch_dt[,L := L1 + L2]
  
  
  #Add regions and areas
  if(regional_test){
    grid10_soils_dt4 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt4.rds")
    regions_dt <- grid10_soils_dt4[,.N, by = .(region, id_10)][,-'N']
    one_batch_dt <- merge(one_batch_dt, regions_dt, by = c('id_10'))
  }else{  
    grid10_soils_dt4 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt4.rds")
    areas_dt <- data.table(grid10_soils_dt4) %>% .[,.(area_ha = sum(area_ha)), by = .(id_10, mukey, region)]
    
    one_batch_dt <- merge(one_batch_dt, areas_dt, by = c('id_10', 'mukey'))
    one_batch_dt[,.N, .(region, id_10)][,.N, region]
  }
  one_batch_dt[region == 1,region_lab := '1-South']
  one_batch_dt[region == 2,region_lab := '2-Central']
  one_batch_dt[region == 3,region_lab := '3-North']
  
  one_batch_dt[,region := factor(region_lab)]
  one_batch_dt[,region_lab := NULL]
  one_batch_dt[,batch := batch_n]
  
  # saveRDS(one_batch_dt, paste0("./n_policy_box/Data/files_rds/one_batch_dt_batch", batch_n, ".rds"))
  two_batches_yc_dt <- rbind(two_batches_yc_dt, one_batch_dt, fill = T)
}

batch148_dt <- readRDS(paste0("./n_policy_box/Data/files_rds/one_batch_dt_batch148.rds"))
two_batches_yc_dt <- rbind(two_batches_yc_dt, batch148_dt, fill = T)

# paste(two_batches_yc_dt[,.N, by = .(region, id_10)][,.SD[sample(.N, 3)],by = region]$id_10, collapse = ', ')
two_batches_yc_dt <- two_batches_yc_dt[sim_name %in% unique(one_batch_dt$sim_name)]
# id_10_v <- c( 43,807,1362)
# two_batches_yc_dt <- two_batches_yc_dt[id_10 %in% id_10_v]


comp <- merge(two_batches_yc_dt[batch == min(batch), .(sim_name, Y_corn1 = Y_corn)] , 
              two_batches_yc_dt[batch == max(batch), .(sim_name, Y_corn2 = Y_corn)], by = 'sim_name')

ggplot(comp) + 
  geom_point(aes(x = Y_corn1, y =Y_corn2))+ theme(aspect.ratio=1) + coord_fixed() + geom_abline() + 
  ylim(0, 18000)+ xlim(0, 18000) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  ggtitle('Cluster vs Server') +
  theme_bw()

# if(FALSE){
#   batch_n = 88
#   two_batches_yc_dt <- readRDS( paste0("./n_policy_box/Data/files_rds/one_batch_dt_batch", batch_n, ".rds"))
# }
# yc_yearly_dt <-readRDS("./n_policy_box/Data/files_rds/yc_yearly_dt3.rds")
# areas_dt[,region := factor(region)]
# yc_yearly_dt <- merge(yc_yearly_dt, areas_dt, by = c('region','id_10', 'mukey'))
# yc_yearly_dt[,batch := 43]
#======================================================================================
#Relative yield plot
# batch_n = 61
# yc_yearly_batches_dt <- readRDS(paste0("./n_policy_box/Data/files_rds/yc_yearly_dt_batch", batch_n, ".rds"))
# yc_yearly_dt <- yc_yearly_batches_dt
if(regional_test){
  two_batches_state_dt  <- two_batches_yc_dt[,.(Y_corn = mean(Y_corn),
                                                L = mean(L)), by = .(N_fert, region, batch)][order(region)]
}else{
  two_batches_state_dt  <- aggrea(data_dt = yc_yearly_dt, variables = c('Y_corn', 'Y_soy', 'L1','L2'), 
                                  weight = 'area_ha', by_c = c('N_fert', 'region', 'batch'))# %>% .[,-'area_ha']
}

# yc_yearly_dt <- yc_yearly_batches_dt[batch %in% c(13,17)]

# state_agg_dt <- state_agg_dt[N_fert < 251]
two_batches_state_dt[,Y_max := max(Y_corn), by = .(region, batch)]
two_batches_state_dt[,Y_rel := Y_corn/Y_max]
two_batches_state_dt[,region := factor(region)]
two_batches_state_dt[N_fert == 0][order(batch, -region)]

two_batches_state_dt[,batch := factor(batch)]

(plot_1 <- ggplot(data = two_batches_state_dt) + 
    geom_line(aes(x = N_fert, y = Y_rel, color = region, linetype = batch)) +
    # geom_line(aes(x = N_fert, y = L1_rel, linetype = "N Leaching", color = region)) +
    #geom_hline(yintercept = baselevel_yld, linetype = 'dashed', color = 'grey', size = 1)+
    # geom_vline(xintercept = baselevel_nfert, linetype = 'dashed', color = 'grey', size = 1)+
    labs(y = 'Corn Yield (kg/ha)',
         x = 'Corn N rate (kg/ha)',
         colour = "Region") +
    # scale_y_continuous(sec.axis = sec_axis(~./200, name = "Corn N leaching (kg/ha)", breaks = seq(30,80,5), labels = seq(30,80,5))) +
    # scale_linetype_manual(values = c("dashed", "solid"))+
    theme_bw())


(plot_2 <- ggplot(data = two_batches_state_dt) + 
    geom_line(aes(x = N_fert, y = Y_corn, color = region, linetype = batch)) +
    # geom_line(aes(x = N_fert, y = L1_rel, linetype = "N Leaching", color = region)) +
    #geom_hline(yintercept = baselevel_yld, linetype = 'dashed', color = 'grey', size = 1)+
    # geom_vline(xintercept = baselevel_nfert, linetype = 'dashed', color = 'grey', size = 1)+
    labs(y = 'Corn Yield (kg/ha)',
         x = 'Corn N rate (kg/ha)',
         colour = "Region") +
    # scale_y_continuous(sec.axis = sec_axis(~./200, name = "Corn N leaching (kg/ha)", breaks = seq(30,80,5), labels = seq(30,80,5))) +
    # scale_linetype_manual(values = c("dashed", "solid"))+
    theme_bw())

# =========================================================================================================================================================
# YIELD BOXPLOTS
mrtn_dt <- readRDS("./n_policy_box/Data/files_rds/mrtn_yields_from_graphs.rds")

two_batches_yc_dt[, P := Y_corn * Pc - N_fert * Pn]  #update profits
two_batches_eonr_dt <- two_batches_yc_dt[, .SD[ P == max( P)], by = .(batch, id_10, mukey, z)] %>% .[, .SD[ N_fert == min( N_fert )], by = .(batch, id_10, mukey, z)]
setnames(two_batches_eonr_dt, 'N_fert', 'eonr')

two_batches_eonr_dt[,source := paste0('sim_', batch)]

# Merge data
data_dt <- data.table(rbind(two_batches_eonr_dt[,.(source, region, Y_corn)], mrtn_dt[,.(source, region, Y_corn)]))
# Region boxplots

ggplot(data = data_dt) +
  geom_boxplot(aes(y = Y_corn, x = source))+
  facet_wrap(region~.) 

data_dt[,.(Y_corn = median(Y_corn)), .(source, region)][order(source, -region)]

# # Explore SOuth low yields
# grid10_horizons_v1_dt <- readRDS("./n_policy_box/Data/Grid/grid10_horizons_v1_dt.rds")
# low_south <- two_batches_eonr_dt[region == '1-South']
# low_south <- merge(low_south, grid10_horizons_v1_dt[,.(mukey, restriction)], by = 'mukey')
# low_south[Y_corn < 5000]
# summary(low_south$root_depth)
# 
# low_south[restriction < 150 & water_table_fw ]
# 
# ggplot(data = low_south) +
#     geom_point(aes(y = root_depth, x = restriction    ))
# 
# paste(names(low_south), collapse = ', ')
# 
# 
# pred_vars <- c('dul_dep', 'll15_dep', 'water_table_year', 'root_depth', 'whc', 'day_sow', 
#                'water_table_fw', 'day_v5', 'sw_dep_v5', 'biomass_v5', 'surfaceom_wt_v5', 'root_wt_v5', 
#                'lai_v5', 'oc_20cm_v5', 'oc_40cm_v5', 'n_20cm_v5', 'n_40cm_v5', 'n_60cm_v5', 'n_deep_v5', 
#                'esw_pct_v5', 'water_table_v5', 'stages_cnt', 'swdef_photo', 'swdef_expan', 'swdef_pheno',
#                'end_crop', 'sand_40cm', 'om_40cm', 'clay_40cm', 'rain_30', 'rain_60', 'rain_90', 't_max_30', 't_max_60',
#                't_max_90', 't_min_30', 't_min_60', 't_min_90', 'restriction') #'LAI_max', 
# 
# TrainSet_eonr2 <- low_south[,c('LAI_max', pred_vars), with = FALSE]
# TrainSet_eonr2[,Y_corn_cat := ifelse(Y_corn > 5000,1,0)]
# best.m = 6
# library(randomForest)
# rf2_eonr <- randomForest(LAI_max ~ ., data = TrainSet_eonr2,
#                          importance = TRUE , mtry = best.m, ntree=2000, nodesize = 30)
# 
# varImpPlot(rf2_eonr, type=2)
#---------------------------------------------
# N Balance
# n_eonr_dt <- two_batches_eonr_dt[, .(n_initial = mean(n_initial), 
#                                                         n_uptake_eonr = mean(n_uptake),
#                                      Y_corn_eonr = mean(Y_corn)), by = .(batch, region) ]
# 
# n_zero_dt <- two_batches_yc_dt[ N_fert == 0, .(n_uptake_zero = mean(n_uptake)), by = .(batch, region) ]
# 
# n_balance_dt <- merge(n_eonr_dt, n_zero_dt, by = c('batch', 'region'))
# n_balance_dt[,n_biom := n_uptake_zero - n_initial]
# n_balance_dt[,n_fert := n_uptake_eonr - n_uptake_zero]
# n_balance_dt[,n_zero_rel := n_uptake_zero/n_uptake_eonr]
# n_balance_dt[]
# 
# n_balance_long_dt <- melt(n_balance_dt, id.vars = 'region', measure.vars = c("n_initial", "n_biom", "n_fert"))
# 
# ggplot(data=n_balance_long_dt, aes(x=region, y=value, fill=variable)) +
#   geom_bar(stat="identity")
#----------------------------------------------------------------
# N uptake yield curve
n_uptake_dt <- two_batches_yc_dt[, .(Y_corn = mean(Y_corn), 
                                     n_uptake_eonr = mean(n_uptake)), by = .(batch, region, N_fert) ]
n_uptake_dt[, batch := factor(batch)]
plot_1 <- ggplot(data = n_uptake_dt) + 
    geom_line(aes(x = N_fert, y = n_uptake_eonr, color = region, linetype = batch)) +
    # geom_line(aes(x = N_fert, y = L1_rel, linetype = "N Leaching", color = region)) +
    #geom_hline(yintercept = baselevel_yld, linetype = 'dashed', color = 'grey', size = 1)+
    # geom_vline(xintercept = baselevel_nfert, linetype = 'dashed', color = 'grey', size = 1)+
    labs(y = 'N Uptake (kg/ha)',
         x = 'N Fert (kg/ha)',
         colour = "Region") +
    # scale_y_continuous(sec.axis = sec_axis(~./200, name = "Corn N leaching (kg/ha)", breaks = seq(30,80,5), labels = seq(30,80,5))) +
    # scale_linetype_manual(values = c("dashed", "solid"))+
    theme_bw()

plot_2 <- ggplot(data = n_uptake_dt) + 
    geom_line(aes(x = n_uptake_eonr, y = Y_corn, color = region, linetype = batch)) +
    # geom_line(aes(x = N_fert, y = L1_rel, linetype = "N Leaching", color = region)) +
    #geom_hline(yintercept = baselevel_yld, linetype = 'dashed', color = 'grey', size = 1)+
    # geom_vline(xintercept = baselevel_nfert, linetype = 'dashed', color = 'grey', size = 1)+
    labs(y = 'Corn Yield (kg/ha)',
         x = 'N Uptake (kg/ha)',
         colour = "Region") +
    # scale_y_continuous(sec.axis = sec_axis(~./200, name = "Corn N leaching (kg/ha)", breaks = seq(30,80,5), labels = seq(30,80,5))) +
    # scale_linetype_manual(values = c("dashed", "solid"))+
    theme_bw()
grid.arrange(plot_1, plot_2)


sample_n = two_batches_yc_dt[sample(1:nrow(two_batches_yc_dt),1)]
yield_curves_dt <- two_batches_yc_dt[id_10 == sample_n$id_10 & mukey == sample_n$mukey & z == sample_n$z]
yield_curves_dt[, batch := factor(batch)]

grid.arrange(ggplot(data = yield_curves_dt) + 
               geom_line(aes(x = N_fert, y = n_uptake, linetype = batch)),
             ggplot(data = yield_curves_dt) + 
               geom_line(aes(x = n_uptake, y = Y_corn, linetype = batch)))

yield_curves_dt[,Y_max := max(Y_corn), by = .(batch)]
yield_curves_dt[,Y_rel := Y_corn/Y_max]

Nuptake_95 <- yield_curves_dt[Y_rel >= 0.95, .SD[N_fert == min( N_fert)], by = batch][,.(batch, N_fert, Y_corn95 = Y_corn, Y_rel, n_uptake95 = n_uptake)]
Nuptake_40 <- yield_curves_dt[Y_rel >= 0.65, .SD[ N_fert == min( N_fert)], by = batch][,.(batch, N_fert, Y_rel, n_uptake40 = n_uptake)]

Nuptake_dt <- merge(Nuptake_95, Nuptake_40, by = c('batch'))
Nuptake_dt[,mrtn_rate := n_uptake95 - n_uptake40]
Nuptake_dt[]

# =========================================================================================================================================================
# CREATE THE REGIONAL MINIMUM MODEL - OK
Yld_response_threshold <- 0  
two_batches_yc_dt[, Yld_response := max(Y_corn) - min(Y_corn), by = .(batch, id_10, mukey,z)]
two_batches_yc_dt[, P := Y_corn * Pc - N_fert * Pn]  #update profits
TrainSet_RMM <- two_batches_yc_dt[Yld_response > Yld_response_threshold] #Needs to be here, to use updated profits 

two_batches_yc_dt[,.N, .(id_10, mukey, z)] %>% nrow() #trials before (all of them)
TrainSet_RMM[,.N, .(id_10, mukey, z)] %>% nrow()#trials after (whith response > threshold)

if(!regional_test){
  model_minimum_ok  <- aggregate_by_area(data_dt = TrainSet_RMM, variables = c('P'), 
                                         weight = 'area_ha', by_c = c('batch', 'region', 'N_fert')) %>% 
    .[, .SD[ P == max( P)], by = .(batch, region)] %>% .[,.(batch, region, eonr_pred = N_fert)]
}else{
  model_minimum_ok <- TrainSet_RMM[,.(P = mean(P)), by = .(batch, region, N_fert)] %>% 
    .[, .SD[ P == max( P)], by = .(batch, region)] %>% .[,.(batch, region, eonr_pred = N_fert)]
}

#North: lower than 148
#Central: higher than 148
model_minimum_ok[order(batch)]
#======================================================================================
#EONR frecuency
# yc_yearly_dt2 <- yc_yearly_dt#[batch %in% c(33,34)]
#Add 5 to half of the rates to avoid being right in the limit
two_batches_eonr_dt[, eonr := as.numeric(lapply(eonr, function(eonr) eonr+sample(-5:5,1)))]

# brks <- c(0,25,50,75,101,125,150,175,201,231,330)
brks <- seq(0,300, 28)
two_batches_eonr_dt[,bin:=findInterval(eonr, brks)]
# yc_region_eonr_dt[eonr==175]$eonr %>% table()
# ggplot(yc_region_eonr_dt, aes(bin     )) + geom_bar()

bins_table <- data.table(bin = 1:(length(brks)-1),
                         eonr_bin = paste(brks[-11], '-' , brks[-1], sep = ''))
bins_table[bin ==10, eonr_bin := '252+']
two_batches_eonr_dt <- merge(two_batches_eonr_dt, bins_table, by = 'bin')

two_batches_eonr_dt$eonr_bin <- factor(two_batches_eonr_dt$eonr_bin, levels = bins_table$eonr_bin)
two_batches_eonr_dt[,region := factor(region)]

(plot_1 <- ggplot(two_batches_eonr_dt, aes(eonr_bin , fill = region    )) + #geom_bar(aes(y = (..count..)/sum(..count..)))+
    geom_bar(aes( y=..count../tapply(..count.., ..fill.. ,sum)[..fill..]))+
    labs(x= 'N fert (kg/ha)', y = "relative frequencies")+
    theme_bw()+
    theme(legend.title =  element_blank(),
          axis.text=element_text(size=14))+
    facet_wrap(region~batch, 
               ncol =2 ,
               #scales="free",
               strip.position = "left"))

# ggsave(plot = grid.arrange(plot_1, plot_2, nrow = 1), 
#        filename = paste0("./n_policy_box/Data/batches_tests/", batch_n, "_eonr_plot.jpg"), width = 5, height = 5,
#        units = 'in')
#======================================================================================
# Explore
last_batch_eonr_dt <- two_batches_eonr_dt[batch == max(batch)]
last_batch_eonr_dt[,z := as.numeric(z)] 

ggplot(two_batches_eonr_dt) +
  geom_density(aes(x = n_deep_v5, color = factor(region), linetype = factor(batch)))

ggplot(last_batch_eonr_dt) + geom_boxplot(aes(x = factor(z), y = eonr))

ggplot(last_batch_eonr_dt) + geom_boxplot(aes(x = factor(z), y = Y_corn))+
  facet_wrap(region~., 
             ncol =1 ,
             #scales="free",
             strip.position = "left")

ggplot(last_batch_eonr_dt) + 
  geom_boxplot(aes(x = factor(region), y = Y_corn))

ggplot(last_batch_eonr_dt) + 
  geom_boxplot(aes(x = factor(region), y = n_deep_v5))



# Summarize all columns
# last_batch_eonr_dt[,region := as.numeric(region)]
cols_num <- sapply(last_batch_eonr_dt, is.numeric)
cols_num[names(cols_num)== 'region'] <- T
last_batch_eonr_dt2 <- last_batch_eonr_dt[,..cols_num]
last_batch_eonr_dt2[, lapply(.SD, mean, na.rm = T), by = region][order(region)]

#==================================================================================================================================
# Low yields
last_batch_eonr_dt[Y_corn < 5000]



# =========================================================================================================================================================
# LAI at v5
ggplot(last_batch_eonr_dt, aes(lai_v5 , fill = region    )) + #geom_bar(aes(y = (..count..)/sum(..count..)))+
  geom_histogram(aes( y=..count../tapply(..count.., ..fill.. ,sum)[..fill..]))+
  # labs(x= 'N fert (kg/ha)', y = "relative frequencies")+
  theme_bw()+
  theme(legend.title =  element_blank(),
        axis.text=element_text(size=14))+
  facet_wrap(region~., 
             ncol = 1,
             #scales="free",
             strip.position = "left")


# =========================================================================================================================================================
#Planting dates
dates_sowing_dt <- last_batch_eonr_dt[,.(eonr = mean(eonr),
                                         Y_corn = mean(Y_corn),
                                         .N), by = .(region, day_sow)][order(region, day_sow)]
dates_sowing_dt[,region := factor(region)]
dates_sowing_dt[]

plot_1 <- grid.arrange(
  ggplot(dates_sowing_dt, aes(x = day_sow, y = eonr, color = region)) +
    geom_path(),
  ggplot(dates_sowing_dt, aes(x = day_sow, y = Y_corn, color = region)) +
    geom_path())

yc_yearly_dt_eonr_dt[Y_soy == 0][,.N, region]
yc_yearly_dt_eonr_dt[Y_prev  == 0][,.N, region]
yc_yearly_dt_eonr_dt[Y_corn == 0][,.N, region]

# ----------------------------------------------------
# More about platning dates
tmp_dt <- readRDS("./n_policy_box/Data/initial_conditions_19/1212_1528968.rds")
dates_dt <- unique(tmp_dt[year == 2010,.(Date, day_sow = day)])
yc_yearly_dt_eonr_dt2 <- merge(yc_yearly_dt_eonr_dt, dates_dt, by = 'day_sow')

yc_yearly_dt_eonr_dt2[,region := factor(region)]

plot_2 <- ggplot(yc_yearly_dt_eonr_dt2[batch== 19], aes(Date, color = region)) + geom_bar()+facet_wrap(region~., 
                                                                                                       ncol = 1,
                                                                                                       #scales="free",
                                                                                                       strip.position = "left")

grid.arrange(plot_1, plot_2 ,nrow=1)


# ----------------------------------------------------
# grid10_soils_dt4 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt4.rds")
# grid10_soils_dt4 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt4.rds")
# 
# regions_dt4 <- data.table(grid10_soils_dt4) %>% .[,.(id_10, region_dt4 = region)] %>% unique()
# regions_dt5 <- data.table(grid10_soils_dt4) %>% .[,.(id_10, region_dt5 = region)] %>% unique()
# regions_comp <- merge(regions_dt4, regions_dt5, by = 'id_10')
# regions_comp[region_dt4 != region_dt5]
# 
# wrong_region_ids <- regions_comp[region_dt4 != region_dt5]$id_10
# saveRDS(wrong_region_ids, "./n_policy_box/Data/files_rds/wrong_region_ids.rds")



# =========================================================================================================================================================
# INITIAL CONDITIONS 
#MERGE FILES
multiple_files <- list.files("./n_policy_box/Data/initial_conditions_98_swat", full.names = T)
# multiple_files <- list.files('S:/Bioinformatics Lab/germanm2/n_policy_box_58/Data/initial_conditions_58', full.names = T)
# multiple_files <- multiple_files[sample(x = 1:7735,size = 300, replace = F)]
length(multiple_files)

# registerDoParallel(detectCores()*0.5) # register the cluster
# registerDoParallel(cores = 10)
output_list = foreach(file_n = multiple_files, .combine = "c", .packages = c("data.table")) %do% {
  # file_n <- multiple_files[1]
  tmp_dt <- readRDS(file_n)
  # cols <- names(tmp_dt)[1:91]
  cols <- c("sim_name", "id_10", "mukey", "z", "n_deep", 'surfaceom_c', 'surfaceom_n', 'surfaceom_wt')
  # tmp_dt <- rbind(tmp_dt[year == 2001 & day == 1 & !is.na(Y), c(cols), with = F], tmp_dt[year == 2010 & month == 12 & day == 365, c(cols), with = F])
  tmp_dt <- tmp_dt[year == 2009 & month == 12 & day == 365, c(cols), with = F]
  # tmp_dt <- tmp_dt[year == 2001 & month == 1 & day == 1 & !is.na(Y)]
  
  list(tmp_dt)
}#end of dopar loop

stopImplicitCluster()
initial_conditions_dt <- rbindlist(output_list)

#Add regions
grid10_soils_dt4 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt4.rds")
regions_dt <- data.table(grid10_soils_dt4) %>% .[,.N,.(id_10, region)] %>% .[,-'N']
regions_dt[region == 1,region_lab := '1-South']
regions_dt[region == 2,region_lab := '2-Central']
regions_dt[region == 3,region_lab := '3-North']

regions_dt[,region := factor(region_lab)]
regions_dt[,region_lab := NULL]
sapply(regions_dt, class)
regions_dt[,id_10 := as.character(id_10)]
initial_conditions_dt <- merge(initial_conditions_dt, regions_dt, by = 'id_10')

initial_conditions_dt[oc_8 > 10]
# Summarize all columns

cols_num <- sapply(initial_conditions_dt, is.numeric)
cols_num[names(cols_num)=='region'] <- TRUE
initial_conditions_dt2 <- initial_conditions_dt[,..cols_num]
initial_conditions_dt2[,surfaceom_cn :=surfaceom_c/surfaceom_n]
initial_conditions_dt2[, lapply(.SD, mean), by = region][order(region)]


#Beautiful density plot
initial_conditions_dt[,region := factor(region)]


ggplot(initial_conditions_dt[n_deep < 100]) +
  geom_density(aes(x = n_deep, color = region))

ggplot(initial_conditions_dt[n_deep < 100]) + 
  geom_boxplot(aes(x = factor(region), y = n_deep))

ggplot(yc_region_eonr_dt2) + 
  geom_boxplot(aes(x = factor(region), y = n_deep_v5))

ggplot(yc_region_eonr_dt2[n_deep_v5 < 100]) +
  geom_density(aes(x = n_deep_v5, color = region))

#---------------------
# Correct n_deep based on initial n_deep
initial_conditions_dt[,.(n_deep = mean(n_deep)), region]

n_deep_target <- c(5,45,100)
n_deep_initial <- c(34,54,56)
n_deep_shift <- n_deep_target - n_deep_initial
n_deep_shift_dt <- data.table(region = 1:3, 
                              shift = n_deep_shift)

initial_conditions_dt2 <- merge(initial_conditions_dt, n_deep_shift_dt, by = 'region')
initial_conditions_dt2[,n_deep2 := n_deep + shift]
initial_conditions_dt2[region == 1 & n_deep2 > 80, n_deep2 := sample(60,100, 1) ]

# yc_region_eonr_dt[, eonr := as.numeric(lapply(eonr, function(eonr) eonr+sample(-5:5,1)))]
initial_conditions_dt2[ n_deep2 > 100, n_deep2 := as.numeric(lapply(n_deep2, function(n_deep2) sample(60:80,1)))]
initial_conditions_dt2[ n_deep2 < 0, n_deep2 := as.numeric(lapply(n_deep2, function(n_deep2) sample(1:10,1)))]

ggplot(initial_conditions_dt2[n_deep < 100]) +
  geom_density(aes(x = n_deep2, color = region, linetype = 'solid'))+
  geom_density(aes(x = n_deep, color = region, linetype = 'dashed'))

initial_conditions_dt2$n_deep2
newrow <- sample(100, size = nrow(data), replace = TRUE)


#---------------------
# Are low initial N asociated with high EONR?
yc_region_eonr_dt2[,id_10 := as.character(id_10)]
yc_region_eonr_dt2[,z := as.character(z)]
yc_region_eonr_dt3 <- merge(yc_region_eonr_dt2, initial_conditions_dt[,.(id_10, mukey, z, n_deep_ini = n_deep)], by = c('id_10', 'mukey', 'z'))
yc_region_eonr_dt3[,region := factor(region)]

ggplot(data=yc_region_eonr_dt3, aes(x = n_deep_ini, y = eonr, color = region)) +
  geom_point()+ theme(aspect.ratio=1)+ #+ coord_fixed() + geom_abline() + ylim(0, 100)+ xlim(0, 14000)+
  geom_smooth()+
  facet_wrap(region~., 
             ncol = 1,
             #scales="free",
             strip.position = "left")
yc_region_eonr_dt3[n_deep_ini < 30]
hist(yc_region_eonr_dt3[region == 3]$n_deep_ini)
yc_region_eonr_dt3[,.(n_deep_ini = mean(n_deep_ini)), by = region]
# =========================================================================================================================================================
# YC OUTPUT EVALUATION
#MERGE FILES
multiple_files <- list.files("./n_policy_box/Data/yc_output_140_swat", full.names = T)

length(multiple_files)

# registerDoParallel(detectCores()*0.5) # register the cluster
# registerDoParallel(cores = 10)
output_list = foreach(file_n = multiple_files, .combine = "c", .packages = c("data.table")) %do% {
  # file_n <- multiple_files[1]
  tmp_dt <- readRDS(file_n)
  names(tmp_dt)
  tmp_dt2 <- tmp_dt[year == 2010 & day == 1 & !is.na(Y)]
  tmp_dt2[,N_fert := lapply(strsplit(as.character(sim_name), split="_"), "[", 5) ]
  tmp_dt3 <- tmp_dt2[N_fert == 0]
  # table(tmp_dt$z)
  # names(tmp_dt)
  # cols <- names(tmp_dt)[1:82]
  # tmp_dt <- rbind(tmp_dt[year == 2001 & day == 1 & !is.na(Y), c(cols), with = F], tmp_dt[year == 2010 & month == 12 & day == 365, c(cols), with = F])
  # tmp_dt <- tmp_dt[year == 2010 & month == 1 & day == 1 & Y == 0][, .SD[1], by = .(id_10, mukey, z)]
  list(tmp_dt3)
  
  
}#end of dopar loop

# stopImplicitCluster()

yc_output_dt <- rbindlist(output_list)
#Add regions
grid10_soils_dt4 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt4.rds")
regions_dt <- data.table(grid10_soils_dt4) %>% .[,.N,.(id_10, region)] %>% .[,-'N']
regions_dt[region == 1,region_lab := '1-South']
regions_dt[region == 2,region_lab := '2-Central']
regions_dt[region == 3,region_lab := '3-North']

regions_dt[,region := factor(region_lab)]
regions_dt[,region_lab := NULL]
sapply(regions_dt, class)
regions_dt[,id_10 := as.character(id_10)]
yc_output_dt <- merge(yc_output_dt, regions_dt, by = 'id_10')

# Summarize all columns

cols_num <- sapply(yc_output_dt, is.numeric)
cols_num[names(cols_num)=='region'] <- TRUE
yc_output_dt2 <- yc_output_dt[,..cols_num]
yc_output_dt3 <- yc_output_dt2[, lapply(.SD, mean), by = region][order(region)]
yc_output_dt3[order(-region)]
yc_output_dt2$n_deep
#Beautiful density plot
yc_output_dt[,region := factor(region)]
yc_output_dt[,n_deep := as.numeric(n_deep)]
yc_output_dt[,.(n_deep = mean(n_deep)), by = region]

ggplot(data = yc_output_dt) +
  geom_density(aes(x = n_deep, color = region)) #

# grid10_horizons_v1_dt <- readRDS("./n_policy_box/Data/Grid/average_regions_soils_dt.rds")

#Work with variables that are layers

variables_layers <- names(yc_output_dt3)[grepl('_[0-9]+$', names(yc_output_dt3))]

layers_numbers <- sort(as.numeric(unique(gsub(pattern = '_', replacement = '', 
                                              stringr::str_extract_all(string = variables_layers, pattern = '_[0-9]+$', simplify = TRUE)))))

variables_layers_unique <- c("oc", "biom_c", "hum_c", "inert_c", "no3", "nh4", "sw")

output_list = foreach(region_n = unique(yc_output_dt3$region), .combine = "c") %do% {
  # region_n = unique(yc_output_dt3$region)[1]
  horizons_dt <- data.table(region = region_n,
                            layer = layers_numbers)
  for( var_n in variables_layers_unique){
    # var_n = 'oc'
    var_n_seq <- paste(var_n, layers_numbers, sep = '_')
    
    horizons_dt <- cbind(horizons_dt, data.table(t(yc_output_dt3[region == region_n, var_n_seq, with=FALSE]))) %>% 
      setnames(., 'V1', var_n)
  } #end var_n loop
  
  list(horizons_dt)
  
}#end region loop
horizons_dt <- rbindlist(output_list)

# horizons_dt[,names(horizons_dt):= lapply(.SD, as.numeric), .SDcols = names(horizons_dt)]

horizons_dt[,Finert := round(inert_c/(hum_c + biom_c),4)]
horizons_dt[,Fbiom := round(biom_c/(hum_c-inert_c),4)]
horizons_dt[,Finert := ifelse(oc == 0 & Finert == 0, 1, Finert)]  #correction for deep layers
horizons_dt[,Fbiom := ifelse(Fbiom > 1 | Finert == 1, 0, Fbiom)]  #correction for deep layers
horizons_dt[]



# =========================================================================================================================================================
# Compare batches

# first_batch_dt <- yc_yearly_dt2
# second_batch_dt <- yc_yearly_dt2


first_batch_dt <- readRDS("./n_policy_box/Data/files_rds/yc_yearly_dt_batch6.rds")
second_batch_dt <- readRDS("./n_policy_box/Data/files_rds/yc_yearly_dt_batch7.rds")
first_batch_dt <- first_batch_dt[id_10 %in% second_batch_dt$id_10]
second_batch_dt <- second_batch_dt[id_10 %in% first_batch_dt$id_10]

first_batch_dt[n_deep_v5 > 100]
second_batch_dt[n_deep_v5 > 100]

first_batch_dt[, P := Y_corn * Pc - N_fert * Pn]  #update profits
first_batch_dt_eonr_dt <- first_batch_dt[, .SD[ P == max( P)], by = .(id_10, mukey, z)] %>%
  .[, .SD[ N_fert == min( N_fert )], by = .(id_10, mukey, z)]
setnames(first_batch_dt_eonr_dt, 'N_fert', 'eonr')

second_batch_dt[, P := Y_corn * Pc - N_fert * Pn]  #update profits
second_batch_dt_dt_eonr_dt <- second_batch_dt[, .SD[ P == max( P)], by = .(id_10, mukey, z)] %>%
  .[, .SD[ N_fert == min( N_fert )], by = .(id_10, mukey, z)]
setnames(second_batch_dt_dt_eonr_dt, 'N_fert', 'eonr')

batch_comp_dt <- merge(first_batch_dt_eonr_dt[,.(region, id_10, mukey, z, Y_corn, eonr, n_deep_v5,biomass_n_v5)],
                       second_batch_dt_dt_eonr_dt[,.(region, id_10, mukey, z, Y_corn, eonr, n_deep_v5,biomass_n_v5)], by = c('region', 'id_10', 'mukey', 'z'), suffixes = c("_1", "_2"))


# Pred vs obs plot  
batch_comp_dt[,region := factor(region)]

ggplot(data=batch_comp_dt, aes(x = eonr_1, y = eonr_2, color = region)) +
  geom_point()+ theme(aspect.ratio=1) + coord_fixed() + geom_abline() + ylim(0, 330)+ xlim(0, 330)+
  facet_wrap(region~., 
             ncol = 1,
             #scales="free",
             strip.position = "left")

ggplot(data=batch_comp_dt, aes(x = Y_corn_1, y = Y_corn_2, color = region)) +
  geom_point()+ theme(aspect.ratio=1) + coord_fixed() + geom_abline() + ylim(0, 14000)+ xlim(0, 14000)+
  facet_wrap(region~., 
             ncol = 1,
             #scales="free",
             strip.position = "left")

ggplot(data=batch_comp_dt, aes(x = n_deep_v5_1, y = n_deep_v5_2, color = region)) +
  geom_point()+ theme(aspect.ratio=1) + coord_fixed() + geom_abline() + ylim(0, 100)+ xlim(0, 100)+
  facet_wrap(region~., 
             ncol = 1,
             #scales="free",
             strip.position = "left")

batch_comp_dt[,.(eonr_1 = mean(eonr_1),
                 eonr_2 = mean(eonr_2)), by = region]      

batch_comp_dt[,.(biomass_n_v5_1 = mean(biomass_n_v5_1),
                 biomass_n_v5_2 = mean(biomass_n_v5_2)), by = region]    

batch_comp_dt[,.(n_deep_v5_1 = mean(n_deep_v5_1, na.rm = T),
                 n_deep_v5_2 = mean(n_deep_v5_2, na.rm = T)), by = region] 

# =========================================================================================================================================================
# Time tracked

multiple_files <- list.files("./n_policy_box/Data/time_track_50", full.names = T)
print(length(multiple_files))

time_track_dt <- rbindlist(lapply(multiple_files, function(file_n) readRDS(file_n)))
time_track_dt[,cell := ceiling(cell)+3]
time_track_dt <- time_track_dt[order(cell)][,.(id_10, dur=cell)]

write.table(time_track_dt, './n_policy_git/id_10_walltime_test.txt', row.names = F, col.names = F)

# =========================================================================================================================================================
#Missing files
multiple_files <- list.files(paste0("./n_policy_box/Data/yc_output_summary_", batch_n, "_swat"), full.names = F)
multiple_files_dt <- data.table(file_name = gsub(pattern = '.rds', replacement = '', x = multiple_files), run = '1')


grid10_soils_dt4 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt4.rds")
files_correct_dt <- grid10_soils_dt4[,.N, by = .(id_10, mukey)] %>% .[,file_name := paste0(id_10, '_', mukey)]
files_correct_dt <- merge(files_correct_dt, multiple_files_dt, by = 'file_name', all.x=T)
files_correct_dt[is.na(run),run := 0]
files_correct_dt[id_10 == 1078]
