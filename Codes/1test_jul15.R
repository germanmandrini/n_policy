rm(list=ls())

# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# codes_folder <-'C:/Users/germanm2/Documents'#CPSC

setwd('~')#Server
codes_folder <-'~' #Server


source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')
source(paste0(codes_folder, '/n_policy_git/Codes/parameters.R'))

#======================================================================================
#Merge all files
# batch_n = 19
yc_yearly_batches_dt  <- data.table()
for(batch_n in c( 42)){
  print(batch_n)
  multiple_files <- list.files(paste0("./n_policy_box/Data/yc_output_summary_", batch_n), full.names = T)
  length(multiple_files)

  yc_yearly_list <- list()
  for(file_n in multiple_files){
    yc_yearly_list[[length(yc_yearly_list)+1]] <- readRDS(file_n)
  }
  
  yc_yearly_dt <- rbindlist(yc_yearly_list)
  yc_yearly_dt[,id_10 := as.integer(id_10)]
  
  yc_yearly_dt[,.N, by = .(id_10, mukey, z)]$N %>% table() #of rates by mukey z, has to be 33
  yc_yearly_dt[,.N, by = .(id_10, mukey, z)][,.N, by = .(id_10, mukey)]$N %>% table()#of z by mukey, has to be 30 or 15
  
  setnames(yc_yearly_dt, 
           c('Yld', 'Yld_soy', 'leach_1', 'leach_2' ),
           c('Y_corn', 'Y_soy', 'L1', 'L2'))
  
  yc_yearly_dt[,L := L1 + L2]
  # yc_yearly_dt[id_10 == 1426, region  := 3]
  # yc_yearly_dt[id_10 == 765, region  := 2]
  # yc_yearly_dt[id_10 == 79, region  := 1]
  
  #Add regions and areas
  grid10_soils_dt4 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt4.rds")
  areas_dt <- data.table(grid10_soils_dt4) %>% .[,.(area_ha = sum(area_ha)), by = .(id_10, mukey, region)]
  
  yc_yearly_dt <- merge(yc_yearly_dt, areas_dt, by = c('id_10', 'mukey'))
  yc_yearly_dt[,.N, .(region, id_10)][,.N, region]
  yc_yearly_dt[,batch := batch_n]
  
  saveRDS(yc_yearly_dt, paste0("./n_policy_box/Data/files_rds/yc_yearly_dt_batch", batch_n, ".rds"))
  yc_yearly_batches_dt <- rbind(yc_yearly_batches_dt, yc_yearly_dt)
}
yc_yearly_batches_dt <- yc_yearly_batches_dt[id_10 %in% c(1212, 1426)]
batch_n = 18
yc_yearly_dt <- readRDS( paste0("./n_policy_box/Data/files_rds/yc_yearly_dt_batch", batch_n, ".rds"))
#======================================================================================
#Relative yield plot
yc_yearly_dt <- yc_yearly_batches_dt
state_agg_dt  <- aggregate_by_area(data_dt = yc_yearly_dt, variables = c('Y_corn', 'Y_soy', 'L1','L2'), 
                                    weight = 'area_ha', by_c = c('N_fert', 'region', 'batch'))# %>% .[,-'area_ha']
# yc_yearly_dt <- yc_yearly_batches_dt[batch %in% c(13,17)]
state_agg_dt  <- yc_yearly_dt[,.(Y_corn = mean(Y_corn),
                                 L = mean(L)), by = .(N_fert, region, batch)][order(region)]

# state_agg_dt <- state_agg_dt[N_fert < 251]
state_agg_dt[,Y_max := max(Y_corn), by = .(region, batch)]
state_agg_dt[,Y_rel := Y_corn/Y_max]
state_agg_dt[,region := factor(region)]
state_agg_dt[N_fert == 0]
# state_agg_dt[,L1_rel := L1/max(L1), by = region]
# state_agg_dt[,L2_rel := L2/max(L2), by = region]
state_agg_dt[,batch := factor(batch)]
(plot_1 <- ggplot(data = state_agg_dt) + 
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


(plot_2 <- ggplot(data = state_agg_dt) + 
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

ggsave(plot = grid.arrange(plot_1, plot_2, nrow = 1), 
       filename = paste0("./n_policy_box/Data/batches_tests/", batch_n, "_yield_plot.jpg"), width = 979/300*3, height = 1042/300*3,
       units = 'in')

#======================================================================================
#EONR frecuency
yc_yearly_dt2 <- yc_yearly_dt#[batch %in% c(33,34)]
yc_yearly_dt2[, P := Y_corn * Pc - N_fert * Pn]  #update profits
yc_region_eonr_dt <- yc_yearly_dt2[, .SD[ P == max( P)], by = .(batch, id_10, mukey, z)] %>% .[, .SD[ N_fert == min( N_fert )], by = .(batch, id_10, mukey, z)]
setnames(yc_region_eonr_dt, 'N_fert', 'eonr')
#Add 5 to half of the rates to avoid being right in the limit
# yc_region_eonr_dt[, eonr2 := as.numeric(lapply(eonr, function(eonr) eonr+sample(-5:5,1)))]

# brks <- c(0,25,50,75,101,125,150,175,201,231,330)
brks <- c(0,25,50,75,100,125,150,175,200,225,330)
yc_region_eonr_dt[,bin:=findInterval(eonr, brks)]
yc_region_eonr_dt[eonr==175]$eonr %>% table()
# ggplot(yc_region_eonr_dt, aes(bin     )) + geom_bar()

bins_table <- data.table(bin = 1:(length(brks)-1),
                         eonr_bin = paste(brks[-11], '-' , brks[-1], sep = ''))
bins_table[bin ==10, eonr_bin := '225+']
yc_region_eonr_dt2 <- merge(yc_region_eonr_dt, bins_table, by = 'bin')

str(yc_region_eonr_dt)

yc_region_eonr_dt2$eonr_bin <- factor(yc_region_eonr_dt2$eonr_bin, levels = bins_table$eonr_bin)

(plot_1 <- ggplot(yc_region_eonr_dt2, aes(eonr_bin     )) + geom_bar(aes(y = (..count..)/sum(..count..)))+
  labs(x= 'N fert (kg/ha)', y = "relative frequencies")+
  theme_bw()+
  theme(legend.title =  element_blank(),
        axis.text=element_text(size=14))+
  facet_wrap(region~batch, 
             ncol = 1,
             #scales="free",
             strip.position = "left"))

ggsave(plot = grid.arrange(plot_1, plot_2, nrow = 1), 
       filename = paste0("./n_policy_box/Data/batches_tests/", batch_n, "_eonr_plot.jpg"), width = 5, height = 5,
       units = 'in')

yc_region_eonr_dt2[,z := as.numeric(z)]  
ggplot(yc_region_eonr_dt2) + geom_boxplot(aes(x = factor(z), y = eonr))

yc_region_eonr_dt2[region == 1 & eonr > 225]

# Summarize all columns

cols_num <- sapply(yc_region_eonr_dt2, is.numeric)
yc_region_eonr_dt3 <- yc_region_eonr_dt2[,..cols_num]
yc_region_eonr_dt3[, lapply(.SD, mean, na.rm = T), by = region]

# =========================================================================================================================================================
#Planting dates
yc_yearly_dt[, P := Y_corn * Pc - N_fert * Pn]  #update profits
yc_yearly_dt_eonr_dt <- yc_yearly_dt[, .SD[ P == max( P)], by = .(id_10, mukey, z)] %>%
  .[, .SD[ N_fert == min( N_fert )], by = .(batch, id_10, mukey, z)]
setnames(yc_yearly_dt_eonr_dt, 'N_fert', 'eonr')

dates_sowing_dt <- yc_yearly_dt_eonr_dt[,.(eonr = mean(eonr),
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
yc_yearly_dt_eonr_dt[Yld_prev  == 0][,.N, region]
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
# CREATE THE REGIONAL MINIMUM MODEL - OK
Yld_response_threshold <- 2000  
yc_yearly_dt2[, Yld_response := max(Y_corn) - min(Y_corn), by = .(batch, id_10, mukey,z)]
yc_yearly_dt2[, P := Y_corn * Pc - N_fert * Pn]  #update profits
TrainSet_RMM <- yc_yearly_dt2[Yld_response > Yld_response_threshold] #Needs to be here, to use updated profits 

TrainSet2[,.N, .(id_10, mukey, z)] %>% nrow() #trials before (all of them)
TrainSet_RMM[,.N, .(id_10, mukey, z)] %>% nrow()#trials after (whith response > threshold)

model_minimum_ok  <- aggregate_by_area(data_dt = TrainSet_RMM, variables = c('P'), 
                                       weight = 'area_ha', by_c = c('batch', 'region', 'N_fert')) %>% 
  .[, .SD[ P == max( P)], by = .(batch, region)] %>% .[,.(batch, region, eonr_pred = N_fert)]

model_minimum_ok[]

# =========================================================================================================================================================
# INITIAL CONDITIONS 
#MERGE FILES
multiple_files <- list.files("./n_policy_box/Data/initial_conditions_7", full.names = T)
length(multiple_files)

files_list <- list()
for(file_n in multiple_files){
  # file_n <- multiple_files[1]
  tmp_dt <- readRDS(file_n)
  cols <- names(tmp_dt)[1:82]
  # tmp_dt <- rbind(tmp_dt[year == 2001 & day == 1 & !is.na(Y), c(cols), with = F], tmp_dt[year == 2010 & month == 12 & day == 365, c(cols), with = F])
  tmp_dt <- tmp_dt[year == 2010 & month == 1 & day == 1 & n_deep >= 0, c(cols), with = F]
  files_list[[length(files_list)+1]] <- tmp_dt
}
initial_conditions_dt <- rbindlist(files_list)

#Add regions
grid10_soils_dt4 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt4.rds")
regions_dt <- data.table(grid10_soils_dt4) %>% .[,.N,.(id_10, region)] %>% .[,-'N']
sapply(regions_dt, class)
regions_dt[,id_10 := as.character(id_10)]
initial_conditions_dt <- merge(initial_conditions_dt, regions_dt, by = 'id_10')

# Summarize all columns

cols_num <- sapply(initial_conditions_dt, is.numeric)
initial_conditions_dt2 <- initial_conditions_dt[,..cols_num]
initial_conditions_dt2[, lapply(.SD, mean), by = region][order(region)]

#Beautiful density plot
initial_conditions_dt[,region := factor(region)]
ggplot(initial_conditions_dt[n_deep < 100]) +
  geom_density(aes(x = n_deep, color = region))

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
multiple_files <- list.files("./n_policy_box/Data/yc_output_23", full.names = T)
# multiple_files <- multiple_files[sample(1:length(multiple_files), 200)]
# length(multiple_files)

files_list <- list()
for(file_n in multiple_files){
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
  files_list[[length(files_list)+1]] <- tmp_dt3
}
yc_output_dt23 <- rbindlist(files_list)

merge(yc_output_dt22[,.(sim_name, id_10, mukey, z, Date, surfaceom_wt, root_wt)], 
      yc_output_dt23[,.(sim_name, id_10, mukey, z, Date, surfaceom_wt, root_wt)], 
      by = c('sim_name', 'id_10', 'mukey', 'z', 'Date'), suffixes = c("_22", "_23"))
yc_output_dt <- rbind(yc_output_dt, yc_output_dt2)

#Add regions
grid10_soils_dt4 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt4.rds")
regions_dt <- data.table(grid10_soils_dt4) %>% .[,.N,.(id_10, region)] %>% .[,-'N']
sapply(regions_dt, class)
regions_dt[,id_10 := as.character(id_10)]
yc_output_dt <- merge(yc_output_dt, regions_dt, by = 'id_10')
yc_output_dt[,.(soilt10d = mean(soilt10d)), by = .(region, z, day)]

saveRDS(yc_output_dt, "./n_policy_box/Data/files_rds/yc_output_dt_batch7.rds")
yc_output_dt <- readRDS("./n_policy_box/Data/files_rds/yc_output_dt_batch7.rds")
# Summarize all columns
summary(yc_output_dt$n_deep)

cols_num <- sapply(yc_output_dt, is.numeric)
yc_output_dt2 <- yc_output_dt[,..cols_num]
yc_output_dt2[, lapply(.SD, mean), by = region][order(region)]

#Beautiful density plot
yc_output_dt2[,region := factor(region)]
ggplot(yc_output_dt2) +
  geom_density(aes(x = n_deep, color = region))

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
