rm(list=ls())

setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
codes_folder <-'C:/Users/germanm2/Documents'#CPSC

# setwd('~')#Server
# codes_folder <-'~' #Server

# library("foreach")
# library("doParallel")

source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')
source(paste0(codes_folder, '/n_policy_git/Codes/parameters.R'))

# YC OUTPUT EVALUATION
#MERGE FILES
multiple_files <- list.files("./n_policy_box/Data/yc_output_92_swat", full.names = T)

length(multiple_files)

# registerDoParallel(detectCores()*0.5) # register the cluster
# registerDoParallel(cores = 10)
output_list = foreach(file_n = multiple_files, .combine = "c", .packages = c("data.table")) %do% {
  # file_n <- multiple_files[1]
  tmp_dt <- readRDS(file_n)
  names(tmp_dt)
  nrow(tmp_dt)
  tmp_dt <- tmp_dt[year == 2010 ]
  table(tmp_dt$fertiliser)
  tmp_dt[,N_fert := as.numeric(lapply(strsplit(sim_name, split="_"), "[", 6)) ]
  tmp_dt <- tmp_dt[N_fert == max(N_fert) ]
  #remove soy columns
  tmp_dt <- tmp_dt[,!str_detect(names(tmp_dt), 'paddock.soybean'), with = F]
  names(tmp_dt) <- str_replace(names(tmp_dt), pattern = 'paddock.maize.', replacement = '')
  names(tmp_dt) 
  ggplot(tmp_dt[stage_name %in% 'flowering']) + geom_point(aes(x = MaxT, y = temp_stress))
  
  tmp_dt2 <- tmp_dt[stage %in% c(7,8),.(stage = min(stage),
            swdef_expan = mean(swdef_expan),
            swdef_pheno = mean(swdef_pheno),
            swdef_photo = mean(swdef_photo),
            water_table = mean(water_table),
            n_total_uptake = sum(n_total_uptake),
            root_depth = mean(root_depth),
            temp_stress = mean(temp_stress),
            Rain = mean(Rain),
            T_mean = (mean(MaxT)+mean(MinT)/2),
            Radn = mean(Radn),
            lai = mean(lai),
            RUE = mean(RUE),
            DeltaGreenWt = sum(DeltaGreenWt),
            day = min(day)), by =.(id_10)] 
  
  #Tables like in summary file
  
  # table(tmp_dt$z)
  # names(tmp_dt)
  # cols <- names(tmp_dt)[1:82]
  # tmp_dt <- rbind(tmp_dt[year == 2001 & day == 1 & !is.na(Y), c(cols), with = F], tmp_dt[year == 2010 & month == 12 & day == 365, c(cols), with = F])
  # tmp_dt <- tmp_dt[year == 2010 & month == 1 & day == 1 & Y == 0][, .SD[1], by = .(id_10, mukey, z)]
  list(tmp_dt2)
  
  
}#end of dopar loop

stopImplicitCluster()

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
yc_output_dt[order(-region)]

yc_output_dt[oc_8 > 10]
# Summarize all columns

cols_num <- sapply(yc_output_dt, is.numeric)
cols_num[names(cols_num)=='region'] <- TRUE
yc_output_dt2 <- yc_output_dt[,..cols_num]
yc_output_dt2[, lapply(.SD, mean), by = region][order(region)]



#Beautiful density plot
yc_output_dt2[,region := factor(region)]
ggplot(yc_output_dt2) +
  geom_density(aes(x = n_deep, color = region))

grid10_horizons_v1_dt <- readRDS("./n_policy_box/Data/Grid/average_regions_soils_dt.rds")
