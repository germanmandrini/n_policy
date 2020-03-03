setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
# setwd('~')

source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')
source('./vr_value_v2/Codes/parameters.R')
# source('./Codes_useful/gm_functions.R')
yc_yearly_dt3 <- readRDS("./vr_value_v2/Data/files_rds/yc_yearly_dt3.rds")
#======================================================================================
# Add sensor information
multiple_files <- list.files("C:/Users/germa/Dropbox/yc_output_summary_2", full.names = T)
multiple_files <- list.files("./vr_value_v2/Data/yc_output_summary_2", full.names = T)
length(multiple_files)

yc_yearly_list <- list()
for(file_n in multiple_files){
  yc_yearly_list[[length(yc_yearly_list)+1]] <- readRDS(file_n)
}

length(yc_yearly_list)
sensor_dt <- rbindlist(yc_yearly_list)
sensor_dt[,id_10 := as.integer(id_10)]
sensor_dt[,treat := as.character(lapply(strsplit(as.character(sim_name), split="_"), "[", 5))]
sensor_dt2 <- sensor_dt[,.(treat, id_10, z, mukey, Yld, biomass_n_v5,
             green_biomass_n_v5, greenn_v5, leafgreennconc_v5, leafgreenn_v5)]

sensor_dt3 <- dcast(sensor_dt2, id_10 + z + mukey ~ treat, value.var = c('Yld','biomass_n_v5', 'green_biomass_n_v5', 
                                                           'greenn_v5', 'leafgreennconc_v5', 'leafgreenn_v5'))

sensor_dt3[,Yld_df := Yld_Nrich - Yld_Nminus]
sensor_dt3[,biomass_n_df := biomass_n_v5_Nrich - biomass_n_v5_Nminus]
sensor_dt3[,green_biomass_n_df := green_biomass_n_v5_Nrich - green_biomass_n_v5_Nminus]
sensor_dt3[,greenn_df := greenn_v5_Nrich - greenn_v5_Nminus]
sensor_dt3[,leafgreennconc_df := leafgreennconc_v5_Nrich - leafgreennconc_v5_Nminus]
sensor_dt3[,leafgreenn_df := leafgreenn_v5_Nrich - leafgreenn_v5_Nminus]

# CHECK A REGRESSION
col_names <- c("biomass_n_v5_Nminus", "biomass_n_v5_Nrich", "green_biomass_n_v5_Nminus",
               "green_biomass_n_v5_Nrich", "greenn_v5_Nminus", "greenn_v5_Nrich", "leafgreennconc_v5_Nminus", 
               "leafgreennconc_v5_Nrich", "leafgreenn_v5_Nminus", "leafgreenn_v5_Nrich", 
               "biomass_n_df", "green_biomass_n_df", "greenn_df", "leafgreennconc_df", "leafgreenn_df")

reg <- lm(data = sensor_dt3, formula = as.formula(paste("Yld_df ~ ", paste(col_names, collapse = ' + '))))
summary(reg)
lapply(yc_yearly_dt3, class)
lapply(sensor_dt3, class)

yc_yearly_dt3 <- merge(yc_yearly_dt3, sensor_dt3[,-c('Yld_Nminus', 'Yld_Nrich')], by = c('id_10', 'mukey', 'z'))

#===========================================================================================================================================================
library(caret)
# library(knitr)     # just using this for kable() to make pretty tables
## STANDARIZE DATA ========
# Standarize the Training set ---------
# calculate the pre-process parameters from the dataset
# preprocessParams <- preProcess(TrainSet_eonr2[,-c('eonr', 'region')], method=c("scale"))
yc_yearly_dt3[, P := Yld * Pc - N_fert * Pn]
yc_yearly_dt3[, n_0_60cm_v5 := n_20cm_v5 + n_40cm_v5 + n_60cm_v5]

sample_for_sandarization <- yc_yearly_dt3[N_fert == 160]




# preprocessParams <- preProcess(sample_for_sandarization[, -c('region', 'mukey', 'id_10', 'z','end_crop', 'N_fert', 'P', 'Yld', 
#                                                              'N', 'stages_cnt', 'day_sow', 'sand_40cm', 'clay_40cm')], 
#                                method=c("range"), rangeBounds = c(-1,1))

preprocessParams <- preProcess(sample_for_sandarization[, -c('region', 'mukey', 'id_10', 'z','end_crop', 'N_fert', 'P', 'Yld',
                                                             'N', 'stages_cnt', 'day_sow', 'sand_40cm', 'clay_40cm')],
                               method=c("scale"))

# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
yc_yearly_dt_trf <- predict(preprocessParams, yc_yearly_dt3)
yc_yearly_dt_trf <- yc_yearly_dt_trf[, -c('region','end_crop', 'P', 'Yld', 'N', 'stages_cnt', 'day_sow', 'sand_40cm', 'clay_40cm')]

col_orig <- c("dul_dep", "ll15_dep", "whc","Yld_prev", "leach_n", "leach_n2", "n_top15_delta",
  "sw_dep_v5","biomass_v5","lai_v5","oc_20cm_v5","oc_40cm_v5", "n_20cm_v5","n_40cm_v5","n_60cm_v5","n_deep_v5","esw_pct_v5",
  "biomass_n_v5","green_biomass_n_v5","greenn_v5","leafgreennconc_v5","leafgreenn_v5","LAI_max","rain_30","rain_60","rain_90","t_max_30", 
  "t_max_60","t_max_90","t_min_30","t_min_60","t_min_90", "Yld_lt_avg","Yld_lt_min","Yld_lt_max","biomass_n_v5_Nminus","biomass_n_v5_Nrich",       
  "green_biomass_n_v5_Nminus", "green_biomass_n_v5_Nrich","greenn_v5_Nminus","greenn_v5_Nrich","leafgreennconc_v5_Nminus", 
  "leafgreennconc_v5_Nrich","leafgreenn_v5_Nminus","leafgreenn_v5_Nrich","Yld_df","biomass_n_df",             
  "green_biomass_n_df","greenn_df","leafgreennconc_df","leafgreenn_df","n_0_60cm_v5")

col_trf <- paste(col_orig, '_t', sep = '')
setnames(yc_yearly_dt_trf, col_orig, col_trf)

yc_yearly_dt4 <- merge(yc_yearly_dt3, yc_yearly_dt_trf, by = c('id_10', 'mukey', 'z', 'N_fert'))

# summarize the transformed dataset
summarizeColumns(TrainSet_eonr3) %>%
  kable(digits = 2)

# Create dummy variables using caret
TrainSet_eonr3[,region := factor(region)]

dummies <- dummyVars(" ~ .", data = TrainSet_eonr3)
TrainSet_eonr3 <- data.table(predict(dummies, newdata = TrainSet_eonr3))
saveRDS(yc_yearly_dt3, "./vr_value_v2/Data/files_rds/yc_yearly_dt3.rds")
