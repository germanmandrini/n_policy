# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
# setwd('~')
rm(list=ls())

source('./Codes_useful/R.libraries.R')
# library(scales)
source('./Codes_useful/gm_functions.R')
# source('./n_policy/Codes/parameters.R')

validation_review_dt <- read.csv('./n_policy_box/Data/validation/validation_review.csv') %>% data.table() %>%
  .[crop == 'Corn' & !is.na(Y_obs)]

ggplot(validation_review_dt) + 
  geom_path(aes(x = N_fert, y = Y_obs , colour = interaction(Author, year, id_field), linetype = Author), size = 1)+
  geom_point(aes(x = N_fert, y = Y_obs , colour = interaction(Author, year, id_field), linetype = Author), size = 3)+
  labs(y= 'Yield (kg/ha)', x= 'N fert (kg/ha)')+
  theme_bw()+
  theme(legend.title =  element_blank())


#Create an id for each curve
setDT(validation_review_dt)[, id_curve := rleid(Author,year,id_field  )]


#Standarize to same N_fert
rates_std <- c(0,50,100,150,200,250)

validation_review_std_list <- list()
for(curve_n in unique(validation_review_dt$id_curve)){
  #curve_n = 3
  yield_curve_tmp <- validation_review_dt[id_curve == curve_n]
  
  for(rate_n  in rates_std){
    #rate_n = 100
    yield_curve_tmp[,N_fert_diff := N_fert - rate_n]
    if(any(yield_curve_tmp$N_fert_diff == 0)){
      yield_tmp <- yield_curve_tmp[N_fert_diff == 0]$Y_obs
    }else if(any(yield_curve_tmp$N_fert_diff >0)){
      interpolate_tmp2 <- rbind(
        yield_curve_tmp[N_fert_diff < 0][N_fert_diff == max(N_fert_diff)],
        yield_curve_tmp[N_fert_diff > 0][N_fert_diff == min(N_fert_diff)]) %>% #select max negative and min positive (to make sure we are interpolating)
              .[order(N_fert)]
      yield_tmp = (interpolate_tmp2$Y_obs[2] - interpolate_tmp2$Y_obs[1])/
        (interpolate_tmp2$N_fert[2] - interpolate_tmp2$N_fert[1])*
        (rate_n-interpolate_tmp2$N_fert[1])+interpolate_tmp2$Y_obs[1]
    }else{
      interpolate_tmp2 <- yield_curve_tmp[order(-N_fert_diff)][c(1,2)][order(N_fert)]
      yield_tmp = (interpolate_tmp2$Y_obs[2] - interpolate_tmp2$Y_obs[1])/
        (interpolate_tmp2$N_fert[2] - interpolate_tmp2$N_fert[1])*
        (rate_n-interpolate_tmp2$N_fert[2])+interpolate_tmp2$Y_obs[2]
      
    }
    
    validation_review_std_list[[length(validation_review_std_list)+1]] <- data.table(
      unique(yield_curve_tmp[,.(Author, year, id_field, id_curve)]),
               N_fert = rate_n,
               Y_obs = yield_tmp)
  
  }}

validation_review_std_dt <- rbindlist(validation_review_std_list)
ggplot(validation_review_std_dt) + 
  geom_path(aes(x = N_fert, y = Y_obs , colour = interaction(Author, year, id_field), linetype = Author), size = 1)

validation_review_std_dt[, Y_max := max(Y_obs), by = .(Author, year, id_field)]
validation_review_std_dt[,Y_rel := Y_obs/max(Y_obs), by = .(Author, year, id_field)]

ggplot(validation_review_std_dt) + 
  geom_path(aes(x = N_fert, y = Y_rel , colour = interaction(Author, year, id_field), linetype = Author), size = 1)+
  labs(y= 'Yield (%)', x= 'N fert (kg/ha)')+
  theme_bw()+
  theme(legend.title =  element_blank())
