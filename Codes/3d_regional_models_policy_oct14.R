rm(list=ls())
# 
# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# codes_folder <-'C:/Users/germa/Documents'#Dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# codes_folder <-'C:/Users/germanm2/Documents'#CPSC
setwd('~')#Server
codes_folder <-'~' #Server



library(randomForest)
library(reticulate)
source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')
source(paste0(codes_folder, '/n_policy_git/Codes/parameters.R'))
# use_condaenv('GEOANN', conda = '/opt/anaconda3/condabin/conda')
# source_python("./n_policy_git/Codes/3c_cnn_functions_sep10.py")

# yc_yearly_dt3 <- readRDS("./n_policy_box/Data/files_rds/yc_yearly_dt3.rds")
# grid10_soils_dt5 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt5.rds") %>% data.table()

reg_model_stuff <- readRDS( "./n_policy_box/Data/files_rds/reg_model_stuff.rds")



remove_list <- names(reg_model_stuff)[str_detect(names(reg_model_stuff), 'bal_')]
for(n in remove_list){
  reg_model_stuff[[n]] <- NULL
}

remove_list <- names(reg_model_stuff)[!names(reg_model_stuff) %in% c("full_fields", "stations", "TrainSet","training_z")]
for(n in remove_list){
  reg_model_stuff[[n]] <- NULL
}




# =========================================================================================================================================================
## SET THE VARIABLES ========
TrainSet2 <- reg_model_stuff[['TrainSet']]

pred_vars <- c("rain_30", "rain_60", "rain_90",
               "t_max_30", "t_max_60", "t_max_90", "t_min_30", "t_min_60",
               "t_min_90", 'Y_corn_lt_avg', "day_sow", "day_v5", "lai_v5",
               "whc",  "oc_20cm_v5", "sw_dep_v5", "n_0_60cm_v5",  "surfaceom_wt_v5", 
               "sand_40cm", "clay_40cm") #"root_wt_v5",, "n_deep_v5", "esw_pct_v5", 

saveRDS(pred_vars, "./n_policy_box/Data/files_rds/pred_vars.rds")

TrainSet2[, n_0_60cm_v5 := n_20cm_v5 + n_40cm_v5 + n_60cm_v5]
TrainSet2[,L := L1 + L2]
TrainSet2[, Yld_response := max(Y_corn) - min(Y_corn), by = .(id_10, mukey,z)]

# Limit the trials included in MRTN

Yld_response_threshold <- 500 


# =========================================================================================================================================================
# CREATE THE N RATIO TAX MODEL

ratio_seq <- sort(c(seq(5, 20, by = 1)))
# ratio_seq <- sort(c(seq(5, 20, by = 5)))
# ratio_seq <- c(5)
set.seed(123)

for(ratio_n in ratio_seq){
  # ratio_n = Pn/Pc
  # ratio_n = 5
  policy_n = paste0('ratio_', ratio_n)
  if(policy_n %in% names(reg_model_stuff)){next}
  
  small_model_list <- list()
  Pn_tmp = ratio_n * Pc
  print(Pn_tmp/Pc)
  # TrainSet2[, P := Y_corn * Pc + Y_soy * Ps - N_fert * Pn_tmp]  #update profits
  TrainSet2[, P := Y_corn * Pc - N_fert * Pn_tmp]  #update profits
  # =========================================================================================================================================================
  # CREATE THE REGIONAL MINIMUM MODEL - OK
  TrainSet_RMM <- TrainSet2[Yld_response > Yld_response_threshold] #Needs to be here, to use updated profits 
  
  TrainSet2[,.N, .(id_10, mukey, z)] %>% nrow() #trials before (all of them)
  TrainSet_RMM[,.N, .(id_10, mukey, z)] %>% nrow()#trials after (whith response > threshold)
  
  model_minimum_ok  <- aggregate_by_area(data_dt = TrainSet_RMM, variables = c('P'), 
                                         weight = 'area_ha', by_c = c('region', 'N_fert')) %>% 
    .[, .SD[ P == max( P)], by = .(region)] %>% .[,.(region, eonr_pred = N_fert)] %>%
    .[order(region)]
  
  
  name_model = paste0('minimum_ok')
  small_model_list[[name_model]] <- model_minimum_ok
  
  # =========================================================================================================================================================
  ## PREPARE THE TRAINING DATA WITH EONR ========
  TrainSet2[,P := floor(P/10)*10]#get out of the flat zone
  
  TrainSet_eonr <- TrainSet2[, .SD[ P == max( P)], by = .(id_10, mukey, z)]
  TrainSet_eonr <- TrainSet_eonr[, .SD[ N_fert == min( N_fert)], by = .(id_10, mukey, z)]
  setnames(TrainSet_eonr, 'N_fert', 'eonr')
  
  TrainSet_eonr2 <- TrainSet_eonr[,c('eonr', pred_vars), with = FALSE]
  
  if(ratio_n == 5){
    saveRDS(TrainSet_eonr2, "./n_policy_box/Data/files_rds/TrainSet_eonr2.rds") #for python
    ratio_eonr_dt <- TrainSet_eonr
    saveRDS(ratio_eonr_dt, "./n_policy_box/Data/files_rds/ratio_eonr_dt.rds") #for graphs later in balance
  }
  # =========================================================================================================================================================
  # RF Model 2------------------------
  # mtry <- tuneRF(TrainSet_eonr2[,c(pred_vars), with = FALSE],TrainSet_eonr2$eonr, ntreeTry=1000,
  #                 stepFactor=1.2,improve=0.01, trace=TRUE, plot=TRUE) # ,mtryStart = 5
  # best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
  
  best.m = 6
  
  rf2_eonr <- randomForest(eonr ~ ., data = TrainSet_eonr2,
                           importance = TRUE , mtry = best.m, ntree=2000, nodesize = 30)
  
  
  varImpPlot(rf2_eonr, type=2)
  plot(rf2_eonr)
  
  if(ratio_n == 5){
    pdf("./n_policy_box/Data/figures/VarImportancePlot.pdf")
    varImpPlot(rf2_eonr, type=2, main = '')
    dev.off() 
  }
  
  
  name_model = paste0('rf2')
  small_model_list[[name_model]] <- rf2_eonr
  
  if(FALSE){ #TESTS
    library('Metrics')
    
    testing_set_dt <- copy(TrainSet_eonr)
    testing_set_dt[,eonr_pred_rf := round(predict(rf2_eonr, testing_set_dt)/10)*10]
    setnames(testing_set_dt, 'eonr', 'eonr_12')
    mean(testing_set_dt$eonr_pred_rf)
    
    rmse(testing_set_dt$eonr_12, testing_set_dt$eonr_pred_rf) #23
    
    validation_set_dt <- readRDS("./n_policy_box/Data/files_rds/prediction_set_aggregated_dt.rds")
    validation_set_dt[,eonr_pred_rf := round(predict(rf2_eonr, validation_set_dt)/10)*10]
    rmse(validation_set_dt$eonr_12, validation_set_dt$eonr_pred_rf) #40
    mean(validation_set_dt$eonr_pred_rf)
    
    plot_top <- grid.arrange(
      ggplot(data=testing_set_dt, aes(x = eonr_12, y = eonr_pred_rf)) +
        geom_point()+ theme(aspect.ratio=1) + coord_fixed() + geom_abline() + 
        ylim(0, 320)+ xlim(0, 320) +
        ggtitle('Testing'),
      
      ggplot(data=validation_set_dt, aes(x = eonr_12, y = eonr_pred_rf)) +
        geom_point()+ theme(aspect.ratio=1) + coord_fixed() + geom_abline() + 
        ylim(0, 320)+ xlim(0, 320) +
        ggtitle('Validation'), ncol = 2)
    
    plot_bottom <- ggplot() + 
      geom_line(data = testing_set_dt[,.(eonr_pred_rf = mean(eonr_pred_rf)), by = eonr_12], 
                aes(x = eonr_12, y = eonr_pred_rf, colour = 'red')) +
      geom_line(data = validation_set_dt[,.(eonr_pred_rf = mean(eonr_pred_rf)), by = eonr_12], 
                aes(x = eonr_12, y = eonr_pred_rf, colour = 'blue')) +
      scale_color_discrete(labels = c("testing", "validation")) +
      theme(aspect.ratio=1) + coord_fixed() + geom_abline() + 
      ylim(0, 320)+ xlim(0, 320)
    
    grid.arrange( plot_top, plot_bottom, ncol = 1)
    
  }
  
  if(ratio_n == 5){
    # Add the results to the prediction set to use in python (first we need to run 4a_fields_splitter)
    prediction_set_aggregated_dt <- readRDS("./n_policy_box/Data/files_rds/prediction_set_aggregated_dt.rds")
    prediction_set_aggregated_dt[,eonr_pred_rf := round(predict(rf2_eonr, prediction_set_aggregated_dt)/10)*10]
    saveRDS(prediction_set_aggregated_dt, "./n_policy_box/Data/files_rds/prediction_set_aggregated_dt.rds")
    rm(prediction_set_aggregated_dt)
  }
  
  
  # =========================================================================================================================================================
  #Call python to build the CNN
  # source_python("./n_policy_git/Codes/3c_cnn_functions_sep10.py")
  # build_cnn(TrainSet_eonr2[,c(pred_vars, 'eonr'), with = FALSE], policy_n, pred_vars)
  
  # --------------------------------------
  # Save it to the big list
  reg_model_stuff[[policy_n]] <- small_model_list
  names(reg_model_stuff)
}


# =========================================================================================================================================================
# CREATE THE LEACHING FEE MODEL
source('./n_policy_git/Codes/parameters.R')
# leach_seq <- sort(c(seq(0, 10, by = 2)))
leach_seq <-  sort(c(seq(0, 30, by = 2)))
# leach_seq <- c(0,4)
length(leach_seq)
set.seed(123)



reg_model_stuff$leach_threshold <- TrainSet2[N_fert == 100, .(L = quantile(L, probs = 0.5)), region][order(region)]$L

# reg_model_stuff$leaching_fee <- 30

# CHECK IF THE DATA FOR CURRENT RATIO IS THE SAME THAN leach_0
# test_comp_dt <- merge(test_comp[[1]][,.(id_10, mukey, z, N_fert, Y_corn, L)], 
#       test_comp[[2]][,.(id_10, mukey, z, N_fert, Y_corn, L)], by = c('id_10', 'mukey', 'z', 'N_fert'))
# 
# test_comp_dt[,Y_corn_same := (Y_corn.x == Y_corn.y)]
# test_comp_dt[,leach_same := (L.x == L.y)]
# table(test_comp_dt$Y_corn_same)
# table(test_comp_dt$leach_same)

for(leach_n in leach_seq){
  # leach_n = 10
  print(leach_n)
  
  policy_n = paste0('leach_', leach_n)
  if(policy_n %in% names(reg_model_stuff)){next}
  
  small_model_list <- list()
  
  no_pay_limit <- reg_model_stuff$leach_threshold# - leach_n
  
  TrainSet2[,L_extra := L - no_pay_limit[region]]
  TrainSet2[L_extra <= 0, L_extra := 0]
  TrainSet2$L_extra %>% summary()
  
  TrainSet2[, P := Y_corn * Pc - N_fert * Pn - L_extra * leach_n] #update profits
  
  TrainSet2[, P1 := Y_corn * Pc - N_fert * Pn]
  TrainSet2[, P2 := Y_corn * Pc - N_fert * Pn - L_extra * leach_n] #update profits
  
  plot_dt <- TrainSet2[, .(P1 = mean(P1), 
                           P2 = mean(P2), 
                           L_extra = round(mean(L_extra),0)), by = .(region, N_fert)][order(region, N_fert)]
  
  ggplot() + 
    geom_line(data = plot_dt, aes(x = N_fert, y = P1, color = region))+
    geom_point(data = plot_dt[,.SD[P1 == max(P1)], by = region], aes(x = N_fert, y = P1))+
    geom_line(data = plot_dt, aes(x = N_fert, y = P2, color = region), linetype = 'dashed')+
    geom_point(data = plot_dt[,.SD[P2 == max(P2)], by = region], aes(x = N_fert, y = P2))
  
  # =========================================================================================================================================================
  # CREATE THE REGIONAL MINIMUM MODEL - OK
  TrainSet_RMM <- TrainSet2[Yld_response > Yld_response_threshold] #Needs to be here, to use updated profits 
  
  TrainSet2[,.N, .(id_10, mukey, z)] %>% nrow() #trials before (all of them)
  TrainSet_RMM[,.N, .(id_10, mukey, z)] %>% nrow()#trials after (whith response > threshold)
  
  model_minimum_ok  <- aggregate_by_area(data_dt = TrainSet_RMM, variables = c('P'), 
                                         weight = 'area_ha', by_c = c('region', 'N_fert')) %>% 
    .[, .SD[ P == max( P)], by = .(region)] %>% .[,.(region, eonr_pred = N_fert)] %>%
    .[order(region)]
  
  name_model = paste0('minimum_ok')
  small_model_list[[name_model]] <- model_minimum_ok
  
  # =========================================================================================================================================================
  ## PREPARE THE TRAINING DATA WITH EONR ========
  TrainSet2[,P := floor(P/10)*10]#get out of the flat zone
  TrainSet_eonr <- TrainSet2[, .SD[ P == max( P)], by = .(id_10, mukey, z)]
  TrainSet_eonr <- TrainSet_eonr[, .SD[ N_fert == min( N_fert)], by = .(id_10, mukey, z)]
  setnames(TrainSet_eonr, 'N_fert', 'eonr')
  
  if(FALSE & leach_n == 0){
    ratio_eonr_dt <- readRDS("./n_policy_box/Data/files_rds/ratio_eonr_dt.rds") #for graphs later
    leach_eonr_dt <- TrainSet_eonr
    paired_dt <- merge(ratio_eonr_dt[,.(id_10, mukey, z, eonr_ratio = eonr)] ,
                       leach_eonr_dt[,.(id_10, mukey, z, eonr_fee = eonr)], by = c('id_10', 'mukey', 'z')) 
    
    ggplot(data=paired_dt, aes(x = eonr_ratio, y = eonr_fee)) +
      geom_point()+ theme(aspect.ratio=1) + coord_fixed() + geom_abline() + 
      ylim(0, 320)+ xlim(0, 320) 
    
  }
  
  TrainSet_eonr2 <- TrainSet_eonr[,c('eonr', pred_vars), with = FALSE]
  
  # =========================================================================================================================================================
  # RF Model 2------------------------
  # mtry <- tuneRF(TrainSet_eonr2[,c(pred_vars), with = FALSE],TrainSet_eonr2$eonr, ntreeTry=1000,
  #                stepFactor=1.2,improve=0.01, trace=TRUE, plot=TRUE)
  # best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
  best.m = 6
  
  rf2_eonr <- randomForest(eonr ~ ., data = TrainSet_eonr2[,c('eonr', pred_vars), with = FALSE],
                           importance = TRUE , mtry = best.m, ntree=2000, nodesize = 30)
  
  varImpPlot(rf2_eonr, type=2)
  
  name_model = paste0('rf2')
  small_model_list[[name_model]] <- rf2_eonr
  # =========================================================================================================================================================
  #Call python to build the CNN
  # build_cnn(TrainSet_eonr2[,c(pred_vars, 'eonr'), with = FALSE], policy_n, pred_vars)
  
  
  # --------------------------------------
  # Save it to the big list
  reg_model_stuff[[policy_n]] <- small_model_list
  # names(reg_model_stuff)
}

# =========================================================================================================================================================
# CREATE THE BALANCE FEE MODEL
source(paste0(codes_folder, '/n_policy_git/Codes/parameters.R'))
bal_seq <- sort(seq(0, 5, by = 0.25))

set.seed(123)

TrainSet2[,N_balance := N_fert - Y_corn * 11/1000]
reg_model_stuff$bal_threshold <- TrainSet2[N_fert == 100, .(N_balance = quantile(N_balance, probs = 0.5)), region][order(region)]$N_balance

# reg_model_stuff$bal_threshold <- c(40, 0, -20)

(p1 <- ggplot(data = TrainSet2[N_fert == 100])+ geom_density(aes(x = N_balance, colour = region), size =1)+
              ggtitle('N Balance'))     
          
# reg_model_stuff$bal_threshold <- c(60, 40, 30)

# CHECK IF THE DATA FOR CURRENT RATIO IS THE SAME THAN leach_0
# test_comp_dt <- merge(test_comp[[1]][,.(id_10, mukey, z, N_fert, Y_corn, L)], 
#       test_comp[[2]][,.(id_10, mukey, z, N_fert, Y_corn, L)], by = c('id_10', 'mukey', 'z', 'N_fert'))
# 
# test_comp_dt[,Y_corn_same := (Y_corn.x == Y_corn.y)]
# test_comp_dt[,leach_same := (L.x == L.y)]
# table(test_comp_dt$Y_corn_same)
# table(test_comp_dt$leach_same)

for(bal_n in bal_seq){
  # bal_n = 1
  print(bal_n)
  
  policy_n = paste0('bal_', bal_n)
  if(policy_n %in% names(reg_model_stuff)){next}
  
  small_model_list <- list()
  
  no_pay_limit <- reg_model_stuff$bal_threshold# - bal_n
  
  TrainSet2[,N_extra := N_balance - no_pay_limit[region]]
  TrainSet2[N_extra <= 0, N_extra := 0]
  
  TrainSet2[, P := Y_corn * Pc - N_fert * Pn - N_extra * bal_n]#update profits
  TrainSet2[, P1 := Y_corn * Pc - N_fert * Pn]
  TrainSet2[, P2 := Y_corn * Pc - N_fert * Pn - N_extra * bal_n]
  
  plot_dt <- TrainSet2[, .(P1 = mean(P1), 
                P2 = mean(P2), 
                N_extra = round(mean(N_extra),0)), by = .(region, N_fert)][order(region, N_fert)]
  
  ggplot() + 
    geom_line(data = plot_dt, aes(x = N_fert, y = P1, color = region))+
    geom_point(data = plot_dt[,.SD[P1 == max(P1)], by = region], aes(x = N_fert, y = P1))+
    geom_line(data = plot_dt, aes(x = N_fert, y = P2, color = region), linetype = 'dashed')+
    geom_point(data = plot_dt[,.SD[P2 == max(P2)], by = region], aes(x = N_fert, y = P2))
  
  # =========================================================================================================================================================
  # CREATE THE REGIONAL MINIMUM MODEL - OK
  TrainSet_RMM <- TrainSet2[Yld_response > Yld_response_threshold] #Needs to be here, to use updated profits 
  
  TrainSet2[,.N, .(id_10, mukey, z)] %>% nrow() #trials before (all of them)
  TrainSet_RMM[,.N, .(id_10, mukey, z)] %>% nrow()#trials after (whith response > threshold)
  
  model_minimum_ok  <- aggregate_by_area(data_dt = TrainSet_RMM, variables = c('P'), 
                                         weight = 'area_ha', by_c = c('region', 'N_fert')) %>% 
    .[, .SD[ P == max( P)], by = .(region)] %>% .[,.(region, eonr_pred = N_fert)] %>%
    .[order(region)]
  
  name_model = paste0('minimum_ok')
  small_model_list[[name_model]] <- model_minimum_ok
  
  # =========================================================================================================================================================
  ## PREPARE THE TRAINING DATA WITH EONR ========
  TrainSet2[,P := floor(P/10)*10]#get out of the flat zone
  TrainSet_eonr <- TrainSet2[, .SD[ P == max( P)], by = .(id_10, mukey, z)]
  TrainSet_eonr <- TrainSet_eonr[, .SD[ N_fert == min( N_fert)], by = .(id_10, mukey, z)]
  setnames(TrainSet_eonr, 'N_fert', 'eonr')
  
  (p1 <- ggplot(data = TrainSet_eonr)+ geom_density(aes(x = N_balance, colour = region), size =1)+
      ggtitle('N Balance'))  
  
  if(FALSE & bal_n == 0){
    bal_eonr_dt <- TrainSet_eonr
    paired_dt <- merge(ratio_eonr_dt[,.(region, id_10, mukey, z, eonr_ratio = eonr)] ,
                       bal_eonr_dt[,.(region, id_10, mukey, z, eonr_bal = eonr)], by = c('region','id_10', 'mukey', 'z')) 
    
    paired_dt[, .(eonr_diff = mean(eonr_ratio - eonr_bal)), by = region]
    ggplot(data=paired_dt, aes(x = eonr_ratio, y = eonr_bal)) +
      geom_point()+ theme(aspect.ratio=1) + coord_fixed() + geom_abline() + 
      geom_jitter()+
      ylim(0, 320)+ xlim(0, 320) 
    
  }
  
  TrainSet_eonr2 <- TrainSet_eonr[,c('eonr', pred_vars), with = FALSE]
  
  # =========================================================================================================================================================
  # RF Model 2------------------------
  # mtry <- tuneRF(TrainSet_eonr2[,c(pred_vars), with = FALSE],TrainSet_eonr2$eonr, ntreeTry=1000,
  #                stepFactor=1.2,improve=0.01, trace=TRUE, plot=TRUE)
  # best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
  best.m = 6
  
  rf2_eonr <- randomForest(eonr ~ ., data = TrainSet_eonr2[,c('eonr', pred_vars), with = FALSE],
                           importance = TRUE , mtry = best.m, ntree=2000, nodesize = 30)
  
  varImpPlot(rf2_eonr, type=2)
  
  name_model = paste0('rf2')
  small_model_list[[name_model]] <- rf2_eonr
  # =========================================================================================================================================================
  #Call python to build the CNN
  # build_cnn(TrainSet_eonr2[,c(pred_vars, 'eonr'), with = FALSE], policy_n, pred_vars)
  
  
  # --------------------------------------
  # Save it to the big list
  reg_model_stuff[[policy_n]] <- small_model_list
  # names(reg_model_stuff)
}


# =========================================================================================================================================================


reg_model_stuff$ratio_5$minimum_ok
reg_model_stuff$leach_0$minimum_ok
reg_model_stuff$bal_0$minimum_ok
reg_model_stuff$nred_1$minimum_ok
reg_model_stuff$target_1$minimum_ok

saveRDS(reg_model_stuff, "./n_policy_box/Data/files_rds/reg_model_stuff.rds")
