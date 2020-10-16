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
# fee_seq <- sort(c(seq(0, 10, by = 2)))
fee_seq <-  sort(c(seq(0, 30, by = 2)))
# fee_seq <- c(0,4)
length(fee_seq)
set.seed(123)



reg_model_stuff$fee_threshold <- TrainSet2[N_fert == 180, .(L = quantile(L, probs = 0.3)), region][order(region)]$L
# reg_model_stuff$leaching_fee <- 30

# CHECK IF THE DATA FOR CURRENT RATIO IS THE SAME THAN FEE_0
# test_comp_dt <- merge(test_comp[[1]][,.(id_10, mukey, z, N_fert, Y_corn, L)], 
#       test_comp[[2]][,.(id_10, mukey, z, N_fert, Y_corn, L)], by = c('id_10', 'mukey', 'z', 'N_fert'))
# 
# test_comp_dt[,Y_corn_same := (Y_corn.x == Y_corn.y)]
# test_comp_dt[,leach_same := (L.x == L.y)]
# table(test_comp_dt$Y_corn_same)
# table(test_comp_dt$leach_same)

for(fee_n in fee_seq){
  # fee_n = 10
  print(fee_n)
  
  policy_n = paste0('fee_', fee_n)
  if(policy_n %in% names(reg_model_stuff)){next}
  
  small_model_list <- list()
  
  no_pay_limit <- reg_model_stuff$fee_threshold# - fee_n
  
  TrainSet2[,L_extra := L - no_pay_limit[region]]
  TrainSet2[L_extra <= 0, L_extra := 0]
  TrainSet2$L_extra %>% summary()
  
  TrainSet2[, P := Y_corn * Pc - N_fert * Pn - L_extra * fee_n] #update profits
  
  TrainSet2[, P1 := Y_corn * Pc - N_fert * Pn]
  TrainSet2[, P2 := Y_corn * Pc - N_fert * Pn - L_extra * fee_n] #update profits
  
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
  
  if(FALSE & fee_n == 0){
    fee_eonr_dt <- TrainSet_eonr
    paired_dt <- merge(ratio_eonr_dt[,.(id_10, mukey, z, eonr_ratio = eonr)] ,
                       fee_eonr_dt[,.(id_10, mukey, z, eonr_fee = eonr)], by = c('id_10', 'mukey', 'z')) 
    
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
# CREATE THE N BALANCE MODEL
source(paste0(codes_folder, '/n_policy_git/Codes/parameters.R'))
bal_seq <- sort(c(seq(0, 30, by = 2)))

set.seed(123)

TrainSet2[,N_balance := N_fert - Y_corn * 11/1000]
# reg_model_stuff$bal_threshold <- TrainSet2[N_fert == 180, .(N_balance = quantile(N_balance, probs = 0.5)), region][order(region)]$N_balance - 30
reg_model_stuff$bal_threshold <- c(70, 20, 0)

# CHECK IF THE DATA FOR CURRENT RATIO IS THE SAME THAN FEE_0
# test_comp_dt <- merge(test_comp[[1]][,.(id_10, mukey, z, N_fert, Y_corn, L)], 
#       test_comp[[2]][,.(id_10, mukey, z, N_fert, Y_corn, L)], by = c('id_10', 'mukey', 'z', 'N_fert'))
# 
# test_comp_dt[,Y_corn_same := (Y_corn.x == Y_corn.y)]
# test_comp_dt[,leach_same := (L.x == L.y)]
# table(test_comp_dt$Y_corn_same)
# table(test_comp_dt$leach_same)

for(bal_n in bal_seq){
  # bal_n = 30
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
  
  if(FALSE & bal_n == 0){
    fee_eonr_dt <- TrainSet_eonr
    paired_dt <- merge(ratio_eonr_dt[,.(id_10, mukey, z, eonr_ratio = eonr)] ,
                       fee_eonr_dt[,.(id_10, mukey, z, eonr_bal = eonr)], by = c('id_10', 'mukey', 'z')) 
    
    ggplot(data=paired_dt, aes(x = eonr_ratio, y = eonr_bal)) +
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
if(FALSE){
  # CREATE THE TARGET N LEACHING REDUCTION (ORIGINAL)
  set.seed(123)
  
  ## PREPARE THE TRAINING DATA ========
  # Part 1
  TrainSet2[, P := Y_corn * Pc - N_fert * Pn] #update profits
  
  baseline_leaching <- merge(TrainSet2, reg_model_stuff$ratio_5$minimum_ok, by = 'region') %>% 
    .[N_fert == eonr_pred] %>% .[,.(id_10, mukey, z, leach_base = L)]
  
  TrainSet_nr <- merge(TrainSet2, baseline_leaching, by = c('id_10', 'mukey', 'z'))
  TrainSet_nr[,leach_rel := L/leach_base]
  summary(TrainSet_nr$leach_rel)
  baseline_leaching[leach_base == 0]
  TrainSet_nr[L == 0 & leach_base == 0, leach_rel := 1] #avoid dividing by 0
  TrainSet_nr[L > 0 & leach_base == 0, leach_rel := L/0.0001] #avoid dividing by 0
  TrainSet_nr[,.(leach_rel = mean(leach_rel)), by = N_fert][order(N_fert)] #show me
  TrainSet_RMM <- TrainSet_nr[Yld_response > Yld_response_threshold]
  
  target_seq <- seq(0.5,0.89, by = 0.01)
  target_seq <- sort(unique(c(seq(0.7,0.89, by = 0.03), seq(0.9,1, by = 0.01))))
  # target_seq <- sort(unique(c(seq(0.7,0.89, by = 0.04), seq(0.9,1, by = 0.05))))
  
  length(target_seq)
  
  for(target_n in target_seq){
    # target_n = 1
    policy_n = paste0('target_', target_n)
    # if(policy_n %in% names(reg_model_stuff)){next}
    
    print(target_n)
    small_model_list <- list()
    
    # CREATE THE REGIONAL MINIMUM MODEL
    model_minimum_ok  <- aggregate_by_area(data_dt = TrainSet_RMM, variables = c('P','leach_rel'), 
                                           weight = 'area_ha', by_c = c('region', 'N_fert')) 
    
    
    ggplot(model_minimum_ok) + 
      geom_line(aes(x = N_fert, y = leach_rel, colour = factor(region)))+ #shift up the curve
      geom_line(aes(x = N_fert, y = P, colour = factor(region)))
    
    model_minimum_ok1 <- model_minimum_ok[leach_rel <= target_n][order(N_fert )]
    
    #Chose the EONR below the target reduction L
    if(nrow(model_minimum_ok1)>0){
      model_minimum_ok1 <- model_minimum_ok1[, .SD[ P == max( P)], by = .(region)] #pick the EONR
      model_minimum_ok1 <- model_minimum_ok1[, .SD[ N_fert == min( N_fert)], by = .(region)] #in case more than one rate had the same P
    }
    
    #Type III: cases where the lowest L is higher than the target. Pick the rate with lowest L
    model_minimum_ok[,leach_rel_min := min(leach_rel), by = .(region)]
    model_minimum_ok2 <- model_minimum_ok[leach_rel_min > target_n]
    if(nrow(model_minimum_ok2)>0){
      model_minimum_ok2 <- model_minimum_ok2[, .SD[ leach_rel  == min(leach_rel )], by = .(region)] #pick the lowest L
      model_minimum_ok2 <- model_minimum_ok2[, .SD[ N_fert == min( N_fert)], by = .(region)] #in case more than one rate had the same L
    }
    model_minimum_ok <- rbind(model_minimum_ok1, model_minimum_ok2, fill = T) %>% .[,.(region, eonr_pred = N_fert)] %>%
      .[order(region)]
    
    # model_minimum_ok <- model_minimum_ok[leach_rel >= n_red] %>%
    #   .[, .SD[ leach_rel == min( leach_rel)], by = .(region)] %>% #select minimum leach_rel
    #   .[, .SD[ N_fert == min( N_fert)], by = .(region)] %>% #select minimum rate in case one is repeated
    #   .[,.(region, eonr_pred = N_fert)]
    
    name_model = paste0('minimum_ok')
    small_model_list[[name_model]] <- model_minimum_ok
    
    ## PREPARE THE TRAINING DATA WITH EONR ========
    
    # Type I and II: cases where there are rates with L below the target
    #TrainSet_nr[,P := floor(P/10)*10]#get out of the flat zone
    TrainSet_nr_tmp1 <- TrainSet_nr[leach_rel <= target_n][order(N_fert )]
    
    #Chose the EONR below the target reduction L
    TrainSet_nr_tmp1 <- TrainSet_nr_tmp1[, .SD[ P == max( P)], by = .(id_10, mukey, z)] #pick the EONR
    TrainSet_nr_tmp1 <- TrainSet_nr_tmp1[, .SD[ N_fert == min( N_fert)], by = .(id_10, mukey, z)] #in case more than one rate had the same P
    
    #Type III: cases where the lowest L is higher than the target. Pick the rate with lowest L
    TrainSet_nr[,leach_rel_min := min(leach_rel), by = .(id_10, mukey, z)]
    TrainSet_nr_tmp2 <- TrainSet_nr[leach_rel_min > target_n]
    if(nrow(TrainSet_nr_tmp2)>0){
      TrainSet_nr_tmp2 <- TrainSet_nr_tmp2[, .SD[ L == min(L)], by = .(id_10, mukey, z)] #pick the EONR
      TrainSet_nr_tmp2 <- TrainSet_nr_tmp2[, .SD[ N_fert == min( N_fert)], by = .(id_10, mukey, z)] #in case more than one rate had the same P
    }
    TrainSet_nr_tmp <- rbind(TrainSet_nr_tmp1, TrainSet_nr_tmp2, fill = T)
    setnames(TrainSet_nr_tmp, 'N_fert', 'eonr')
    
    if(FALSE & target_n ==1) {
      target_eonr_dt <- TrainSet_nr_tmp
      paired_dt <- merge(ratio_eonr_dt[,.(id_10, mukey, z, eonr_ratio = eonr)] ,
                         target_eonr_dt[,.(id_10, mukey, z, eonr_target = eonr)], by = c('id_10', 'mukey', 'z')) 
      
      ggplot(data=paired_dt, aes(x = eonr_ratio, y = eonr_target)) +
        geom_point()+ theme(aspect.ratio=1) + coord_fixed() + geom_abline() + 
        ylim(0, 320)+ xlim(0, 320) 
      
    }
    
    
    
    table(TrainSet_nr_tmp[,.N, by = .(id_10, mukey, z)]$N)
    
    TrainSet_nr_tmp[,.N, by = .(id_10, mukey, z)]
    TrainSet_nr_tmp <- TrainSet_nr_tmp[,c('eonr', pred_vars), with = FALSE]
    
    # =========================================================================================================================================================
    # RF Model 2------------------------
    # mtry <- tuneRF(TrainSet_nr_tmp[,c(pred_vars), with = FALSE],TrainSet_nr_tmp$eonr, ntreeTry=1000,
    #                stepFactor=1.2,improve=0.01, trace=TRUE, plot=TRUE)
    # best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
    
    best.m = 6
    
    rf2_eonr <- randomForest(eonr ~ ., data = TrainSet_nr_tmp[,c('eonr', pred_vars), with = FALSE],
                             importance = TRUE , mtry = best.m, ntree=2000, nodesize = 30)
    
    varImpPlot(rf2_eonr, type=2)
    name_model = paste0('rf2')
    small_model_list[[name_model]] <- rf2_eonr
    
    # =========================================================================================================================================================
    #Call python to build the CNN
    #build_cnn(TrainSet_eonr2[,c(pred_vars, 'eonr'), with = FALSE], policy_n, pred_vars)
    
    #----------------------------------------------------------------------------------------------------------------------
    reg_model_stuff[[policy_n]] <- small_model_list
    names(reg_model_stuff)
  }
  
  # =========================================================================================
  # CREATE THE N LEACHING REDUCTION LONG TERM OPTIMIZATION
  set.seed(123)
  
  ## PREPARE THE TRAINING DATA ========
  # Part 1
  TrainSet2[, P := Y_corn * Pc - N_fert * Pn] #update profits
  TrainSet2[,P := floor(P/10)*10] #get out of the flat zone
  
  baseline_leaching <- merge(TrainSet2, reg_model_stuff$ratio_5$minimum_ok, by = 'region') %>% 
    .[N_fert == eonr_pred] %>% .[,.(id_10, mukey, z, z_type, leach_base = L)]
  
  TrainSet2_nr <- merge(TrainSet2, baseline_leaching, by = c('id_10', 'mukey', 'z', 'z_type'))
  TrainSet2_nr[,leach_rel := L/leach_base]
  summary(TrainSet2_nr$leach_rel)
  baseline_leaching[leach_base == 0]
  TrainSet2_nr[L == 0 & leach_base == 0, leach_rel := 1] #avoid dividing by 0
  TrainSet2_nr[L > 0 & leach_base == 0, leach_rel := L/0.0001] #avoid dividing by 0
  
  TrainSet2_nr[,.(leach_rel = mean(leach_rel)), by = N_fert][order(N_fert)]
  TrainSet_RMM <- TrainSet2_nr[Yld_response > Yld_response_threshold]
  
  red_seq <- seq(0.5,0.89, by = 0.01)
  red_seq <- sort(unique(c(seq(0.7,0.89, by = 0.03), seq(0.9,1, by = 0.01))))
  # red_seq <- seq(0.7,1, by = 0.01)
  # red_seq <- c(0.8, 0.95, 1)
  # red_seq <- c(0.85, 0.95)
  length(red_seq)
  all_optimized_training_sets_list <- list()
  rm(Trainset_optimized_tmp)
  
  for(n_red in sort(red_seq, decreasing = T)){
    # n_red = 0.8
    policy_n = paste0('nred_', n_red)
    # if(name_model %in% names(reg_model_stuff)){next}
    
    print(n_red)
    small_model_list <- list()
    
    # CREATE THE REGIONAL MINIMUM MODEL
    model_minimum_ok  <- aggregate_by_area(data_dt = TrainSet_RMM, variables = c('P','leach_rel'), 
                                           weight = 'area_ha', by_c = c('region', 'N_fert')) 
    
    ggplot(model_minimum_ok) + 
      geom_line(aes(x = N_fert, y = leach_rel, colour = factor(region)))+ #shift up the curve
      geom_line(aes(x = N_fert, y = P, colour = factor(region)))
    
    
    model_minimum_ok1 <- model_minimum_ok[leach_rel <= n_red][order(N_fert )]
    
    #Chose the EONR below the target reduction L
    if(nrow(model_minimum_ok1)>0){
      model_minimum_ok1 <- model_minimum_ok1[, .SD[ P == max( P)], by = .(region)] #pick the EONR
      model_minimum_ok1 <- model_minimum_ok1[, .SD[ N_fert == min( N_fert)], by = .(region)] #in case more than one rate had the same P
    }
    #Type III: cases where the lowest L is higher than the target. Pick the rate with lowest L
    model_minimum_ok[,leach_rel_min := min(leach_rel), by = .(region)]
    model_minimum_ok2 <- model_minimum_ok[leach_rel_min > n_red]
    if(nrow(model_minimum_ok2)>0){
      model_minimum_ok2 <- model_minimum_ok2[, .SD[ leach_rel  == min(leach_rel )], by = .(region)] #pick the lowest L
      model_minimum_ok2 <- model_minimum_ok2[, .SD[ N_fert == min( N_fert)], by = .(region)] #in case more than one rate had the same L
    }
    model_minimum_ok <- rbind(model_minimum_ok1, model_minimum_ok2, fill = T) %>% .[,.(region, eonr_pred = N_fert)] %>%
      .[order(region)]
    
    # model_minimum_ok <- model_minimum_ok[leach_rel >= n_red] %>%
    #   .[, .SD[ leach_rel == min( leach_rel)], by = .(region)] %>% #select minimum leach_rel
    #   .[, .SD[ N_fert == min( N_fert)], by = .(region)] %>% #select minimum rate in case one is repeated
    #   .[,.(region, eonr_pred = N_fert)]
    
    name_model = paste0('minimum_ok')
    small_model_list[[name_model]] <- model_minimum_ok
    
    # =========================================================================================================================================================
    # ## PREPARE THE TRAINING DATA WITH EONR ========
    soils_training <- unique(TrainSet2_nr[,.(id_10, mukey, z_type)])
    
    library("foreach")
    library("doParallel")
    registerDoParallel(10) # register the cluster
    # registerDoParallel(cores = 10)
    
    TrainSet_nr_tmp_list = foreach(i = 1:nrow(soils_training), .combine = "c", .packages = c("data.table")) %dopar% {
      # i= 320
      print(i)
      soils_training_n <- soils_training[i]
      TrainSet2_nr_field <- filter_dt_in_dt(TrainSet2_nr, soils_training_n, return_table = T)
      leach_base_field <- filter_dt_in_dt(baseline_leaching, soils_training_n, return_table = T)
      leach_base_sum <- sum(leach_base_field$leach_base)
      
      # if(!exists( 'Trainset_optimized_tmp')){
      #start with the rates that maximize profits
      opt_dt <- TrainSet2_nr_field[,.SD[P == max(P)],by = z] %>% 
        .[,.SD[N_fert == min( N_fert)],by = z]
      sum(opt_dt$L)/leach_base_sum
      # }else{
      #   #start with the rates from last optimization
      #   opt_dt <- filter_dt_in_dt(Trainset_optimized_tmp, soils_training_n, return_table = T)
      #   setnames(opt_dt, 'eonr', 'N_fert')
      #   if(n_red > unique(opt_dt$target)){stop()}
      #   opt_dt <- opt_dt[,-'target']
      #   sum(opt_dt$L)/leach_base_sum
      # }
      
      #find what z can have a lower rate with the lowest hurt in P
      hit_the_target <- sum(opt_dt$L) <= leach_base_sum * n_red
      previous<- sum(opt_dt$L)
      lower_than_before <- TRUE
      
      while(lower_than_before & !hit_the_target){
        #1- Lower the rates by 10 kg
        next_step_rates <- opt_dt[,.(id_10, mukey, z, N_fert_new = N_fert)]
        next_step_rates[, N_fert_new := ifelse(N_fert_new <= 10, 10, N_fert_new - 10)]
        
        #2- go get the data for the lower rates
        next_step_data <- merge(TrainSet2_nr_field, next_step_rates, by = c('id_10', 'mukey', 'z'))[N_fert == N_fert_new]
        next_step_efficiency <- merge(opt_dt[,.(id_10, mukey, z, N_fert, P, L)],
                                      next_step_data[,.(id_10, mukey, z, N_fert, P, L)], by = c('id_10', 'mukey', 'z'))
        next_step_efficiency[,P_diff := P.y - P.x]
        next_step_efficiency[,L_diff := L.y - L.x]
        next_step_efficiency[,P_L_efficiency := P_diff/L_diff]
        
        if(all(!(next_step_efficiency$L_diff < 0))){break}
        #3 - find what z can have a lower rate with the lowest hurt in P
        decrease_me <- next_step_efficiency[L_diff < 0][P_L_efficiency == min(P_L_efficiency)][N_fert.y == max(N_fert.y)][1,] #if there is more than one with the same eff, pick the highest N rate
        next_step_rates <- opt_dt[,.(id_10, mukey, z, N_fert_new = N_fert)]
        next_step_rates[z == decrease_me$z, N_fert_new := N_fert_new-10]
        opt_dt <- merge(TrainSet2_nr_field, next_step_rates, by = c('id_10', 'mukey', 'z')) 
        opt_dt <- opt_dt[N_fert == N_fert_new, -'N_fert_new']
        lower_than_before <- sum(opt_dt$L) < previous
        previous = sum(opt_dt$L)
        # print(sum(opt_dt$L))
        hit_the_target <- sum(opt_dt$L) <= leach_base_sum * n_red
        print(sum(opt_dt$L)/leach_base_sum)
      } #end of while loop
      
      # return the results
      list(opt_dt)
      
    }#end of dopar loop
    
    stopImplicitCluster()
    
    Trainset_optimized_tmp <- rbindlist(TrainSet_nr_tmp_list, use.names=TRUE)
    setnames(Trainset_optimized_tmp, 'N_fert', 'eonr')
    sum(Trainset_optimized_tmp$L)/sum(Trainset_optimized_tmp$leach_base)#show me
    
    
    all_optimized_training_sets_list[[length(all_optimized_training_sets_list)+1]] <- Trainset_optimized_tmp[,target := n_red]
  
    
    # =========================================================================================================================================================
    # RF Model 2------------------------
    # mtry <- tuneRF(TrainSet_nr_tmp[,c(pred_vars), with = FALSE],TrainSet_nr_tmp$eonr, ntreeTry=1000,
    #                stepFactor=1.2,improve=0.01, trace=TRUE, plot=TRUE)
    # best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
    
    best.m = 6
    
    rf2_eonr <- randomForest(eonr ~ ., data = Trainset_optimized_tmp[,c('eonr', pred_vars), with = FALSE],
                             importance = TRUE , mtry = best.m, ntree=2000, nodesize = 30)
    
    # plot(rf2_eonr)
    varImpPlot(rf2_eonr, type=2)
    name_model = paste0('rf2')
    small_model_list[[name_model]] <- rf2_eonr
    
    # =========================================================================================================================================================
    #Call python to build the CNN
    #build_cnn(Trainset_optimized_tmp[,c(pred_vars, 'eonr'), with = FALSE], policy_n, pred_vars)
    
    #--------------------------------------------------------------------------------------------------------------
    reg_model_stuff[[policy_n]] <- small_model_list
    names(reg_model_stuff)
  }
  saveRDS(all_optimized_training_sets_list, "./n_policy_box/Data/files_rds/all_optimized_training_sets_list.rds")
}
# =========================================================================================================================================================
names(reg_model_stuff)


# reg_model_stuff[['pred_vars']] <-  pred_vars
# reg_model_stuff[['crop_varb']] <-  crop_varb
# reg_model_stuff[['low_var_trf']] <-  low_var
# reg_model_stuff[['preprocessParams']] <-  preprocessParams
# 
# reg_model_stuff_tmp[['TrainSet_eonr3']] <-  TrainSet_eonr3
# 
# reg_model_stuff_tmp[['rf1_eonr']] <-  rf1_eonr
# reg_model_stuff_tmp[['rf2_eonr']] <-  rf2_eonr
# reg_model_stuff_tmp[['rf3_eonr']] <-  rf3_eonr
# reg_model_stuff_tmp[['reg_lm4']] <-  reg_lm4

reg_model_stuff$ratio_5$minimum_ok
reg_model_stuff$fee_0$minimum_ok
reg_model_stuff$bal_0$minimum_ok
reg_model_stuff$nred_1$minimum_ok
reg_model_stuff$target_1$minimum_ok

saveRDS(reg_model_stuff, "./n_policy_box/Data/files_rds/reg_model_stuff.rds")
