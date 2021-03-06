# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
# setwd('~')

source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')
source('./vr_value/Codes/parameters.R')

library(randomForest)

# eonr_mukey_dt3 <- readRDS("./vr_value/Data/files_rds/eonr_mukey_dt3.rds")

grid10_soils_sf6 <- readRDS("./vr_value/Data/Grid/grid10_soils_sf6.rds")

grid10_tiles_sf2 <- readRDS("./vr_value/Data/Grid/grid10_tiles_sf2.rds") 

yc_yearly_dt3 <- readRDS("./vr_value/Data/files_rds/yc_yearly_dt3.rds")

reg_model_stuff <- readRDS( "./vr_value/Data/files_rds/reg_model_stuff.rds")
reg_model_stuff_policy <- readRDS( "./vr_value/Data/files_rds/reg_model_stuff_policy.rds")

#======================================================================================
# GET THE FIELDS THAT CAN BE RUN
# stations_dt2 <- reg_model_stuff$stations
full_fields_dt3 <- reg_model_stuff$full_fields #one row by field x soil

training_z <- reg_model_stuff$training_z
no_cost_var <- reg_model_stuff$no_cost_var
ss_var <- reg_model_stuff$ss_var

fields_list_dt <- full_fields_dt3[,.(.N), by = .(id_10, id_field)][,-'N'] #one row by field

#======================================================================================
# # CLEAN DATA (SHOULD BE DONE IN 2)
# yc_yearly_dt3[,Yld_max_z := max(Yld), by = .(id_10, mukey, z, prev_crop)]
# 
# hist(yc_yearly_dt3$Yld_max_z)
# yc_yearly_dt3[Yld_max_z < 10] # killed by frost
# yc_yearly_dt3[Yld_max_z < 2000] # bad data
# yc_yearly_dt3 <- yc_yearly_dt3[Yld_max_z > 2000 ]


#======================================================================================
# 


process_field_economics <- function(j){
  # j = 602
  small_list <- list()
  print(j)
  field_n <- fields_list_dt[j,]
  
  field_soils_dt <- full_fields_dt3[id_10 == field_n$id_10 &
                    id_field == field_n$id_field,]
  sum(field_soils_dt$area_ha)
  
  #======================================================================================
  # # ADD GEOGRAPHIC INFORMATION 
  # one_field_dt <- data.table(one_field_sf, st_coordinates(st_centroid(one_field_sf))) %>% .[,-'geometry']
  # setnames(one_field_dt, c('X', 'Y'), c('long', 'lat'))
  # one_field_dt <- one_field_dt[,.(area_ha = sum(area_ha),
  #                                        region = max(region), 
  #                                        long = mean(long), 
  #                                         lat = mean(lat)), by = .(id_tile, id_10, state_name, county_name, mukey)]
  # sum(one_field_dt$area_ha)
  #---------------------------------------------------------------------------
  #MAKE A MAP OF THE FIELD
  if(FALSE){

    one_field_sf <- grid10_soils_sf6[grid10_soils_sf6$id_10 == field_n$id_10 &
                                       grid10_soils_sf6$id_field == field_n$id_field,]
    
    (field <- tm_shape(one_field_sf) + tm_polygons("mukey") + 
       tm_layout(legend.text.size = 0.7,
                 main.title = paste('ONE FIELD MAP -', round(sum(one_field_sf$area_ha),1),' ha'),
                 main.title.position = "center",
                 main.title.size = 1))
    tmap_save(field, filename = "./vr_value/Data/figures/field.jpg", scale = 2)  
  }
  #---------------------------------------------------------------------------
  # FILTER APSIM DATA AND ADD VARIABLES NEEDED FOR PREDICTIONS
  ic_field_dt <- yc_yearly_dt3[id_10 == field_n$id_10 & mukey %in% unique(field_soils_dt$mukey), -c('area_ha', 'long', 'lat')]
  
  # Update area and coordinates using field level information
  ic_field_dt <- merge(ic_field_dt, field_soils_dt[,.(id_10, mukey, area_ha, lat, long)], by = c('id_10', 'mukey'))
  
  
  ic_field_dt[,.N, by = .(prev_crop, z, mukey)]
  all(ic_field_dt[,.N, by = mukey]$N == 1650)
  
  # if(!all(unique(one_field_dt$mukey) %in% unique(ic_field_dt$mukey))){next} #if not all the mukeys of the field are in the apsim files skip
  
  #===================================================================================================================
  # 1) PREDICT WITH REGIONAL MINIMUM MODEL UR
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  # the model is trained with z1-10 and testing is evaluated with z11-25
  testing_set <- merge(ic_field_dt[!z %in% training_z],
                           reg_model_stuff$model_minimum_regional, by = c('region', 'prev_crop')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    .[N_fert == eonr_pred] %>%
    .[,c("mukey", "z", "prev_crop", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5","N_fert",'P')]
  
  testing_set[, P := Yld * Pc - N_fert * Pn] #update profits
  testing_set[, gov := 0] #gov collection
  areas <- testing_set[,.(mukey, area_ha)] %>% unique()
  
  sum(areas$area_ha)
  
  small_list[[length(small_list)+1]] <- testing_set[,model := '1']

  #===================================================================================================================
  # 4) PREDICT WITH REGIONAL MODEL 2 - UR 
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  prediction_set <- data.table(unique(ic_field_dt[!z %in% training_z,
                             c('mukey', 'z','area_ha', no_cost_var, ss_var), with = FALSE])) #this is unique v5 conditions, doesn't have the different N rates
  
  test <- copy(prediction_set) #need to be here, before the columns are updated
  # We need to aggregate at the field level because is UR
  
  do_not_aggregate = c("mukey", "z", "area_ha", "prev_crop")
  do_aggregate = setdiff(c(no_cost_var, ss_var), do_not_aggregate)
  
  prediction_set_aggregated  <- aggregate_by_area(data_dt = prediction_set, variables = do_aggregate, 
                                                  weight = 'area_ha', by_c = c('z', 'prev_crop'))# %>% .[,-'area_ha']
  
  test[,n_20cm_v5 := n_20cm_v5 * area_ha]
  
  test2 <- merge(test[,.(n_20cm_v5 = sum(n_20cm_v5) / sum(area_ha)),by= c("z", "prev_crop")], 
                 prediction_set_aggregated[,.(z, prev_crop, n_20cm_v5)], by = c('z', 'prev_crop'))
  
  test2[, ok := round(n_20cm_v5.x,1) == round(n_20cm_v5.y,1)]
  
  all(test2$ok )
  
  prediction_set_aggregated[,eonr_pred := ceiling(predict(reg_model_stuff$model2b_eonr, prediction_set_aggregated, type = "class")/10)*10]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <- merge(ic_field_dt[!z %in% training_z],
                           prediction_set_aggregated[,.(z, prev_crop, eonr_pred)], by = c('z', 'prev_crop')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    .[N_fert == eonr_pred] %>%
    .[,c("mukey", "z", "prev_crop", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
  testing_set[, P := Yld * Pc - N_fert * Pn] #update profits
  testing_set[, gov := 0] #gov collection
  
  small_list[[length(small_list)+1]] <- testing_set[,model := '4']
  
  #===================================================================================================================
  # 13) PREDICT WITH MODEL ZERO
  models_yr <- names(reg_model_stuff_policy)[str_detect(names(reg_model_stuff_policy), pattern = 'model_yr')]
  
  #---------------------------------------------------------------------------
  # PERFORMANCE EVALUATION
  testing_set <-ic_field_dt[!z %in% training_z] %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    .[N_fert == 10] %>%
    .[,c("mukey", "z", "prev_crop", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
  
  testing_set[, P := Yld * Pc - N_fert * Pn] #update profits
  testing_set[, gov := 0] #gov collection
  
  
  # model_name <- c("14min", "14med", "14max")[which(c("model_fee_min", "model_fee_med", "model_fee_max") == model_n)]
  small_list[[length(small_list)+1]] <- testing_set[,model := 'model_zero']
  
  #===================================================================================================================
  # 13) PREDICT WITH YR MODEL
  models_yr <- names(reg_model_stuff_policy)[str_detect(names(reg_model_stuff_policy), pattern = 'model_yr')]
  
  for(model_n in models_yr){
    # model_n = models_yr[1]
    
    prediction_set_aggregated[,eonr_pred := ceiling(predict(reg_model_stuff_policy[[model_n]], prediction_set_aggregated, type = "class")/10)*10]
    
    #---------------------------------------------------------------------------
    # PERFORMANCE EVALUATION
    testing_set <- merge(ic_field_dt[!z %in% training_z],
                         prediction_set_aggregated[,.(z, prev_crop, eonr_pred)], by = c('z', 'prev_crop')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      .[N_fert == eonr_pred] %>%
      .[,c("mukey", "z", "prev_crop", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
    
    testing_set[, P := Yld * Pc - N_fert * Pn] #update profits
    testing_set[, gov := 0] #gov collection
    
    
    # model_name <- c("14min", "14med", "14max")[which(c("model_fee_min", "model_fee_med", "model_fee_max") == model_n)]
    small_list[[length(small_list)+1]] <- testing_set[,model := model_n]
  }
  
  #===================================================================================================================
  # 14) MODEL N-LEACHING FEE
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  models_fee <- names(reg_model_stuff_policy)[str_detect(names(reg_model_stuff_policy), pattern = 'model_fee')]
  
  for(model_n in models_fee){
    # model_n = models_fee[1]
    
    prediction_set_aggregated[,eonr_pred := ceiling(predict(reg_model_stuff_policy[[model_n]], prediction_set_aggregated, type = "class")/10)*10]
  
    #---------------------------------------------------------------------------
    # PERFORMANCE EVALUATION
    testing_set <- merge(ic_field_dt[!z %in% training_z],
                         prediction_set_aggregated[,.(z, prev_crop, eonr_pred)], by = c('z', 'prev_crop')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      .[N_fert == eonr_pred] %>%
      .[,c("mukey", "z", "prev_crop", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
    
    Pe <- as.numeric(str_extract(model_n,pattern = '[0-9.]+'))
    
    testing_set[, P := Yld * Pc - N_fert * Pn - leach_n2 * Pe] #update profits
    testing_set[, gov := leach_n2 * Pe] #gov collection
    
    
    # model_name <- c("14min", "14med", "14max")[which(c("model_fee_min", "model_fee_med", "model_fee_max") == model_n)]
    small_list[[length(small_list)+1]] <- testing_set[,model := model_n]
  }
  
  #===================================================================================================================
  # 15) MODEL TAX
  # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH MUKEY
  models_tax <- names(reg_model_stuff_policy)[str_detect(names(reg_model_stuff_policy), pattern = 'model_tax')]
  
  for(model_n in models_tax){
    # model_n =  "model_tax_0.05"
    
    prediction_set_aggregated[,eonr_pred := ceiling(predict(reg_model_stuff_policy[[model_n]], prediction_set_aggregated, type = "class")/10)*10]
    
    #---------------------------------------------------------------------------
    # PERFORMANCE EVALUATION
    testing_set <- merge(ic_field_dt[!z %in% training_z],
                         prediction_set_aggregated[,.(z, prev_crop, eonr_pred)], by = c('z', 'prev_crop')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      .[N_fert == eonr_pred] %>%
      .[,c("mukey", "z", "prev_crop", "id_10", "area_ha", "Yld", "leach_n2", "n_deep_v5", "N_fert", 'P' )]
    
    tax_n <- as.numeric(str_extract(model_n,pattern = '[0-9.]+'))
    testing_set[, P := Yld * Pc - N_fert * (Pn * (1+tax_n))] #update profits
    testing_set[, gov := N_fert * (Pn * (tax_n))] #gov collection
    
    small_list[[length(small_list)+1]] <- testing_set[,model := model_n]
  }
  
  #===================================================================================================================
  testing_set_dt <- rbindlist(small_list)
  # testing_set_dt[,model := as.numeric(model)]
  
  testing_set_dt[, .(Yld =  mean(Yld),
                      leach_n2 = mean(leach_n2),
                      N_fert = mean(N_fert),
                      P = mean(P),
                     gov = mean( gov),
                      area_ha = sum(area_ha)), by = .( model, prev_crop)][order(prev_crop, model)]
  
   testing_set_dt <- cbind(field_soils_dt[1,.(id_10, id_field, region)], testing_set_dt[,-'id_10'])
  testing_set_dt[,.N, by = model]
  
  #===================================================================================================================
  #PLOT ME
  
  if(FALSE){
    
    mukey_n <- field_soils_dt[area_ha == max(area_ha)]
    mukey_n <- mukey_n$mukey[1]
    testing_set_plot <- testing_set_dt[prev_crop == 1 & mukey == mukey_n ]
    testing_set_plot[,method := factor(model, levels= c('1', '2', '3','4', '5', '6', '7', '8', '9', '10', '11', '12'))]
    ic_field_plot <- ic_field_dt[!z %in% training_z & prev_crop == 1 & mukey == mukey_n ]

    # library(RColorBrewer)
    # n <- 12
    # qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
    # col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
    # colors_sample =sample(col_vector, n)
    # pie(rep(1,n), colors_sample)
    
    colors_sample=c( "#7570B3", "#FFED6F", "#666666", "#7FC97F", "#386CB0", "#B3B3B3", "#FFFFCC", "#A65628", "#F4CAE4", "#E41A1C", "#E6AB02", "black")
    
    # Y plot with Yld at eonr
    z_labels <- ic_field_plot[N_fert == max(ic_field_plot$N_fert), .(N_fert, Yld, z)][order(-Yld)]
    z_labels[seq(1, nrow(z_labels), by = 2), N_fert := N_fert - 20]
    
    (plot_n1 <- ggplot() +
        geom_point(data = testing_set_plot, aes(x = N_fert, y = Yld, colour = method, size = method)) +
        geom_line(data = ic_field_plot, aes(x = N_fert, y = Yld, group=z), show.legend = FALSE) +
        scale_size_manual(values=c(rep(2, 11), 4)) +
        scale_color_manual(values=colors_sample)+
        ylab('Yield (kg/ha)')+
        xlab('N rate (kg/ha)')+
        geom_text(data = z_labels, aes(x = N_fert, y = Yld, label = z))+
        theme_bw()+
        theme(panel.grid = element_blank()))
    
    # Y plot with Yld at eonr
    z_labels <- ic_field_plot[N_fert == max(ic_field_plot$N_fert), .(N_fert, Yld, z, leach_n2)][order(-leach_n2)]
    z_labels[seq(1, nrow(z_labels), by = 2), N_fert := N_fert - 20]
    
    (plot_n2 <- ggplot() +
        geom_point(data = testing_set_plot, aes(x = N_fert, y = leach_n2, colour = method, size = method)) +
        geom_line(data = ic_field_plot, aes(x = N_fert, y = leach_n2, group=z), show.legend = FALSE) +
        scale_size_manual(values=c(rep(2, 11), 4)) +
        scale_color_manual(values=colors_sample)+
        ylab('N Leaching (kg/ha)')+
        xlab('N rate (kg/ha)')+
        geom_text(data = z_labels, aes(x = N_fert, y = leach_n2, label = z))+
        theme_bw()+
        theme(panel.grid = element_blank()))
    
    
    ggsave(grid.arrange(plot_n1, plot_n2), filename = "./vr_value/Data/figures/yield_curve_example.jpg")
    
    # Y plot with Yld at eonr
    (plot_n1 <- ggplot() +
        geom_point(data = testing_set_plot[prev_crop == 1 & model != 12], aes(x = N_fert, y = Yld, colour = model)) +
        geom_point(data = testing_set_plot[prev_crop == 1 & model == 12], aes(x = N_fert, y = Yld), colour = 'black', size = 3, show.legend = FALSE) +
        geom_line(data = ic_field_plot[mukey == mukey_n & prev_crop == 1], aes(x = N_fert, y = Yld, group=interaction(z, prev_crop)), show.legend = FALSE) +
        # ggtitle(paste('Yld plot with Yld at eonr', mukey_n)))
        ggtitle('Yield'))
    
    ic_field_dt[mukey == mukey_n & prev_crop == 0][order(P)]
    ggsave(plot_n1, filename = "./vr_value/Data/figures/yield_curve_example.jpg")
    
    
    # P plot with P at eonr
    (plot_n <- ggplot() +
        geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 1 & model != 12], aes(x = N_fert, y = P, colour = model)) +
        geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 1 & model == 12], aes(x = N_fert, y = P), size = 3, show.legend = FALSE) +
        geom_line(data = ic_field_plot[mukey == mukey_n & prev_crop == 1], aes(x = N_fert, y = P, group=interaction(z, prev_crop)), show.legend = FALSE) +
        ggtitle(paste('P plot with P at eonr', mukey_n)))
    
    ic_field_dt[mukey == mukey_n & prev_crop == 0][order(P)]
    ggsave(plot_n, filename = "./vr_value/Data/figures/yield_curve_example.jpg")
    
    # leach_n2o3 plot with leaching at eonr
    (plot_n2 <- ggplot() +
        geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 0 & model != 12], aes(x = N_fert, y = leach_n2, colour = model)) +
        geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 0 & model == 12], aes(x = N_fert, y = leach_n2), size = 3, show.legend = FALSE) +
        geom_line(data = ic_field_plot[mukey == mukey_n & prev_crop == 0], aes(x = N_fert, y = leach_n2, group=interaction(z, prev_crop)), show.legend = FALSE) +
        # ggtitle(paste('leach_n2o3 plot with leaching at eonr', mukey_n)))
        ggtitle('N Leaching'))
    
    ggsave(plot_n2, filename = "./vr_value/Data/figures/leaching_curve_example.jpg")
    #---------------------------------------------------------------------------
    # ESL510
    (plot_n1 <- ggplot() +
        geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 1 & model != 12], aes(x = N_fert, y = Yld), size = 0.2) +
        geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 1 & model == 12], aes(x = N_fert, y = Yld), size = 3, show.legend = FALSE) +
        geom_line(data = ic_field_plot[mukey == mukey_n & prev_crop == 1], aes(x = N_fert, y = Yld, colour=z)) +
        # ggtitle(paste('Yld plot with Yld at eonr', mukey_n)))
        ggtitle('Yield'))
    
    (plot_n2 <- ggplot() +
        geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 1 & model != 12], aes(x = N_fert, y = leach_n2), size = 0.2) +
        geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 1 & model == 12], aes(x = N_fert, y = leach_n2), size = 3, show.legend = FALSE) +
        geom_line(data = ic_field_plot[mukey == mukey_n & prev_crop == 1], aes(x = N_fert, y = leach_n2, colour=z)) +
        # ggtitle(paste('Yld plot with Yld at eonr', mukey_n)))
        ggtitle('N Leaching'))
    
    grid.arrange(plot_n1, plot_n2)
    
    #---------------------------------------------------------------------------
    #NOW WITH N_TOTAL (FERT + SOIL)
    ic_field_plot[,N_total := N_fert + n_deep_v5]
    testing_set_plot[,N_total := N_fert + n_deep_v5]
    
    # P plot with P at eonr
    (plot_n <- ggplot() +
        geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 0 & model != 12], aes(x = N_total, y = P, colour = model)) +
        geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 0 & model == 12], aes(x = N_total, y = P), size = 3, show.legend = FALSE) +
        geom_line(data = ic_field_plot[mukey == mukey_n & prev_crop == 0], aes(x = N_total, y = P, group=interaction(z, prev_crop)), show.legend = FALSE) +
        ggtitle(paste('P plot with P at eonr', mukey_n)))
    
    ic_field_dt[mukey == mukey_n & prev_crop == 0][order(P)]
    ggsave(plot_n, filename = "./vr_value/Data/figures/yield_curve_example_ntotal.jpg")
    
    # leach_n2o3 plot with leaching at eonr
    (plot_n <- ggplot() +
        geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 0 & model != 12], aes(x = N_total, y = leach_n2, colour = model)) +
        geom_point(data = testing_set_plot[mukey == mukey_n & prev_crop == 0 & model == 12], aes(x = N_total, y = leach_n2), size = 3, show.legend = FALSE) +
        geom_line(data = ic_field_plot[mukey == mukey_n & prev_crop == 0], aes(x = N_total, y = leach_n2, group=interaction(z, prev_crop)), show.legend = FALSE) +
        ggtitle(paste('leach_n2o3 plot with leaching at eonr', mukey_n)))
    
    ggsave(plot_n, filename = "./vr_value/Data/figures/leaching_curve_example_ntotal.jpg")
  }#end of PLOT ME
  
  return(testing_set_dt)
}

time1 <- Sys.time()
big_list <- foreach(j=1:nrow(fields_list_dt)) %do% {
  process_field_economics(j)
  }
time2 <- Sys.time()


nopar <- time2 - time1

perfomances_dt <- rbindlist(big_list)
table(perfomances_dt[,.N, by = .(id_10, id_field, mukey, model)]$N)

perfomances_dt[, .(Yld =  mean(Yld),
                       leach_n2 = mean(leach_n2),
                       N_fert = mean(N_fert),
                       P = mean(P), 
                       gov = mean(gov),
                       area_ha = sum(area_ha)), by = .( model, prev_crop)][order(prev_crop, model)]


saveRDS(perfomances_dt, "./vr_value/Data/files_rds/perfomances_eonr_dt.rds")
perfomances_dt <- readRDS("./vr_value/Data/files_rds/perfomances_eonr_dt.rds")


