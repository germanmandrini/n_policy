# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
# setwd('~')

source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')
source('./vr_value/Codes/parameters.R')


grid10_soils_sf6 <- readRDS("./vr_value/Data/Grid/grid10_soils_sf6.rds")
grid10_tiles_sf2 <- readRDS("./vr_value/Data/Grid/grid10_tiles_sf2.rds")  
yc_yearly_dt2 <- readRDS("./vr_value/Data/files_rds/yc_yearly_dt2.rds")
eonr_mukey_dt3 <- readRDS("./vr_value/Data/files_rds/eonr_mukey_dt3.rds")
perfomances_dt <- readRDS("./vr_value/Data/files_rds/perfomances_dt.rds")
reg_model_stuff <- readRDS( "./vr_value/Data/files_rds/reg_model_stuff.rds")

yc_yearly_dt2[,rotation := ifelse(rotation == 'MSM', 0, 1)]
#======================================================================================

# GET THE FIELDS THAT CAN BE RUN
stations_dt <- reg_model_stuff$stations
full_fields_dt <- reg_model_stuff$full_fields
training_z <- reg_model_stuff$training_z
rm(reg_model_stuff)

#--------------------------------------------------------------------------------------
# MAP CHAMPAIGN
tm_shape(grid10_tiles_sf2[grid10_tiles_sf2$county_name == 'Champaign', ]) + 
  tm_polygons() + tm_text('id_10') +
  tm_shape(grid10_soils_sf6[grid10_soils_sf6$county_name == 'Champaign', ]) + 
  tm_polygons('id_field')
  
target_region_dt <- data.table(grid10_soils_sf6) %>%
  .[county_name == 'Champaign', ]

# PICK A FIELD
largest_mukey <- target_region_dt %>% .[,.(area_ha = sum(area_ha)), by = mukey] %>% .[order(-area_ha)]
mukey_n = largest_mukey$mukey[2]
id_10_n = target_region_dt[mukey == mukey_n][10,]$id_10
id_field_n = target_region_dt[mukey == mukey_n][10,]$id_field

one_field_sf <- grid10_soils_sf6[grid10_soils_sf6$id_10 == id_10_n & grid10_soils_sf6$id_field == id_field_n,]

#MAKE A MAP OF THE FIELD
if(FALSE){
  (field <- tm_shape(one_field_sf) + tm_polygons("mukey") + 
     tm_layout(legend.text.size = 0.7,
               main.title = paste('ONE FIELD MAP -', round(sum(one_field_sf$area_ha),1),' ha'),
               main.title.position = "center",
               main.title.size = 1))
  tmap_save(field, filename = "./vr_value/Data/figures/field.jpg", scale = 2)  
}

#===================================================================================================================
#PLOT ME
performance_set_plot <- perfomances_dt[mukey == mukey_n & id_10 == id_10_n & id_field == id_field_n]

summary_performance_dt <- performance_set_plot[, .(Yld =  mean(Yld),
                                               leach_n = mean(leach_n),
                                               leach_ext = max(leach_n), #leaching in the year with max leaching
                                               N_fert = mean(N_fert),
                                               P = mean(P)), by = .(id_10, region, model, tech, rotation)] %>% .[order(rotation)]

performance_set_plot[,model := as.character(model)]

ic_field_plot <- yc_yearly_dt2[mukey == mukey_n & id_10 == id_10_n] %>%
  .[!z %in% training_z]

# Y plot with Yld at eonr
(plot_n <- ggplot() +
    geom_point(data = performance_set_plot[rotation == 0 & model %in% c(4,6)], aes(x = N_fert, y = P, colour = model)) +
    geom_point(data = performance_set_plot[rotation == 0 & model == 12], aes(x = N_fert, y = P), size = 3, show.legend = FALSE) +
    geom_line(data = ic_field_plot[rotation == 0], aes(x = N_fert, y = P, group=interaction(z, rotation)), show.legend = FALSE) +
    ggtitle(paste('P plot with Yld at eonr', mukey_n)))

ic_field_dt[mukey == mukey_n & rotation == 0][order(P)]
ggsave(plot_n, filename = "./vr_value/Data/figures/yield_curve_example.jpg")

# EONR vs recommended

paired <- merge(performance_set_plot[, .(id_10, id_field, region,  mukey,   z, rotation, model, tech, N_fert, n_deep_v5,P)], 
                performance_set_plot[model == 12, .(id_10, id_field, region,  mukey,   z, rotation, N_fert, P)], 
                by = c('id_10', 'id_field', 'region', 'mukey', 'z', 'rotation'), suffixes=c("", "_12"))
setnames(paired, c('N_fert_12', 'N_fert'), c('eonr', 'N_rec'))
paired <- paired[rotation == 0]
ggplot(data = paired[model %in% c(5,6)], aes(x = eonr, y = N_rec, colour = as.factor(model))) +
  geom_point()+
  geom_smooth(se=FALSE)+
  coord_fixed() + geom_abline() + ylim(0, 350)+ xlim(0, 350) +
  theme(aspect.ratio=1, 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_bw()

#RMSE
paired[, N_diff := N_rec - eonr]
paired[,.(N_diff = mean(N_diff)), by = model]
paired[,res1 := abs(N_diff)]
paired[,.(RMSE = sd(res1)), by = model ]
sd(paired$res1)




p1 <- ggplot(data = paired[rotation == 0 & model %in% c(5,6,12)], aes(x = n_deep_v5, y = N_rec, colour = as.factor(model))) +
  geom_point()+
  #geom_smooth(se=FALSE)+
  #coord_fixed() + 
  #geom_abline() + ylim(0, 350)+ xlim(0, 350) +
  theme(#aspect.ratio=1, 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_bw()

p2 <- ggplot(data = paired[rotation == 0 & model %in% c(5,6,12)], aes(x = n_deep_v5, y = P, colour = as.factor(model))) +
  geom_point()+
  # geom_smooth(se=FALSE)+
  #coord_fixed() + 
  #geom_abline() + ylim(0, 350)+ xlim(0, 350) +
  theme(#aspect.ratio=1, 
    axis.text=element_text(size=12),
    axis.title=element_text(size=14,face="bold"))+
  theme_bw()

grid.arrange(p1, p2)




paired[,P_diff := P - P_12]
paired[,N_diff := N_rec - eonr]

ggplot(data = paired, aes(x = N_diff, y = P_diff)) +
  geom_point()+
  geom_smooth(se=FALSE)+
  coord_fixed() + 
  #geom_abline() + 
  # ylim(0, 350)+ xlim(0, 350) +
  theme(aspect.ratio=1, 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_bw()



ggplot() +
  geom_point(data = performance_set_plot[model %in% c(12)], aes(x = n_deep_v5, y = N_fert, colour = as.factor(model)))

ggplot() +
  geom_point(data = performance_set_plot[model %in% c(4,6)], aes(x = n_deep_v5, y = N_fert, colour = rotation))

ggplot() +
  geom_point(data = performance_set_plot[model == 4], aes(x = n_deep_v5, y = N_fert, colour = rotation))

#Difference in profits
paired <- merge(performance_set_plot[model == 6], performance_set_plot[model == 4], by = c('id_10', 'id_field', 'region', 'mukey', 'z', 'rotation', 'area_ha', 'n_deep_v5'), suffixes=c("_6", "_4"))
paired[,P_diff := P_4 - P_6]

ggplot() +
  geom_point(data = paired, aes(x = n_deep_v5, y = P_diff))

#Difference in profits for all
paired <- merge(perfomances_dt[model == 12], perfomances_dt[model == 6], by = c('id_10', 'id_field', 'region', 'mukey', 'z', 'rotation', 'area_ha', 'n_deep_v5'), suffixes=c("_12", "_4"))
paired[,P_diff := P_4 - P_12]

ggplot() +
  geom_point(data = paired, aes(x = N_fert_12, y = jitter(N_fert_4)))+
  coord_fixed() + geom_abline() + ylim(0, 350)+ xlim(0, 350) +
  geom_smooth()+
  theme(aspect.ratio=1, 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_bw()

#RMSE
paired[,res1 := abs(N_fert_4 - N_fert_12)]
paired[,res2 := abs(N_fert_6 - N_fert_12)]
ValidSet[,res2 := abs(eonr - eonr_pred2b)]
sd(paired$res1)
sd(ValidSet$res2)


  
  
  
  # P plot with P at eonr
  (plot_n <- ggplot() +
      geom_point(data = performance_set_plot[mukey == mukey_n & rotation == 1 & model != 12], aes(x = N_fert, y = P, colour = model)) +
      geom_point(data = performance_set_plot[mukey == mukey_n & rotation == 1 & model == 12], aes(x = N_fert, y = P), size = 3, show.legend = FALSE) +
      geom_line(data = ic_field_plot[mukey == mukey_n & rotation == 1], aes(x = N_fert, y = P, group=interaction(z, rotation)), show.legend = FALSE) +
      ggtitle(paste('P plot with P at eonr', mukey_n)))
  
  ic_field_dt[mukey == mukey_n & rotation == 0][order(P)]
  ggsave(plot_n, filename = "./vr_value/Data/figures/yield_curve_example.jpg")
  
  # leach_no3 plot with leaching at eonr
  (plot_n <- ggplot() +
      geom_point(data = performance_set_plot[mukey == mukey_n & rotation == 0 & model != 12], aes(x = N_fert, y = leach_n, colour = model)) +
      geom_point(data = performance_set_plot[mukey == mukey_n & rotation == 0 & model == 12], aes(x = N_fert, y = leach_n), size = 3, show.legend = FALSE) +
      geom_line(data = ic_field_plot[mukey == mukey_n & rotation == 0], aes(x = N_fert, y = leach_n, group=interaction(z, rotation)), show.legend = FALSE) +
      ggtitle(paste('leach_no3 plot with leaching at eonr', mukey_n)))
  
  ggsave(plot_n, filename = "./vr_value/Data/figures/leaching_curve_example.jpg")
  #---------------------------------------------------------------------------
  #NOW WITH N_TOTAL (FERT + SOIL)
  ic_field_plot[,N_total := N_fert + n_deep_v5]
  performance_set_plot[,N_total := N_fert + n_deep_v5]
  
  # P plot with P at eonr
  (plot_n <- ggplot() +
      geom_point(data = performance_set_plot[rotation == 0 & model %in% c(4,6)], aes(x = N_total, y = P, colour = model)) +
      geom_point(data = performance_set_plot[mukey == mukey_n & rotation == 0 & model == 12], aes(x = N_total, y = P), size = 3, show.legend = FALSE) +
      geom_line(data = ic_field_plot[mukey == mukey_n & rotation == 0], aes(x = N_total, y = P, group=interaction(z, rotation)), show.legend = FALSE) +
      ggtitle(paste('P plot with P at eonr', mukey_n)))
  
  ic_field_dt[mukey == mukey_n & rotation == 0][order(P)]
  ggsave(plot_n, filename = "./vr_value/Data/figures/yield_curve_example_ntotal.jpg")
  
  # leach_no3 plot with leaching at eonr
  (plot_n <- ggplot() +
      geom_point(data = performance_set_plot[mukey == mukey_n & rotation == 0 & model != 12], aes(x = N_total, y = leach_n, colour = model)) +
      geom_point(data = performance_set_plot[mukey == mukey_n & rotation == 0 & model == 12], aes(x = N_total, y = leach_n), size = 3, show.legend = FALSE) +
      geom_line(data = ic_field_plot[mukey == mukey_n & rotation == 0], aes(x = N_total, y = leach_n, group=interaction(z, rotation)), show.legend = FALSE) +
      ggtitle(paste('leach_no3 plot with leaching at eonr', mukey_n)))
  
  ggsave(plot_n, filename = "./vr_value/Data/figures/leaching_curve_example_ntotal.jpg")

}