setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
# setwd('~')

source('./Codes_useful/R.libraries.R')
library(scales)
source('./Codes_useful/gm_functions.R')
source('./vr_value/Codes/parameters.R')

# source('./Codes_useful/gm_functions.R')

grid10_soils_sf6 <- readRDS("./vr_value/Data/Grid/grid10_soils_sf6.rds")

grid10_tiles_sf3 <- readRDS("./vr_value/Data/Grid/grid10_tiles_sf3.rds") 
grid10_fields_sf <- readRDS("./vr_value/Data/Grid/grid10_fields_sf.rds") 

# perfomances_dt <- readRDS("./vr_value/Data/files_rds/perfomances_dt.rds")
perfomances_dt <- readRDS("./vr_value/Data/files_rds/perfomances_eonr_dt.rds")
# perfomances_dt[,model := as.numeric(model)]

# reg_model_stuff <- readRDS( "./vr_value/Data/files_rds/reg_model_stuff.rds")
# full_fields_dt3 <- reg_model_stuff$full_fields #one row by field x soil
# rm(reg_model_stuff)
# 
# perfomances_dt <- merge(perfomances_dt[,-'area_ha'], full_fields_dt3[,.(id_10, id_field, mukey, area_ha)], by = c('id_10', 'id_field', 'mukey'))
summary(perfomances_dt[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, prev_crop, model, z)]$area_ha)

#-------------------------------------------------------------------------
# AGGREGATE THE DATA TO CELL X Z LEVEL CONSIDERING THE AREA
names(perfomances_dt)
do_not_aggregate = c('id_10', 'region','model', 'z', 'prev_crop')
do_aggregate =  c("Yld", "leach_n2", "N_fert","P", 'gov')

#First aggregate without z so then we can get the leach_extreme
perfomances_dt2 <- aggregate_by_area(data_dt = perfomances_dt, variables = do_aggregate, 
                                        weight = 'area_ha', by_c = do_not_aggregate) #cell x z level (mukey and field are out)

perfomances_dt3 <- perfomances_dt2[, .(Yld =  mean(Yld),
                                       leach_n2 = mean(leach_n2),
                                       leach_ext = max(leach_n2), #leaching in the year with max leaching. Most of the time will be after corn. Pushed UP
                                       N_fert = mean(N_fert),
                                       P = mean(P), 
                                       gov = mean(gov), 
                                       area_ha = mean(area_ha)), by = .(region, id_10, model)] #field level (z and prev_crop are out)
perfomances_dt3[,.(mean(leach_ext))]

#---------------------------------------------------------------------------
# AGGREGATE AGAIN CONSIDERING THE CORN PRODUCTION OF THE CELL
grid10_tiles_dt <- data.table(grid10_tiles_sf3)[,.(id_tile,id_10, corn_avg_ha,corn5_tile )]

summary(grid10_tiles_dt$corn_avg_ha)

perfomances_dt3 <- merge(perfomances_dt3, grid10_tiles_dt, by = 'id_10')

perfomances_dt4 <- aggregate_by_area(data_dt = perfomances_dt3, variables = c("Yld", "leach_n2", "leach_ext", "N_fert","P", 'gov'), 
                                         weight = 'corn_avg_ha', by_c = c('model')) #state level, weighted by corn_ha
#---------------------------------------------------------------------------
# YR CHART

perfomances_dt4[,model_val := as.numeric(str_extract(model,pattern = '[0-9.]+'))]
perfomances_dt4[,model_name := lapply(perfomances_dt4$model, function(x) str_split(x, pattern = '_')[[1]][2])]

plot_dt <- perfomances_dt4[model_name == 'yr'] 

plot_dt1 <- melt(plot_dt, id.vars = 'model_val', measure.vars = c('Yld', 'leach_n2', 'N_fert', 'P'))
plot_dt2 <- melt(plot_dt[model_val == 1], id.vars = 'model_val', measure.vars = c('Yld', 'leach_n2', 'N_fert', 'P'))

plot_dt3 <- merge(plot_dt1, plot_dt2[,.(variable, value_max = value)], by = c('variable'))
plot_dt3[, value_rel := value/value_max]

ggplot(plot_dt3)+
  geom_line(aes(x = model_val, y =  value_rel, colour = variable))

#---------------------------------------------------------------------------
# TAX CHART

plot_dt <- perfomances_dt4[model_name == 'tax'] 

plot_dt1 <- melt(plot_dt, id.vars = 'model_val', measure.vars = c('Yld', 'leach_n2', 'N_fert', 'P'))
plot_dt2 <- melt(plot_dt[model_val == 0], id.vars = 'model_val', measure.vars = c('Yld', 'leach_n2', 'N_fert', 'P'))

plot_dt3 <- merge(plot_dt1, plot_dt2[,.(variable, value_max = value)], by = c('variable'))
plot_dt3[, value_rel := value/value_max]

ggplot(plot_dt3) +
  geom_line(aes(x = model_val, y =  value_rel, colour = variable))


#---------------------------------------------------------------------------
# FEE CHART

plot_dt <- perfomances_dt4[model_name == 'fee'] 

plot_dt1 <- melt(plot_dt, id.vars = 'model_val', measure.vars = c('Yld', 'leach_n2', 'N_fert', 'P'))
plot_dt2 <- melt(plot_dt[model_val == 8.88], id.vars = 'model_val', measure.vars = c('Yld', 'leach_n2', 'N_fert', 'P'))

plot_dt3 <- merge(plot_dt1, plot_dt2[,.(variable, value_max = value)], by = c('variable'))
plot_dt3[, value_rel := value/value_max]

ggplot(plot_dt3) +
  geom_line(aes(x = model_val, y =  value_rel, colour = variable))

#---------------------------------------------------------------------------
# BAR CHART PROFITS
perfomances_dt4[,model := factor(model, levels= c('1', '2', '3','4', '5', '6', '7', '8', '9', '10', '11', '12'))]

(p1 <- ggplot(perfomances_dt4, aes(x = model, y =  Yld, fill = tech))+
    geom_bar(stat="identity") +
    scale_y_continuous(limits=c(min(perfomances_dt4$Yld)-100,max(perfomances_dt4$Yld)+50),oob = rescale_none)+
    ggtitle('Yield')+
    ylab('Yield (kg/ha)') +
    # ylab('Precipitation (mm)') +
    theme_bw())

(p2 <- ggplot(perfomances_dt4, aes(x = model, y = P, fill = tech))+
  geom_bar(stat="identity") +
  scale_y_continuous(limits=c(min(perfomances_dt4$P)-10,max(perfomances_dt4$P)+10),oob = rescale_none)+
  ggtitle('Profits')+
  ylab('Profits ($/ha)') +  
  theme_bw())

(p3 <- ggplot(perfomances_dt4, aes(x = model, y = N_fert, fill = tech))+
    geom_bar(stat="identity") +
    scale_y_continuous(limits=c(min(perfomances_dt4$N_fert)-10,max(perfomances_dt4$N_fert)+10),oob = rescale_none)+
    ylab('N (kg/ha)') +
    ggtitle('N Fertilizer')+
    theme_bw())
  
(p4 <- ggplot(perfomances_dt4, aes(x = model, y = leach_n2, fill = tech))+
    geom_bar(stat="identity") +
    scale_y_continuous(limits=c(min(perfomances_dt4$leach_n2)-1,max(perfomances_dt4$leach_n2)+1),oob = rescale_none)+
    ggtitle('Mean Leaching')+
    ylab('N Leaching (kg/ha)') +
    theme_bw())

47/51.4
80/99

(p5 <- ggplot(perfomances_dt4, aes(x = model, y = leach_ext, fill = tech))+
    geom_bar(stat="identity") +
    scale_y_continuous(limits=c(min(perfomances_dt4$leach_ext)-1,max(perfomances_dt4$leach_ext)+1),oob = rescale_none)+
    ggtitle('Extreme Leaching')+
    ylab('N Leaching (kg/ha)') +
    theme_bw())
  
ggsave(grid.arrange(p1, p2, p3, ncol = 1), filename = "./vr_value/Data/figures/state_total_barchart1.jpg")

ggsave(grid.arrange(p4, p5, ncol = 1), filename = "./vr_value/Data/figures/state_total_barchart2.jpg")
#---------------------------------------------------------------------------
# Method 4 vs 1 total savings and more
total_values_1_4_dt <- melt(data = perfomances_dt4[model %in% c(1,4)],id.vars = c('model', 'tech', 'corn_avg_ha'), measure.vars = c('Yld', 'leach_n2', 'leach_ext',   'N_fert', 'P'))
total_values_1_4_dt[,value := value * corn_avg_ha]
total_values_1_4_dt[variable == 'Yld',value2 := value /1000000000]
total_values_1_4_dt[variable == 'Yld',unit := 'billion_tn']
total_values_1_4_dt[variable %in% c('leach_n2', 'leach_ext',   'N_fert') ,value2 := value /1000000]
total_values_1_4_dt[variable %in% c('leach_n2', 'leach_ext',   'N_fert'),unit := 'million_tn']
total_values_1_4_dt[variable == 'P',value2 := value /1000000000]
total_values_1_4_dt[variable == 'P',unit := 'billion_usd']

total_values_1_4_dt <- merge(total_values_1_4_dt, 
                             data.table(variable = c('Yld', 'leach_n2', 'leach_ext',   'N_fert', 'P'), 
                                        title = c('a)', 'b)', 'c)', 'd)', 'e)')))


# total_values_1_4_dt <- total_values_1_4_dt[variable %in% c('leach_n2', 'leach_ext',   'N_fert')]

plot_dt <- total_values_1_4_dt[variable %in% c('Yld')]
(p1 <- ggplot(plot_dt, aes(x = model, y = value2))+
    geom_bar(stat="identity") +
    # geom_point()+
    scale_y_continuous(limits=c(min(plot_dt$value2)-1,max(plot_dt$value2)+1.5),oob = rescale_none)+
    # scale_fill_manual(values=c("#999999", "#E69F00"))+
    # ggtitle('Mean Leaching')+
    ylab('Yield (billion/tn)') +
    # geom_blank(data = dummy, aes(model, value2))+
    theme_bw()+
    facet_wrap(~title, scales = "free"))

plot_dt <- total_values_1_4_dt[variable %in% c('leach_n2')]
(p2 <- ggplot(plot_dt, aes(x = model, y = value2))+
    geom_bar(stat="identity") +
    # geom_point()+
    # scale_y_continuous(limits=c(min(plot_dt$value2)-1,max(plot_dt$value2)+1.5),oob = rescale_none)+
    # scale_fill_manual(values=c("#999999", "#E69F00"))+
    # ggtitle('Mean Leaching')+
    ylab('N leaching (million/tn)') +
    # geom_blank(data = dummy, aes(model, value2))+
    theme_bw()+
    facet_wrap(~title, scales = "free"))

plot_dt <- total_values_1_4_dt[variable %in% c('leach_ext')]
(p3 <- ggplot(plot_dt, aes(x = model, y = value2))+
    geom_bar(stat="identity") +
    # geom_point()+
    # scale_y_continuous(limits=c(min(plot_dt$value2)-1,max(plot_dt$value2)+1.5),oob = rescale_none)+
    # scale_fill_manual(values=c("#999999", "#E69F00"))+
    # ggtitle('Mean Leaching')+
    ylab('N leaching (million/tn)') +
    # geom_blank(data = dummy, aes(model, value2))+
    theme_bw()+
    facet_wrap(~title, scales = "free"))

plot_dt <- total_values_1_4_dt[variable %in% c('N_fert')]
(p4 <- ggplot(plot_dt, aes(x = model, y = value2))+
    geom_bar(stat="identity") +
    # geom_point()+
    # scale_y_continuous(limits=c(min(plot_dt$value2)-1,max(plot_dt$value2)+1.5),oob = rescale_none)+
    # scale_fill_manual(values=c("#999999", "#E69F00"))+
    # ggtitle('Mean Leaching')+
    ylab('N fertilizer use (million/tn)') +
    # geom_blank(data = dummy, aes(model, value2))+
    theme_bw()+
    facet_wrap(~title, scales = "free"))

plot_dt <- total_values_1_4_dt[variable %in% c('P')]
(p5 <- ggplot(plot_dt, aes(x = model, y = value2))+
    geom_bar(stat="identity") +
    # geom_point()+
    scale_y_continuous(limits=c(6,7),oob = rescale_none)+
    # scale_fill_manual(values=c("#999999", "#E69F00"))+
    # ggtitle('Mean Leaching')+
    ylab('Profits (billion $)') +
    # geom_blank(data = dummy, aes(model, value2))+
    theme_bw()+
    facet_wrap(~title, scales = "free"))


ggsave(gridExtra::grid.arrange(p1, p2, p3, p4, p5, ncol=2), filename = "./vr_value/Data/figures/state_total_1vs4.jpg")

#---------------------------------------------------------------------------
# RMSE
paired <- merge(perfomances_dt[, .(id_10, id_field, region, mukey,z, prev_crop, model, N_fert)],
                perfomances_dt[model == 12, .(id_10, id_field, region, mukey,z, prev_crop, N_fert)],
                by = c('id_10', 'id_field', 'region', 'mukey', 'z', 'prev_crop'), suffixes=c("", "_12"))

paired[,res1 := abs(N_fert - N_fert_12)]

rmse_dt <- paired[,.(RMSE = sqrt(mean(res1^2)),
          MAD = mean(res1)), by = .(model) ]
rmse_dt[,model := factor(model, levels= c('1', '2', '3','4', '5', '6', '7', '8', '9', '10', '11', '12'))]

(p1 <- ggplot(rmse_dt, aes(x = model, y = RMSE))+
    geom_bar(stat="identity") )

latex_table_dt <- merge(perfomances_dt4[,-'corn_avg_ha'], rmse_dt[,-'MAD'], by = 'model')

cols <- names(latex_table_dt)[sapply(latex_table_dt,is.numeric)]
latex_table_dt[,(cols) := round(.SD,1), .SDcols=cols]
setnames(latex_table_dt, c('Yld', 'leach_n2', 'leach_ext', 'N_fert', 'P', 'RMSE'),
         c('Yield', 'N leaching', 'N leach ext', 'N rate', 'Profits', 'RMSE'))

library('xtable')
print(xtable(latex_table_dt, type = "latex", auto = TRUE, label = 'tab:state_output', 
             caption = 'Results for the State of Illinois. RMSE was calculated using \ref{eq_rmse} 
             and aggregation of other indicators was done using \ref{eq_I_state}'), 
      file = "./vr_value/Data/figures/state_output.tex", include.rownames=FALSE)

?print.xtable
?xtable

latex_table_dt[model==2, ]$Profits - latex_table_dt[model==1, ]$Profits
latex_table_dt[model==4, ]$Profits - latex_table_dt[model==1, ]$Profits
latex_table_dt[model==5, ]$Profits - latex_table_dt[model==4, ]$Profits

latex_table_dt[model==2, ]$'N leaching' - latex_table_dt[model==1, ]$'N leaching'
latex_table_dt[model==4, ]$'N leaching' - latex_table_dt[model==1, ]$'N leaching'
latex_table_dt[model==5, ]$'N leaching' - latex_table_dt[model==4, ]$'N leaching'

#=====================================================================================================================
# MRTN vs Minimum Model

reg_model_stuff <- readRDS( "./vr_value/Data/files_rds/reg_model_stuff.rds")
model_minimum_regional <- reg_model_stuff$model_minimum_regional
rm(reg_model_stuff)

mrtn_dt <- data.table(region = c(3,3,2,2,1,1), 
           prev_crop = c(0,1,0,1,0,1),
           MRTN_Rate_lbN_ac = c(161, 200, 175, 193,187, 192))
mrtn_dt[,MRTN_rate := round(MRTN_Rate_lbN_ac * 1.12,0)] #1 pound per acre = 1.12 kilograms per hectare

model_minimum_regional2 <- merge(model_minimum_regional, mrtn_dt[,-'MRTN_Rate_lbN_ac'], by = c('region', 'prev_crop'))
model_minimum_regional2[,prev_crop := ifelse(prev_crop == 0, 'Soybean', 'Corn')]
model_minimum_regional2[,region := ifelse(region == 1, '1_South', ifelse(region == 2, '2_Central', '3_North'))]
setnames(model_minimum_regional2, 'eonr_pred', 'method1_rate')
model_minimum_regional2[order(-region, prev_crop)]

print.xtable(xtable(model_minimum_regional2, type = "latex", auto = TRUE, 
             label = 'tab:method1', 
             caption = 'Method 1 predictions paired with MRTN recommendations for the same region'),
             file = "./vr_value/Data/figures/method1.tex", include.rownames=FALSE)

ggplot(model_minimum_regional2, aes(x = MRTN_rate, y = method1_rate)) + 
  geom_point()+geom_smooth(se=FALSE)+
  coord_fixed() + geom_abline() + ylim(0, 350)+ xlim(0, 350) +
  theme(aspect.ratio=1, 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_bw()


#=====================================================================================================================
#-----------------------------------------VALUE OF INFORMATION--------------------------------------------------------
#=====================================================================================================================
# MAKE A MAP OF TOTAL LEACHING WITH MODEL 1 AND REDUCTION WITH MODEL 4
value_dt <- perfomances_dt3[model %in% c(1, 4), .(id_10, model, leach_n2, corn_avg_ha, leach_ext)]

value_dt[, leach_n2_cell := leach_n2 * corn_avg_ha]
value_dt[, leach_ext_cell := leach_ext * corn_avg_ha]

value_dt <- dcast(value_dt, id_10 ~ model, value.var = c('leach_n2_cell', 'leach_ext_cell'))
# setnames(value_dt, c('1', '4'), c('leach_n2_m1', 'leach_n2_m2'))
#make one negative
value_dt[, leach_val := leach_n2_cell_4-leach_n2_cell_1]


value_sf <- merge(grid10_tiles_sf3, value_dt[,.(id_10, leach_n2_cell_1,leach_val, leach_ext_cell_1)], by = 'id_10', all.x = T)


(p1 <- tm_shape(value_sf) + tm_polygons(c('corn_avg_ha'), n =10, title = c("Corn area (ha/cell)"),
                                        style ="cont", palette = "Greys")+
    tm_layout(panel.labels = 'a)',
              legend.text.size = 0.7,
              main.title.size = 1.2,
              legend.position = c('left', 'bottom')))

breaks_n <- c(50000,100000,200000,300000,400000)

(p2 <- tm_shape(value_sf) + tm_polygons(c('leach_n2_cell_1'), breaks = breaks_n, title = c("N Leaching (kg/cell)"),
                                        style ="cont", palette = "Greys")+
    tm_layout(panel.labels = 'b)',
              legend.text.size = 0.7,
              main.title.size = 1.2,
              legend.position = c('left', 'bottom')))

# breaks_n <- c(100000,250000,500000,750000,1000000)
# 
# (p2b <- tm_shape(value_sf) + tm_polygons(c('leach_ext_cell_1'), breaks = breaks_n, title = c("Leaching (kg/cell)"),
#                                         style ="cont", palette = "Greys")+
#     tm_layout(panel.labels = 'b)',
#               legend.text.size = 0.7,
#               main.title.size = 1.2,
#               legend.position = c('left', 'bottom')))


# tmap_save(p1, "./vr_value/Data/figures/baseline_characterization_map.jpg")

#---------------------------------------------------------------------------------------------------
# MAKE A MAP OF VALUE ECONOMIC OF INFORMATION SS
value_dt <- perfomances_dt3[model %in% c(1,4)]

#make one negative
value_dt[model == 1, Yld := -Yld]
value_dt[model == 1, leach_n2 := -leach_n2]
value_dt[model == 1, leach_ext := -leach_ext]
value_dt[model == 1, N_fert := -N_fert]
value_dt[model == 1, P := -P]

# Add values by group
value_dt <- value_dt[, .(Yld =  sum(Yld),
                         leach_n2 = sum(leach_n2),
                         leach_ext = sum(leach_ext), 
                         N_fert = sum(N_fert),
                         P = sum(P)), by = .(id_10)]

value_sf <- merge(value_sf, value_dt[,.(id_10, leach_n2,P)], by = 'id_10', all.x = T)

breaks_n <- c(-60000,-40000,-20000,0)

(p3 <- tm_shape(value_sf) + tm_polygons(c('leach_val'), breaks = breaks_n, title = expression(paste('EB'^'I-SS-ex ante', '(kg/cell)')),
                                        style ="cont", palette = "-Greys", midpoint = -10000, colorNA = 'white')+
    tm_layout(panel.labels = 'c)',
              legend.text.size = 0.7,
              main.title.size = 1.2,
              legend.position = c('left', 'bottom')))

value_sf <- dplyr::mutate(value_sf, P_r = round(P, 0))

breaks_n <- c(-20,5,10,20, 30,40)

(p4 <- tm_shape(value_sf) + tm_polygons(c('P'), title = expression(paste('V'^'I-SS-ex ante', '($/ha)')), 
                                        breaks = breaks_n, 
                                        style ="cont", palette = "Greys", colorNA = 'white', midpoint = 0)+
    tm_layout(panel.labels = 'd)',
              legend.text.size = 0.7,
              main.title.size = 1.2,
              legend.position = c('left', 'bottom')))

tmap_save(tmap_arrange(p1, p2, p3, p4, ncol = 2) , "./vr_value/Data/figures/information_characterization_map.jpg")


#---------------------------------------------------------------------------
# PDF OF LEACHING



#=====================================================================================================================
#-----------------------------------------VALUE OF TECHNOLOGY--------------------------------------------------------
#=====================================================================================================================
#1) MAKE A MAP OF VALUE TECHNOLOGY (EX POST VALUE)
#Select the two models of interest
value_dt <- perfomances_dt3[model %in% c(11,12)]

#make one negative
value_dt[model == 11, Yld := -Yld]
value_dt[model == 11, leach_n2 := -leach_n2]
value_dt[model == 11, leach_ext := -leach_ext]
value_dt[model == 11, N_fert := -N_fert]
value_dt[model == 11, P := -P]

# Add values by group
value_dt <- value_dt[, .(Yld =  sum(Yld),
                                        leach_n2 = sum(leach_n2),
                                        leach_ext = sum(leach_ext), 
                                        N_fert = sum(N_fert),
                                        P = sum(P)), by = .(id_10)]
value_dt[P <0, P := 0]

value_sf <- merge(grid10_tiles_sf3, value_dt[,.(id_10, P)], by = 'id_10', all.x = T) %>%
  dplyr::mutate(method = 'ex_post')

# (p1 <- tm_shape(value_sf) + tm_polygons(c('P'), n =10)+
#     tm_layout(legend.text.size = 0.7,
#               main.title = paste('Ex-post'),
#               main.title.position = "center",
#               main.title.size = 1.2))


breaks_n <- c(-20,0, 5,10,15,20, 30,40)
(p1 <- tm_shape(value_sf) + tm_polygons(c('P'), 
                                        # textNA="Not VR area", 
                                        #title = expression(paste('VR market', '($/cell)')), 
                                        title = "",
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
value_dt <- perfomances_dt3[model %in% c(4,5)]
#make one negative
value_dt[model == 4, Yld := -Yld]
value_dt[model == 4, leach_n2 := -leach_n2]
value_dt[model == 4, leach_ext := -leach_ext]
value_dt[model == 4, N_fert := -N_fert]
value_dt[model == 4, P := -P]

# Add values by group
value_dt <- value_dt[, .(Yld =  sum(Yld),
                         leach_n2 = sum(leach_n2),
                         leach_ext = sum(leach_ext), 
                         N_fert = sum(N_fert),
                         P = sum(P)), by = .(id_10)]

value_sf <- merge(grid10_tiles_sf3, value_dt[,.(id_10, P)], by = 'id_10', all.x = T) %>%
  dplyr::mutate(method = 'ex_ante')
# value_sf$P[is.na(value_sf$P)] <- 0 


# (p2 <- tm_shape(value_sf) + tm_polygons(c('P'), n =10)+
#     tm_layout(legend.text.size = 0.7,
#               main.title = paste('Ex-ante'),
#               main.title.position = "center",
#               main.title.size = 1.2))

breaks_n <- c(-20,0,2.5, 5, 7.5, 10, 15)

(p2 <- tm_shape(value_sf) + tm_polygons(c('P'), 
                                        # textNA="Not VR area", 
                                        #title = expression(paste('VR market', '($/cell)')), 
                                        title = "",
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




# tmap_mode("view")
# 
# tm_basemap("OpenStreetMap.DE") +
#   tm_shape(value_sf) + tm_polygons(c('P'), n =10)+
#   tm_layout(legend.text.size = 0.7,
#             main.title = paste('EX-ANTE'),
#             main.title.position = "center",
#             main.title.size = 1.2)
#   
# 
  

# ---------------------------------------------------------------------------
# 3) MAKE A MAP OF TECHNOLOGY MARKET CAP BY CELL

# MAKE A MAP OF VALUE TECHNOLOGY (EX ANTE VALUE)
value_dt <- perfomances_dt3[model %in% c(4,5)]
#make one negative
value_dt[model == 4, Yld := -Yld]
value_dt[model == 4, leach_n2 := -leach_n2]
value_dt[model == 4, leach_ext := -leach_ext]
value_dt[model == 4, N_fert := -N_fert]
value_dt[model == 4, P := -P]

# Add values by group
value_dt <- value_dt[, .(Yld =  sum(Yld),
                         leach_n2 = sum(leach_n2),
                         leach_ext = sum(leach_ext), 
                         N_fert = sum(N_fert),
                         P = sum(P),
                         corn_avg_ha = mean(corn_avg_ha)), by = .(id_10)]

value_dt <- value_dt[P > 2] #
value_dt[,mkt_value := P * corn_avg_ha]
value_dt[,.(sum(mkt_value))]

value_sf <- merge(grid10_tiles_sf3, value_dt[,.(id_10, mkt_value)], by = 'id_10', all.x = T) %>%
  dplyr::mutate(method = 'ex_ante')


sum(value_sf$mkt_value, na.rm = TRUE)

# (p3 <- tm_shape(value_sf) + 
#     tm_polygons("mkt_value", textNA="Not VR area", title="VR Value (USD/Cell)", n = 10) +
#     tm_layout(legend.text.size = 0.7,
#               main.title = paste('VR Market Value'),
#               main.title.position = "center",
#               main.title.size = 1.2))

breaks_n <- c(0, 5000,10000,15000,20000, 30000,40000,50000)

(p3 <- tm_shape(value_sf) + tm_polygons(c('mkt_value'), textNA="Not VR area", 
                                        #title = expression(paste('VR market', '($/cell)')), 
                                        title = "",
                                        breaks = breaks_n, 
                                        border.col = 'black',
                                        #style ="cont", 
                                        palette = "Greys", colorNA = 'white', midpoint = 20000)+
    tm_layout(panel.labels = expression(paste('c)')),
              # main.title = 'f',
              main.title.position = c(0,0),
              legend.text.size = 0.7,
              legend.position = c('left', 'bottom'),
              main.title.size = 1.2))
              #title.snap.to.legend = F,
              #legend.width = 1,
              

tmap_arrange(p1, p2, p3, nrow = 1)
tmap_save(tmap_arrange(p1, p2, p3, nrow = 1), "./vr_value/Data/figures/techonology_characterization_map.jpg", width = 10.5, height = 4)

st_write(value_sf, "./vr_value/Data/shapefiles/vr_cell_value_sf.shp", delete_dsn = TRUE)

#---------------------------------------------------------------------------
# 4) MAKE A MAP OF TECHNOLOGY MARKET CAP BY FIELD (for QG)
# AGGREGATE THE DATA TO CELL X Z LEVEL CONSIDERING THE AREA
names(perfomances_dt)
do_not_aggregate = c('id_10', 'id_field','region','model', 'tech')
do_aggregate =  c("Yld", "leach_n2", "N_fert","P")

perfomances_field_dt <- aggregate_by_area(data_dt = perfomances_dt, variables = do_aggregate, 
                                     weight = 'area_ha', by_c = do_not_aggregate) #cell x z level (mukey and field are out)

# MAKE A MAP OF VALUE TECHNOLOGY (EX ANTE VALUE)
value_dt <- perfomances_field_dt[model %in% c(4,5)]
#make one negative
value_dt[model == 4, Yld := -Yld]
value_dt[model == 4, leach_n2 := -leach_n2]
value_dt[model == 4, N_fert := -N_fert]
value_dt[model == 4, P := -P]

# Add values by group
value_dt <- value_dt[, .(Yld =  sum(Yld),
                         leach_n2 = sum(leach_n2),
                         N_fert = sum(N_fert),
                         VR_value = sum(P)), by = .(id_10, id_field)]

# value_dt <- value_dt[P > 3] #considering a cost of VR Cost of 3 usd
# value_dt[,mkt_value := P * corn_avg_ha]
# value_dt[,.(sum(mkt_value))]

value_sf <- merge(grid10_fields_sf, value_dt[,.(id_10, id_field, VR_value)], by = c('id_10', 'id_field'), all.x = T) %>%
  dplyr::mutate(method = 'ex_ante')

value_sf <- value_sf[!is.na(value_sf$VR_value),]

(p <- tm_shape(value_sf) + 
    tm_polygons("VR_value", textNA="Not VR area", title="VR Value (USD/Cell)", n = 10) +
    tm_layout(legend.text.size = 0.7,
              main.title = paste('VR Market Value'),
              main.title.position = "center",
              main.title.size = 1.2))


st_write(value_sf, "./vr_value/Data/shapefiles/vr_field_value_sf.shp", delete_dsn = TRUE)


#---------------------------------------------------------------------------
# MAKE A MAP OF THE BEST METHOD
#Select the two models of interest
best_method_dt <- perfomances_dt3[model %in% 1:10]
# best_method_dt[model == 5, P := P-3]
best_method_dt <- best_method_dt[,.SD[P==max(P)], by = id_10]

best_method_dt[,.N, by = .(region, model)][order(region, model)]

value_sf <- merge(grid10_tiles_sf3, best_method_dt[,.(id_10, model)], 
                   by = 'id_10', all = T)

value_sf <- dplyr::mutate(value_sf, model = ifelse(model <6, NA, model))


(p <- tm_shape(value_sf) + tm_polygons(c('model'), n =10)+
    tm_text('model')+
    tm_layout(legend.text.size = 0.7,
              main.title = paste('Best Method by cell'),
              main.title.position = "center",
              main.title.size = 1.2))

tmap_save(p, "./vr_value/Data/figures/best_method_map.jpg")

#==============================================================================================================
#==============================================================================================================
#=============================                  YIELD CURVE EXAMPLE              ==============================   
#==============================================================================================================
#==============================================================================================================


yc_yearly_dt3 <- readRDS("./vr_value/Data/files_rds/yc_yearly_dt3.rds")
reg_model_stuff <- readRDS( "./vr_value/Data/files_rds/reg_model_stuff.rds")
training_z <- reg_model_stuff$training_z
rm(reg_model_stuff)

# tile_n = 10
cell_n = 765#755#763#765
mukey_n = 243024


testing_set_dt <- perfomances_dt[id_10 == cell_n]

testing_set_dt[,mean(Yld), by = mukey]

testing_set_plot <- testing_set_dt[mukey == mukey_n]
testing_set_plot[,method := factor(model, levels= c('1', '2', '3','4', '5', '6', '7', '8', '9', '10', '11', '12'))]
ic_field_plot <- yc_yearly_dt3[mukey == mukey_n & id_10 == cell_n ] %>% .[!z %in% training_z ]


ic_field_plot <-  ic_field_plot[z != 'A19']
testing_set_plot <- testing_set_plot[z != 'A19']

testing_set_plot[,z := gsub(pattern = 'A', replacement = 'z', x = z)]
ic_field_plot[,z := gsub(pattern = 'A', replacement = 'z', x = z)]

# library(RColorBrewer)
# n <- 12
# qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
# col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# colors_sample =sample(col_vector, n)
# pie(rep(1,n), colors_sample)

# colors_sample=c( "#7570B3", "#FFED6F", "#666666", "#7FC97F", "#386CB0", "#B3B3B3", "#FFFFCC", "#A65628", "#F4CAE4", "#E41A1C", "#E6AB02", "black")

# Y plot with Yld at eonr
z_labels <- ic_field_plot[N_fert == max(ic_field_plot$N_fert), .(N_fert, Yld, z)][order(-Yld)]
z_labels[seq(1, nrow(z_labels), by = 2), N_fert := N_fert - 50]

ggplot() +
    geom_point(data = testing_set_plot[prev_crop == 1], aes(x = N_fert, y = Yld, colour = method, size = method)) +
    geom_line(data = ic_field_plot[prev_crop == 1], aes(x = N_fert, y = Yld, group=z), show.legend = FALSE) +
    scale_size_manual(values=c(rep(2, 11), 4)) +
    scale_color_manual(values=colors_sample)+
    ylab('Yield (kg/ha)')+
    xlab('N rate (kg/ha)')+
    geom_text(data = z_labels, aes(x = N_fert, y = Yld, label = z))+
    theme_bw()+
    theme(panel.grid = element_blank())

z_n = 'z21'
# (plot_n1 <- ggplot() +
#     geom_point(data = testing_set_plot[z == z_n & method == '12' & prev_crop == 1], 
#                aes(x = N_fert, y = Yld ,  size = method)) +
#     geom_line(data = ic_field_plot[z == z_n & prev_crop == 1], aes(x = N_fert, y = Yld, linetype = "Yield")) +
#     geom_point(data = testing_set_plot[z == z_n & method == '12' & prev_crop == 1], 
#                aes(x = N_fert, y = leach_n2*150,  size = method)) +
#     geom_line(data = ic_field_plot[z == z_n & prev_crop == 1], aes(x = N_fert, y = leach_n2*150, linetype = "N Leaching")) +
#     # scale_size_manual(values=c(rep(2, 11), 4)) +
#     ## scale_color_manual(values=colors_sample)+
#     labs(y = 'Yield (kg/ha)',
#          x = 'N rate (kg/ha)',
#          colour = "Parameter")+
#     scale_y_continuous(sec.axis = sec_axis(~./150, name = "N leaching (kg/ha)"))+
#     scale_linetype_manual(values = c("dashed", "solid"))+
#     scale_size_manual(values = 4,
#                       labels = expression(paste('EONR'^'ex post')))+
#     #geom_text(data = z_labels, aes(x = N_fert, y = Yld, label = z))+
#     theme_bw()+
#     guides(linetype = guide_legend(order=2),
#            size = guide_legend(order=1)) +
#     theme(legend.title =  element_blank(),
#           legend.position = c(0.85, 0.15),
#           panel.grid = element_blank())+
#     annotate("text", x=300, y=11500, label= "a)", size = 10) )

ic_field_plot2 <- melt(ic_field_plot[z == z_n & prev_crop == 1], id.vars = 'N_fert', measure.vars = c('Yld', 'leach_n2'))
ic_field_plot2[variable == 'leach_n2', value := value * 150]

testing_set_plot2 <- melt(testing_set_plot[z == z_n & method == '12' & prev_crop == 1], id.vars = 'N_fert', measure.vars = c('Yld', 'leach_n2'))
testing_set_plot2[variable == 'leach_n2', value := value * 150]


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
  annotate("text", x=300, y=11500, label= "a)", size = 10) )

ic_field_plot[,prev_crop:= as.character(prev_crop)]

(plot_n2 <- ggplot() +
    geom_point(data = testing_set_plot[method == '12'], aes(x = N_fert, y = Yld , shape = 'EONR')) +
    scale_shape_manual( values = 16,
      labels = c(bquote (paste('EONR'^'ex post'))))+
    geom_line(data = ic_field_plot, aes(x = N_fert, y = Yld, group = interaction(z, prev_crop))) +
    # scale_size_manual(values=c(rep(2, 11), 4)) +
    ## scale_color_manual(values=colors_sample)+
    labs(y = 'Yield (kg/ha)',
         x = 'N rate (kg/ha)')+
    #scale_y_continuous(sec.axis = sec_axis(~./150, name = "N Leaching (kg/ha)"))+
    #scale_linetype_manual(values = c("solid", "dashed"))+
    #geom_text(data = z_labels, aes(x = N_fert, y = Yld, label = z))+
    theme_bw()+
    theme(legend.title =  element_blank(),
          legend.position = c(0.85, 0.15),
          panel.grid = element_blank())+
  annotate("text", x=300, y=15000, label= "b)", size = 10)) 

summary(testing_set_plot[method == '12']$N_fert)

perfomances_champaign_dt <- perfomances_dt[id_10 %in% unique(dplyr::filter(grid10_tiles_sf3, county_name == 'Champaign')$id_10) & model == 12]
perfomances_champaign_dt[,prev_crop:= as.character(prev_crop)]

(plot_n3 <- ggplot() + 
    geom_density(data = perfomances_champaign_dt, aes( x= N_fert,  y = ..density.., fill = prev_crop, linetype = prev_crop), alpha = 0.4)+
    labs(x = expression(paste('EONR'^'ex post', '(kg/ha)')))+
    theme_bw()+  
    theme(panel.grid = element_blank(),
          legend.position = c(0.85, .7))+
    scale_fill_manual(name = "Previous crop", labels = c("Soybean", "Corn"), values = c('#696969', '#D3D3D3'))+
    scale_linetype_manual(values=c("twodash", "dotted"))+
    guides(linetype = FALSE)+
    annotate("text", x=300, y=0.009, label= "c)", size = 10) )

(plot_n4 <- ggplot() + 
    geom_density(data = perfomances_champaign_dt, aes( x= leach_n2,  y = ..density.., fill = prev_crop, linetype = prev_crop), alpha = 0.4)+
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

ggsave(grid.arrange(plot_n1, plot_n2, plot_n3, plot_n4, nrow = 2), 
       filename = "./vr_value/Data/figures/yield_curve_example.jpg", width = 8, height =  5.67)


grid.arrange(grid.arrange(plot_n1, plot_n2, nrow=1), plot_n3, nrow = 2)























#---------------------------------------------------------------------------
# MAKE A MAP OF THE BEST METHOD
#Select the two models of interest
best_method_dt <- perfomances_dt3[model %in% 1:10]
best_method_dt <- best_method_dt[,.SD[P==max(P)], by = id_10]
best_method_dt <- merge(best_method_dt, perfomances_dt3[model == 1, .(id_10, P_1 = P)], by = 'id_10')
best_method_dt[,P_improve := P-P_1]

best_method_dt[,.N, by = .(region, model)]

best_method_sf <- merge(grid10_tiles_sf3, best_method_dt, by = 'id_10', all.x = T)

(p <- tm_shape(best_method_sf) + tm_polygons(c('model','P_improve'), n =10)+
    tm_text('model')+
    tm_layout(legend.text.size = 0.7,
              main.title = paste('VALUE OF TECHNOLOGY AND INFORMATION'),
              main.title.position = "center",
              main.title.size = 1.2))

tmap_save(p, "./vr_value/Data/figures/value_t_i.jpg")

#---------------------------------------------------------------------------
# PLOT WEIRD CASE
yc_yearly_dt <- readRDS('./vr_value/Data/files_rds/yc_yearly_dt.rds')
length(unique(yc_yearly_dt$id_10))

yc_yearly_dt3 <- readRDS('./vr_value/Data/files_rds/yc_yearly_dt3.rds')
length(unique(yc_yearly_dt3$id_10))

length(unique(full_fields_dt2$id_10))


id_10_n <- vr_value_expost_dt[order(-leach_n22)][1,]$id_10
yc_yearly_dt <- readRDS('./vr_value/Data/files_rds/yc_yearly_dt.rds')
one_field_dt <- data.table(grid10_soils_sf6[grid10_soils_sf6$id_10 == id_10_n,])
mukey_n <- one_field_dt[area_ha == max(area_ha)][1,] %>% .[,.(id_10, mukey)]

ic_field_dt <- filter_dt_in_dt(yc_yearly_dt , filter_dt = mukey_n, return_table = TRUE)
ic_field_dt[,prev_crop := ifelse(prev_crop == 'MSM', 0, 1)]
ic_field_dt[, P := Yld * Pc - N_fert * Pn]

performance_set_dt <- filter_dt_in_dt(perfomances_dt , filter_dt = mukey_n, return_table = TRUE)
 
performance_set_dt[,model := as.character(model)]

performance_set_dt[prev_crop == 0 & model != 11, .N, by = .(model, z)]

# P plot with P at eonr
(plot_n <- ggplot() +
    geom_point(data = performance_set_dt[prev_crop == 0 & model != 11], aes(x = N_fert, y = P, colour = model)) +
    geom_point(data = performance_set_dt[prev_crop == 0 & model == 11], aes(x = N_fert, y = P), size = 3, show.legend = FALSE) +
    geom_line(data = ic_field_dt[prev_crop == 0], aes(x = N_fert, y = P, group=interaction(z, prev_crop)), show.legend = FALSE) +
    ggtitle(paste('P plot with P at eonr', mukey_n$mukey)))

ggsave(plot_n, filename = "./vr_value/Data/figures/yield_curve_example.jpg")

# leach_n2o3 plot with leaching at eonr
(plot_n <- ggplot() +
    geom_point(data = performance_set_dt[prev_crop == 0 & model != 11], aes(x = N_fert, y = leach_n22, colour = model)) +
    geom_point(data = performance_set_dt[prev_crop == 0 & model == 11], aes(x = N_fert, y = leach_n22), size = 3, show.legend = FALSE) +
    geom_line(data = ic_field_dt[prev_crop == 0], aes(x = N_fert, y = leach_n22, group=interaction(z, prev_crop)), show.legend = FALSE) +
    ggtitle(paste('leach_n2o3 plot with leaching at eonr', mukey_n$mukey)))

ggsave(plot_n, filename = "./vr_value/Data/figures/leaching_curve_example.jpg")

#---------------------------------------------------------------------------
# CALCULATE STATE TOTAL VARIABLES
perfomances_dt5 <- copy(perfomances_dt4)
do_aggregate =  c("Yld", "leach_n22", "leach_ext", "N_fert","P")
perfomances_dt5[,(do_aggregate) := (.SD * corn_avg_ha/1000), .SDcols=do_aggregate]

state_total_production_dt <- perfomances_dt4[, lapply(.SD, function(x) sum(x)), .SDcols= do_aggregate] 
2.2 * 10^9 * 25.4 /1000 #IL production in tons https://www.nass.usda.gov/Statistics_by_State/Illinois/Publications/Current_News_Release/2018/20180112-IL_Annual_Crop_Production.pdf
10.95 * 10^6 *0.4046#IL harvested area in ha
201 * 25.4/0.4046 #IL Yield

#---------------------------------------------------------------------------
# SELECT THE WORST YEAR FOR LEACHING AND SEE THE BENEFIT THERE


all_perfomances_dt2 <- aggregate_by_area(data_dt = all_perfomances_dt, variables = do_aggregate, 
                                         weight = 'area_ha', by_c = c('id_10', 'z', 'prev_crop'))

extreme_year_dt <- all_perfomances_dt2[, .SD[ nleach_1 == max( nleach_1)], by = .(id_10)]
extreme_year_dt[, lapply(.SD, function(x) mean(x)), .SDcols= do_aggregate] 



eonr_mukey_dt <- yc_yearly_dt[, .SD[ P == max( P)], by = .(id_10, mukey, z, prev_crop)]

1804.744/1814.46

all_perfomances_dt[id_10 == 5]
all_perfomances_dt[id_10 == 5, .(Yld_1 = sum(Yld_1), area_ha = sum(area_ha))]

   
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
saveRDS(economics_dt, './vr_value/Data/files_rds/economics_dt.rds')

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

tmap_save(p, "./vr_value/Data/figures/value_t_i.jpg")
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

tmap_save(p, "./vr_value/Data/figures/nvalue_t_i.jpg")
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

tmap_save(p, "./vr_value/Data/figures/profits.jpg")


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

ggsave(p, filename = "./vr_value/Data/figures/value_boxplot.jpg")
