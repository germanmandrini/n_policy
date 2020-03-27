# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
# setwd('~')

source('./Codes_useful/R.libraries.R')
library(scales)
source('./Codes_useful/gm_functions.R')
source('./vr_value/Codes/parameters.R')
# source('./Codes_useful/gm_functions.R')

grid10_soils_sf6 <- readRDS("./vr_value/Data/Grid/grid10_soils_sf6.rds")

grid10_tiles_sf2 <- readRDS("./vr_value/Data/Grid/grid10_tiles_sf2.rds") 

# perfomances_dt <- readRDS("./vr_value/Data/files_rds/perfomances_dt.rds")
perfomances_dt <- readRDS("./vr_value/Data/files_rds/perfomances_eonr_dt.rds")

#-------------------------------------------------------------------------
#Figure out missing runs
perfomances_dt[model == 6, .(N_fert = mean(N_fert)), by = .(region, prev_crop)]
table(perfomances_dt[model == 6 & prev_crop == 1 & region == 2]$N_fert)


perfomances_dt[,.N, by = model]


counts_dt <- perfomances_dt[,.N, by = .(id_10, id_field, mukey, z)]
table(counts_dt$N)
counts_dt[N == 12]

#---------------------------------------------------------------------------
# EDA
do_not_aggregate = c('id_10','id_field', 'region','model', 'tech', 'z', 'prev_crop')
do_aggregate =  c("Yld", "leach_n", "N_fert","P", "n_deep_v5")

eda_dt <- perfomances_dt[model %in% c(4,6, 12)]

eda_dt2 <- aggregate_by_area(data_dt = eda_dt, variables = do_aggregate, 
                                     weight = 'area_ha', by_c = do_not_aggregate)

# eda_dt3 <- merge(eda_dt2[model == 4, -'model'],
#                  eda_dt2[model == 6, -'model'],
#                 by = c('id_10', 'region', 'z', 'prev_crop', 'tech', 'area_ha', 'n_deep_v5'), suffixes=c("_4", "_6"))

eda_dt3 <- merge(eda_dt2[model %in% c(4,6)],
                 eda_dt2[model == 12, c('id_10', 'region', 'id_field','z', 'prev_crop', 'area_ha', 'n_deep_v5', 'N_fert', "P")],
                 by = c('id_10', 'region','id_field', 'z', 'prev_crop', 'area_ha', 'n_deep_v5'), suffixes=c("", "_12"))

eda_dt3[,N_diff := N_fert - N_fert_12]
eda_dt3[,P_diff := P - P_12]


eda3_sample_dt <- eda_dt3[sample(1:nrow(eda_dt3), 500)]

ggplot(eda3_sample_dt, aes(x = N_diff, y = P_diff, colour = factor(model))) +
  geom_point() +
  geom_smooth(se = FALSE)


ggplot(eda3_sample_dt, aes(x = P_diff, colour = factor(model)))+
  geom_density()

eda3_sample_dt[,.(N_diff = mean(N_diff),
           P_diff = mean(P_diff)), by = model]

#Probability of being over or down
beat_test <- merge(eda_dt3[model == 4] ,
      eda_dt3[model == 6, .(id_10, region, id_field,z, prev_crop,  N_fert, P)],
      by = c('id_10', 'region','id_field', 'z', 'prev_crop'), suffixes=c("_4", "_6"))

beat_test[,N_high_4 := ifelse(N_fert_4 > N_fert_12,1,0)]
beat_test[,N_high_6 := ifelse(N_fert_6 > N_fert_12,1,0)]

beat_test[,n_v5_bin :=  round(n_deep_v5/10,0)*10]

beat_test[,.(N_fert_6 = mean(N_fert_6)), by = .(region,prev_crop)]  

beat_test[,.(N_high_4 = mean(N_high_4),
             N_high_6 = mean(N_high_6)), by = n_v5_bin][order(n_v5_bin)]


beat_test[,P_beat := P_4 - P_6]

ggplot(beat_test[sample(1:nrow(beat_test), 500)], aes(x = N_fert_12, y = P_beat)) +
  geom_point() +
  geom_smooth(se = FALSE)



test[N_fert == N_fert_6]


,
                 eda_dt2[model == 12, c('id_10', 'region', 'id_field','z', 'prev_crop', 'area_ha', 'n_deep_v5', 'N_fert', "P")],
                 by = c('id_10', 'region','id_field', 'z', 'prev_crop', 'area_ha', 'n_deep_v5'), suffixes=c("", "_12"))

ggplot(data = eda_dt4) +
  geom_density(aes(x = P_diff, colour = factor(prev_crop)))

hist(eda_dt4[prev_crop == 1]$P_diff)
hist(eda_dt4[prev_crop == 0]$P_diff)

eda_dt4[,P_diff_eonr_4 := P_4 - P_12]
eda_dt4[,P_diff_eonr_6 := P_6 - P_12]
eda_dt4[,N_diff_eonr_4 := N_fert_4 - N_fert_12]
eda_dt4[,N_diff_eonr_6 := N_fert_6 - N_fert_12]

eda_dt4[,.(N_diff_eonr_4 = mean(N_diff_eonr_4),
           P_diff_eonr_4 = mean(P_diff_eonr_4),
           N_diff_eonr_6 = mean(N_diff_eonr_6),
           P_diff_eonr_6 = mean(P_diff_eonr_6)), by = prev_crop]

ggplot(eda_dt4[sample(1:nrow(eda_dt4), 500)], aes(x = N_diff_eonr_4, y = P_diff_eonr_4, colour = factor(prev_crop))) +
  geom_point() +
  geom_smooth(se = FALSE)

ggplot(eda_dt4[sample(1:nrow(eda_dt4), 500)], aes(x = N_diff_eonr_6, y = P_diff_eonr_6, colour = factor(prev_crop))) +
  geom_point() +
  geom_smooth(se = FALSE)


#---------------------------------------------------------------------------
# AGGREGATE THE DATA TROUGH THE DIFFERENT Z TO ID_10 CONSIDERING THE AREA
names(perfomances_dt)
do_not_aggregate = c('id_10', 'region','model', 'tech', 'z', 'prev_crop')
do_aggregate =  c("Yld", "leach_n", "N_fert","P")

perfomances_dt[,.N, by = .(id_10, model, tech, prev_crop, z)][,.N, by = .(id_10, model, tech)]

#First aggregate without z so then we can get the leach_extreme
perfomances_dt2 <- aggregate_by_area(data_dt = perfomances_dt, variables = do_aggregate, 
                                        weight = 'area_ha', by_c = do_not_aggregate)

perfomances_dt3 <- perfomances_dt2[, .(Yld =  mean(Yld),
                                       leach_n = mean(leach_n),
                                       leach_ext = max(leach_n), #leaching in the year with max leaching
                                       N_fert = mean(N_fert),
                                       P = mean(P), 
                                       area_ha = mean(area_ha)), by = .(id_10, region, model, tech, prev_crop)]
#---------------------------------------------------------------------------
#AGGREGATE JUST CONSIDERING THE SAMPLED AREA 
perfomances_dt4 <- perfomances_dt3[, .(Yld =  mean(Yld),
                                       leach_n = mean(leach_n),
                                       leach_ext = max(leach_n), #leaching in the year with max leaching
                                       N_fert = mean(N_fert),
                                       P = mean(P), 
                                       area_ha = sum(area_ha)), by = .( model, tech, prev_crop)][order(prev_crop)]

#---------------------------------------------------------------------------
# AGGREGATE AGAIN CONSIDERING THE CORN PRODUCTION OF THE CELL
grid10_tiles_dt <- data.table(grid10_tiles_sf2)[,.(id_10, corn5_cell )]
grid10_tiles_dt[,corn_ha_cell := corn5_cell* 30*30/10000/11]


perfomances_dt3 <- merge(perfomances_dt3, grid10_tiles_dt, by = 'id_10')

perfomances_dt4 <- aggregate_by_area(data_dt = perfomances_dt3, variables = c("Yld", "leach_n", "leach_ext", "N_fert","P"), 
                                         weight = 'corn_ha_cell', by_c = c('model', 'tech')) #%>% .[order(prev_crop)]

perfomances_dt4 <- aggregate_by_area(data_dt = perfomances_dt3, variables = c("Yld", "leach_n", "leach_ext", "N_fert","P"), 
                                     weight = 'corn_ha_cell', by_c = c('model', 'tech', 'prev_crop', 'region')) %>% .[order(prev_crop)]


#---------------------------------------------------------------------------
# BAR CHART PROFITS
perfomances_dt4[,model := factor(model, levels= c('1', '2', '3','4', '5', '6', '7', '8', '9', '10', '11', '12'))]

(p1 <- ggplot(perfomances_dt4, aes(x = model, y = P, fill = tech))+
  geom_bar(stat="identity") +
  scale_y_continuous(limits=c(1300,1500),oob = rescale_none) +
  facet_free(.~ prev_crop))
  
  ggtitle('Yield')+
  theme_bw()

#---------------------------------------------------------------------------
# RMSE
paired <- merge(perfomances_dt[, .(id_10, id_field, region, mukey,z, prev_crop, model, N_fert)],
                perfomances_dt[model == 12, .(id_10, id_field, region, mukey,z, prev_crop, N_fert)],
                by = c('id_10', 'id_field', 'region', 'mukey', 'z', 'prev_crop'), suffixes=c("", "_12"))

paired[,res1 := abs(N_fert - N_fert_12)]
rmse(actual, predicted)

RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}



paired[,.(RMSE = sqrt(mean(res1^2)),
          MAD = mean(res1)), by = .(model, prev_crop) ]
sd(paired$res1)
sd(ValidSet$res2)

perfomances_dt4[,model := factor(model, levels = c(1,2,3,4,5,6,7,8,9,10,11,12))]
(p1 <- ggplot(paired[,.(RMSE = sd(res1)), by = .(model, prev_crop, region) ], aes(x = model, y = RMSE))+
    geom_bar(stat="identity") +
    facet_free(region ~ prev_crop))

ggtitle('Yield')+
  theme_bw()

#---------------------------------------------------------------------------
# Where can be model 5 improved?
paired <- merge(perfomances_dt[model %in% c(5), .(id_10, id_field, region , mukey, z, prev_crop, Yld, n_deep_v5,  N_fert, P, model)], 
                perfomances_dt[model == 12, .(id_10, id_field, region, mukey,z, prev_crop, N_fert)],
                by = c('id_10', 'id_field', 'region', 'mukey', 'z', 'prev_crop'), suffixes=c("", "_12"))


paired[,prev_crop := factor(prev_crop)]
# p1 <- 
  ggplot(data = paired) + 
  geom_density(aes(x = N_fert, colour = prev_crop)) +
  geom_density(aes(x = N_fert_12, colour = prev_crop))
  
hist(paired$N_fert)

paired2 <- copy(paired)
paired2[,N_diff := N_fert - N_fert_12]
paired2[,N_fert_12 := factor(N_fert_12)]

p2 <- ggplot(paired2) + geom_boxplot(aes(x = N_fert_12, N_fert))+
  facet_free(prev_crop ~ .)

grid.arrange(p1, p2, nrow = 1)

ggplot() +
  geom_point(data = paired, aes(x = N_fert_12, y = jitter(N_fert), color = prev_crop))  +
  coord_fixed() + geom_abline() + 
  ylim(0, 330)+ xlim(0, 330) +
  geom_smooth()+
  theme(aspect.ratio=1, 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_bw()



n_diff_table <- paired[,.(N_diff = mean(N_diff)), by = .(prev_crop,
                                         N_fert_12)][order(prev_crop, -N_diff)]

ggplot(n_diff_table) + geom_point(aes(x = N_fert_12, y = N_diff, colour = prev_crop))




paired[,N_fert_12 := as.numeric(N_fert_12)]



ggplot(df, aes(x=weight, color=sex)) +
  geom_density()

             p <- ggplot(mpg, aes(class, hwy))
             p + geom_boxplot()
             
             
paired[,.(N_diff = mean(N_diff), P = mean(P),
          n_deep_v5 = mean(n_deep_v5)), by = model]


paired2 <- merge(soybean_dt[model == 5, .(id_10, id_field, region , mukey, z, prev_crop, Yld, n_deep_v5,  N_fert, P, model)], 
                 soybean_dt[model == 6, .(id_10, id_field, region, mukey,z, prev_crop, N_fert, P)],
                 by = c('id_10', 'id_field', 'region', 'mukey', 'z', 'prev_crop'), suffixes=c("", "_6"))

paired3 <- merge(paired2, 
                 soybean_dt[model == 12, .(id_10, id_field, region, mukey,z, prev_crop, N_fert)],
                 by = c('id_10', 'id_field', 'region', 'mukey', 'z', 'prev_crop'), suffixes=c("", "_12"))

paired3[,P_diff := P - P_6]
paired3[,N_v5_bin := as.numeric(round(n_deep_v5/10, 0))]

paired4 <- paired3[,.(P_diff = mean(P_diff),
                      N_fert_5 = mean(N_fert),
                      N_fert_6 = mean(N_fert_6),
                      eonr = mean(N_fert_12),
                      N = .N), by = N_v5_bin][order(N_v5_bin)]

ggplot(paired4) +
  geom_line(aes(x = N_v5_bin, y = eonr), color = 'blue') +
  geom_line(aes(x = N_v5_bin, y = N_fert_5), color = 'darkgreen') +
  geom_line(aes(x = N_v5_bin, y = N_fert_6), color = 'darkred') 

yc_yearly_dt2 <- readRDS("./vr_value/Data/files_rds/yc_yearly_dt2.rds")
yc_yearly_dt2[,prev_crop := ifelse(prev_crop == 'MSM', 0, 1)]
yc_yearly_dt2[,N_v5_bin := as.numeric(round(n_deep_v5/10, 0))]
yc_yearly_dt2[,N_fert_bin := as.numeric(round(N_fert/10, 0))]

mean_response <- yc_yearly_dt2[prev_crop == 0,.( P = mean( P),
                                                N = .N), by = .(N_fert_bin, prev_crop)][order(prev_crop, N_fert_bin)]
mean_response[,prev_crop := as.factor(prev_crop)]

predictions_pdf <- perfomances_dt[prev_crop == 0 & model %in% c(5,6)]
predictions_pdf[,N_fert_bin := as.numeric(round(N_fert/10, 0))]
predictions_pdf <- predictions_pdf[,.N, by = .(N_fert_bin, model)]
predictions_pdf[,N := N/10]

mean_response <- merge(mean_response, 
                       predictions_pdf[model == 5,.(N_fert_bin, count_5 = N)])

mean_response <- merge(mean_response, 
                       predictions_pdf[model == 6,.(N_fert_bin, count_6 = N)])

ggplot(mean_response) +
  geom_line(aes(x = N_fert_bin, y = P)) +
  geom_line(aes(x = N_fert_bin, y = count_5), color = 'darkred') +
  geom_line(aes(x = N_fert_bin, y = count_6), color = 'blue') +
  geom_vline(xintercept = 13) 



#---------------------------------------------------------------------------
# Why is model 6 better than 5 in prev_crop 0?
soybean_dt <- perfomances_dt[model %in% c(5,6,12) & prev_crop == 0]

paired <- merge(soybean_dt[model %in% c(5,6), .(id_10, id_field, region , mukey, z, prev_crop, Yld, n_deep_v5,  N_fert, P, model)], 
                soybean_dt[model == 12, .(id_10, id_field, region, mukey,z, prev_crop, N_fert)],
                  by = c('id_10', 'id_field', 'region', 'mukey', 'z', 'prev_crop'), suffixes=c("", "_12"))

paired[,N_diff := N_fert - N_fert_12]
paired[,.(N_diff = mean(N_diff), P = mean(P),
          n_deep_v5 = mean(n_deep_v5)), by = model]


paired2 <- merge(soybean_dt[model == 5, .(id_10, id_field, region , mukey, z, prev_crop, Yld, n_deep_v5,  N_fert, P, model)], 
                soybean_dt[model == 6, .(id_10, id_field, region, mukey,z, prev_crop, N_fert, P)],
                by = c('id_10', 'id_field', 'region', 'mukey', 'z', 'prev_crop'), suffixes=c("", "_6"))

paired3 <- merge(paired2, 
                 soybean_dt[model == 12, .(id_10, id_field, region, mukey,z, prev_crop, N_fert)],
                 by = c('id_10', 'id_field', 'region', 'mukey', 'z', 'prev_crop'), suffixes=c("", "_12"))

paired3[,P_diff := P - P_6]
paired3[,N_v5_bin := as.numeric(round(n_deep_v5/10, 0))]

paired4 <- paired3[,.(P_diff = mean(P_diff),
           N_fert_5 = mean(N_fert),
           N_fert_6 = mean(N_fert_6),
           eonr = mean(N_fert_12),
           N = .N), by = N_v5_bin][order(N_v5_bin)]

ggplot(paired4) +
  geom_line(aes(x = N_v5_bin, y = eonr), color = 'blue') +
  geom_line(aes(x = N_v5_bin, y = N_fert_5), color = 'darkgreen') +
  geom_line(aes(x = N_v5_bin, y = N_fert_6), color = 'darkred') 

yc_yearly_dt2 <- readRDS("./vr_value/Data/files_rds/yc_yearly_dt2.rds")
yc_yearly_dt2[,prev_crop := ifelse(prev_crop == 'MSM', 0, 1)]
yc_yearly_dt2[,N_v5_bin := as.numeric(round(n_deep_v5/10, 0))]
yc_yearly_dt2[,N_fert_bin := as.numeric(round(N_fert/10, 0))]

mean_response <- yc_yearly_dt2[prev_crop == 0,.( P = mean( P),
                  N = .N), by = .(N_fert_bin, prev_crop)][order(prev_crop, N_fert_bin)]
mean_response[,prev_crop := as.factor(prev_crop)]

predictions_pdf <- perfomances_dt[prev_crop == 0 & model %in% c(5,6)]
predictions_pdf[,N_fert_bin := as.numeric(round(N_fert/10, 0))]
predictions_pdf <- predictions_pdf[,.N, by = .(N_fert_bin, model)]
predictions_pdf[,N := N/10]

mean_response <- merge(mean_response, 
      predictions_pdf[model == 5,.(N_fert_bin, count_5 = N)])

mean_response <- merge(mean_response, 
                       predictions_pdf[model == 6,.(N_fert_bin, count_6 = N)])

ggplot(mean_response) +
  geom_line(aes(x = N_fert_bin, y = P)) +
  geom_line(aes(x = N_fert_bin, y = count_5), color = 'darkred') +
  geom_line(aes(x = N_fert_bin, y = count_6), color = 'blue') +
  geom_vline(xintercept = 13) 

#---------------------------------------------------------------------------
# BAR CHART AT STATE LEVEL

perfomances_long_dt4 <- melt(perfomances_dt4, id.vars = c("model", "tech"), measure.vars = c( 'Yld',  'leach_n', 'leach_ext',   'N_fert', 'P'))


perfomances_dt4[,model := factor(model, levels = c(1,2,3,4,5,6,7,8,9,10,11,12))]
class(perfomances_dt4$model)

  
p1 <- ggplot(perfomances_dt4, aes(x = model, y = Yld, fill = tech))+
  geom_bar(stat="identity") +
  scale_y_continuous(limits=c(11000,11500),oob = rescale_none) +
  ggtitle('Yield')+
  theme_bw()

p2 <- ggplot(perfomances_dt4, aes(x = model, y = P, fill = tech))+
  geom_bar(stat="identity") +
  scale_y_continuous(limits=c(1300,1480),oob = rescale_none) +
  ggtitle('Profits')+
  theme_bw()

p3 <- ggplot(perfomances_dt4, aes(x = model, y = leach_n, fill = tech))+
  geom_bar(stat="identity") +
  scale_y_continuous(limits=c(20,25),oob = rescale_none) +
  ggtitle('N leaching')+
  theme_bw()

p4 <- ggplot(perfomances_dt4, aes(x = model, y = leach_ext, fill = tech))+
  geom_bar(stat="identity") +
  scale_y_continuous(limits=c(40,46),oob = rescale_none) +
  ggtitle('N leaching extreme')+
  theme_bw()

p5 <- ggplot(perfomances_dt4, aes(x = model, y = N_fert, fill = tech))+
  geom_bar(stat="identity") +
  scale_y_continuous(limits=c(100,190),oob = rescale_none) +
  ggtitle('N')+
  theme_bw()

ggsave(grid.arrange(p1, p2, p5), filename = "./vr_value/Data/figures/state_total_barchart1.jpg")

ggsave(grid.arrange(p3, p4), filename = "./vr_value/Data/figures/state_total_barchart2.jpg")


(fig4 <- ggplot(yearly.weather.extra2.long,aes(year.real,value,fill=variable))+
   geom_bar(stat="identity",position="stack") + 
   scale_fill_manual(name='Weather variable', 
                     labels=c(Annualpp=expression(paste('pp'^'A')),
                              Seasonpp=expression(paste('pp'^'S')),
                              Julpp=expression(paste('pp'^'J'))),
                     values = c("Annualpp" = "#CCCCCC", "Seasonpp" = "#999999", "Julpp" = "#666666")) + 
   geom_hline(yintercept = julpp.mean, color = "#000000") + 
   annotate("text",x=2010,y=julpp.mean+20,size=3,label=c('Historic'), color = "#000000") +
   annotate("text",x=2010,y=julpp.mean-15,size=3,label=c('mean'), color = "#000000") +
   geom_hline(yintercept = seassonpp.mean, color = "#000000") + 
   annotate("text",x=2010,y=seassonpp.mean+20,size=3,label=c('Historic'), color = "#000000") +
   annotate("text",x=2010,y=seassonpp.mean-15,size=3,label=c('mean'), color = "#000000") +
   geom_hline(yintercept = annualpp.mean, color = "#000000") +
   annotate("text",x=2010,y=annualpp.mean+20,size=3,label=c('Historic'), color = "#000000") +
   annotate("text",x=2010,y=annualpp.mean-15,size=3,label=c('mean'), color = "#000000") +
   scale_x_continuous(breaks = 1979:2010,'year') +
   # scale_color_discrete(name='Wather Variable') +
   # scale_fill_manual(values = c("MRTN" = "#CCCCCC", "Stg. 1" = "#CC9900", "Stg. 2" = "#996600", "Stg. 3" = "#663300", "Stg. 4" = "black"))+
   xlab('Year') +
   ylab('Precipitation (mm)') +
   theme_bw() +
   theme(legend.position='bottom',
         panel.grid = element_blank(),
         strip.background = element_blank(),
         legend.text.align = 0,
         legend.title = element_blank(),
         strip.text = element_blank()))




#---------------------------------------------------------------------------
# PDF OF LEACHING


#---------------------------------------------------------------------------
# MAKE A MAP OF VALUE OF I + T (POTENTIAL VALUE)
#Select the two models of interest
vr_value_expost_dt <- perfomances_dt3[model %in% c(11,12)]
#make one negative
vr_value_expost_dt[model == 11, Yld := -Yld]
vr_value_expost_dt[model == 11, leach_n := -leach_n]
vr_value_expost_dt[model == 11, leach_ext := -leach_ext]
vr_value_expost_dt[model == 11, N_fert := -N_fert]
vr_value_expost_dt[model == 11, P := -P]

# Add values by group
vr_value_expost_dt <- vr_value_expost_dt[, .(Yld =  sum(Yld),
                                        leach_n = sum(leach_n),
                                        leach_ext = sum(leach_ext), 
                                        N_fert = sum(N_fert),
                                        P = sum(P)), by = .(id_10)]

vr_value_expost_sf <- merge(grid10_tiles_sf2, vr_value_expost_dt, by = 'id_10', all.x = T)

(p <- tm_shape(vr_value_expost_sf) + tm_polygons(c('P','leach_n'), n =10)+
    tm_layout(legend.text.size = 0.7,
              main.title = paste('VALUE OF TECHNOLOGY AND INFORMATION'),
              main.title.position = "center",
              main.title.size = 1.2))

tmap_save(p, "./vr_value/Data/figures/value_t_i.jpg")

#---------------------------------------------------------------------------
# MAKE A MAP OF THE BETH METHOD
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


id_10_n <- vr_value_expost_dt[order(-leach_n2)][1,]$id_10

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

# leach_no3 plot with leaching at eonr
(plot_n <- ggplot() +
    geom_point(data = performance_set_dt[prev_crop == 0 & model != 11], aes(x = N_fert, y = leach_n2, colour = model)) +
    geom_point(data = performance_set_dt[prev_crop == 0 & model == 11], aes(x = N_fert, y = leach_n2), size = 3, show.legend = FALSE) +
    geom_line(data = ic_field_dt[prev_crop == 0], aes(x = N_fert, y = leach_n2, group=interaction(z, prev_crop)), show.legend = FALSE) +
    ggtitle(paste('leach_no3 plot with leaching at eonr', mukey_n$mukey)))

ggsave(plot_n, filename = "./vr_value/Data/figures/leaching_curve_example.jpg")

#---------------------------------------------------------------------------
# CALCULATE STATE TOTAL VARIABLES
perfomances_dt5 <- copy(perfomances_dt4)
do_aggregate =  c("Yld", "leach_n2", "leach_ext", "N_fert","P")
perfomances_dt5[,(do_aggregate) := (.SD * corn_ha_cell/1000), .SDcols=do_aggregate]

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
