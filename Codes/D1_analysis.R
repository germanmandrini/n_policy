rm(list=ls())

# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# codes_folder <-'C:/Users/germa/Documents'#Dell
setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
codes_folder <-'C:/Users/germanm2/Documents'#CPSC
# setwd('~')#Server
# codes_folder <-'~' #Server


source('./Codes_useful/R.libraries.R')
# library(scales)
source('./Codes_useful/gm_functions.R')
source(paste0(codes_folder, '/n_policy_git/Codes/parameters.R'))
"~/n_policy_git/Codes/parameters.R"


#--------------------------------------------------------------------------------
# Prepare data
field_perfomances_dt <- readRDS("./n_policy_box/Data/files_rds/perfomances_dt2.rds") %>% .[policy == 'ratio_5'] #field x z level (mukey is out)

testing_set_dt <- readRDS("./n_policy_box/Data/files_rds/testing_set_dt.rds")
grid10_tiles_sf7 <- readRDS("./n_policy_box/Data/Grid/grid10_tiles_sf7.rds") 

#--------------------------------------------------------------------------------
#Add the N_fert_12

# AGGREGATE THE DATA TO FIELD X Z LEVEL CONSIDERING THE AREA
do_not_aggregate = c('region' , 'id_10', 'z', 'id_field', "N_fert")
do_aggregate =  c( 'Y_corn' ,  'L', "P")

testing_set_dt[, P := Y_corn * Pc - N_fert * Pn]  #update profits
testing_set_dt[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z, N_fert)]$area_ha %>% summary()

field_yc_dt <- aggregate_by_area(data_dt = testing_set_dt, variables = do_aggregate, 
                                      weight = 'area_ha', by_c = do_not_aggregate) #field x z level (mukey is out)

field_ex_post_dt <- field_yc_dt[, .SD[ P == max( P)], by = .(id_10, z, id_field)] %>% 
                  .[, .SD[ N_fert == min( N_fert)], by = .(id_10, z, id_field)] %>%
                  .[,.(id_10, z, id_field, EONR = N_fert, P_12 = P, L_12 = L)]

#=======================================================================================
# Prelim figure
field_yc_dt2 <- merge(field_yc_dt, field_ex_post_dt, by =  c('id_10', 'z', 'id_field'))
field_yc_dt2[,N_dlt := N_fert - EONR]
field_yc_dt2[,P_rel := P/P_12] #relative to EONR
field_yc_dt2[,L_rel := L/L_12] #relative to EONR
field_yc_dt2[,P_dlt := P - P_12]#difference with EONR in $/ha
field_yc_dt2[,L_dlt := L - L_12]#difference with EONR in kg/ha

#Fix some relative values
field_yc_dt2[is.infinite(P_rel)]
field_yc_dt2[is.infinite(P_rel), P_rel := 0]
field_yc_dt2[is.na(P_rel)]
field_yc_dt2[is.na(P_rel), P_rel := 1]
field_yc_dt2[is.infinite(L_rel)]
field_yc_dt2[is.infinite(L_rel), L_rel := 1000]
field_yc_dt2[is.na(L_rel)]
field_yc_dt2[is.na(L_rel), L_rel := 1]


plot_dt <- field_yc_dt2[N_dlt >= - 100 & N_dlt <= 100,
                        .(P_rel = mean(P_rel),
                          L_rel = mean(L_rel),
                          P_dlt = mean(P_dlt),
                          L_dlt = mean(L_dlt)),
                       by = .(N_dlt)]

(p1 <- grid.arrange(ggplot(data = plot_dt) +
               geom_line(aes(x = N_dlt, y = P_dlt)), 
             ggplot(data = plot_dt)+
               geom_line(aes(x = N_dlt, y = L_dlt))))

ggsave(plot = p1,  filename = "./n_policy_box/Data/figures/D_leachingvsprofits_v1.png", width = 564/300*3, height = 691/300*3, units = 'in')


(p2 <- ggplot(data = plot_dt) +
  geom_line(aes(x = N_dlt, y = P_dlt), color = 'blue', size = 1)+
  geom_line(aes(x = N_dlt, y = L_dlt*6-240), color = 'red', size = 1)+
  scale_y_continuous(
    name = "Profits change (kg/ha)",
    sec.axis = sec_axis(~(.+240)/6, name="Leaching change (kg/ha)")
    ) + theme_bw() + 
  theme(
    axis.title.y = element_text(color = 'blue', size=13),
    axis.title.y.right = element_text(color = 'red', size=13)
    ) +
  xlab('N delta (kg/ha) (N obs - N pred)'))

ggsave(plot = p2, 
       filename = "./n_policy_box/Data/figures/D_leachingvsprofits_v2.png")

#---------------------------------------------------------------------------------
#What do they recommend?

field_perfomances_dt2 <- merge(field_perfomances_dt, field_ex_post_dt, by =  c('id_10', 'z', 'id_field'))
field_perfomances_dt2[,N_dlt := N_fert - EONR]

#Smooth N_fert
field_perfomances_dt2[, N_dlt := sapply(N_dlt, function(x) x + sample(x = -5:5, 1)[[1]][1])]

field_perfomances_dt2$N_dlt

field_perfomances_dt2[region == 1,region_lab := '1-South']
field_perfomances_dt2[region == 2,region_lab := '2-Central']
field_perfomances_dt2[region == 3,region_lab := '3-North']

(p1 <- ggplot(data = field_perfomances_dt2)+ geom_density(aes(x = N_dlt, colour = NMS), size =1)+
    facet_free(region_lab~.)+
    xlab('N delta (kg/ha) (N obs - N pred)')+
    ggtitle('Prediction error (N pred - N obs'))

ggsave(plot = p1, 
       filename = "./n_policy_box/Data/figures/D_recommendationbynrm_v1.png")

(p2 <- ggplot(data = field_perfomances_dt2)+ geom_density(aes(x = N_dlt, colour = NMS, fill = NMS), size =1, alpha = 0.1)+
    #facet_free(region_lab~.)+
    xlab('N delta (kg/ha) (N obs - N pred)'))

ggsave(plot = p2, 
       filename = "./n_policy_box/Data/figures/D_recommendationbynrm_v2.png", 
       width = 564/300*3, height = 320/300*3, units = 'in')



#--------------------------------------------------------------------------------






plot_dt <- field_yc_dt2[N_dlt > - 100 & N_dlt < 100]


perfomances_dt2[,N_balance := N_fert - Y_corn * 11/1000]

perfomances_dt2[,L_ton_grain := L/(Y_corn / 1000)]
balance_dt <- perfomances_dt2

balance_dt[region == 1,region_lab := '1-South']
balance_dt[region == 2,region_lab := '2-Central']
balance_dt[region == 3,region_lab := '3-North']

(p1 <- ggplot(data = balance_dt)+ geom_density(aes(x = P, colour = NMS), size =1)+
    facet_free(region_lab~.)+
    ggtitle('N Balance'))

ggsave(plot = p1, 
       filename = "./n_policy_box/Data/figures/balance1.png")

(p2 <- ggplot(data = balance_dt)+ geom_density(aes(x = L, colour = NMS), size =1)+
    facet_free(region_lab~.)+
    ggtitle('Leaching'))

ggsave(plot = p2, 
       filename = "./n_policy_box/Data/figures/balance2.png")

(p1 <- ggplot(data = balance_dt)+ geom_density(aes(x = N_balance, colour = NMS), size =1)+
    facet_free(region_lab~.)+
    ggtitle('N Balance'))

ggsave(plot = p1, 
       filename = "./n_policy_box/Data/figures/balance1.png")



(p3 <- ggplot(data = balance_dt[sample(1:nrow(balance_dt), 5000)], aes(x = N_balance, y = L)) + 
    geom_point()+
    geom_smooth()+
    facet_free(region_lab~.))

ggsave(plot = p3, 
       filename = "./n_policy_box/Data/figures/balance3.png")

(p4 <- ggplot(data = balance_dt[sample(1:nrow(balance_dt), 5000)], aes(x = N_balance, y = L_ton_grain)) + 
    geom_point()+
    geom_smooth()+
    facet_free(region_lab~.))

ggsave(plot = p4, 
       filename = "./n_policy_box/Data/figures/balance4.png")

#-------------------------------------------------------------------------
# YC graph
# Load Simulated data
yc_yearly_dt3 <- readRDS("./n_policy_box/Data/files_rds/yc_yearly_dt3.rds")


#Add regions and areas
grid10_soils_dt5 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt5.rds")
areas_dt <- data.table(grid10_soils_dt5) %>% .[,.(area_ha = sum(area_ha)), by = .(id_10, mukey)]

areas_dt <- grid10_soils_dt5[,.(area_ha = sum(area_ha)), by = .(id_10, mukey)]

state_agg_dt <- merge(yc_yearly_dt3[,-c('area_ha')], areas_dt, by = c('id_10', 'mukey'))

state_agg_dt2  <- aggregate_by_area(data_dt = state_agg_dt, variables = c('Y_corn', 'Y_soy', 'L1','L2'), 
                                    weight = 'area_ha', by_c = c('N_fert', 'region'))# %>% .[,-'area_ha']

state_agg_dt2[,N_balance := N_fert - Y_corn * 11/1000]
state_agg_dt2[,L := L1 + L2]
state_agg_dt2[,L_ton_grain := L/(Y_corn / 1000)]

state_agg_dt2[region == 1,region_lab := '1-South']
state_agg_dt2[region == 2,region_lab := '2-Central']
state_agg_dt2[region == 3,region_lab := '3-North']

ggplot() + 
  geom_line(data = state_agg_dt2, aes(x = N_fert, y = L_ton_grain))+
  geom_point(data = state_agg_dt2[,.SD[L_ton_grain == min(L_ton_grain)], by = region], aes(x = N_fert, y = L_ton_grain))+
  facet_free(region_lab~.)

ggplot(data = state_agg_dt2) + 
  geom_point( aes(x = N_balance, y = L))+
  facet_free(region_lab~.)

ggplot(data = state_agg_dt2) + 
  geom_point( aes(x = N_balance, y = L_ton_grain))+
  geom_point( aes(x = N_balance, y =  Y_corn/1000))+
  facet_free(region_lab~.)