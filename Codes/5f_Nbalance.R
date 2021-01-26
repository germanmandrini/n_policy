rm(list=ls())

# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# codes_folder <-'C:/Users/germa/Documents'#Dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# codes_folder <-'C:/Users/germanm2/Documents'#CPSC
setwd('~')#Server
codes_folder <-'~' #Server


source('./Codes_useful/R.libraries.R')
# library(scales)
source('./Codes_useful/gm_functions.R')
source(paste0(codes_folder, '/n_policy_git/Codes/parameters.R'))
"~/n_policy_git/Codes/parameters.R"


#--------------------------------------------------------------------------------
yc_field_dt2 <- readRDS("./n_policy_box/Data/files_rds/yc_field_dt2.rds")
yc_field_dt2[,N_balance := N_fert - Y_corn * 11/1000]

set.seed(123)
trials_sample_dt <- yc_field_dt2[,.N, by = .(id_10, id_field, region, z)][,.SD[sample(.N, 15)],by = region]

# id10_sample <- c(100,  865, 1416)
# id10_sample <- yc_field_dt2[,.N, by = .(id_10, region)][,.SD[sample(.N, 1)],by = region]$id_10

yc_field_sample_dt <- filter_dt_in_dt(x_dt = yc_field_dt2[id_10 %in% trials_sample_dt$id_10], filter_dt = trials_sample_dt, return_table = T)

balance_dt <- filter_dt_in_dt(x_dt = yc_field_sample_dt, filter_dt = trials_sample_dt[,.SD[1],region], return_table = T) %>%
    .[N_fert %in% c(0,100,200,320)]

(p1 <- ggplot(data = balance_dt, aes(x = N_balance, y = L)) + 
    geom_point()+
    geom_smooth()+
    facet_free(region~.)+
    ggtitle('One trial'))

balance_dt <- yc_field_sample_dt[N_fert %in% c(0,100,200,320)]

(p2 <- ggplot(data = balance_dt, aes(x = N_balance, y = L)) + 
    geom_point()+
    geom_smooth()+
    facet_free(region~.)+
    ggtitle('45 trials (0,100,200,320 kg/ha)'))

balance_dt <- yc_field_sample_dt[N_fert %in% c(180)]


(p3 <- ggplot(data = balance_dt, aes(x = N_balance, y = L)) + 
    geom_point()+
    geom_smooth()+
    facet_free(region~.)+
    ggtitle('45 fields (180 kg/ha)'))


field_perfomances_dt <- readRDS("./n_policy_box/Data/files_rds/field_perfomances_dt.rds")
field_perfomances_dt[,N_balance := N_fert - Y_corn * 11/1000]

balance_dt <- field_perfomances_dt[policy == 'ratio_5' & NRT == 'dynamic']
balance_dt <- filter_dt_in_dt(x_dt = balance_dt[id_10 %in% trials_sample_dt$id_10], filter_dt = trials_sample_dt, return_table = T)

(p4 <- ggplot(data = balance_dt, aes(x = N_balance, y = L)) + 
    geom_point()+
    geom_smooth()+
    facet_free(region~.)+
    ggtitle('45 fields (using dynamic rec)'))

balance_dt <- field_perfomances_dt[policy == 'ratio_5' & NRT == 'dynamic']

(p5 <- ggplot(data = balance_dt[sample(1:nrow(balance_dt), 5000)], aes(x = N_balance, y = L)) + 
        geom_point()+
        geom_smooth()+
        facet_free(region~.)+
        ggtitle('5000 fields (using dynamic rec)'))

library(ggpubr)
ggarrange(p1,p2,p3,p4, labels = c("a)","b)", "c)", "d)"), label.x = 0)


#--------------------------------------------------------------------------------
# N Balance

field_perfomances_dt <- readRDS("./n_policy_box/Data/files_rds/field_perfomances_dt.rds")
field_perfomances_dt[,N_balance := N_fert - Y_corn * 11/1000]

field_perfomances_dt[,L_ton_grain := L/(Y_corn / 1000)]
balance_dt <- field_perfomances_dt[policy == 'ratio_5' & NRT == 'dynamic']


(p1 <- ggplot(data = balance_dt)+ geom_density(aes(x = N_balance, colour = region_eq), size =1)+
    ggtitle('N Balance'))

# ggsave(plot = p1, 
#        filename = "./n_policy_box/Data/figures/balance1.png")

(p2 <- ggplot(data = balance_dt[L < 100])+ geom_density(aes(x = L, colour = region_eq), size =1)+
    ggtitle('Leaching'))

# ggsave(plot = p2, 
#        filename = "./n_policy_box/Data/figures/balance2.png")

(p3 <- ggplot(data = balance_dt[sample(1:nrow(balance_dt), 5000)], aes(x = N_balance, y = L)) + 
    geom_point()+
    geom_smooth()+
    facet_free(region_eq~.))

ggsave(plot = p3, 
       filename = "./n_policy_box/Data/figures/balance3.png")

(p4 <- ggplot(data = balance_dt[sample(1:nrow(balance_dt), 5000)], aes(x = N_balance, y = L_ton_grain)) + 
    geom_point()+
    geom_smooth()+
    facet_free(region_eq~.))

ggsave(plot = p4, 
       filename = "./n_policy_box/Data/figures/balance4.png")

