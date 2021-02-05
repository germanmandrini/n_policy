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
library(plyr)
library(ggpubr)
#--------------------------------------------------------------------------------
yc_field_dt2 <- readRDS("./n_policy_box/Data/files_rds/yc_field_dt2.rds")
yc_field_dt2[,N_balance := N_fert - Y_corn * 11/1000]

field_perfomances_dt <- readRDS("./n_policy_box/Data/files_rds/field_perfomances_dt.rds")
field_perfomances_dt[,N_balance := N_fert - Y_corn * 11/1000]

#--------------------------------------------------------------------------------

set.seed(123)
trials_sample_dt <- yc_field_dt2[,.N, by = .(id_10, id_field, region, z)][,.SD[sample(.N, 15)],by = region]

# id10_sample <- c(100,  865, 1416)
# id10_sample <- yc_field_dt2[,.N, by = .(id_10, region)][,.SD[sample(.N, 1)],by = region]$id_10

yc_field_sample_dt <- filter_dt_in_dt(x_dt = yc_field_dt2[id_10 %in% trials_sample_dt$id_10], filter_dt = trials_sample_dt, return_table = T)

balance_dt <- filter_dt_in_dt(x_dt = yc_field_sample_dt, filter_dt = trials_sample_dt[,.SD[1],region], return_table = T) %>%
    .[N_fert %in% c(0,100,200,320)]

get_r2 <- function(balance_dt){
    r2<- data.table(ddply(balance_dt,.(region),function(x) summary(lm(x$L ~ x$N_balance))$r.squared))
    names(r2)<-c("region","r2")
    r2[,r2 := round(r2,2)]
    r2 <- merge(r2, balance_dt[,.(x_label =min(N_balance), y_label = max(L)-10), by = region], by = 'region')
    r2[,x_label := min(x_label)+15]
    return(r2)
}

r2 <- get_r2(balance_dt)

(p1 <- ggplot(data = balance_dt, aes(x = N_balance, y = L)) + 
    geom_point()+
    geom_smooth(method="lm", se=F) +
    theme_bw()+ xlab("N Balance (kg/ha)") + ylab("N Leaching (kg/ha)")+
    geom_text(data=r2,aes(label = paste("R^2: ", r2,sep=""), x=x_label,y=y_label),parse=T, show.legend=F)+
    facet_free(region~.)+
    ggtitle('One trial (0,100,200,320 kg/ha)'))

balance_dt <- yc_field_sample_dt[N_fert %in% c(0,100,200,320)]
r2 <- get_r2(balance_dt)

(p2 <- ggplot(data = balance_dt, aes(x = N_balance, y = L)) + 
    geom_point()+
    geom_smooth(method="lm", se=F) +
    theme_bw()+ xlab("N Balance (kg/ha)") + ylab("N Leaching (kg/ha)") +
    geom_text(data=r2,aes(label = paste("R^2: ", r2,sep=""), x=x_label,y=y_label),parse=T, show.legend=F)+
    facet_free(region~.)+
    ggtitle('45 trials (0,100,200,320 kg/ha)'))

balance_dt <- yc_field_sample_dt[N_fert %in% c(180)]
r2 <- get_r2(balance_dt)
r2[,y_label := y_label + c(5,0,50)]

(p3 <- ggplot(data = balance_dt, aes(x = N_balance, y = L)) + 
    geom_point()+
    geom_smooth(method="lm", se=F)+
    theme_bw()+ xlab("N Balance (kg/ha)") + ylab("N Leaching (kg/ha)") +
    geom_text(data=r2,aes(label = paste("R^2: ", r2,sep=""), x=x_label-5,y=y_label),parse=T, show.legend=F)+
    facet_free(region~.)+
    ggtitle('45 fields (180 kg/ha)'))




balance_dt <- field_perfomances_dt[policy == 'ratio_5' & NRT == 'dynamic']
balance_dt <- filter_dt_in_dt(x_dt = balance_dt[id_10 %in% trials_sample_dt$id_10], filter_dt = trials_sample_dt, return_table = T)

r2 <- get_r2(balance_dt)
r2[,y_label := y_label + c(5,0,0)]

(p4 <- ggplot(data = balance_dt, aes(x = N_balance, y = L)) + 
    geom_point()+
    geom_smooth(method="lm", se=F)+
    theme_bw()+ xlab("N Balance (kg/ha)") + ylab("N Leaching (kg/ha)") +
    geom_text(data=r2,aes(label = paste("R^2: ", r2,sep=""), x=x_label-5,y=y_label),parse=T, show.legend=F)+
    facet_free(region~.)+
    ggtitle('45 fields (using dynamic rec)'))

balance_dt <- field_perfomances_dt[policy == 'ratio_5' & NRT == 'dynamic'] %>%
    .[sample(1:.N, 5000)]

r2 <- get_r2(balance_dt)

(p5 <- ggplot(data = balance_dt, aes(x = N_balance, y = L)) + 
        geom_point()+
        geom_smooth(method="lm", se=F)+
        theme_bw()+ xlab("N Balance (kg/ha)") + ylab("N Leaching (kg/ha)") +
        geom_text(data=r2,aes(label = paste("R^2: ", r2,sep=""), x=x_label-5,y=y_label),parse=T, show.legend=F)+
        facet_free(region~.)+
        ggtitle('5000 fields (using dynamic rec)'))


ggarrange(p1,p2,p4, p5, labels = c("a)","b)", "c)", "d)"), label.x = 0)


#--------------------------------------------------------------------------------
# N Balance

field_perfomances_dt <- readRDS("./n_policy_box/Data/files_rds/field_perfomances_dt.rds")
field_perfomances_dt[,N_balance := N_fert - Y_corn * 11.5/1000]

field_perfomances_dt[,L_ton_grain := L/(Y_corn / 1000)]
balance_dt <- field_perfomances_dt[policy == 'ratio_5' & NRT == 'dynamic']


(p1 <- ggplot(data = balance_dt)+ geom_density(aes(x = N_balance, colour = region_eq), size =1)+
        theme_bw()+
    ggtitle('N Balance'))

# ggsave(plot = p1, 
#        filename = "./n_policy_box/Data/figures/balance1.png")

(p2 <- ggplot(data = balance_dt[L < 100])+ geom_density(aes(x = L, colour = region_eq), size =1)+
        theme_bw()+
    ggtitle('Leaching'))

ggarrange(p1,p2, labels = c("a)","b)"), label.x = 0, common.legend = TRUE, legend = 'bottom')

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


#==========================================================================================================
# Why balance does not hurt high leaching areas?


field_perfomances_dt2[P_diff == min(P_diff)]

field_perfomances_dt2[id_10 == 256 & id_field == 4 & policy_name %in% c('bal', 'leach')]
field_perfomances_dt[id_10 == 256 & id_field == 4 & policy_name %in% c('bal', 'leach')]

ggplot(data = field_perfomances_dt[policy_name %in% c('leach')]) +
    geom_point(aes(x = L, y = G))




yc_field_dt2 <- readRDS( "./n_policy_box/Data/files_rds/yc_field_dt2.rds")
bal_threshold <- yc_field_dt2[station == 1 & N_fert == 100] %>%
    .[,N_balance := N_fert - Y_corn * 11.5/1000] %>%
    .[, .(N_balance_thr = quantile(N_balance, probs = 0.5)), region] %>%
    .[order(region)] # not change with z_n

field_perfomances_dt <- merge(field_perfomances_dt[,-'N_balance_thr'], bal_threshold, by = 'region')
field_perfomances_dt[,N_balance := N_fert - Y_corn * 11.5/1000]
field_perfomances_dt[,N_extra := N_balance - N_balance_thr]
field_perfomances_dt[N_extra <= 0, N_extra := 0]

plot_dt <- field_perfomances_dt[policy_name %in% c('bal')][sample(1:.N, 1000)]
ggplot(data = plot_dt, aes(x = N_balance, y = L)) +
    geom_point()+
    geom_smooth()+
    facet_free(.~region_eq, scale = 'free')

#----------------------------------------------------------
# Does it hurt high N balance areas?
balance_dt <- field_perfomances_dt[policy_name == 'bal']
baselevel_dt <- field_perfomances_dt[policy == 'ratio_5' & NRT == 'dynamic', 
                                     .(id_10, id_field, z, N_base = N_fert, L_base = L, P_base = P, Nbalance_base = N_balance)]
# Combine with baselevel
balance_dt <- merge(balance_dt, baselevel_dt, by = c('id_10', 'id_field', 'z'))

#Add the return: Farmers get returned the long term G + policy_cost from the region as a lump sum
perfomances_dt4[,return := G + policy_cost]
balance_dt <- merge(balance_dt, perfomances_dt4[,.(region_eq, policy, NRT, return)], by =c('region_eq', 'policy', 'NRT'))
balance_dt[,P_return := P + return]
balance_dt[, P_diff := P_return - P_base]


(p1 <- ggplot(data = balance_dt, aes(x = N_balance, y = P_return)) +
        geom_point()+
        geom_smooth()+
        theme_bw()+ xlab("N Balance (kg/ha)") + ylab("After policy income ($/ha)")+
        facet_free(region_eq~., scale = 'free')) #fields with high N balance are poor

(p2 <- ggplot(data = balance_dt, aes(x = N_balance, y = L)) +
        geom_point()+
        geom_smooth()+
        theme_bw()+ xlab("N Balance (kg/ha)") + ylab("Leaching ($/ha)")+
        facet_free(region_eq~., scale = 'free')) 

ggarrange(p1,p2 , labels = c("a)","b)"), label.x = 0)


balance_dt[, N_diff := N_fert - N_base]

(p1 <- ggplot(data = balance_dt, aes(x = N_balance, y = P_base)) +
        geom_point()+
        geom_smooth()+
        theme_bw()+ xlab("N Balance (kg/ha)") + ylab("After policy income ($/ha)")+
        facet_free(region_eq~., scale = 'free')) #fields with high N balance are poor



(p2 <- ggplot(data = balance_dt, aes(x = N_balance, y = P_diff)) +
        geom_point()+
        geom_smooth()+
        theme_bw()+ xlab("N Balance (kg/ha)") + ylab("After policy income change ($/ha)")+
        facet_free(region_eq~., scale = 'free')) 

(p3 <- ggplot(data = balance_dt, aes(x = N_balance, y = L)) +
        geom_point()+
        geom_smooth()+
        theme_bw()+ xlab("N Balance (kg/ha)") + ylab("After policy income change ($/ha)")+
        facet_free(region_eq~., scale = 'free')) 

(p4 <- ggplot(data = balance_dt, aes(x = N_base, y = N_fert)) +
        geom_point()+
        geom_smooth()+
        facet_free(.~region_eq, scale = 'free'))

ggarrange(p1,p2,p3,p4)
