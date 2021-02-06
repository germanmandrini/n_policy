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
require(plyr)

perfomances_dt4 <- readRDS("./n_policy_box/Data/files_rds/perfomances_dt4.rds")
percent20_dt <- readRDS("./n_policy_box/Data/files_rds/percent20_dt.rds")
field_perfomances_dt <- readRDS("./n_policy_box/Data/files_rds/field_perfomances_dt.rds")

#-----------------
# Prepare data
percent20_dt <- percent20_dt[policy_name %in% c('ratio', 'leach', 'bal', 'red'),.(policy, NRT)]
percent20_dt <- rbind(data.table(policy = c('ratio_5'), NRT = c('dynamic')), 
                      percent20_dt)

perfomances_dt4 <- filter_dt_in_dt(perfomances_dt4, filter_dt = percent20_dt, return_table = T)
field_perfomances_dt <- filter_dt_in_dt(field_perfomances_dt, filter_dt = percent20_dt, return_table = T)
field_perfomances_dt[,N_balance := N_fert - Y_corn * 11.5/1000]
field_perfomances_dt[, c("policy_name", "policy_val") := tstrsplit(policy, "_", fixed=TRUE)]

# AGGREGATE THE DATA TO FIELD LEVEL (z is out)
names(field_perfomances_dt)
do_not_aggregate = c('policy', 'policy_name', 'policy_val','id_10', 'id_field','region_eq','NRT')

field_perfomances_dt2 <- field_perfomances_dt[,.(Y_corn = mean(Y_corn), 
                                     L = mean(L),
                                     N_fert = mean(N_fert),
                                     P = mean(P),
                                     G = mean(G),
                                     N_balance = mean(N_balance)), by = do_not_aggregate]

#=============================================================================================================================================
# Farmers get returned the long term G + policy_cost from the region as a lump sum
perfomances_dt4[,return := G + policy_cost]
field_perfomances_dt2 <- merge(field_perfomances_dt2, perfomances_dt4[,.(region_eq, policy, NRT, return)], by =c('region_eq', 'policy', 'NRT'))
field_perfomances_dt2[,P_return := P + return]

# ---------
# Make leaching relative to baselevel
baselevel_dt <- field_perfomances_dt2[policy == 'ratio_5' & NRT == 'dynamic', .(id_10, id_field, L_base = L, P_base = P, Nbalance_base = N_balance)]

field_perfomances_dt2 <- merge(field_perfomances_dt2[policy != 'ratio_5'], baselevel_dt, by = c('id_10', 'id_field'))

field_perfomances_dt2[,policy_labels := factor(policy_name, levels = c('ratio', 'leach', 'bal', 'red'),
                                 labels = c("N:Corn price ratio",
                                            "Leaching fee",
                                            "N balance fee",
                                            "N reduction"))]
#-----
lm_eqn = function(dt){
  m = lm(P_return~P_base, dt)
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 3),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

reg_dt <- ddply(field_perfomances_dt2,.(policy_labels),lm_eqn)
lm(data = field_perfomances_dt2[policy_name == 'bal'], formula = P_return~P_base)

(p1 <- ggplot(data=field_perfomances_dt2,aes(x = P_base, y = P_return, color = policy_labels)) +
  geom_point()+ #theme(aspect.ratio=1) + #coord_fixed() + 
  geom_smooth(color = 'blue', formula = y~x, method = 'lm')+
  geom_abline(linetype = 'dashed') + ylim(700, 2000)+ xlim(700, 2000) +
  theme_bw()+
  theme(#axis.text=element_text(size=12),
        #axis.title=element_text(size=14),
        legend.position = "none")+
  geom_text(data = reg_dt, aes(x = 700, y = 1800, label = V1, hjust = 0), parse = TRUE, inherit.aes=FALSE)+
  xlab('Baselevel profits ($/ha)')+
  ylab('After policy income ($/ha)')+
  # geom_text(aes(x= 1000,y=2000,label='(a)'),size=8,family="serif")+
  facet_free(.~policy_labels, scale = 'free'))


#-----
lm_eqn = function(dt){
  m = lm(L~L_base, dt)
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

reg_dt <- ddply(field_perfomances_dt2,.(policy_labels),lm_eqn)


# geom_text(data=r2,aes(label = paste("R^2: ", r2,sep=""), x=x_label,y=y_label),parse=T, show.legend=F)+
  
 
(p2 <- ggplot(data=field_perfomances_dt2, aes(x = L_base, y = L, color = policy_labels)) +
  geom_point()+ #theme(aspect.ratio=1) + #coord_fixed() + 
  geom_smooth(color = 'blue', formula = y~x, method = 'lm')+
  geom_abline(linetype = 'dashed') +  ylim(0, 136)+ xlim(0, 136) +
  theme_bw()+
  theme(#axis.text=element_text(size=12),
        #axis.title=element_text(size=14),
        legend.position = "none")+
    # geom_text(data=eq,aes(x = 25, y = 300,label=V1), parse = TRUE, inherit.aes=FALSE) + facet_grid(group~.)
  geom_text(data = reg_dt, aes(x = 0, y = 120, label = V1, hjust = 0), parse = TRUE, inherit.aes=FALSE)+
  xlab('Baselevel N leaching (kg/ha)')+
  ylab('After policy N leaching (kg/ha)')+
  # geom_text(aes(x= 1000,y=2000,label='(a)'),size=8,family="serif")+
  facet_free(.~policy_labels, scale = 'free'))



#-----
field_perfomances_dt2[,P_diff := P_return - P_base]

lm_eqn = function(dt){
  m = lm(P_diff~L_base, dt)
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

reg_dt <- ddply(field_perfomances_dt2,.(policy_labels),lm_eqn)


(p3 <- ggplot(data=field_perfomances_dt2, aes(x = L_base, y = P_diff, color = policy_labels)) +
  geom_point()+ #theme(aspect.ratio=1) + #coord_fixed() + 
  geom_smooth(color = 'blue', formula = y~x, method = 'lm')+  
    # geom_smooth(color = 'black')+
  # geom_histogram(aes(x = P_base))+
  # geom_abline() +  ylim(0, 100)+ xlim(0, 100) +
  theme_bw()+
  theme(#axis.text=element_text(size=12),
        #axis.title=element_text(size=14),
        legend.position = "none")+
  geom_text(data = reg_dt, aes(x = 5, y = -500, label = V1, hjust = 0), parse = TRUE, inherit.aes=FALSE)+
  xlab('Baselevel N leaching (kg/ha)')+
  ylab('Profits difference ($/ha)')+
  # geom_text(aes(x= 1000,y=2000,label='(a)'),size=8,family="serif")+
  facet_free(.~policy_labels, scale = 'free'))

library(ggpubr)
ggarrange(p1,p2,p3 , ncol = 1, labels = c("a)","b)", "c)"), label.x = 0)



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
#==========================================================================================================
# Why N balance transfer funds from low income fields to high income fields?

balance_dt <- field_perfomances_dt2[policy_name == 'bal']

#-----
lm_eqn = function(dt){
  m = lm(P_return~P_base, dt)
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

reg_dt <- ddply(field_perfomances_dt2,.(region_eq),lm_eqn)

(p1 <- ggplot(data=balance_dt,aes(x = P_base, y = P_return, color = policy_labels)) +
    geom_point()+ #theme(aspect.ratio=1) + #coord_fixed() + 
    geom_smooth(color = 'blue', formula = y~x, method = 'lm')+
    geom_abline(linetype = 'dashed') + ylim(700, 2000)+ xlim(700, 2000) +
    theme_bw()+
    theme(#axis.text=element_text(size=12),
      #axis.title=element_text(size=14),
      legend.position = "none")+
    geom_text(data = reg_dt, aes(x = 700, y = 1900, label = V1, hjust = 0), parse = TRUE, inherit.aes=FALSE)+
    xlab('Baselevel profits ($/ha)')+
    ylab('After policy income ($/ha)')+
    # geom_text(aes(x= 1000,y=2000,label='(a)'),size=8,family="serif")+
    facet_free(.~region_eq, scale = 'free'))

#-----
lm_eqn = function(dt){
  m = lm(N_balance~P_base, dt)
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

reg_dt <- ddply(balance_dt,.(region_eq),lm_eqn)

(p2 <- ggplot(data=balance_dt,aes(x = P_base, y = N_balance, color = policy_labels)) +
    geom_point()+ #theme(aspect.ratio=1) + #coord_fixed() + 
    geom_smooth(color = 'blue', formula = y~x, method = 'lm')+
    # geom_abline(linetype = 'dashed') + #ylim(700, 2000)+ xlim(700, 2000) +
    theme_bw()+
    theme(#axis.text=element_text(size=12),
      #axis.title=element_text(size=14),
      legend.position = "none")+
    geom_text(data = reg_dt, aes(x = 900, y = 90, label = V1, hjust = 0), parse = TRUE, inherit.aes=FALSE)+
    xlab('Baselevel profits ($/ha)')+
    ylab('N Balance (kg/ha)')+
    # geom_text(aes(x= 1000,y=2000,label='(a)'),size=8,family="serif")+
    facet_free(.~region_eq, scale = 'free'))

#-----
lm_eqn = function(dt){
  m = lm(L_base~P_base, dt)
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}
reg_dt <- ddply(balance_dt,.(region_eq),lm_eqn)

(p3 <- ggplot(data=balance_dt,aes(x = P_base, y = L_base, color = policy_labels)) +
    geom_point()+ #theme(aspect.ratio=1) + #coord_fixed() + 
    geom_smooth(color = 'blue', formula = y~x, method = 'lm')+
    # geom_abline(linetype = 'dashed') + #ylim(700, 2000)+ xlim(700, 2000) +
    theme_bw()+
    theme(#axis.text=element_text(size=12),
      #axis.title=element_text(size=14),
      legend.position = "none")+
    geom_text(data = reg_dt, aes(x = 700, y = 125, label = V1, hjust = 0), parse = TRUE, inherit.aes=FALSE)+
    xlab('Baselevel profits ($/ha)')+
    ylab('Baselevel leaching ($/ha)')+
    # geom_text(aes(x= 1000,y=2000,label='(a)'),size=8,family="serif")+
    facet_free(.~region_eq, scale = 'free'))
ggarrange(p1,p2,p3 , ncol = 1, labels = c("a)","b)", "c)"), label.x = 0)

#==========================================================================================================
# Barplot with error bars (BY REGION)
# http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
field_perfomances_dt2[,L_diff := L - L_base]
field_perfomances_dt2[,P_diff := P_return - P_base]


field_perfomances_dt2[,L_base_bin := cut(L_base, breaks = quantile(round(L_base,0), probs=seq(0,1, by=0.25)), ordered = TRUE, dig.lab=4, 
                                         labels = NULL, include.lowest=TRUE), by = region_eq]
field_perfomances_dt2[is.na(L_base_bin)]

field_perfomances_dt2[,P_base_bin := cut(P_base, breaks = quantile(round(P_base,0), probs=seq(0,1, by=0.25)), ordered = TRUE, dig.lab=4,
                                         labels = NULL, include.lowest=TRUE), by = region_eq]

#------------------
# Profits

plot_bar_dt <- field_perfomances_dt2[!is.na(P_base_bin), .(P_diff_mean = mean(P_diff),
                                                           P_diff_sd = sd(P_diff)), by = .(region_eq, policy_name, P_base_bin)]

(p1 <- ggplot(plot_bar_dt, aes(x=P_base_bin , y=P_diff_mean , fill=policy_name )) + 
    geom_errorbar(aes(ymin=P_diff_mean-P_diff_sd, ymax=P_diff_mean+P_diff_sd), width=.2,
                  position=position_dodge(.9))+ #first to avoid errors in both directions
    geom_bar(stat="identity", position=position_dodge()) +
    xlab('Baselevel profits ($/ha)')+
    ylab('Profits difference ($/ha)')+
    facet_free(~region_eq, scale = 'free'))

#------------------
# Leaching
plot_bar_dt <- field_perfomances_dt2[!is.na(L_base_bin), .(L_diff_mean = mean(L_diff),
                                         L_diff_sd = sd(L_diff)), by = .(region_eq, policy_name, L_base_bin)]


(p2 <- ggplot(plot_bar_dt, aes(x=L_base_bin , y=L_diff_mean , fill=policy_name )) + 
  geom_errorbar(aes(ymin=L_diff_mean-L_diff_sd, ymax=L_diff_mean+L_diff_sd), width=.2,
                position=position_dodge(.9))+ #first to avoid errors in both directions
  geom_bar(stat="identity", position=position_dodge()) +
    xlab('Baselevel N leaching (kg/ha)')+
    ylab('N leaching difference (kg/ha)')+
  facet_free(~region_eq, scale = 'free'))

#------------------
# Externality
plot_bar_dt <- field_perfomances_dt2[!is.na(L_base_bin), .(P_diff_mean = mean(P_diff),
                                                           P_diff_sd = sd(P_diff)), by = .(region_eq, policy_name, L_base_bin)]

(p3 <- ggplot(plot_bar_dt, aes(x=L_base_bin , y=P_diff_mean , fill=policy_name )) + 
   geom_errorbar(aes(ymin=P_diff_mean-P_diff_sd, ymax=P_diff_mean+P_diff_sd), width=.2,
                 position=position_dodge(.9))+ #first to avoid errors in both directions
   geom_bar(stat="identity", position=position_dodge()) +
   xlab('Baselevel N leaching (kg/ha)')+
    ylab('Profits difference ($/ha)')+
   facet_free(~region_eq, scale = 'free'))


ggarrange(p1,p2,p3 , ncol = 1, labels = c("a)","b)", "c)"), label.x = 0)

#=============================================================================================================================================
# Barplot with error bars (STATE)
# http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
field_perfomances_dt2[,L_diff := L - L_base]
field_perfomances_dt2[,P_diff := P_return - P_base]


field_perfomances_dt2[,L_base_bin := cut(L_base, breaks = quantile(round(L_base,0), probs=seq(0,1, by=0.25)), ordered = TRUE, dig.lab=4, 
                                         labels = NULL, include.lowest=TRUE)]
field_perfomances_dt2[is.na(L_base_bin)]

field_perfomances_dt2[,P_base_bin := cut(P_base, breaks = quantile(round(P_base,0), probs=seq(0,1, by=0.25)), ordered = TRUE, dig.lab=4,
                                         labels = NULL, include.lowest=TRUE)]

#------------------
# Profits

plot_bar_dt <- field_perfomances_dt2[!is.na(P_base_bin), .(P_diff_mean = mean(P_diff),
                                                           P_diff_sd = sd(P_diff)), by = .(policy_name, P_base_bin)]

(p1 <- ggplot(plot_bar_dt, aes(x=P_base_bin , y=P_diff_mean  )) + 
    geom_errorbar(aes(ymin=P_diff_mean-P_diff_sd, ymax=P_diff_mean+P_diff_sd), width=.2,
                  position=position_dodge(.9))+ #first to avoid errors in both directions
    geom_bar(stat="identity", position=position_dodge()) +
    xlab('Baselevel profits ($/ha)')+
    ylab('Profits difference ($/ha)')+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    facet_free(~policy_name, scale = 'free'))

#------------------
# Leaching
plot_bar_dt <- field_perfomances_dt2[!is.na(L_base_bin), .(L_diff_mean = mean(L_diff),
                                                           L_diff_sd = sd(L_diff)), by = .(policy_name, L_base_bin)]


(p2 <- ggplot(plot_bar_dt, aes(x=L_base_bin , y=L_diff_mean  )) + 
    geom_errorbar(aes(ymin=L_diff_mean-L_diff_sd, ymax=L_diff_mean+L_diff_sd), width=.2,
                  position=position_dodge(.9))+ #first to avoid errors in both directions
    geom_bar(stat="identity", position=position_dodge()) +
    xlab('Baselevel N leaching (kg/ha)')+
    ylab('N leaching difference (kg/ha)')+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    facet_free(~policy_name, scale = 'free'))

#------------------
# Externality
plot_bar_dt <- field_perfomances_dt2[!is.na(L_base_bin), .(P_diff_mean = mean(P_diff),
                                                           P_diff_sd = sd(P_diff)), by = .( policy_name, L_base_bin)]

(p3 <- ggplot(plot_bar_dt, aes(x=L_base_bin , y=P_diff_mean)) + 
    geom_errorbar(aes(ymin=P_diff_mean-P_diff_sd, ymax=P_diff_mean+P_diff_sd), width=.2,
                  position=position_dodge(.9))+ #first to avoid errors in both directions
    geom_bar(stat="identity", position=position_dodge()) +
    xlab('Baselevel N leaching (kg/ha)')+
    ylab('Profits difference ($/ha)')+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    facet_free(~policy_name, scale = 'free'))


ggarrange(p1,p2,p3 , ncol = 1, labels = c("a)","b)", "c)"), label.x = 0)


#=============================================================================================================================================
# Are Farmers profits less variable?

