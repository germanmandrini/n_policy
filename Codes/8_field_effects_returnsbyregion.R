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

perfomances_dt4 <- readRDS("./n_policy_box/Data/files_rds/perfomances_dt4.rds")
percent20_dt <- readRDS("./n_policy_box/Data/files_rds/percent20_dt.rds")
field_perfomances_dt <- readRDS("./n_policy_box/Data/files_rds/field_perfomances_dt.rds")

#-----------------
# Prepare data
percent20_dt <- percent20_dt[policy_name %in% c('ratio', 'leach', 'bal', 'red'),.(policy, NRT)]
percent20_dt <- rbind(data.table(policy = c('ratio_5'), NRT = c('static')), 
                      percent20_dt)

perfomances_dt4 <- filter_dt_in_dt(perfomances_dt4, filter_dt = percent20_dt, return_table = T)
field_perfomances_dt <- filter_dt_in_dt(field_perfomances_dt, filter_dt = percent20_dt, return_table = T)

# AGGREGATE THE DATA TO FIELD LEVEL (z is out)
names(field_perfomances_dt)
do_not_aggregate = c('policy', 'policy_name', 'policy_val','id_10', 'id_field','region_eq','NRT')

field_perfomances_dt2 <- field_perfomances_dt[,.(Y_corn = mean(Y_corn), 
                                     L = mean(L),
                                     N_fert = mean(N_fert),
                                     P = mean(P),
                                     G = mean(G)), by = do_not_aggregate]


#=============================================================================================================================================
# Farmers get returned the long term G + policy_cost from the region as a lump sum
perfomances_dt4[,return := G - policy_cost]
field_perfomances_dt2 <- merge(field_perfomances_dt2, perfomances_dt4[,.(region_eq, policy, NRT, return)], by =c('region_eq', 'policy', 'NRT'))
field_perfomances_dt2[,P_return := P + return]

# ---------
# Make leaching relative to baselevel
baselevel_dt <- field_perfomances_dt2[policy == 'ratio_5' & NRT == 'static', .(id_10, id_field, L_base = L, P_base = P)]

field_perfomances_dt2 <- merge(field_perfomances_dt2[NRT != 'static'], baselevel_dt, by = c('id_10', 'id_field'))

ggplot(data=field_perfomances_dt2, aes(x = P_base, y = P_return, color = policy_name)) +
  geom_point()+ theme(aspect.ratio=1) + coord_fixed() + geom_abline() + ylim(700, 2000)+ xlim(700, 2000) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  # ggtitle('Sequential vs Continuous') +
  theme_bw()

ggplot(data=field_perfomances_dt2, aes(x = L_base, y = L, color = policy_name)) +
  geom_point()+ theme(aspect.ratio=1) + coord_fixed() + geom_abline() + ylim(0, 100)+ xlim(0, 100) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  # ggtitle('Sequential vs Continuous') +
  theme_bw()

#----------------------------------------------------------
# Barplot with error bars
# http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
field_perfomances_dt2[,L_diff := L - L_base]
field_perfomances_dt2[,P_diff := P_return - P_base]


field_perfomances_dt2[,L_base_bin := cut(L_base, breaks = quantile(round(L_base,0), probs=seq(0,1, by=0.25)), ordered = TRUE, dig.lab=4, 
                                         labels = NULL, include.lowest=TRUE), by = region_eq]
field_perfomances_dt2[is.na(L_base_bin)]

field_perfomances_dt2[,P_base_bin := cut(P_base, breaks = quantile(round(P_base,0), probs=seq(0,1, by=0.25)), ordered = TRUE, dig.lab=4,
                                         labels = NULL, include.lowest=TRUE), by = region_eq]


#------------------
# Leaching
plot_bar_dt <- field_perfomances_dt2[!is.na(L_base_bin), .(L_diff_mean = mean(L_diff),
                                         L_diff_sd = sd(L_diff)), by = .(region_eq, policy_name, L_base_bin)]


ggplot(plot_bar_dt, aes(x=L_base_bin , y=L_diff_mean , fill=policy_name )) + 
  geom_errorbar(aes(ymin=L_diff_mean-L_diff_sd, ymax=L_diff_mean+L_diff_sd), width=.2,
                position=position_dodge(.9))+ #first to avoid errors in both directions
  geom_bar(stat="identity", position=position_dodge()) +
  facet_free(~region_eq, scale = 'free')

#------------------
# Profits

plot_bar_dt <- field_perfomances_dt2[!is.na(P_base_bin), .(P_diff_mean = mean(P_diff),
                                                           P_diff_sd = sd(P_diff)), by = .(region_eq, policy_name, P_base_bin)]

ggplot(plot_bar_dt, aes(x=P_base_bin , y=P_diff_mean , fill=policy_name )) + 
  geom_errorbar(aes(ymin=P_diff_mean-P_diff_sd, ymax=P_diff_mean+P_diff_sd), width=.2,
                position=position_dodge(.9))+ #first to avoid errors in both directions
  geom_bar(stat="identity", position=position_dodge()) +
  facet_free(~region_eq, scale = 'free')


# Standard deviation of the mean as error bar
p <- ggplot(df3, aes(x=dose, y=len, fill=supp)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=len-sd, ymax=len+sd), width=.2,
                position=position_dodge(.9))

p + scale_fill_brewer(palette="Paired") + theme_minimal()



ggplot(field_perfomances_dt2, aes(fill=policy_name, y=L_diff, x=L_base)) + 
  geom_bar(stat="identity")



#=============================================================================================================================================
# Are Farmers profits less variable?


