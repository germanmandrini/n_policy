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
field_perfomances_dt2[, farm_income := P + return]
field_perfomances_dt2[policy == 'ratio_5', policy_name := 'base']
#=============================================================================================================================================
plot_dt_long <- melt(field_perfomances_dt2, id.vars = c('region_eq','id_10','policy', 'policy_name'), 
                    measure.vars = c('N_fert', 'L', 'N_balance','farm_income')) # , , 


plot_dt_long[,variable_labels := factor(variable, levels = c('N_fert', 'L', 'N_balance', 'farm_income'),
                                 labels = c(expression("N rate (kg " * ha^"-1"* ")"), 
                                            expression("N leaching (kg " * ha^"-1"*")"),
                                            expression("N Balance (kg " * ha^"-1" * ")"), 
                                            expression("Farm income ($ " * ha^"-1" * ")")))]


plot_dt_long[,policy_labels := factor(policy_name, levels = c('ratio', 'leach', 'bal', 'red', 'base'),
                                               labels = c("Price ratio", 
                                                          "Leaching fee",
                                                          "Balance fee",
                                                          "Vol. reduction",
                                                          "Baselevel"))]

# get the quantile of each row (weird function)
plot_dt_long <- plot_dt_long[order(region_eq, variable, value)]
plot_dt_long[, q := seq_len(.N)/.N, by = .(region_eq, variable)]
# plot_dt_long <- plot_dt_long[q > 0.1 & q < 0.9]

library(scales)
show_col(hue_pal()(6))

(p <- ggplot(data = plot_dt_long[!(variable == 'farm_income' & value < 1500)], 
       aes(x=value, color = policy_labels, linetype = policy_labels, fill = policy_labels, alpha = policy_labels)) +
  geom_density(size = 1)+
  scale_color_manual(values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF", '#619CFF'),
                     name="Policy")+
  scale_fill_manual(values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF", '#619CFF'),
                     name="Policy")+
  scale_alpha_manual(values=c(0, 0, 0, 0, 0.5),
                    name="Policy")+
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid","dashed"),
                        name="Policy")+
  facet_wrap(region_eq ~ variable_labels, 
             labeller = label_parsed,
             scales="free",
             nrow = 3,
             as.table = F)+
  theme_bw()+
  theme(panel.grid = element_blank(), 
        # strip.placement = "outside",
        # panel.spacing = unit(1.5, "lines"),
        text=element_text(size=13),
        # strip.background = element_blank(),
        axis.title.x=element_blank(),
        strip.background =element_rect(fill="white"),
        # legend.justification = c(0, 0),
        # legend.direction = "horizontal",
        legend.position = "bottom"
        ))


ggsave(plot = p, 
       filename = "./n_policy_box/Data/figures/pdf_variables.pdf", width = 780/300*3, height = 680/300*3,
       units = 'in')
ggsave(plot = p, 
       filename = "./n_policy_box/Data/figures/pdf_variables.png", width = 780/300*3, height = 680/300*3,
       units = 'in')
