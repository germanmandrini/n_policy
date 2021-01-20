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

#--------------------------------------------------
# STATE LEVEL PLOT 
perfomances_dt4 <- readRDS("./n_policy_box/Data/files_rds/perfomances_dt4.rds")

perfomances_dt4[policy %in% c('ratio_5', 'leach_0', 'bal_0', 'red_1', 'lag_0') & NRT == 'static']
perfomances_dt4[policy %in% c('ratio_5', 'leach_0', 'bal_0', 'red_1') & NRT == 'dynamic']


plot_dt <- perfomances_dt4[policy_name %in% c('ratio', 'leach', 'bal', 'red') & NRT %in% c('static', 'dynamic')] 

# plot_dt[policy_name%in% c('red'), policy_val  := (1-policy_val )*100]

# plot_dt[policy_name%in% c('nred') & NRT == 'dynamic' & policy_val > 15, policy_val  := -round(L_change) ]
# plot_dt[policy_name%in% c('nred') & NRT == 'dynamic' & policy_val > 15]

# baselevel_L <- plot_dt[policy == 'ratio_5' & NRT == 'static', L]
# baselevel_Y_corn <- plot_dt[policy == 'ratio_5' & NRT == 'static', Y_corn ]


plot_dt_long <- melt(plot_dt, id.vars = c('region','policy_name','policy_val', 'NRT'), measure.vars = c('Y_corn', 'L_change', 'N_fert', 
                                                                                               'P', 'G', 'net_balance'))

plot_dt_long[,y_labels := factor(variable, levels = c('N_fert', 'L_change', 'Y_corn', 'P', 'G', 'net_balance'),
                                 labels = c(expression("N Fertilizer \n (N kg " * ha^"-1" *yr^"-1"* ")"), 
                                            expression("N Leaching\n ("*'%'*" change)"),
                                            expression("Corn Yield \n (kg N " * ha^"-1" *yr^"-1"* ")"), 
                                            expression("Farm profits \n ($ " * ha^"-1" * yr^"-1"* ")"),
                                            expression("Gov. collections \n ($ " * ha^"-1" * yr^"-1"* ")"),
                                            expression("Net income \n ($ " * ha^"-1" * yr^"-1"* ")")))]



plot_dt_long[,x_labels := factor(policy_name, levels = c('ratio', 'leach', 'bal', 'red'),
                                 labels = c(expression("N:Corn price"*" ratio"),
                                            expression("Leaching fee ($ " * kg^"-1" * ha^"-1"*")"),
                                            expression("N balance fee($ " * kg^"-1" * ha^"-1"*")"),
                                            expression("N reduction (%"*")")))]
plot_dt_long[,policy_val := as.numeric(policy_val)]

# plot_dt_long[variable == 'N_fert', plot_name := 'a) N Rate kg/ha']
# plot_dt_long[variable == 'L', plot_name := 'b) L (% change)']
# plot_dt_long[variable == 'Y_corn', plot_name := 'c) Yield kg/ha']
# plot_dt_long[variable == 'P', plot_name := 'd) Profits $/ha']
# plot_dt_long[variable == 'G', plot_name := 'e) G $/ha']
# plot_dt_long[variable == 'E', plot_name := 'f) E $/ha']
# plot_dt_long[variable == 'W', plot_name := 'g) W $/ha']
# plot_dt_long[order(variable)]

#use https://ggplot2.tidyverse.org/reference/labellers.html
hline_dt <- data.table(unique(plot_dt_long[,.(policy_name, variable, y_labels, x_labels)]))
hline_dt[variable == 'Y_corn', y_line := baselevel_Y_corn*0.95]
hline_dt[policy_name == 'ratio' & variable == 'Y_corn', y_label := '95% base-level']

plot_dt_long[,.N, .(NRT, policy_name, variable)]

ggplot(data = plot_dt_long) +
  geom_line(aes(x = policy_val, y =  value, color = NRT), size = 1)+
  facet_wrap(variable~policy_name, scales="free", ncol = 4)


(p <- ggplot(data = plot_dt_long) +
    # geom_line(data = plot_dt_long1, aes(x = policy_val, y =  value, colour = NRT)) +
    # scale_colour_manual(values = c("black", "brown"))+
    geom_line(aes(x = policy_val, y =  value, color = NRT), size = 1) +
    # scale_linetype_manual(values = c("dashed", "solid"))+
    # geom_hline(data = hline_dt, aes(yintercept = y_line), linetype = 'dashed', color = 'grey', size = 1)+
    # geom_text(data = hline_dt, aes(x = 5, y = y_line+50, label =y_label ), hjust = 'left', vjust = 'center') +
    # scale_color_manual(values=c("royalblue2", "tomato3"))+   
    # geom_text(data = ann_text[variable %in% unique(plot_dt_long$variable)], aes(y = value, x = x, label = lab), 
    #           hjust = 0, size = 8) +
    #   coord_cartesian(xlim = c(min(plot_dt_long$policy_val), max(plot_dt_long$policy_val)), # This focuses the x-axis on the range of interest
    #                   clip = 'off') +   # This keeps the labels from disappearing
    facet_free(y_labels~x_labels,
               labeller = label_parsed,
               scales="free",
               switch = 'x') +
    theme_bw()+
    theme(# panel.grid = element_blank(), 
      strip.background.x = element_blank(),
      strip.placement.x = "outside",
      # panel.spacing = unit(1.5, "lines"),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.position = "bottom",
      plot.margin =  unit(c(1,1,1,1), "lines")
    ))

ggsave(plot = p, 
       filename = "./n_policy_box/Data/figures/policies_multiplot.pdf", width = 831/300*3, height = 963/300*3,
       units = 'in')