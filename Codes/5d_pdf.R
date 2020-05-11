# setwd('C:/Users/germa/Box Sync/My_Documents') #dell

# setwd("/home/germanm2")

rm(list=ls())

# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# codes_folder <-'C:/Users/germanm2/Documents'#CPSC

setwd('~')#Server
codes_folder <-'~' #Server


source('./Codes_useful/R.libraries.R')
# library(scales)
source('./Codes_useful/gm_functions.R')
source(paste0(codes_folder, '/n_policy_git/Codes/parameters.R'))

  # grid10_tiles_sf7 <- readRDS("./n_policy_box/Data/Grid/grid10_tiles_sf7.rds") 
  # grid10_soils_dt5 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt5.rds") %>% data.table()
  # grid10_fields_sf2 <- readRDS('./n_policy_box/Data/Grid/grid10_fields_sf2.rds')
  # reg_model_stuff <- readRDS("./n_policy_box/Data/files_rds/reg_model_stuff.rds")
  
perfomances_dt2 <- readRDS("./n_policy_box/Data/files_rds/perfomances_dt2.rds") #data by field x z

perfomances_dt2[,E := L * 0.4 * Pe_med]
perfomances_dt2[,W := P + G - E]

pdf_dt <- perfomances_dt2[((policy == 'ratio_6' & NMS == 1) | (policy == 'ratio_10' & NMS == 2)) & L < 400]

ggplot(data = pdf_dt) +
  geom_density(aes(x=L, color = policy, fill = policy), alpha = 0.4)

pdf_dt_long <- melt(pdf_dt, id.vars = c('id_10', 'id_field', 'z','policy', 'NMS'), 
                    measure.vars = c('Y_corn','Y_soy', 'L1', 'L2', 'N_fert', 'P', 'E','W')) # , 

#Smooth N_fert
pdf_dt_long[policy == 'ratio_10'& variable == 'N_fert',
            value:= lapply(pdf_dt_long[policy == 'ratio_10'& variable == 'N_fert']$value, function(x) x + sample(x = -5:5, 1)[[1]][1])]
pdf_dt_long[policy == 'ratio_10'& variable == 'N_fert']

pdf_dt_long <- pdf_dt_long[!(variable == 'L1' & value > 100)]
pdf_dt_long <- pdf_dt_long[!(variable == 'L2' & value > 100)]
pdf_dt_long <- pdf_dt_long[!(variable == 'Y_corn' & value < 5000)]
pdf_dt_long <- pdf_dt_long[!(variable == 'Y_soy' & value < 2000)]
pdf_dt_long <- pdf_dt_long[!(variable == 'P' & value < 1000)]
pdf_dt_long <- pdf_dt_long[!(variable == 'W' & value < 1000)]

# FROM https://stackoverflow.com/questions/37573155/showing-different-axis-labels-using-ggplot2-with-facet-wrap
ggplot(data = pdf_dt_long) +
  geom_density(aes(x=value, color = policy, fill = policy), alpha = 0.4)+
  #scale_linetype_manual(values = c("dashed", "solid"))+
  #geom_hline(data = hline_dt, aes(yintercept = y_line), linetype = 'dashed', color = 'grey', size = 1)+
  #geom_text(data = hline_dt, aes(x = 18, y = y_line, label =y_label ))+
  facet_wrap(. ~ variable, scales="free") +
  #scale_x_continuous(breaks = seq(1,20,1), labels = seq(1,20,1)) + 
  #xlab('N:Corn price ratio')+
  #geom_vline(xintercept = Pn/Pc, linetype = 'dashed', color = 'grey', size = 1)+
  theme_bw()+
  theme(panel.grid = element_blank(), 
        legend.position = "none")
