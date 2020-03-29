# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
# setwd('~')
rm(list=ls())

source('./Codes_useful/R.libraries.R')
# library(scales)
source('./Codes_useful/gm_functions.R')
source('./n_policy/Codes/parameters.R')

price_ratio_historic <- fread('./n_policy/Data/price_ratio_historic.xlsx')
# install.packages('xlsx')
library('xlsx')

price_ratio_historic_dt <- read.xlsx('./n_policy/Data/price_ratio_historic.xlsx', sheetName = "price_ratio_historic") %>% data.table()
setnames(price_ratio_historic_dt, c('Corn.USD.tn', 'NH3.USD.N.tn'), c('Pc', 'Pn'))

price_ratio_long_dt <- melt(price_ratio_historic_dt, id.vars = c("year"), measure.vars = c("Pc", "Pn", "Ratio")) 
price_ratio_long_dt[variable == 'Ratio' , value := value * 100]

plot_1 <- ggplot() +
  geom_line(data = price_ratio_long_dt, aes(x = year, y =  value, 
                                            linetype = variable), size = 0.8) +
  scale_y_continuous(sec.axis = sec_axis(~./100, name = "N:Corn price ratio"))+
  labs(y = 'Price ($/Mg)',
       x = 'Year',
       colour = "Variable")+
  scale_linetype_manual(values = c('dotted', "dashed", "solid"))+
  theme_bw()+
  theme(legend.title =  element_blank(),
        legend.position = c(0.15, 0.8),
        # legend.text=element_text(size=8),
        panel.grid = element_blank())

ggsave(plot = plot_1, filename = "./n_policy/Data/figures/price_ratio_historic.jpg", width = 5, height = 3.5,
       units = 'in')
