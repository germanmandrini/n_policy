# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
# setwd('~')
rm(list=ls())

source('./Codes_useful/R.libraries.R')
# library(scales)
source('./Codes_useful/gm_functions.R')
source('./n_policy_git/Codes/parameters.R')
source('C:/Users/germanm2/Documents/n_policy_git/Codes/parameters.R')



# install.packages('xlsx')
library('xlsx')

price_ratio_historic_dt <- read.xlsx('./n_policy_box/Data/price_ratio_historic.xlsx', sheetName = "price_ratio_historic") %>% data.table()
setnames(price_ratio_historic_dt, c('Corn.USD.tn', 'NH3.USD.N.tn'), c('Pc', 'Pn'))

#-------------
# Try csv
price_ratio_historic_dt <- fread('./n_policy_box/Data/price_ratio_historic.csv')
names(price_ratio_historic_dt)[2:3] <- c('Pc', 'Pn')

#--------

price_ratio_long_dt <- melt(price_ratio_historic_dt, id.vars = c("year"), measure.vars = c("Pc", "Pn", "Ratio")) 
price_ratio_long_dt[variable == 'Ratio' , value := value * 100]

(plot_1 <- ggplot() +
  geom_line(data = price_ratio_long_dt, 
            aes(x = year, y =  value,color = variable, linetype = variable), size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~./100, name = "N:Maize price ratio"))+
  labs(y = 'Price ($  /  Mg)',
       x = 'Year',
       colour = "Variable")+
  scale_linetype_manual(labels = c("Maize Price", "Nitrogen Price", "Ratio"), values = c('dashed', "dashed", "solid"))+
  scale_color_discrete(labels = c("Maize Price", "Nitrogen Price", "Ratio"))+
  theme_bw()+
    labs(color  = "Guide name", linetype = "Guide name")+
  theme(legend.title =  element_blank(),
        legend.position = c(0.15, 0.8),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        panel.grid = element_blank()))

ggsave(plot = plot_1, 
       filename = "./n_policy_box/Data/figures/price_ratio_historic.pdf", width = 740/300*3, height = 460/300*3,
       units = 'in')
