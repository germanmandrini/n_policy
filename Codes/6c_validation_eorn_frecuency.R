# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
setwd('~')
rm(list=ls())

source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')
source('./n_policy_git/Codes/parameters.R')
source('C:/Users/germanm2/Documents/n_policy_validation_git/Codes/parameters.R')

# grid10_tiles_dt6 <- readRDS("./n_policy_box/Data/Grid/grid10_tiles_sf6.rds") %>% data.table()
yc_yearly_dt3 <- readRDS("./n_policy_box/Data/files_rds/yc_yearly_dt3.rds")
Pn_tmp = Pn * 0.9
yc_yearly_dt3[, P_1 := Yld * Pc - N_fert * Pn_tmp]  #update profits

# yc_yearly_dt3 <- merge(yc_yearly_dt3, unique(grid10_tiles_dt6[,.(id_10, region)]), by = 'id_10')
yc_north_dt <- yc_yearly_dt3[region == 3]
yc_north_dt <- yc_north_dt[mukey %in% sample(unique(yc_north_dt$mukey), 500)]
yc_north_eonr_dt <- yc_north_dt[, .SD[ P_1 == max( P_1)], by = .(id_10, mukey, z)]
yc_north_eonr_dt <- yc_north_eonr_dt[, .SD[ N_fert == min( N_fert )], by = .(id_10, mukey, z)]
setnames(yc_north_eonr_dt, 'N_fert', 'eonr')
brks <- c(0,30,60,90,120,150,180,210,240,270,300,330)
yc_north_eonr_dt[,bin:=findInterval(eonr, brks)]
ggplot(yc_north_eonr_dt, aes(bin     )) + geom_bar()

bins_table <- data.table(bin = 1:(length(brks)-1),
           eonr_bin = paste(brks[-12], '-' , brks[-1], sep = ''))
bins_table[bin ==11, eonr_bin := '300+']
yc_north_eonr_dt2 <- merge(yc_north_eonr_dt, bins_table, by = 'bin')

str(yc_north_eonr_dt2)

yc_north_eonr_dt2$eonr_bin <- factor(yc_north_eonr_dt2$eonr_bin, levels = bins_table$eonr_bin)
ggplot(yc_north_eonr_dt2, aes(eonr_bin     )) + geom_bar()+
  labs(x= 'N fert (kg/ha)')+
  theme_bw()+
  theme(legend.title =  element_blank(),
        axis.text=element_text(size=14))

yc_north_count_dt <- yc_north_eonr_dt[,.N, by = eonr_bin]
yc_north_count_dt[,sites_pct := N/sum(N)]

ggplot(data = yc_north_count_dt) + 
  geom_bar(aes(x = eonr_bin, y = sites_pct))

hist(yc_north_eonr_dt$N_fert)

ggplot(data = yc_north_eonr_dt) + 
  geom_histogram(aes(x = eonr), alpha=0.6, breaks = brks)
