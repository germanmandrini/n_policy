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

# source('./Codes_useful/gm_functions.R')

grid10_tiles_sf7 <- readRDS("./n_policy_box/Data/Grid/grid10_tiles_sf7.rds") 
grid10_soils_dt5 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt5.rds") %>% data.table()
grid10_fields_sf2 <- readRDS('./n_policy_box/Data/Grid/grid10_fields_sf2.rds')

perfomances_dt3 <- readRDS("./n_policy_box/Data/files_rds/perfomances_dt3.rds") #cell x z level (field is out)
#---------------------------------------------------------------------------
# AGGREGATE AGAIN BY REGION CONSIDERING THE CORN PRODUCTION OF THE CELL
grid10_tiles_dt <- data.table(grid10_tiles_sf7)[,.N, .(id_tile,id_10, corn_avg_ha,corn5_tile )][,-'N']

summary(grid10_tiles_dt$corn_avg_ha)
perfomances_dt3[,id_10 := as.integer(id_10)]
perfomances_dt3 <- merge(perfomances_dt3, grid10_tiles_dt, by = 'id_10')

# perfomances_dt3[,L_cell := L*corn_avg_ha]

# ---------
#Get baselevel data
baselevel_dt <- perfomances_dt3[policy == 'ratio_5' & NMS == 'static', .(id_10, z, L_base = L, P_base = P)]

percent20_dt <- readRDS("./n_policy_box/Data/files_rds/percent20_dt.rds")

percent20_dt <- percent20_dt[NMS == 'static',.(policy, NMS)]

percent20_dt2 <- filter_dt_in_dt(perfomances_dt3, filter_dt = percent20_dt, return_table = T)

percent20_dt3 <- merge(percent20_dt2, baselevel_dt, by = c('id_10', 'z'))

percent20_dt3[, policy_cost_cell_year := (P - P_base + G) * corn_avg_ha]
percent20_dt3[,L_reduction_cell_year := (L - L_base) * corn_avg_ha]

#Accumulate by cell (z is out)
percent20_dt4 <- percent20_dt3[,.(policy_cost_kcell = sum(policy_cost_cell_year)/1000, 
                                  L_reduction_tncell = sum(L_reduction_cell_year)/1000,
                                  corn_avg_ha = mean(corn_avg_ha)), by = .(id_10 , policy,region )][order(region, policy, L_reduction_tncell)]

# percent20_dt4 <- percent20_dt4[policy == 'red_18.5' & region == '3']

#Accumulate by ranking order
percent20_dt4[, rank := order(L_reduction_tncell), by  = .(region, policy)]

ggplot(data = percent20_dt4) +
  geom_point(aes(x = rank, y = L_reduction_tncell))+
  facet_wrap(region~policy,
             scales="free")

ggplot(data = percent20_dt4) +
  geom_point(aes(x = rank, y = policy_cost_kcell))+
  facet_wrap(region~policy,
             scales="free")

percent20_dt4[, L_reduction_tncell_cum := cumsum(L_reduction_tncell), by = .(region, policy)]
percent20_dt4[, policy_cost_kcell_cum  := cumsum(policy_cost_kcell), by = .(region, policy)]

#Express in proportion to last cumulative values
last_dt <- percent20_dt4[,.SD[.N], by = .(region, policy)] %>% 
  .[,.(region, policy, L_last=L_reduction_tncell_cum, cost_last=policy_cost_kcell_cum)]

percent20_dt4 <- merge(percent20_dt4,last_dt, by = c('region', 'policy'))

percent20_dt4[, L_reduction_prop := L_reduction_tncell_cum/L_last*100]
percent20_dt4[, policy_cost_prop := policy_cost_kcell_cum/cost_last*100]
percent20_dt4[, rank_cell_prop := rank/max(rank)*100, by  = .(region, policy)]

percent20_dt4[,.SD[.N], by = .(region, policy)]


percent20_dt4[region == 1,region_lab := '1-South']
percent20_dt4[region == 2,region_lab := '2-Central']
percent20_dt4[region == 3,region_lab := '3-North']

percent20_dt4[,policy_val := as.numeric(str_extract(policy,pattern = '[0-9.]+'))]
percent20_dt4[,policy_name := as.character(lapply(policy, function(x) str_split(x, pattern = '_')[[1]][1]))]

plot_dt_long <- melt(percent20_dt4, 
                     id.vars = c('policy_name', 'region_lab', 'rank_cell_prop'), 
                     measure.vars = c('policy_cost_prop', 'L_reduction_prop'))

(p<- ggplot(data = plot_dt_long) +
  geom_path(aes(x = rank_cell_prop, y = value, color = variable), size = 1) +
  facet_free(region_lab~policy_name,
             scales="free") +
  theme_bw()+
  ylab("Proportion of total (%)") +
  xlab("Cell rank (%)")+
  theme(# panel.grid = element_blank(), 
    strip.background.x = element_blank(),
    strip.placement.x = "outside",
    # panel.spacing = unit(1.5, "lines"),
    # axis.title.x=element_blank(),
    # axis.title.y=element_text('Proportion of total'),
    legend.position = "bottom",
    plot.margin =  unit(c(1,1,1,1), "lines")
  ))
  
Caption <- "Cumulative policy cost and leaching reduction across cells of the grid. 
            X axis is the cell rank, order by their contribution to L reduction after the policy is implemented, from highest to lowest.
            Y axis is the proportion of the final cumulative value of the variable. 
            The values are calculated considering the area of corn of each cell"

ggsave(plot = p, 
       filename = "./n_policy_box/Data/figures/cumulative_by_cell.png", width = 997/300*3, height = 899/300*3,
       units = 'in')

ggsave(plot = p, 
       filename = "./n_policy_box/Data/figures/cumulative_by_cell.pdf", width = 997/300*3, height = 899/300*3,
       units = 'in')



  
ggplot(data = percent20_dt4) +
  geom_path(aes(x = rank_cell_prop, y = L_reduction_prop, colour = 'Leaching'))+
  geom_path(aes(x = rank_cell_prop, y = policy_cost_prop, colour = 'Policy cost'))+
  facet_free(region_lab~policy_name,
             scales="free") +
  theme_bw()+
  ylab("Proportion of total (%)") +
  xlab("Cell rank (%)")+
  scale_colour_manual("", 
                      breaks = c("Leaching", "Policy cost"),
                      values = c("blue", "darkgreen")) +
  theme(# panel.grid = element_blank(), 
    strip.background.x = element_blank(),
    strip.placement.x = "outside",
    # panel.spacing = unit(1.5, "lines"),
    # axis.title.x=element_blank(),
    # axis.title.y=element_text('Proportion of total'),
    legend.position = "bottom",
    plot.margin =  unit(c(1,1,1,1), "lines")
  )


ggplot(percent20_dt4, aes(L_reduction_tncell)) + stat_ecdf(geom = "point")+
  facet_free(region_lab~.,
             scales="free") 
  
 