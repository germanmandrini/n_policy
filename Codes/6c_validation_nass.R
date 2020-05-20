# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# codes_folder <-'C:/Users/germanm2/Documents'#CPSC
# setwd("/home/germanm2")

setwd('~')#Server
codes_folder <-'~' #Server
rm(list=ls())

source('./Codes_useful/R.libraries.R')
# library(scales)
source('./Codes_useful/gm_functions.R')


nass_county_yied_sf <- readRDS("./n_policy_box/Data/validation/nass_county_yied_sf.rds")
perfomances_dt3 <- readRDS("./n_policy_box/Data/files_rds/perfomances_dt3.rds") #for 5e_validation.R

grid10_tiles_sf7 <- readRDS("./n_policy_box/Data/Grid/grid10_tiles_sf7.rds") 

##=========================================================================================================###
# PROCESS PERFORMANCE DATA

#Summarize performances to county level
validation_dt <- perfomances_dt3[policy == 'ratio_6' & NMS ==1]
validation_dt[,year := as.numeric(z)+1988]
validation_dt1 <- validation_dt[,.(id_10, year,   Y_corn)]
validation_dt2 <- validation_dt[,.(id_10, year,   Y_soy)] %>% .[,year := year +1] #shift soy yield to next year

validation_dt3 <- merge(validation_dt1, validation_dt2, by = c('id_10', 'year')) #in the right year

validation_dt4 <- merge(validation_dt3, data.table(grid10_tiles_sf7) %>% .[,.(id_10, county_name, corn_avg_ha)], by = 'id_10')

# AGGREGATE THE DATA TO COUNTY X YEAR LEVEL CONSIDERING THE corn_avg_ha
names(perfomances_dt)
do_not_aggregate = c('county_name', 'year')
do_aggregate =  c("Y_corn", 'Y_soy')


validation_dt5 <- aggregate_by_area(data_dt = validation_dt4, variables = do_aggregate, 
                                       weight = 'corn_avg_ha', by_c = do_not_aggregate) #cell x z level (mukey and field are out)

# clean names
validation_dt5[,county_name := tolower(county_name)]
validation_dt5[, county_name := gsub("\\s", "", county_name)]
validation_dt5[, county_name := gsub("\\.", "", county_name)]
validation_dt5[, county_name := gsub("\\'", "", county_name)]

##=========================================================================================================###
#PROCESS THE NASS TABLE DATA
# https://quickstats.nass.usda.gov/#CED9AC45-21BA-3EC5-8717-A4D5BE127BC2https://quickstats.nass.usda.gov/#CED9AC45-21BA-3EC5-8717-A4D5BE127BC2
# nass_county_yied_dt <- fread('C:/Users/germanm2/Box Sync/TCC2019_windows/seed_advisor_value/data/nass_county_yield.csv')
# Open NASS data
nass_county_yied_corn_dt <- fread('./n_policy_box/Data/validation/nass_county_yields_corn.csv')
nass_county_yied_corn_dt <- nass_county_yied_corn_dt[,c("Year", "County" ,  "Value")]
setnames(nass_county_yied_corn_dt, c("Year", "County" ,  "Value"), c("year", "county_name" ,  "Y_corn_nass"))
# Corn: 1 bushel/acre = 62.77 (63) kilograms/hectare
nass_county_yied_corn_dt[,Y_corn_nass := 62.77*Y_corn_nass]


nass_county_yied_soy_dt <- fread('./n_policy_box/Data/validation/nass_county_yields_soy.csv')
nass_county_yied_soy_dt <- nass_county_yied_soy_dt[,c("Year", "County" ,  "Value")]
setnames(nass_county_yied_soy_dt, c("Year", "County" ,  "Value"), c("year", "county_name" ,  "Y_soy_nass"))
# Soy: 1 bushel/acre = 67.25 (67) kilograms/hectare
nass_county_yied_soy_dt[,Y_soy_nass := 67.25*Y_soy_nass]

nass_county_yied_dt <- merge(nass_county_yied_corn_dt, nass_county_yied_soy_dt, by = c('year', 'county_name'))

nass_county_yied_dt[, county_name := tolower(county_name)]
nass_county_yied_dt[, county_name := gsub("\\s", "", county_name)]
nass_county_yied_dt[, county_name := gsub("\\.", "", county_name)]
nass_county_yied_dt[, county_name := gsub("\\'", "", county_name)]

##=========================================================================================================###
# Combine files
unique(validation_dt5$county_name) %in% unique(nass_county_yied_dt$county_name) 
unique(nass_county_yied_dt$county_name)[!unique(nass_county_yied_dt$county_name) %in% unique(validation_dt5$county_name)]

validation_nass_dt <- merge(validation_dt5, nass_county_yied_dt, by = c('year', 'county_name'))

# Pred vs obs plot  

(plot_1 <- ggplot(data=validation_nass_dt, aes(x = Y_corn_nass, y = Y_corn, color = year)) +
  geom_point()+ theme(aspect.ratio=1) + coord_fixed() + geom_abline() + ylim(0, 15500)+ xlim(0, 15500) +
  labs(# x=expression(paste("Grafton N-", NO[3],"(kg sec-1)",sep=""))
       # expression(paste("4"^"th")
       y = expression(paste("APSIM County yield (kg ha"^"-1", "year"^"-1",")")),
       x = expression(paste("NASS County yield (kg ha"^"-1", "year"^"-1",")")))+
  annotate("text", x=0, y=15500, label= "a)", size = 10)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(legend.position="bottom")
  )

(plot_2 <- ggplot(data=validation_nass_dt, aes(x = Y_soy_nass, y = Y_soy, color = year)) +
    geom_point()+ theme(aspect.ratio=1) + coord_fixed() + geom_abline() + ylim(0, 6000)+ xlim(0, 6000) +
    labs(# x=expression(paste("Grafton N-", NO[3],"(kg sec-1)",sep=""))
      # expression(paste("4"^"th")
      y = expression(paste("APSIM County yield (kg ha"^"-1", "year"^"-1",")")),
      x = expression(paste("NASS County yield (kg ha"^"-1", "year"^"-1",")")))+
    annotate("text", x=0, y=6000, label= "b)", size = 10)+
    theme_bw()+
    theme(panel.grid = element_blank(), 
          legend.position = "none")
)

# Create one single legend grid arrange
# http://www.sthda.com/english/wiki/wiki.php?id_contents=7930

# To save the legend of a ggplot, the helper function below can be used :
# install.packages("cowplot")
library(cowplot)
library(gridExtra)
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# 1. Create the plots
#++++++++++++++++++++++++++++++++++
# Create a box plot
# bp <- ggplot(df, aes(x=dose, y=len, color=dose)) +
#   geom_boxplot()
# # Create a violin plot
# vp <- ggplot(df, aes(x=dose, y=len, color=dose)) +
#   geom_violin()+
#   geom_boxplot(width=0.1)+
#   theme(legend.position="none")
# 2. Save the legend
#+++++++++++++++++++++++
legend <- get_legend(plot_1)
# 3. Remove the legend from the box plot
#+++++++++++++++++++++++
plot_1 <- plot_1 + theme(legend.position="none")
# 4. Create a blank plot
blankPlot <- ggplot()+geom_blank(aes(1,1)) + 
  cowplot::theme_nothing()

# Bottom-left legend
grid.arrange(plot_1, plot_2, legend, blankPlot,
             ncol=2, nrow = 2, 
             widths = c(2.7, 2.7), heights = c(2.5, 0.2))
# Bottom-center legend:
  
plot_grid <- grid.arrange(plot_1, plot_2, legend, ncol=2, nrow = 2, 
               layout_matrix = rbind(c(1,2), c(3,3)),
               widths = c(2.5, 2.5), heights = c(2.5, 0.2))

ggsave(plot_grid, 
       filename = "./n_policy_box/Data/figures/validation_nass_xyplot.jpg", 
       width = 1286/300*3, height = 781/300*3,
       units = 'in')




##=========================================================================================================###
# Map the residuals 1 = county level
validation_nass_dt[, Y_corn_res := Y_corn - Y_corn_nass]
validation_nass_dt[, Y_soy_res := Y_soy - Y_soy_nass]

validation_nass_res_dt <- validation_nass_dt[,.(Y_corn_res = mean(Y_corn_res),
                                                 Y_soy_res = mean(Y_soy_res)), by = county_name][order(-Y_corn_res)]

ggplot(data=validation_nass_res_dt, aes(x = Y_corn_res, y = Y_soy_res)) +
  geom_point() #soy res vs corn res
# 
# 
# grid10_tiles_sf7$county_name <- tolower(grid10_tiles_sf7$county_name)
# grid10_tiles_sf7$county_name <- gsub("\\s", "", grid10_tiles_sf7$county_name)
# grid10_tiles_sf7$county_name <- gsub("\\.", "", grid10_tiles_sf7$county_name)
# grid10_tiles_sf7$county_name <- gsub("\\'", "", grid10_tiles_sf7$county_name)
# 
# counties_sf <- grid10_tiles_sf7 %>% group_by(county_name) %>% summarise(corn_avg_ha = sum(corn_avg_ha))
# counties_res_sf <- merge(counties_sf, validation_nass_res_dt, by = 'county_name')
# 
# 
# 
# tm_shape(counties_res_sf) + tm_polygons(c('Y_corn_res', 'Y_soy_res'))



#OBTAIN THE COUNTY AND STATE BOUNDARIES
us_counties_sf <- sf::read_sf('./n_policy_box/Data/validation/StCoGenAll17_WGS84WMAS/CoUS17_WGS84WMAS.shp') %>% 
  st_transform(4326) %>% dplyr::select(county_name = atlas_name)

us_states_sf <- sf::read_sf("./n_policy_box/Data/validation/StCoGenAll17_WGS84WMAS/StUS17_WGS84WMAS.shp") %>% 
  st_sf() %>% st_transform(4326) %>% dplyr::select(state_name = atlas_name)

us_counties_sf <- st_join(us_counties_sf, us_states_sf, join = st_intersects, left = TRUE, largest = TRUE)

us_counties_sf <- us_counties_sf %>% group_by(county_name, state_name ) %>% summarize() %>% ungroup()

us_counties_sf <- dplyr::mutate(us_counties_sf, county_name = tolower(county_name),
                                state_name = tolower(state_name))

illinois_counties_sf <- dplyr::filter(us_counties_sf, state_name == 'illinois')

illinois_counties_sf$county_name <- gsub("\\s", "", illinois_counties_sf$county_name)
illinois_counties_sf$county_name <- gsub("\\.", "", illinois_counties_sf$county_name)
illinois_counties_sf$county_name <- gsub("\\'", "", illinois_counties_sf$county_name)
tm_shape(illinois_counties_sf) + tm_polygons('county_name')
  
counties_res_sf <- merge(illinois_counties_sf, validation_nass_res_dt, by = 'county_name', all.x = T)
tm_shape(counties_res_sf) + tm_polygons(c('Y_corn_res', 'Y_soy_res'))

(p1 <- tm_shape(counties_res_sf) + tm_polygons(c('Y_corn_res', 'Y_soy_res'), 
                                        n =5, 
                                        title = c(expression(paste("Mean residual (kg ha"^"-1", "year"^"-1",")"))),
                                        style ="cont")+
    tm_layout(title = c('a)', 'b)'),
              main.title.position = c(0,0),
              legend.text.size = 0.7,
              main.title.size = 1.5,
              title.snap.to.legend =F,
              legend.width = 1,
              legend.position = c('left', 'bottom')))

tmap_save(p1, "./n_policy_box/Data/figures/validation_nass_map.jpg", 
          width = 10, height = 15,
          units = 'in')



##=========================================================================================================###
# Map the residuals 2 = cell level
validation_dt4[,county_name := tolower(county_name)]
validation_dt4[, county_name := gsub("\\s", "", county_name)]
validation_dt4[, county_name := gsub("\\.", "", county_name)]
validation_dt4[, county_name := gsub("\\'", "", county_name)]

cell_residuals_dt <- merge(validation_dt4, nass_county_yied_dt, by = c('year', 'county_name'))

cell_residuals_dt[, Y_corn_res := Y_corn - Y_corn_nass]
cell_residuals_dt[, Y_soy_res := Y_soy - Y_soy_nass]

cell_residuals_dt2 <- cell_residuals_dt[,.(Y_corn_res = mean(Y_corn_res),
                                                Y_soy_res = mean(Y_soy_res)), by = .(id_10, county_name)][order(-Y_corn_res)]

ggplot(data=cell_residuals_dt2, aes(x = Y_corn_res, y = Y_soy_res)) +
  geom_point() #soy res vs corn res

grid10_tiles_sf7$county_name <- tolower(grid10_tiles_sf7$county_name)
grid10_tiles_sf7$county_name <- gsub("\\s", "", grid10_tiles_sf7$county_name)
grid10_tiles_sf7$county_name <- gsub("\\.", "", grid10_tiles_sf7$county_name)
grid10_tiles_sf7$county_name <- gsub("\\'", "", grid10_tiles_sf7$county_name)

grid10_res_sf <- merge(grid10_tiles_sf7, cell_residuals_dt2, by = c('id_10', 'county_name'))

tm_shape(grid10_res_sf) + tm_polygons(c('Y_corn_res', 'Y_soy_res'))


















nass_county_yied_dt[,year := paste0('yield_', Year)]
nass_yield_wide_dt <- dcast(nass_county_yied_dt, state_name + county_name ~ year, value.var = "Value", fun.agg = function(x) mean(x))

nass_yield_wide_dt$county_name <- gsub("\\s", "", nass_yield_wide_dt$county_name)
nass_yield_wide_dt$state_name <- gsub("\\s", "", nass_yield_wide_dt$state_name)
nass_yield_wide_dt$county_name <- gsub("\\.", "", nass_yield_wide_dt$county_name)
nass_yield_wide_dt$state_name <- gsub("\\.", "", nass_yield_wide_dt$state_name)
nass_yield_wide_dt$county_name <- gsub("\\'", "", nass_yield_wide_dt$county_name)
nass_yield_wide_dt$state_name <- gsub("\\'", "", nass_yield_wide_dt$state_name)
nass_yield_wide_dt$county_name <- gsub("saint", "st", nass_yield_wide_dt$county_name)
nass_yield_wide_dt$county_name <- gsub("saint", "st", nass_yield_wide_dt$county_name)




growers_sf <- st_join(st_transform(grid10_tiles_sf7, 4326), nass_county_yied_sf, join = st_intersects, left = TRUE)

growers_sf <- st_join(performance_by_product_field_sf, nass_county_yied_sf, join = st_intersects, left = TRUE)
growers_sf2 <- growers_sf %>% 
  dplyr::filter(crop == 'corn') %>%
  dplyr::mutate(yld_nass = get(paste0('yield_', year))) %>% 
  dplyr::select(-c('yield_2013', 'yield_2014', 'yield_2015', 'yield_2016', 'yield_2017', 'yield_2018', 'state_county'))

neighbors_dt <- data.table(growers_sf2) %>% .[,-'geometry']
##=========================================================================================================###
# PLOT
us_states_sf <- us_counties_sf %>% group_by(state_name) %>% summarise()
idx <- unique(unlist(st_intersects(growers_sf2, nass_county_yied_sf)))
states_idx <- unique(nass_county_yied_sf[idx, ]$state_name)
nass_county_yied_sf2 <- nass_county_yied_sf[nass_county_yied_sf$state_name %in% states_idx,]
us_states_sf<- us_states_sf[us_states_sf$state_name %in% states_idx,]


(plot_n1 <- tm_shape(nass_county_yied_sf2) + tm_polygons('yield_2018', alpha = 0.7, legend.show = F) +
    # tm_shape(buffer_sf_plot_sf) + tm_dots(size = 1, col = 'blue', shape = 15)+
    tm_shape(growers_sf2) + tm_dots() +
    #tm_shape(us_counties_sf) + tm_borders() +
    tm_shape(us_states_sf) + tm_borders(lwd = 2))

tmap_save(plot_n1, 'C:/Users/germanm2/Box Sync/TCC2019_windows/seed_advisor_value/figures/nass_value_approach.jpg')