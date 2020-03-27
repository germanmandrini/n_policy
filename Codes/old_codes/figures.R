# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
# setwd('~')

source('./Codes_useful/R.libraries.R')

grid10_tiles_sf <- readRDS("./vr_value/Data/Grid/grid10_tiles.sf5.rds") 
grid10_soils_v3_sf <- readRDS("./vr_value/Data/Grid/grid10_soils_v3_sf.rds")  
grid10_horizons_v2_dt <- readRDS("./vr_value/Data/Grid/grid10_horizons_v2_dt.rds")

#----------------------------------------------------------------------------
# MAKE A MAP OF THE 10 KM GRID
p <- tm_shape(grid10_tiles_sf) + tm_polygons("corn5_cell") +
  tm_layout(legend.text.size = 0.7,
            main.title = paste('GRID MAP - Res = 10 km - Corn presence'),
            main.title.position = "center",
            main.title.size = 1)

tmap_save(p, filename = "./vr_value/Data/figures/grid_corn_presence.jpg", scale = 2)

#----------------------------------------------------------------------------
#OPEN GEOGRAPHIC DATA
library(USAboundaries)
us_counties <- us_counties %>% dplyr::select(state_name, county_name = name) %>% dplyr::filter(state_name == 'Illinois')
#----------------------------------------------------------------------------
# MAKE A MAP OF THE FIELDS

total_fields_num <- data.table(grid10_soils_v3_sf) %>% .[, .(id_tile, id_10, id_field)] %>% unique() %>% nrow()

p <- tm_shape(grid10_tiles_sf) + tm_borders()+ 
  tm_shape(grid10_soils_v3_sf) + tm_polygons("mukey", legend.show = F) +
  tm_layout(legend.text.size = 0.7,
            title = paste(total_fields_num, "Total fields"),
            title.position = c('left', 'bottom'),
            main.title = paste('GRID MAP - Res = 10 km'),
            main.title.position = "center",
            main.title.size = 1)

tmap_save(p, filename = "./vr_value/Data/figures/grid_fields_selection.jpg", scale = 2)

#----------------------------------------------------------------------------
# MAKE A MAP OF THE # SOILS

grid10_soils_count_dt <- data.table(grid10_soils_v3_sf) %>% .[,.N, by = .(id_tile, id_10)]
grid10_tiles_sf <- left_join(grid10_tiles_sf, grid10_soils_count.dt, by = c('id_tile', 'id_10'))

p <- tm_shape(grid10_tiles_sf) + tm_polygons("N")+
  tm_layout(legend.text.size = 0.7,
            main.title = paste('GRID MAP - Res = 10 km - Numbers of soils'),
            main.title.position = "center",
            main.title.size = 1)

tmap_save(p, filename = "./vr_value/Data/figures/grid_number_soils.jpg", scale = 2)

#----------------------------------------------------------------------------
# MAP ONE FIELD
sort(unique(grid10_soils_v3_sf$county_name))
field <- grid10_soils_v3_sf[grid10_soils_v3_sf$county_name == 'Champaign',]
sort(unique(field$id_10))
field2 <- field[field$id_10 == 730 & field$id_field == 2,]

p <- tm_shape(field2) + tm_polygons("mukey") +
  tm_layout(legend.text.size = 0.7,
            main.title = paste('ONE FIELD MAP - Champaign - 730'),
            main.title.position = "center",
            main.title.size = 1)

tmap_save(p, filename = "./vr_value/Data/figures/one_field_map.jpg", scale = 2)

#----------------------------------------------------------------------------
