setwd("~/")
setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
setwd('C:/Users/germa/Box Sync/My_Documents') 

source('./Codes_useful/R.libraries.R')
source('./vr_value_v2/Codes/functions_vr.R')
#install.packages('fasterize')
# library('fasterize')


#Load Data
CFL_r <- raster('S:/Bioinformatics Lab/germanm2/vr_value/crop_frequency_layer/crop_frequency_corn_small.img')

CFL_r <- raster::raster("./crop_frequency_layer/crop_frequency_corn_small.img")

#Old files
grid10_fields_sf <- readRDS("./vr_value/Data/Grid/grid10_fields_sf.rds") 
grid10_tiles.sf <- readRDS("./vr_value/Data/Grid/grid10_tiles.sf5.rds")
grid10_soils_sf6 <- readRDS("./vr_value/Data/Grid/grid10_soils_sf6.rds")

#New files
grid10_tiles_sf6 <- readRDS("./vr_value_v2/Data/Grid/grid10_tiles_sf6.rds") 
grid10_soils_dt5 <- readRDS("./vr_value_v2/Data/Grid/grid10_soils_dt5.rds") %>% data.table()
grid10_soils_sf2 <- readRDS('./vr_value_v2/Data/Grid/grid10_soils_sf2.rds')
grid10_fields_sf2 <- readRDS('./vr_value_v2/Data/Grid/grid10_fields_sf2.rds')

tm_shape(grid10_tiles_sf6) + tm_polygons('id_tile')
tile_n = 10
cell_n = 765#755#763#765

fields_sf <- grid10_fields_sf2[grid10_fields_sf2$id_10 == cell_n,]
soils_sf <- grid10_soils_sf2[grid10_soils_sf2$id_10 == cell_n,]

one_cell.sf <- grid10_tiles_sf6[grid10_tiles_sf6$id_10 == cell_n,]

#tm_shape(one_cell.sf)+tm_polygons()
CFL_cell <- raster::crop(CFL_r,one_cell.sf)
CFL_cell <- raster::mask(CFL_cell, one_cell.sf)

#-------------------------------
#1) Corn frequency map
CFL_cell_sp <- rasterToPolygons(CFL_cell, dissolve = T)
CFL_cell_sf <- st_as_sf(CFL_cell_sp) %>% dplyr::rename(corn_years = crop_frequency_corn_small)
CFL_cell_sf <- st_cast(CFL_cell_sf, 'POLYGON')
table(CFL_cell_sf$corn_years)
CFL_cell_sf <- st_utm(CFL_cell_sf)

tm_shape(CFL_cell_sf) + tm_polygons('corn_years')

breaks_n <- c(-20,5,10,20, 30,40)


(p1 <- tm_shape(CFL_cell_sf) + tm_polygons(c('corn_years'), title = 'years of corn', 
                                        #breaks = breaks_n, 
                                        style ="cont", palette = "Greys", colorNA = 'white', midpoint = 6) +
    tm_layout(panel.labels = 'a)',
              legend.text.size = 0.7,
              main.title.size = 1.2,
              legend.position = c('left', 'bottom')))



CFL_target_sf <- CFL_cell_sf[CFL_cell_sf$corn_years > 4,]

(p2 <- tm_shape(CFL_target_sf) + tm_polygons(c('corn_years'), title = 'years of corn', 
                                           #breaks = breaks_n, 
                                           style ="cont", palette = "Greys", colorNA = 'white', midpoint = 6)+
    tm_layout(panel.labels = 'b)',
              legend.text.size = 0.7,
              main.title.size = 1.2,
              legend.position = c('left', 'bottom')))


(p3 <- tm_shape(CFL_target_sf) + tm_polygons(c('corn_years'), title = 'years of corn', 
                                             #breaks = breaks_n, 
                                             style ="cont", palette = "Greys", colorNA = 'white', midpoint = 6)+
    tm_layout(panel.labels = 'c)',
              legend.text.size = 0.7,
              main.title.size = 1.2,
              legend.position = c('left', 'bottom')) +
    tm_shape(fields_sf) + tm_borders(col = 'red', lwd = 3) + tm_text('id_field', col = 'red', size = 0.7))


field_n =3 

three_soils <- data.table(grid10_soils_sf2) %>% .[,.N, by = .(id_10, id_field, mukey)] %>% .[,.N, by = .(id_10, id_field)] %>% .[N==3 & id_10 > 500 & id_10 < 800] 


(p4 <- tm_shape(grid10_soils_sf2[grid10_soils_sf2$id_10 == 724 & grid10_soils_sf2$id_field == 1,]) + 
    tm_polygons(c('mukey'), #title = 'mukey', #breaks = breaks_n, 
                            style ="cont", palette = "Greys", colorNA = 'white')+
    tm_layout(panel.labels = 'd)',
              legend.text.size = 0.7,
              main.title.size = 1.2,
              legend.position = c('left', 'bottom')))

tmap_arrange(p1, p2, p3, p4, ncol = 2)

tmap_save(tmap_arrange(p1, p2, p3, p4, ncol = 2) , "./vr_value_v2/Data/figures/field_selection_steps.jpg", 
          width = 8, height =  5, units = 'in')






