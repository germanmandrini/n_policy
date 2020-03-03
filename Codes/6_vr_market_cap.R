# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
# setwd('~')

source('./Codes_useful/R.libraries.R')
library(scales)
source('./Codes_useful/gm_functions.R')
source('./vr_value/Codes/parameters.R')
# source('./Codes_useful/gm_functions.R')

grid10_soils_sf6 <- readRDS("./vr_value/Data/Grid/grid10_soils_sf6.rds")

grid10_tiles_sf2 <- readRDS("./vr_value/Data/Grid/grid10_tiles_sf2.rds") 
grid10_fields_sf <- readRDS("./vr_value/Data/Grid/grid10_fields_sf.rds") 

# perfomances_dt <- readRDS("./vr_value/Data/files_rds/perfomances_dt.rds")
perfomances_dt <- readRDS("./vr_value/Data/files_rds/perfomances_eonr_dt.rds")
perfomances_dt[,model := as.numeric(model)]

reg_model_stuff <- readRDS( "./vr_value/Data/files_rds/reg_model_stuff.rds")
full_fields_dt3 <- reg_model_stuff$full_fields #one row by field x soil
rm(reg_model_stuff)

perfomances_dt <- merge(perfomances_dt[,-'area_ha'], full_fields_dt3[,.(id_10, id_field, mukey, area_ha)], by = c('id_10', 'id_field', 'mukey'))
#---------------------------------------------------------------------------
#OPEN CITIES SF
library(maps)

cities_sf  <-  st_as_sf(us.cities, coords = c('long', 'lat'), crs = 4326) %>% 
  dplyr::filter(country.etc == 'IL') %>% dplyr::mutate(dist_chic = as.numeric(st_distance(., cities_sf[.$name == "Chicago IL",]))) %>%
  dplyr::filter(dist_chic > 40000 | dist_chic == 0 & pop > 70000) %>%
  dplyr::filter(pop > 70000)
###===================================================================###
# MARKET VALUE OF TECHNOLOGY 
names(perfomances_dt)


summary(perfomances_dt[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z, prev_crop, model)]$area_ha)

mkt_value_dt <- merge(perfomances_dt[model %in% c(4,5)], data.table(grid10_fields_sf) %>% .[,.(id_10, id_field, county_name)], by = c('id_10', 'id_field'))

do_not_aggregate = c('county_name','model', 'id_10', 'id_field', 'z', 'prev_crop')
do_aggregate =  c("Yld", "leach_n2", "N_fert","P")

mkt_value_dt2 <- aggregate_by_area(data_dt = mkt_value_dt, variables = do_aggregate, 
                                   weight = 'area_ha', by_c = do_not_aggregate)

#make one negative
mkt_value_dt2[model == 4, Yld := -Yld]
mkt_value_dt2[model == 4, leach_n2 := -leach_n2]
mkt_value_dt2[model == 4, N_fert := -N_fert]
mkt_value_dt2[model == 4, P := -P]

# Add values by group to get the difference
mkt_value_dt3 <- mkt_value_dt2[, .(Yld =  sum(Yld),
                                   leach_n2 = sum(leach_n2),
                                   N_fert = sum(N_fert),
                                   P = sum(P),
                                   area_ha = mean(area_ha)), by = .(county_name, id_10, id_field, z, prev_crop)]

# Now get the mean increase of profits for each field, accross years
mkt_value_dt4 <- mkt_value_dt3[, .(Yld =  mean(Yld),
                                   leach_n2 = mean(leach_n2),
                                   N_fert = mean(N_fert),
                                   P = mean(P)), by = .(county_name, id_10, id_field)]

mkt_value_dt4[order(-P)]

summary(mkt_value_dt4$P)

# Get the mean increase in profits for the county, accross fields
mkt_value_dt5 <- mkt_value_dt4[, .(Yld =  mean(Yld),
                                   leach_n2 = mean(leach_n2),
                                   N_fert = mean(N_fert),
                                   P = mean(P),
                                   field_c = .N), by = .(county_name)]
mkt_value_dt5 <- mkt_value_dt5[field_c > 7]
mkt_value_dt5[P <0, P := 0] #if less than 0 assume 0. 

# value_fields_sf <- merge(grid10_fields_sf, mkt_value_dt5[,.(id_10, id_field, P)], by = c('id_10', 'id_field'), all.x = T)
# value_fields_sf <- value_fields_sf[!is.na(value_fields_sf$P),]

#--------------------------------------------------
# GET THE COUNT OF CORN HA 

CFL_r <- raster('~/crop_frequency_layer/crop_frequency_corn_small.img') #shortcut for the server
CFL_r <- raster('S:/Bioinformatics Lab/germanm2/vr_value/crop_frequency_layer/crop_frequency_corn_2008-2018.img')
# install.packages('USAboundaries')
library(USAboundaries)
us_counties <- us_counties() 
crs_cdl <- crs(CFL_r)

us_states_GRS80_sf <- st_transform(us_counties, crs_cdl@projargs)
us_states_GRS80_sf <- us_states_GRS80_sf %>% dplyr::select(state_name, county_name = name) %>% dplyr::filter(state_name == 'Illinois')

us_states_GRS80_sf$county_code <- 1: nrow(us_states_GRS80_sf)

# CFL_r[CFL_r == 0] <- NA #0 means no corn. 
# CFL_r[CFL_r == 255] <- NA # 255 means background

# install.packages('fasterize')
library('fasterize')
r <- fasterize(us_states_GRS80_sf, CFL_r, field = "county_code")

county_count_dt <- data.table( county_code = getValues(r),
                               years_corn = getValues(CFL_r))

county_count_dt2 <- county_count_dt[!is.na(years_corn) & !is.na(county_code)]

county_count_dt3 <- county_count_dt2[,.N, by = .(county_code, years_corn)]
county_count_dt3[, corn_ha_11 := years_corn * N * 30 * 30 /10000]
county_count_dt4 <- county_count_dt3[,.(corn_ha = sum(corn_ha_11)/11), by = county_code]


county_count_dt5 <- merge(county_count_dt4, data.table(us_states_GRS80_sf) %>% .[,.(county_name, county_code)], by = 'county_code') %>% .[,-'county_code']


mkt_value_dt6 <- merge(mkt_value_dt5 ,county_count_dt5, by = 'county_name')
mkt_value_dt6[P > 4 ,mkt_value := P * corn_ha]

vr_mkt_value_sf <- left_join(us_states_GRS80_sf, mkt_value_dt6, by = 'county_name')

tmap_mode("plot")

tm_shape(vr_mkt_value_sf) + tm_polygons(c('P', 'mkt_value'), n = 10)+
  tm_text('county_name')



tmap_mode("view")

tm_basemap("OpenStreetMap.DE") +
  tm_shape(vr_mkt_value_sf) + tm_polygons('corn_ha', 'P')

county_count_dt



tm_basemap("OpenStreetMap.DE") +
  tm_shape(vr_mkt_value_sf) + tm_polygons('mkt_value')

tmap_save(p, filename = "./vr_value/Data/figures/vr_mkt.html")
tmap_save(p, "./vr_value/Data/figures/vr_mkt.jpg")
st_write(vr_mkt_value_sf, "./vr_value/Data/shapefiles/vr_mkt_sf.shp", delete_dsn = TRUE)

#--------------------------------------------------
# GET THE COUNT OF CORN HA 

CFL_r <- raster('~/crop_frequency_layer/crop_frequency_corn_small.img') #shortcut for the server
CFL_r <- raster('S:/Bioinformatics Lab/germanm2/vr_value/crop_frequency_layer/crop_frequency_corn_2008-2018.img')

CFL_r[CFL_r == 0] <- NA #0 means no corn. 
CFL_r[CFL_r == 255] <- NA # 255 means background


# install.packages('fasterize')
library('fasterize')
r <- fasterize(grid10_tiles_dt, CFL_r, field = "id_tile")

county_count_dt <- data.table( county_code = getValues(r),
                               years_corn = getValues(CFL_r))

county_count_dt2 <- county_count_dt[!is.na(years_corn) & !is.na(county_code)]

county_count_dt3 <- county_count_dt2[,.N, by = .(county_code, years_corn)]
county_count_dt3[, corn_ha_11 := years_corn * N * 30 * 30 /10000]
county_count_dt4 <- county_count_dt3[,.(corn_ha = sum(corn_ha_11)/11), by = county_code]


county_count_dt5 <- merge(county_count_dt4, data.table(us_states_GRS80_sf) %>% .[,.(county_name, county_code)], by = 'county_code') %>% .[,-'county_code']


mkt_value_dt6 <- merge(mkt_value_dt5 ,county_count_dt5, by = 'county_name')
mkt_value_dt6[P > 4 ,mkt_value := P * corn_ha]

vr_mkt_value_sf <- left_join(us_states_GRS80_sf, mkt_value_dt6, by = 'county_name')

tmap_mode("plot")

tm_shape(vr_mkt_value_sf) + tm_polygons(c('P', 'mkt_value'), n = 10)+
  tm_text('county_name')



tmap_mode("view")

tm_basemap("OpenStreetMap.DE") +
  tm_shape(vr_mkt_value_sf) + tm_polygons('corn_ha', 'P')

county_count_dt



tm_basemap("OpenStreetMap.DE") +
  tm_shape(vr_mkt_value_sf) + tm_polygons('mkt_value')

tmap_save(p, filename = "./vr_value/Data/figures/vr_mkt.html")
tmap_save(p, "./vr_value/Data/figures/vr_mkt.jpg")
st_write(vr_mkt_value_sf, "./vr_value/Data/shapefiles/vr_mkt_sf.shp", delete_dsn = TRUE)




