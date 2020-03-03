# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
setwd('~')

source('./Codes_useful/R.libraries.R')
library(scales)
source('./Codes_useful/gm_functions.R')
source('./vr_value_v2/Codes/parameters.R')
# source('./Codes_useful/gm_functions.R')

# grid10_soils_sf6 <- readRDS("./vr_value_v2/Data/Grid/grid10_soils_sf6.rds")

grid10_tiles_sf6 <- readRDS("./vr_value_v2/Data/Grid/grid10_tiles_sf6.rds") 
# grid10_fields_sf <- readRDS("./vr_value_v2/Data/Grid/grid10_fields_sf.rds") 

#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
# GET THE COUNT OF CORN HA 

CFL_r <- raster('~/crop_frequency_layer/crop_frequency_corn_small.img') #shortcut for the server
# CFL_r <- raster('S:/Bioinformatics Lab/germanm2/vr_value/crop_frequency_layer/crop_frequency_corn_2008-2018.img')

# install.packages('USAboundaries')
crs_cdl <- crs(CFL_r)

grid10_tiles_GRS80_sf <- st_transform(grid10_tiles_sf6, crs_cdl@projargs)
grid10_tiles_GRS80_sf <- grid10_tiles_GRS80_sf %>% dplyr::select(id_10)

# CFL_r[CFL_r == 0] <- NA #0 means no corn. 
# CFL_r[CFL_r == 255] <- NA # 255 means background

# install.packages('fasterize')
library('fasterize')
r <- fasterize(grid10_tiles_GRS80_sf, CFL_r, field = "id_10") # for each cell in CFL_r it has the id_10

corn_count_dt <- data.table( id_10 = raster::getValues(r),
                               years_corn = raster::getValues(CFL_r))

corn_count_dt2 <- corn_count_dt[!is.na(years_corn) & !is.na(id_10)]

# corn_count_dt3 <- corn_count_dt[,.N, by = .(id_10)]
# corn_count_dt3 <- corn_count_dt3[!is.na(id_10)]
# summary(corn_count_dt3$N)
# 111556*30*30/10000
# 10000*10000/10000

corn_count_dt3 <- corn_count_dt2[,.N, by = .(id_10, years_corn)]
corn_count_dt3[, corn_ha_11 := years_corn * N * 30 * 30 /10000]
corn_count_dt4 <- corn_count_dt3[,.(corn_avg_ha = sum(corn_ha_11)/11), by = id_10]
summary(corn_count_dt4$corn_avg_ha)

grid10_tiles_sf7 <- merge(grid10_tiles_sf6, corn_count_dt4, by = 'id_10') 


saveRDS(grid10_tiles_sf7, "./vr_value_v2/Data/Grid/grid10_tiles_sf7.rds")  





