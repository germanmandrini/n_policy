# Load libraries and functions #################################################
library(maps)
library(maptools)


setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('C:/Users/germanm2/Box Sync/My_Documents') #CPSC
source('./Codes_useful/R.libraries.R')

# grid10_fields_sf <- readRDS('./vr_value_v2/Data/Grid/grid10_fields_sf.rds')
field_sf <- read_sf('./n_policy_box/Data/validation/validation_fields_sf.shp')
# source("R/moldSSURGO_gm.R")

#---------------------------------------------------------------
# Step 1 Get the soils for each field (MAKE IT BETTER, SEQUENTIAL (PARALELL DOESN'T WORK, NOT BY TILE, PUT WHILE STATEMENT AND A TRY))

get_soils <- function(cell_n){
  packages_need <- c('sf', 'soilDB', 'dplyr', 'data.table')
  lapply(packages_need, require, character.only = TRUE)
  field_tmp <- one_tile_sf[cell_n,]
  
  possibleError <- tryCatch({
    ssurgo_pol <- mapunit_geom_by_ll_bbox(st_bbox(field_tmp))
    ssurgo_sf <- st_as_sf(ssurgo_pol)
    st_crs(ssurgo_sf) <- 4326
    ssurgo_sf <- dplyr::select(ssurgo_sf, musym, mukey)
    
    # ssurgo_sf_utm <- st_utm(ssurgo_sf)
    # field_tmp_utm  <- st_utm(field_tmp)
    
    field_soils_tmp <- st_intersection(field_tmp, ssurgo_sf)
    
    # tm_shape(field_soils_tmp) + tm_polygons('mukey') +
    #   tm_layout(main.title = 'Map units')# + tm_text('mukey')
    
    field_soils_tmp$area_ha <- round(as.numeric(st_area(field_soils_tmp))/10000,6)
  }, error = function(e) e)
  
  #REAL WORK:if there is no error
  if(!inherits(possibleError, "error")){
    return(field_soils_tmp)
  } else {
    return()
  }
  
}



#---------------------------------------------------------------
# Merge and re-run missing fields as many t to get all of them
grid10_soils_sf1 <- do.call(what = base::rbind, args = grid10_soils_list)

obtained_dt <- data.table(grid10_soils_sf2) %>% .[,.N, by = .(id_tile, id_10, id_field)] %>% .[,-'N'] %>% .[,ok := 1]
missing_sf <- left_join(grid10_fields_sf, obtained_dt, by = c('id_tile', 'id_10', 'id_field'))
missing_sf <- missing_sf[is.na(missing_sf$ok),] %>% dplyr::select(-ok)

source('./vr_value_v2/Data/APssurgo_master/R/get_soils_seq.R')

results_list <- list()
for(field_n in 1:nrow(missing_sf)){
  # field_n = 1
  print(field_n)
  results_list[[field_n]] <- get_soils(field_n)
}
length(results_list)
results_list_clean <- results_list[vapply(results_list, Negate(is.null), NA)]
fields_sf <- do.call(what = base::rbind, args = results_list_clean)
rownames(fields_sf) <- 1:nrow(fields_sf)


nrow(grid10_soils_sf1)

grid10_soils_sf2 <- rbind(grid10_soils_sf1, fields_sf)
nrow(grid10_soils_sf2)
grid10_soils_sf2 <- unique(grid10_soils_sf2)

grid10_soils_list[[length(grid10_soils_list)+1]] <- dplyr::select(fields_sf, -ok)
  
saveRDS(grid10_soils_sf2, "./vr_value_v2/Data/Grid/grid10_soils_sf2.rds")
grid10_soils_sf2 <- readRDS("./vr_value_v2/Data/Grid/grid10_soils_sf2.rds")

#---------------------------------------------------------------  
# Clean the soils (replace small areas by biggest)
source('./vr_value_v2/Codes/clean_fields_step1.R')
grid10_soils_dt3 <- readRDS("./vr_value_v2/Data/Grid/grid10_soils_dt3.rds")
length(unique(grid10_soils_dt3$mukey)) #total soils to download

#---------------------------------------------------------------  
# Step 2 get the horizons information
source('./vr_value_v2/Data/APssurgo_master/R/get_horizons_parallel.R')
grid10_horizons_v1_dt <- readRDS("./vr_value_v2/Data/Grid/grid10_horizons_v1_dt.rds")

# Clean the soils not available in SSURGO
source('./vr_value_v2/Codes/clean_fields_step2.R')
grid10_soils_dt4 <- readRDS("./vr_value_v2/Data/Grid/grid10_soils_dt4.rds")
grid10_fields_sf2 <- readRDS("./vr_value_v2/Data/Grid/grid10_fields_sf2.rds")

#---------------------------------------------------------------
# Step 3 Add more information and save
# info <- data.table(grid10_fields_sf2, st_coordinates(st_centroid(grid10_fields_sf2))) %>% .[, .(id_tile, id_cell, id_field, X, Y)]
# info <- info[,.(id_tile = min(id_tile), X = mean(X), Y = mean(Y)), by = .(mukey)] #remove repeated mukeys in same tile

# Add the id_tile to make folders based on it. If a mukey is in more than 1 tile, it will be located in the lower id_tile
# grid10_horizons_v2_dt <- merge(info, grid10_horizons_v1_dt, by = 'mukey')
# setcolorder(grid10_horizons_v2_dt, c('id_tile','mukey', 'X', 'Y'))
# saveRDS(grid10_horizons_v2_dt, "./vr_value_v2/Data/Grid/grid10_horizons_v2_dt.rds")
# grid10_horizons_v2_dt <- readRDS("./vr_value_v2/Data/Grid/grid10_horizons_v2_dt.rds")

#---------------------------------------------------------------
grid10_soils_dt4 <- readRDS("./vr_value_v2/Data/Grid/grid10_soils_dt4.rds")

grid10_soils_dt4_old <- readRDS("./vr_value_v2/Data/Grid/grid10_soils_dt4_old.rds")
grid10_soils_dt4_old[,id10_field_mukey := paste(id_10, id_field, mukey, sep = '_')]

grid10_soils_dt4 <- grid10_soils_dt4[!id10_field_mukey %in% unique(grid10_soils_dt4_old$id10_field_mukey)]
saveRDS(grid10_soils_dt4, "./vr_value_v2/Data/Grid/grid10_soils_dt4_difference.rds")
