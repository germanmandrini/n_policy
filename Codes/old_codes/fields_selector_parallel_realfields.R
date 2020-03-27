#===================================
# prepare clusters
#===================================


no_cores <- detectCores() - 1
cl <- makeCluster(no_cores,type='SOCK')

#===================================
# parallelized simulations 
#===================================

# cell_n=704
45*10000/(30*30) #45 ha = 500 cells (30*30)

600*600/10000

+21*30*21*30/10000
#FUNCTIONS TO REMEMBER
# boundaries(CFL_cell, type='inner', classes=T, directions=8, asNA=FALSE)
# cell_clump <- clump(CFL_cell, directions=8)


process_cells <- function(cell_n){
  library(data.table)
  library(raster)
  library(sf)
  library(dplyr)
  
  one_cell.sf <- grid5000_tiles.sf[grid5000_tiles.sf$id_5000 == cell_n,]
  
  #tm_shape(one_cell.sf)+tm_polygons()
  CFL_cell <- raster::crop(CFL_r,one_cell.sf)
  CFL_cell <- raster::mask(CFL_cell, one_cell.sf)
  CFL_cell[CFL_cell[] < 4] <- NA
  plot(CFL_cell)
  
  CFL_boundaries <- boundaries(CFL_cell, type='inner', classes=T, directions=4, asNA=FALSE)
  #CFL_boundaries[is.na(CFL_boundaries)] <- 0
  plot(CFL_boundaries)
  CFL_boundaries[CFL_boundaries==0] <- NA
  
  b_sp <- rasterToPolygons(CFL_boundaries, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=T)
  b_sf <- st_as_sf(b_sp)
  st_write(b_sf, "./vr_value/Data/Grid/CFL_boundaries.shp", delete_dsn=TRUE)
  
  
  CFL_cell[CFL_boundaries[] == 1] <- NA
 
  
  cell_clump <- clump(CFL_cell, directions=4)
  plot(cell_clump)
  
  #convert to sf
  field_sp <- rasterToPolygons(cell_clump, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=T)
  field_sf <- st_as_sf(field_sp) # convert polygons to 'sf' object
  tm_shape(field_sf) + tm_polygons()
  field_sf$area_ha <- as.numeric(st_area(field_sf))/10000
  field_sf <- field_sf %>% dplyr::filter(area_ha > 20)
  nrow(field_sf)
  st_write(field_sf, "./vr_value/Data/Grid/field_sf.shp", delete_dsn=TRUE)
  
  (600*600)/(30*30)
  
  
  field_id_info <- fasterize::fasterize(sf = field_sf, raster = CFL_cell, field = 'id_field')
  corn_prop_tb <- table(field_id_info[], CFL_cell[])/400
  field_sf$corn_prop <- as.numeric(corn_prop_tb)
  
  tm_shape(field_sf) + tm_polygons('corn_prop')
  
  field_sf[order(-field_sf$corn_prop),]
  
  
  
  CFL_cell4 <- CFL_cell > 4
  CFL_cellw <- focal(CFL_cell4, matrix(1, nrow = 21, ncol = 21), sum)
  plot(CFL_cellw)
  
  CFL_cellw[CFL_cellw < 21*21] <- NA
  plot(CFL_cellw)
  pol <- rasterToPolygons(CFL_cellw, dissolve = TRUE)
  pol <- st_as_sf(pol)
  pol <- st_cast(pol, 'POLYGON')
  pol$Area <- as.numeric(st_area(pol))
  pol <- pol[order(pol$Area, decreasing = TRUE),]
  pnt <- st_centroid(pol)
  pntb <- st_buffer(pnt, dist = 0.5*sqrt(4e5), nQuadSegs = 1)
  pntb <- st_rotate(pntb, pi/4)
  
  plot(CFL_cell4)
  plot(st_geometry(pntb), add = TRUE)
  
  
  CFL_cell[!is.na(CFL_cell)] <- 1
  plot(CFL_cell)
  
  field_r <- raster::raster(CFL_cell)
  res(field_r) <- 600
  field_r[] <- 1:ncell(field_r)
  names(field_r) <- 'id_field'
  plot(field_r)
  
  
  
 
  
  
  
  

  
  
  #convert to sf for visualization
  cell_clump_sp <- rasterToPolygons(cell_clump, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=TRUE)
  names(cell_clump_sp@data) <- 'id_clump'
  cell_clump_sf <- st_as_sf(cell_clump_sp) # convert polygons to 'sf' object
  nrow(cell_clump_sf)
  tm_shape(cell_clump_sf) + tm_polygons('id_clump')
  
  
  grid5000_tiles.sf <- dplyr::left_join(grid5000.sf, tiles_key.df)
  head(grid5000_tiles.sf)
  saveRDS(grid5000_tiles.sf, "./Project.Grid/Grid/rds.files/grid5000_tiles.sf1.rds")
  

  
  install.packages('SDMTools')
  library(SDMTools)
  ?ConnCompLabel
  ConnCompLabel(mat)
  
  ext<-getValues(CDL.stk_cell_mask)
  
  grid5000_landuse_10yr_onetile.dt <- data.table()
  for(year.n in 1:10){
    # year.n =1
    year.real <- ifelse(year.n <11, year.n+2007, NA)
    tbl <- table(ext[,year.n]) %>% data.table()
    names(tbl) <- c('id.crop', 'value')
    tbl <- tbl[id.crop %in% cdl.info.dt$id.crop][,id.crop := as.numeric(id.crop)]
    tbl[,year := year.real]
    tbl[,source := 'crop.CDL']
    tbl[,unit := 'count30x30']
    tbl <- merge(tbl, cdl.info.dt, by = 'id.crop', all.x = TRUE)
    tbl[,id.crop := NULL]
    tbl[,id_5000 := one_cell.sf$id_5000]
    tbl[,id_tile := one_cell.sf$id_tile]
    setcolorder(tbl, c('id_tile', 'id_5000', 'year', 'source', 'unit', 'variable', 'value'))
    grid5000_landuse_10yr_onetile.dt <- rbind(grid5000_landuse_10yr_onetile.dt, tbl)
  }#end of 10 years loop
  return(grid5000_landuse_10yr_onetile.dt)
}
keep <- c('keep', 'process_cells', 'grid5000_tiles.sf', 'cdl.info.dt', 'CDL.stk', 'cl','ids_5000_seq')

#rm(list = ls()[!ls() %in% keep])
#ids_5000_seq2 <- ids_5000_seq[10:12]

clusterExport(cl, varlist = keep, envir=environment())

         
results_list <- parLapply(cl, ids_5000_seq, function(x) process_cells(x))

results <- do.call("rbind", results_list) %>% data.table()

stopCluster(cl)
