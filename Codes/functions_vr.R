st_utm <- function(sf_obj) {
  # Function to get UTM Zone from mean longitude:
  long2UTM <- function(long) {
    (floor((long + 180) / 6) %% 60) + 1
  }
  
  # Check if the object class is 'sf':
  obj_c <- class(sf_obj)[1]
  if (obj_c == "sf") {
    # In case the object has no projectin assigned,
    #  assume it to geographic WGS84 :
    if (is.na(sf::st_crs(sf_obj))) {
      sf::st_crs(sf_obj) <- sf::st_crs(4326)
    }
    
    # Get the center longitude in degrees:
    bb <- sf::st_as_sfc(sf::st_bbox(sf_obj))
    bb <- sf::st_transform(bb, sf::st_crs(4326))
    
    # Get UTM Zone from mean longitude:
    utmzone <- long2UTM(mean(sf::st_bbox(bb)[c(1, 3)]))
    
    # Get the hemisphere based on the latitude:
    NS <- 100 * (6 + (mean(sf::st_bbox(bb)[c(2, 4)]) < 0))
    
    # Add all toghether to get the EPSG code:
    projutm <- sf::st_crs(32000 + NS + utmzone)
    
    # Reproject data:
    sf_obj <- sf::st_transform(sf_obj, projutm)
    return(sf_obj)
  } else {
    options(error = NULL)
    stop("Object class is not 'sf', please insert a sf object!")
  }
}


make_squares <- function(pnt_sf, area_ha){
  nrows <- nrow(pnt_sf)
  fields <- list()
  for(i in 1:nrows){
    pnt <- pnt_sf[i,]
    dist_sq <- sqrt(area_ha*10000)/2
    centroid <- st_coordinates(pnt)
    pnt1 <- centroid + matrix(c(-dist_sq, dist_sq), ncol = 2)
    pnt2 <- centroid + matrix(c(dist_sq, dist_sq), ncol = 2)
    pnt3 <- centroid + matrix(c(dist_sq, -dist_sq), ncol = 2)
    pnt4 <- centroid + matrix(c(-dist_sq, -dist_sq), ncol = 2)
    pts = list(rbind(pnt1, pnt2, pnt3, pnt4, pnt1))
    pol <- st_sf(id_field = i, geometry = st_sfc(st_polygon(pts)))
    fields[[i]] <- pol
  }  
  
  fields_sf <- do.call(what = base::rbind, args = fields)
  st_crs(fields_sf) <- st_crs(pnt_sf)
  return(fields_sf)
}

sample_fields <- function(CFL_cellw, dist_min, fields_num){
  keep <- TRUE
  sample <- 0
  while(keep & sample < 50){
    pnt_sp <- sampleRandom(CFL_cellw, fields_num, na.rm=TRUE, ext=NULL, 
                           cells=FALSE, rowcol=FALSE, xy=FALSE, sp=T, asRaster=FALSE)
    pnt_sf <- st_as_sf(pnt_sp)
    pnt_sf <- st_utm(pnt_sf)
    if(fields_num == 1){break} #if only one field is searched finish the loop, no test needed
    dist_calc <- as.numeric(st_distance(pnt_sf))
    dist_calc <- min(dist_calc[dist_calc != 0])
    keep <- dist_calc < dist_min
    sample = sample + 1
    if(sample == 50){#if couldn't find # fields in a X tries, get 1 less
        fields_num <- fields_num-1
        sample = 0
    }
  }
  pnt_sf <- st_as_sf(pnt_sp)
  pnt_sf <- st_utm(pnt_sf)
  return(pnt_sf)
}

# install.packages('panelaggregation')
# library(panelaggregation)
# computeWeightedMeans(data_table, variables, weight, by)
