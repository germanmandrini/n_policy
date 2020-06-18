

grid10_horizons_v1_dt <- readRDS("./vr_value_v2/Data/Grid/grid10_horizons_v1_dt.rds")

grid10_soils_dt3 <- readRDS("./vr_value_v2/Data/Grid/grid10_soils_dt3.rds")

grid10_soils_dt3[,apsoil := ifelse(mukey %in% unique(grid10_horizons_v1_dt$mukey), 1, 0)]

table(grid10_soils_dt3$apsoil)

grid10_soils_dt3[apsoil == 0,]

grid10_soils_dt4 <- grid10_soils_dt3[grid10_soils_dt3$apsoil == 1, ] %>% dplyr::select(-c('apsoil')) 


#--------------------------------------------------------------------------------
#Check the area by field
grid10_soils_dt4[, field_ha := sum(area_ha), by = .(id_tile, id_10, id_field)]
grid10_soils_dt4 <- grid10_soils_dt4[field_ha > 39]

grid10_fields_sf <- readRDS('./vr_value_v2/Data/Grid/grid10_fields_sf.rds')
table(st_is_empty(grid10_fields_sf))

good_fields_dt <- grid10_soils_dt4[,.N, by = .(id_10, id_field)] %>% .[,-'N'] %>% .[,ok := 1]

grid10_fields_sf2 <- dplyr::left_join(grid10_fields_sf, good_fields_dt, by = c('id_10', 'id_field'))
grid10_fields_sf2 <- grid10_fields_sf2[!is.na(grid10_fields_sf2$ok),] %>% dplyr::select(-ok)
table(st_is_empty(grid10_fields_sf2))

#--------------------------------------------------------------------------------
#add coordinates
grid10_fields_sf2 <- cbind(grid10_fields_sf2, st_coordinates(st_centroid(grid10_fields_sf2))) %>% setnames(c('X', "Y"), c('long', 'lat'))
grid10_soils_dt4 <- merge(grid10_soils_dt4, data.table(grid10_fields_sf2) %>% .[,.(id_10, id_field, long, lat)], by = c('id_10', 'id_field'))

saveRDS(grid10_soils_dt4, "./vr_value_v2/Data/Grid/grid10_soils_dt4.rds")
saveRDS(grid10_fields_sf2, "./vr_value_v2/Data/Grid/grid10_fields_sf2.rds")

summary(grid10_horizons_v1_dt$restriction)
