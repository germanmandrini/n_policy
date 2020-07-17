# library(data.table)
library(dplyr)
library(parallel)
library(XML)

grid10_soils_dt4 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt4.rds")

grid10_horizons_v1_dt <- readRDS("./n_policy_box/Data/Grid/grid10_horizons_v1_dt.rds")
grid10_horizons_v1_dt <- grid10_horizons_v1_dt[bottom <=200] #make soils to only 150 cm
grid10_fields_sf2 <- readRDS("./n_policy_box/Data/Grid/grid10_fields_sf2.rds")

if(FALSE){ #test if regions are correct
  regions1 <- unique(grid10_soils_dt4[,.(id_10, region)])
  regions2 <- data.table(grid10_fields_sf2) %>% .[,.(id_10, region)] %>% unique()
  comp_dt <- merge(regions1, regions2, by = 'id_10')
  comp_dt[region.x != region.y]
  grid10_tiles_sf6 <- readRDS("./n_policy_box/Data/Grid/grid10_tiles_sf6.rds")
  tm_shape(grid10_tiles_sf6)+ tm_polygons('region')
  regions3 <- data.table(grid10_tiles_sf6) %>% .[,.(id_10, region)] %>% unique()
  comp_dt <- merge(comp_dt, regions3, by = 'id_10')
  comp_dt[region.x != region]
}

source('./n_policy_box/Data/APssurgo_master/R/calc_apsim_variables_onesoil.R')
source('./n_policy_box/Data/APssurgo_master/R/make_apsoils_toolbox.R')
source(paste0(codes_folder, '/n_policy_git/Codes/make_met_files_foreach.R'))
"C:/Users/germanm2/Documents/n_policy_git/Codes/make_met_files.R"
"./n_policy_git/Codes/make_met_files.R"

list.files('./n_policy_box/Data/APssurgo_master/')

if(server){
  directory <- paste('/home/germanm2/apsim_temp/n_policy/batch_', batch_n, '/cell', id10_n, sep = '')
}else if(cpsc){
  directory <- paste('C:/apsim_temp/', Sys.info()["nodename"],'/n_policy/batch_', batch_n, '/cell', id10_n, sep = '')
}else if(cluster){
  directory <- paste('/projects/aces/germanm2/n_policy/batch_', batch_n, '/cell', id10_n, sep = '')
}

unlink(directory ,recursive=TRUE)

one_cell_dt <- grid10_soils_dt4[id_10 == id10_n,]

#----------------------------------------------------------------------------
#OJO!!!
#Select largest mukey by field
if(server){
  one_cell_dt <- one_cell_dt[,.SD[prop_area == max(prop_area)], by = id_field]
}
#----------------------------------------------------------------------------

cell_coords <- data.table(grid10_fields_sf2[grid10_fields_sf2$id_10 == id10_n,]) %>% .[,.(X = mean(long), Y = mean(lat))]

#----------------------------------------------------------------------------
# WEATHER FILES
weather_file <- paste('./n_policy_box/Data/met_files/weather_z_cell', id10_n, '_dt.rds', sep = '')
weather_cell.dt <- readRDS(weather_file)

make_met_files_paralell(weather_cell.dt, directory)

#----------------------------------------------------------------------------
# INITIAL SOIL FILES (WE WILL UPDATE THEM AFTER STABILIZATION)
horizons_cell_dt <- grid10_horizons_v1_dt[mukey %in% one_cell_dt$mukey,]
horizons_cell_dt[is.na(ph), ph := 6] #a few soils didn't have ph and apsim doesn't use it
horizons_cell2_dt <- calc_apsim_variables(horizons_cell_dt)
horizons_cell2_dt <- cbind(horizons_cell2_dt,cell_coords)

make_apsoils_toolbox(data_soils = horizons_cell2_dt, badge_name = 'soils_vr_value', path = directory, crops = tolower(c("Maize","Soybean")))

#----------------------------------------------------------------------------
# CREATE THE INSTRUCTIONS FOR THE STABILIZATION PERIOD 
is_even <- function(x) x %% 2 == 0
z_seq <- unique(weather_cell.dt$z)
z_even = z_seq[is_even(z_seq)]
z_odd = z_seq[!is_even(z_seq)]

if(any(one_cell_dt$id_field %in% c(1,3))){
  instructions1 <- data.table(id_10 = id10_n,
                              type = 'stab', 
                              region = one_cell_dt$region[1],
                              expand.grid(z = z_odd,
                                          mukey = sort(unique(one_cell_dt[id_field %in% c(1,3)]$mukey)),
                                          stringsAsFactors = FALSE),
                              stringsAsFactors = FALSE) 
}else{instructions1 <- data.table()}


if(any(one_cell_dt$id_field %in% c(2,4))){
  instructions2 <- data.table(id_10 = id10_n,
                              type = 'stab', 
                              region = one_cell_dt$region[1],
                              expand.grid(z = z_even,
                                          mukey = sort(unique(one_cell_dt[id_field %in% c(2,4)]$mukey)),
                                          stringsAsFactors = FALSE),
                              stringsAsFactors = FALSE) 
}else{instructions2 <- data.table()}

instructions <- rbind(instructions1, instructions2) %>% setcolorder(c('id_10',  'mukey', 'z', 'type'))
instructions[,batch := batch_n]
if(test_small) {instructions <- instructions[1,]}
print(instructions )
"C:/Users/germanm2/Documents/n_policy_git/Codes/apsim_create_files_jul15.R"
"./n_policy_git/Codes/apsim_create_files_jul15.R"
source(paste0(codes_folder, '/n_policy_git/Codes/apsim_create_files_jul15.R'))
