# library(data.table)
library(dplyr)
library(parallel)
library(XML)

grid10_soils_dt4 <- readRDS("./vr_value_v2/Data/Grid/grid10_soils_dt4.rds")
grid10_horizons_v1_dt <- readRDS("./vr_value_v2/Data/Grid/grid10_horizons_v1_dt.rds")
grid10_fields_sf2 <- readRDS("./vr_value_v2/Data/Grid/grid10_fields_sf2.rds")

source('./vr_value_v2/Data/APssurgo_master/R/calc_apsim_variables_onesoil.R')
source('./vr_value_v2/Data/APssurgo_master/R/make_apsoils_toolbox.R')
source('./vr_value_v2/Codes/make_met_files.R')



if(server){
  directory <- paste('/home/germanm2/apsim_temp/vr_value_v2/cell', id10_n, sep = '')
}else if(cpsc){
  directory <- paste('C:/apsim_temp/', Sys.info()["nodename"],'/vr_value_v2/cell', id10_n, sep = '')
}else if(cluster){
  directory <- paste('/projects/aces/germanm2/vr_value_v2/apsim_temp/cell', id10_n, sep = '')
}

unlink(directory ,recursive=TRUE)

one_cell_dt <- grid10_soils_dt4[id_10 == id10_n,]

cell_coords <- data.table(grid10_fields_sf2[grid10_fields_sf2$id_10 == id10_n,]) %>% .[,.(X = mean(long), Y = mean(lat))]


#----------------------------------------------------------------------------
# WEATHER FILES
weather_file <- paste('./vr_value_v2/Data/met_files/weather_z_cell', id10_n, '_dt.rds', sep = '')
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
                              expand.grid(z = z_odd,
                                          mukey = sort(unique(one_cell_dt[id_field %in% c(1,3)]$mukey)),
                                          stringsAsFactors = FALSE),
                              stringsAsFactors = FALSE) 
}else{instructions1 <- data.table()}


if(any(one_cell_dt$id_field %in% c(2,4))){
  instructions2 <- data.table(id_10 = id10_n,
                              type = 'stab', 
                              expand.grid(z = z_even,
                                          mukey = sort(unique(one_cell_dt[id_field %in% c(2,4)]$mukey)),
                                          stringsAsFactors = FALSE),
                              stringsAsFactors = FALSE) 
}else{instructions2 <- data.table()}

instructions <- rbind(instructions1, instructions2) %>% setcolorder(c('id_10',  'mukey', 'z', 'type'))
if(cpsc) {instructions <- instructions[1,]}
print(instructions )
source('./vr_value_v2/Codes/apsim_create_files_nov18.R')
