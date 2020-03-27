cpsc <- FALSE

setwd('/projects/aces/germanm2/')
if(cpsc){setwd('C:/Users/germanm2/Box Sync/My_Documents')}#CPSC
  
library(data.table)
library(dplyr)
library(parallel)
library(XML)


grid10_soils_dt4 <- readRDS("./vr_value_v2/Data/Grid/grid10_soils_dt4.rds")
grid10_horizons_v1_dt <- readRDS("./vr_value_v2/Data/Grid/grid10_horizons_v1_dt.rds")
grid10_fields_sf2 <- readRDS("./vr_value_v2/Data/Grid/grid10_fields_sf2.rds")

source('./vr_value_v2/Data/APssurgo_master/R/calc_apsim_variables_onesoil.R')
source('./vr_value_v2/Data/APssurgo_master/R/make_apsoils_toolbox.R')
source('./vr_value_v2/Codes/make_met_files.R')
source('./vr_value_v2/Codes/apsim_merge_data.R')

# id_10_seq <- unique(grid10_soils_dt4$id_10)

# write.csv(id_10_seq, './vr_value_v2/Codes/id_10_out.csv')

# grid10_soils_dt4[,field_area := sum(area_ha), by = .(id_10, id_field)]

#----------------------------------------------------------------------------
# LIST RUNNED FILES

# runned <- list.files('./vr_value_v2/Data/yc_output/')
# id_10_runned <- unique(as.numeric(unlist(str_extract_all(runned, pattern = '[0-9]+'))))
#           
# id_10_seq <- sort(unique(grid10_soils_sf4$id_10))
# id_10_seq <- id_10_seq[!id_10_seq %in% id_10_runned] %>% .[!id_10_seq %in% problems]
# id_10_seq <- sample(id_10_seq)

# time_track <- readRDS('./vr_value_v2/Data/yc_output/time_track.rds')

# for(id10_n in id_10_seq[1]){
  # id10_n = 5#id_10_seq[2]
id10_n = as.numeric(commandArgs(trailingOnly=TRUE)[1])
if(cpsc){id10_n = 5}
print(id10_n)
start1 <- Sys.time()

#Get the directory to save the run

# 

directory <- paste('/projects/aces/germanm2/vr_value_v2/apsim_temp/cell', id10_n, sep = '')
if(cpsc){directory <- paste('C:/apsim_temp/', Sys.info()["nodename"],'/vr_value_v2/cell', id10_n, sep = '')} #CPSC}

unlink(directory ,recursive=TRUE)

one_cell_dt <- grid10_soils_dt4[id_10 == id10_n,]

cell_coords <- data.table(grid10_fields_sf2[grid10_fields_sf2$id_10 == id10_n,]) %>% .[,.(X = mean(long), Y = mean(lat))]


#----------------------------------------------------------------------------
# WEATHER FILES
weather_cell.dt <- readRDS(paste('./vr_value_v2/Data/met_files/weather_z_cell', id10_n, '_dt.rds', sep = ''))

make_met_files_paralell(weather_cell.dt, directory)

#----------------------------------------------------------------------------
# INITIAL SOIL FILES (WE WILL UPDATE THEM AFTER STABILIZATION)
horizons_cell_dt <- grid10_horizons_v1_dt[mukey %in% one_cell_dt$mukey,]
horizons_cell_dt[is.na(ph), ph := 6] #a few soils didn't have ph and apsim doesn't use it
horizons_cell2_dt <- calc_apsim_variables(horizons_cell_dt)
horizons_cell2_dt <- cbind(horizons_cell2_dt,cell_coords)

make_apsoils_toolbox(data_soils = horizons_cell2_dt, badge_name = 'soils_vr_value', path = directory, crops = tolower(c("Maize","Soybean")))

#----------------------------------------------------------------------------
#RUN THE STABILIZATION PERIOD AND GET IC

instructions <- data.table(id_10 = id10_n,
                           type = 'stab', 
                           expand.grid(z = unique(weather_cell.dt$z),
                                       mukey = sort(unique(horizons_cell2_dt$mukey)),
                                       stringsAsFactors = FALSE),
                           stringsAsFactors = FALSE) %>% setcolorder(c('id_10',  'mukey', 'z', 'type'))
print(instructions)
instructions1_tmp = nrow(instructions)

#CREATE ALL APSIM FILES
start2 <- Sys.time()
source('./vr_value_v2/Codes/apsim_create_files_nov18.R')
