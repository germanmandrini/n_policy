# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('~')
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")

source('./Codes_useful/R.libraries.R')
# source('./Codes_useful/gm_functions.R')

grid10_soils_dt4 <- readRDS("./vr_value_v2/Data/Grid/grid10_soils_dt4.rds")
grid10_horizons_v1_dt <- readRDS("./vr_value_v2/Data/Grid/grid10_horizons_v1_dt.rds")
grid10_fields_sf2 <- readRDS("./vr_value_v2/Data/Grid/grid10_fields_sf2.rds")
# all_locs_weather_dt <- readRDS('./vr_value_v2/Data/met_files/all_locs_weather_dt.rds')

source('./vr_value_v2/Data/APssurgo_master/R/calc_apsim_variables_onesoil.R')
source('./vr_value_v2/Data/APssurgo_master/R/make_apsoils_toolbox.R')
source('./vr_value_v2/Codes/make_met_files.R')
source('./vr_value_v2/Codes/apsim_merge_data.R')

id_10_seq <- unique(grid10_soils_dt4$id_10)

id_10_walltime_dt <- grid10_soils_dt4[,.N, by =.(id_10, mukey)][,.N, by = id_10]
table(id_10_walltime_dt$N)
saveRDS(id_10_walltime_dt, "./vr_value_v2/Data/files_rds/id_10_walltime_dt.rds")


grid10_soils_dt4[,field_area := sum(area_ha), by = .(id_10, id_field)]

#----------------------------------------------------------------------------
# LIST RUNNED FILES

runned <- list.files('./vr_value_v2/Data/yc_output/')
id_10_runned <- unique(as.numeric(unlist(str_extract_all(runned, pattern = '[0-9]+'))))
id_10_seq <- id_10_seq[!id_10_seq %in% id_10_runned]
#           
#  id_10_seq <- sort(unique(grid10_soils_sf4$id_10))
# id_10_seq <- id_10_seq[!id_10_seq %in% id_10_runned] %>% .[!id_10_seq %in% problems]
# id_10_seq <- sample(id_10_seq)

# 
time_track <- data.table()
# time_track <- readRDS('./vr_value_v2/Data/yc_output/time_track.rds')

for(id10_n in id_10_seq){
  # id10_n = id_10_seq[525]
  print(id10_n)
  start1 <- Sys.time()
  
  #Get the directory to save the run
  server <- ifelse(Sys.info()["nodename"] == "campodonico", TRUE, FALSE)
  
  if(server){
    directory <- paste('/home/germanm2/apsim_temp/vr_value_v2/cell', id10_n, sep = '')
  }else{
    directory <- paste('C:/apsim_temp/', Sys.info()["nodename"],'/vr_value_v2/cell', id10_n, sep = '')
  }
  
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
  
  # instructions <- instructions[1]
  
  instructions1_tmp = nrow(instructions)
  
  #CREATE ALL APSIM FILES
  start2 <- Sys.time()
  source('./vr_value_v2/Codes/apsim_create_files_May25.R')
  
  # print(paste('CREATE', round(difftime(Sys.time(), start2, units = "mins"),2), 'mins'))
  
  #RUN ALL APSIM FILES
  start3 <- Sys.time()
  source('./vr_value_v2/Codes/apsim_run_files_apr4.R')
  
  # print(paste('RUN', round(difftime(Sys.time(), start3, units = "mins"),2), 'mins'))
  
  #MERGE ALL THE OUTPUT
  start4 <- Sys.time()
  apsim_merge_data(instructions, directory_output= paste0('./vr_value_v2/Data/initial_conditions/cell_', id10_n))

  # Delete all files created
  for(x in instructions$dir_path){ unlink(x,recursive=TRUE)}
  
  #----------------------------------------------------------------------------
  #RUN THE YC PERIOD
  instructions[,type := 'YC']
  instructions[, dir_path := NULL]
  # instructions <- instructions[z == 'A1'][1]
  
  #CREATE ALL APSIM FILES
  start5 <- Sys.time()
  source('./vr_value_v2/Codes/apsim_create_files_May25.R')
  
  # print(paste('CREATE ALL', round(difftime(Sys.time(), start5, units = "mins"),2), 'mins'))
  
  #RUN ALL APSIM FILES
  start6 <- Sys.time()
  source('./vr_value_v2/Codes/apsim_run_files_apr4.R')
  
  # print( paste('RUN ALL', round(difftime(Sys.time(), start6, units = "mins"),2), 'mins') )

  #MERGE ALL THE OUTPUT
  start7 <- Sys.time()
  apsim_merge_data(instructions, directory_output= paste0('./vr_value_v2/Data/yc_output/cell_', id10_n))
  
  # print(paste('MERGE ALL', round(difftime(Sys.time(), start, units = "mins"),2), 'mins'))
  # table(yc_results$year)
  # table(yc_results$NRate)
  
  # Delete all files created
  unlink(directory ,recursive=TRUE)
  
  #----------------------------------------------------------------------------
  if(FALSE){
    #COMPARE THE CONTINUOUS SIMULATION VS THE SEQUENTIAL
    # COMPARE THE YIELD
    ic_files <- list.files(paste0('./vr_value_v2/Data/initial_conditions/cell_', id10_n), full.names = TRUE)
    ic_dt <- data.table()
    for(file_n in ic_files){
      ic_dt <- rbind(ic_dt, readRDS(file_n))}
    
    yc_files <- list.files(paste0('./vr_value_v2/Data/yc_output/cell_', id10_n), full.names = TRUE)
    yc_dt <- data.table()
    for(file_n in yc_files){
      yc_dt <- rbind(yc_dt, readRDS(file_n))}
    
    sim_name_n <- unique(yc_dt$sim_name)[str_detect(unique(yc_dt$sim_name), pattern = '_150')]
    ic_dt2 <- ic_dt[year %in% c(2009, 2010), .(z, mukey, year, day, sim_name, fertiliser, Y_ic = Y)]
    yc_dt2 <- yc_dt[year %in% c(2009, 2010) & sim_name %in% sim_name_n,.(z, mukey, year, day, sim_name, fertiliser, Y_yc = Y)]
    
    ic_dt2 <- ic_dt2[,.(Y_ic = max(Y_ic, na.rm = T), fertiliser = sum(fertiliser)), by = .(z, mukey, year, sim_name)]
    yc_dt2 <- yc_dt2[,.(Y_yc = max(Y_yc, na.rm = T), fertiliser = sum(fertiliser)), by = .(z, mukey, year, sim_name)]
    
    paired <- merge(ic_dt2[,-'sim_name'], yc_dt2[,-'sim_name'], by = c('z', 'mukey', 'year', 'fertiliser'), al.x = T)
    res_col_names <- c('Y_yc', 'Y_ic')
    paired[, (res_col_names) := lapply(.SD, as.numeric), .SDcols = res_col_names]
    
    ggplot(data=paired, aes(x = Y_ic, y = Y_yc)) +
      geom_point()+ theme(aspect.ratio=1) + coord_fixed() + geom_abline() + ylim(8000, 12000)+ xlim(8000, 12000) +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))+
      ggtitle('Sequential vs Continuous') +
      theme_bw()
    
    paired[,Y_diff := abs(Y_ic - Y_yc)][order(-Y_diff)]
    # CHECK THE IC
    fc <- initial_conditions[year == 2008 & day == 366][1]
    ic <- yc_results[year == 2009 & day == 1][2]
    
    rbind(fc, initial_conditions_tmp)
    
    # SHOW YIELD CURVES
    yc <- yc2[year == 2010]
    yc[,mukey_rotation := paste(mukey, rotation, sep = '_')]
    
    ggplot(data=yc, aes(x = fertiliser, y = Y_yc, color = mukey_rotation)) +
      geom_point(size = 3) +
      stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE) +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))+
      ggtitle(paste('Summarized Response by mukey'))
  }#end of the graph part
  start8 <- Sys.time()
  time_track_tmp <- data.table(id_10 = id10_n,
                               time = start1,
                               inst1 = instructions1_tmp,
                               build1 = as.numeric(difftime(start2, start1, units = "mins")),
                               create1 = as.numeric(difftime(start3, start2, units = "mins")),
                               run1 = as.numeric(difftime(start4, start3, units = "mins")),
                               merge_save1 = as.numeric(difftime(start5, start4, units = "mins")),
                               inst2 = nrow(instructions),
                               create2 = as.numeric(difftime(start6, start5, units = "mins")),
                               run2 = as.numeric(difftime(start7, start6, units = "mins")),
                               merge_save2 = as.numeric(difftime(start8, start7, units = "mins")),
                               cell = as.numeric(difftime(start8, start1, units = "mins")))
  print(time_track_tmp)
  
  time_track <- rbind(time_track, time_track_tmp)
  saveRDS(time_track, './vr_value_v2/Data/yc_output/time_track.rds')
  
  
} #end of the id10_n loop

