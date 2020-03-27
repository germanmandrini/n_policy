# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('~')
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")

source('./Codes_useful/R.libraries.R')
# source('./Codes_useful/gm_functions.R')

grid10_soils_sf4 <- readRDS("./vr_value/Data/Grid/grid10_soils_sf4.rds")  
grid10_horizons_v2_dt <- readRDS("./vr_value/Data/Grid/grid10_horizons_v2_dt.rds")
# all_locs_weather_dt <- readRDS('./vr_value/Data/met_files/all_locs_weather_dt.rds')

source('./vr_value/Data/APssurgo_master/R/calc_apsim_variables_onesoil.R')
source('./vr_value/Data/APssurgo_master/R/make_apsoils_toolbox.R')
source('./vr_value/Codes/make_met_files.R')
source('./vr_value/Codes/apsim_merge_data.R')


# all_soils <- readRDS('./Trial_crct_DIFM/Data/APssurgo_master/APSIM_soils/all_soils.rds')
# unlink("/home/germanm2/apsim_temp/trial_crct" ,recursive=TRUE)

#----------------------------------------------------------------------------
# MAKE SOME MAPS AND CHOOSE THE TILES
# grid10_tiles_sf <- readRDS("./vr_value/Data/Grid/grid10_tiles.sf5.rds") 
# tm_shape(grid10_tiles_sf) + tm_polygons("county_name")

#----------------------------------------------------------------------------
# LIST RUNNED FILES
runned <- list.files('./vr_value/Data/yc_output/', pattern = 'ic_cell')
id_10_runned <- unique(as.numeric(unlist(str_extract_all(runned, pattern = '[0-9]+'))))
          
# problems <- as.numeric(c(420, 421, 462))

id_10_seq <- sort(unique(grid10_soils_sf4$id_10))
id_10_seq <- id_10_seq[!id_10_seq %in% id_10_runned] %>% .[!. %in% problems]

# 90*10/60
# time_track <- data.table()
time_track <- readRDS('./vr_value/Data/yc_output/time_track.rds')
# time_track <- data.table()

for(id10_n in id_10_seq[2:3]){
  # id10_n = id_10_seq[25]
  print(id10_n)
  start1 <- Sys.time()
  
  #Get the directory to save the run
  server <- ifelse(Sys.info()["nodename"] == "campodonico", TRUE, FALSE)
  
  if(server){
    directory <- paste('/home/germanm2/apsim_temp/vr_value/cell', id10_n, sep = '')
  }else{
    directory <- paste('C:/apsim_temp/', Sys.info()["nodename"],'/vr_value/cell', id10_n, sep = '')
  }
  
  unlink(directory ,recursive=TRUE)
  
  one_cell_sf <- grid10_soils_sf4[grid10_soils_sf4$id_10 == id10_n,]
  
  #----------------------------------------------------------------------------
  # WEATHER FILES
  weather_cell.dt <- readRDS(paste('./vr_value/Data/met_files/weather_z_cell', id10_n, '_dt.rds', sep = ''))
  
  make_met_files_paralell(weather_cell.dt, directory)
  
  #----------------------------------------------------------------------------
  # INITIAL SOIL FILES (WE WILL UPDATE THEM AFTER STABILIZATION)
  horizons_cell_dt <- grid10_horizons_v2_dt[mukey %in% one_cell_sf$mukey,]
  horizons_cell_dt[is.na(ph), ph := 6] #a few soils didn't have ph and apsim doesn't use it
  horizons_cell2_dt <- calc_apsim_variables(horizons_cell_dt)
  
  make_apsoils_toolbox(data_soils = horizons_cell2_dt, badge_name = 'soils_vr_value', path = directory, crops = tolower(c("Maize","Soybean")))
  
  #----------------------------------------------------------------------------
  #RUN THE STABILIZATION PERIOD AND GET IC
  
  instructions <- data.table(id_10 = id10_n,
                             type = 'stab', 
                             z = 'A1',
                             expand.grid(mukey = sort(unique(horizons_cell2_dt$mukey)),
                                         rotation = c('MSM', 'SMM'), stringsAsFactors = FALSE),
                             stringsAsFactors = FALSE) %>% setcolorder(c('id_10',  'mukey', 'rotation',  'z', 'type'))
  # instructions <- instructions[1]
  
  instructions1_tmp = nrow(instructions)
  #CREATE ALL APSIM FILES
  start2 <- Sys.time()
  # source('./vr_value/Codes/apsim_create_files_apr19.R')
  source('./vr_value/Codes/apsim_create_files_May23.R')
  
  # print(paste('CREATE', round(difftime(Sys.time(), start2, units = "mins"),2), 'mins'))
  
  #RUN ALL APSIM FILES
  start3 <- Sys.time()
  source('./vr_value/Codes/apsim_run_files_apr4.R')
  # print(paste('RUN', round(difftime(Sys.time(), start3, units = "mins"),2), 'mins'))
  
  #MERGE ALL THE OUTPUT
  start4 <- Sys.time()
  apsim_merge_data(instructions, save = TRUE, directory_output= paste0('./vr_value/Data/initial_conditions/cell_', id10_n))
  
  #initial_conditions <- initial_conditions[year == 2009 & day == 365 & month == 12]
  
  
  # initial_conditions <- readRDS(paste('./vr_value/Data/initial_conditions/ic_cell', id10_n, '.rds', sep = ''))
  
  # table(initial_conditions$year)
  # table(yc_results$NRate)
  
  # Delete all files created
  for(x in instructions$dir_path){ unlink(x,recursive=TRUE)}
  
  #----------------------------------------------------------------------------
  #RUN THE YC PERIOD
  instructions <- data.table(id_10 = id10_n,
                             type = 'YC', 
                             expand.grid(z = unique(weather_cell.dt$z),
                                         mukey = sort(unique(horizons_cell2_dt$mukey)),
                                         rotation = c('MSM', 'SMM'), 
                                         N = seq(0,250,10),
                                         stringsAsFactors = FALSE),
                             stringsAsFactors = FALSE) %>% setcolorder(c('id_10',  'mukey', 'rotation',  'z', 'type'))
  # instructions <- instructions[z == 'A1'][1:40]
  
  #CREATE ALL APSIM FILES
  start5 <- Sys.time()
  source('./vr_value/Codes/apsim_create_files_May23.R')
  # print(paste('CREATE ALL', round(difftime(Sys.time(), start5, units = "mins"),2), 'mins'))
  
  #RUN ALL APSIM FILES
  start6 <- Sys.time()
  source('./vr_value/Codes/apsim_run_files_apr4.R')
  # print( paste('RUN ALL', round(difftime(Sys.time(), start6, units = "mins"),2), 'mins') )

  #MERGE ALL THE OUTPUT
  start7 <- Sys.time()
  apsim_merge_data(instructions, save = TRUE, directory_output= paste0('./vr_value/Data/yc_output/cell_', id10_n))
  
  # print(paste('MERGE ALL', round(difftime(Sys.time(), start, units = "mins"),2), 'mins'))
  # table(yc_results$year)
  # table(yc_results$NRate)
  
  # Delete all files created
  unlink(directory ,recursive=TRUE)
  
  #----------------------------------------------------------------------------
  if(FALSE){
    #COMPARE THE CONTINUOUS SIMULATION VS THE SEQUENTIAL
    # COMPARE THE YIELD
    ic <- initial_conditions[year > 2008, .(z, mukey, year, day, rotation, sim_name, fertiliser, Y_ic = Y)]
    yc <- yc_results[year > 2008,.(z, mukey, year, day, rotation, sim_name, fertiliser, Y_yc = Y)]
    
    ic2 <- ic[,.(Y_ic = max(Y_ic, na.rm = T), fertiliser = sum(fertiliser)), by = .(z, mukey, year, rotation, sim_name)]
    yc2 <- yc[,.(Y_yc = max(Y_yc, na.rm = T), fertiliser = sum(fertiliser)), by = .(z, mukey, year, rotation, sim_name)]
    
    
    
    sapply(ic2, class)
    sapply(yc2, class)
    
    paired <- merge(ic2[,-'sim_name'], yc2[sim_name == 'N150',-'sim_name'], by = c('z', 'mukey', 'year', 'rotation', 'fertiliser'), al.x = T)
    res_col_names <- c('Y_yc', 'Y_ic')
    paired[, (res_col_names) := lapply(.SD, as.numeric), .SDcols = res_col_names]
    ggplot(data=paired, aes(x = Y_ic, y = Y_yc, color = rotation)) +
      geom_point()+ theme(aspect.ratio=1) + coord_fixed() + geom_abline() + ylim(1000, 12000)+ xlim(1000, 12000) +
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
  saveRDS(time_track, './vr_value/Data/yc_output/time_track.rds')
  
  
} #end of the id10_n loop

