#===================================
# prepare clusters
#===================================


no_cores <- detectCores() - 1
cl <- makeCluster(no_cores,type='SOCK')

#===================================
# parallelized simulations 
#===================================




# install.packages('APSIM')

#install.packages("daymetr")

#setwd('C:/Users/germa/Box Sync/My_Documents') #Dell
#setwd("C:/Users/germanm2/Box Sync/My_Documents") #CPSC
#setwd("~") #server
#source('./Codes_useful/R.libraries.R')

weather_historic_dt <- readRDS('./vr_value/Data/met_files/weather_historic_dt.rds')

create_z <- function(id_10_n, weather_historic_dt){
  # id_10_n = 730
  print(id_10_n)
  
  packages_need <- c('APSIM', 'daymetr','dplyr', 'data.table', 'sf')
  lapply(packages_need, require, character.only = TRUE)
  
  
  daymet.dt <- weather_historic_dt[id_10 == id_10_n]
  
  # setcolorder(daymet.dt, c('year', 'day', 'radn', 'maxt', 'mint', 'rain', 'dayl'))
  
  #------------------------------------------------------------------
  # CREATE THE Za
  daymet_z_list <- list()
  for(z1_n in LETTERS[1:5]){
    # z1_n = 'A'
    #print(z_n)
    years_seq <- sample(1980:2018, 9, replace = FALSE)
    daymet_z1_dt <- data.table()
    year_counter = 2001 #2010 is the year of the yield curve
    for(year_n in years_seq){
      # year_n = years_seq[2]
      daymet_tmp <- daymet.dt[year == year_n]
      daymet_tmp[,year := year_counter]
      daymet_z1_dt <- rbind(daymet_z1_dt, daymet_tmp)
      year_counter = year_counter + 1
    } #end of year_n loop
    for(z2_n in 1:4){
      # z2_n = 1
      year_n <- sample(1980:2018, 1, replace = FALSE)
      daymet_tmp <- daymet.dt[year == year_n]
      daymet_tmp[,year := 2010]
      daymet_z2_dt <- rbind(daymet_z1_dt, daymet_tmp)
      
      daymet_z2_dt[,z:=paste(z1_n, z2_n, sep = "")]
      daymet_z2_dt[,z1:=z1_n]
      daymet_z2_dt[,z2:=z2_n]
      
      daymet_z_list[[paste(z1_n, z2_n, sep = "")]] <- daymet_z2_dt
    }#end of z2_n loop

  }#end of z_n loop
  daymet_z_cell <- rbindlist(daymet_z_list)
  #------------------------------------------------------------------
  #CORRECT LEAP YEAR
  # The Daymet calendar is based on a standard calendar year. All Daymet years have 1 - 365 days, including leap years. 
  # For leap years, the Daymet database includes leap day. Values for December 31 are discarded from leap years to maintain a 365-day year.
  leap_years <- seq(1980,2019, by = 4)
  correction_leap.dt <- daymet_z_cell[year %in% leap_years & day %in% 59:60]
  correction_leap.dt <- correction_leap.dt[,.(day = mean(day),
                                              radn = mean(radn),
                                              maxt = mean(maxt),
                                              mint = mean(mint),
                                              rain = mean(rain),
                                              dayl = mean(dayl)), by = .(year, lat,lon, id_10, z, z1, z2)]
  nrow(correction_leap.dt)
  
  daymet_z_cell2 <- rbind(daymet_z_cell, correction_leap.dt) %>% .[order(year, day)]
  daymet_z_cell2[, day2 := seq_len(.N), by = .(year, lat,lon, id_10, z, z1, z2)]
  daymet_z_cell2[,day := NULL]
  setnames(daymet_z_cell2, 'day2', 'day')
  
  daymet_z_cell2[,.N, by = .(year, z)][,.(mean(N)), by = year]
  
  
  #------------------------------------------------------------------
  # hist(daymet_z_cell[,.(rain = sum(rain)), by = .(z, year)][,rain]) #check. They have to be different
  
  saveRDS(daymet_z_cell2, paste('./vr_value/Data/met_files/weather_cell', id_10_n, '_dt.rds', sep = '')) #save each id, it gets to heavy if all together
  
  return(daymet_z_cell2)
}

keep <- c('keep', 'create_z', 'weather_historic_dt')

clusterExport(cl, varlist = keep, envir=environment())

# ids_10_seq <- sort(unique(grid10_tiles_sf$id_10))
grid10_tiles_sf <- readRDS("./vr_value/Data/Grid/grid10_tiles.sf5.rds") 
id_10_seq <- sort(unique(weather_historic_dt$id_10))

# daymet_z_cell <- create_z(ids_10_seq[100], grid10_tiles_sf)
seed(123)
results_list <- parLapply(cl, id_10_seq, function(x) create_z(x, weather_historic_dt))

# results_list <- list()
# for(id_10_n in ids_10_seq){
#   results_list[[length(results_list)+1]] <- create_z(id_10_n, grid10_tiles_sf)
# }

all_locs_weather_dt <-  rbindlist(results_list)

saveRDS(all_locs_weather_dt, './vr_value/Data/met_files/weather_z_dt.rds')

stopCluster(cl)

