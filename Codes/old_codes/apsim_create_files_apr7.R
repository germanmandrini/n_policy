######################################
# Parallelized Simulations
######################################

#===================================
# prepare clusters
#===================================

no_cores <- detectCores() - 8
cl <- makeCluster(no_cores,type='SOCK')

#===================================
# parallelized simulations 
#===================================
#i =   10

apsim_create_files <- function(i){
  #i =  150
  #--------------------------
	# preparation
	#--------------------------
	#--- load libraries ---#
  library(xml2)
  library(data.table)
  library(dplyr)
  
  instructions_tmp <- instructions[i]
  
 	#--- load the base apsim file ---# 
 
  base_doc <- xml2::read_xml("./vr_value/Data/apsim_files/vr_value_annual.apsim")
 
  #Clean the plots
  # for(x in xml2::xml_find_all(base_doc, '//Graph')[2:10]){xml2::xml_remove(x)}

  #--- edit the met module directory ---#
  met_dir <- paste(directory, 'met_files/z1.met', sep = '/')
  met_dir <- gsub("/", "\\", met_dir, fixed=TRUE)
  
  for(x in xml_find_all(base_doc,'//metfile/filename')){
    xml_text(x) <- met_dir}
  
  #--------------------------
	# CLOCK
	#--------------------------

  # for(x in xml_find_all(base_doc,'//clock/start_date')){
  #   xml_text(x) <- paste('01/01/', instructions_tmp$Year, sep = '')
  # }
  # 
  # for(x in xml_find_all(base_doc,'//clock/end_date')){
  #   xml_text(x) <- paste('31/12/', instructions_tmp$Year, sep = '')
  # }
  
  #--------------------------
  # MANAGER FOLDER
  #--------------------------

  #--- edit sowing date ---#
  # date_trial_date <- as.Date(instructions_tmp$Planting_Date, format = "%m/%d/%Y")
  # 
  # for(x in xml_find_all(base_doc,'//manager/ui/date')){
  #   xml_text(x) <- strftime(date_trial_date, format = "%d-%b")
  # }
  
  # node <- base_doc['//manager/ui/date'][[2]]
  # xmlValue(node) <- paste(instructions_tmp$Planting_Date, instructions_tmp$Year, sep = '-')

  #--- edit tillage date ---#
  #20 days before sowing
  # dates_tillage <- as.Date(instructions_tmp$Planting_Date, format = "%d-%b")-30
  # dates_tillage2 <- strftime(dates_tillage, format = "%d-%b")
  #
  # for (x in getNodeSet(base_doc,'//manager /ui/tillage_date')){
  #   xmlValue(x) <- dates_tillage2
  # }

  #--- edit plant density ---#
  # for (x in xml_find_all(base_doc,'//manager/ui/density')){
  #   xml_text(x) <- as.character(instructions_tmp$seed_m2_bin)
  # }

  #--- edit cultivar ---#
  # for (x in xml_find_all(base_doc,'//manager/ui/cultivar')){
  #   xml_text(x) <- instructions_tmp$hybrid
  # }

  #--- edit fertilizer module ---#
  # date_fert_date <- date_trial_date + 20

  # dates_fert <- strftime(dates_fert, format = "%d-%b-%Y")

  # for (x in xml_find_all(base_doc,'//manager2/ui/FertDatesStr')){
  #   xml_text(x) <- strftime(date_fert_date, format = "%d-%b")
  # }
  # 
  # for (x in xml_find_all(base_doc,'//manager2/ui/FertAmt')){
  #   xml_text(x) <- as.character(instructions_tmp$ntotal_kgha * instructions_tmp$n_efficiency) #efficiency ;)
  # }


  #--- edit irrigation ---#
  # if(instructions_tmp$Irrigation == 'Yes'){
  #   for (x in xml_find_all(base_doc,'//manager2/ui/deficit')){
  #     xml_text(x) <- '50'
  #   }
  # }
 
  #--------------------------
  # SOIL
  #--------------------------

  #SOILWAT SOILS: first two
  #--- extract soil data from the soil data base ---#
  # soils_database <- xmlParse('./Trial_crct_DIFM/Data/APssurgo_master/APSIM_soils/Soils_DIFM_bysoil.soils')
  soils_database <- xml2::read_xml(paste(directory, 'soils_vr_value.soils', sep = '/'))
  
  soil_name2 <- paste('.//Soil[@name="', tolower(instructions_tmp$mukey), '"]', sep = '')
  
  #--- replace soil ---#
  soil_temp2 <- xml_find_all(soils_database, soil_name2)
  base_doc_soil <- xml_find_all(base_doc, "//Soil")

  xml_replace(base_doc_soil, soil_temp2, .copy = TRUE)
  
  rm(soils_database, soil_temp2,base_doc_soil)
  
  
  #--- Add nodes for different NRates ---#
  base_doc_sim_node <- xml_find_all(base_doc, "//simulation")
  
  
  xml_add_sibling(.x, .value, ..., .where = c("after", "before"),
                  .copy = TRUE
  
  
  
  #--- save as a temporary xml file ---#
  folder_name <- paste(directory, '/z',instructions_tmp$z, '_', instructions_tmp$mukey, sep = '')
  
  if(file.exists(folder_name)){unlink(folder_name ,recursive=TRUE)}
  dir.create(folder_name, recursive = TRUE)
  
  filename <- 'temp.apsim'

  xml2::write_xml(base_doc, paste(folder_name,'/',filename,sep=''))
  
  apsimExe <- 'C:/Program Files (x86)/APSIM710-r4171/Model/Apsim.exe'
  simulation <- suppressWarnings(apsimr::apsim(exe=apsimExe, wd=folder_name, files=filename))
  simulation_dt <- data.table(simulation)
  
  # apsimWd <- folder_name2
  # apsimExe <- '/opt/apsim_dev/trunk/Model/ApsimModel.sh'
  # 
  # simulation <- suppressWarnings(apsimr::apsim(exe=apsimExe, wd=apsimWd, files=filename))
  # 
  
  instructions_tmp[,dir_path := folder_name2]
  
	#return(one_treat.tmp)
  return(instructions_tmp)
}


keep <- c('keep', 'apsim_create_files', 'instructions')
# # #rm(list = ls()[!ls() %in% keep])

clusterExport(cl, varlist = keep, envir=environment())

results.list <- parallel::parLapply(cl, 1:nrow(instructions), function(x) apsim_create_files(x))

instructions <- rbindlist(results.list, fill = TRUE)

stopCluster(cl)

# 
# results.list <- list()
# for(i in 1:nrow(instructions)){
#   # print(i)
#   results.list[[i]] <- apsim_create_files(i)
# }
# 
# instructions <- rbindlist(results.list, fill = TRUE)
 
# 

# Delete temporary folders created during simulations
# for(N_n in N_seq){
#   folder.name <- paste('C:/apsim_temp/', Sys.info()["nodename"],'/EONR_irr/loc_id_',LOC_id_n ,'_z_', z_n, '_N_',N_n, sep = '')
#   unlink(folder.name ,recursive=TRUE)}

       

