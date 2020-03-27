######################################
# Parallelized Simulations
######################################

#===================================
# prepare clusters
#===================================

no_cores <- detectCores() * 7/8

cl <- makeCluster(no_cores,type='SOCK')

#===================================
# parallelized simulations 
#===================================
#i =   2

apsim_create_files <- function(i){
  # i = 27
  #--------------------------
	# preparation
	#--------------------------
	#--- load libraries ---#
  library(xml2)
  library(data.table)
  library(dplyr)
  
  instructions_tmp <- instructions[i]
  sim_name <- paste(unlist(instructions_tmp[,.(id_10, mukey, z, type)]), collapse = '_')
 	
  #--- load the base apsim file ---# 
 
  base_doc <- xml2::read_xml("./vr_value_v2/Data/apsim_files/vr_value_v6.apsim")
 
  #Clean the plots
  # for(x in xml2::xml_find_all(base_doc, '//Graph')[2:10]){xml2::xml_remove(x)}

  #--- edit the met module directory ---#
  met_dir <- paste(directory, '/met_files/z_',instructions_tmp$z,'.met', sep = '')
  met_dir <- gsub("/", "\\", met_dir, fixed=TRUE)
  
  node <-  xml_find_all(base_doc,'//metfile/filename')
  xml_text(node) <- met_dir
  
  #--------------------------
  # CLOCK
  #--------------------------
  year_start <- ifelse(instructions_tmp$type == 'stab', 2001, 2010)
  date_end <- ifelse(instructions_tmp$type == 'stab', '31/12/2010', '30/04/2011') #should be 2009. This is to compare cont vs seq
                   
  node <- xml_find_all(base_doc,'//clock/start_date')
  xml_text(node) <- paste('01/01/',year_start, sep = '')
  
  node <-  xml_find_all(base_doc,'//clock/end_date')
  xml_text(node) <- date_end
  
  #--------------------------
  # ROTATION
  #--------------------------
  if(instructions_tmp$type == 'stab'){
       crop_seq <- c('soybean', 'maize', 'soybean', 'maize', 'soybean', 'maize', 'soybean', 'maize', 'soybean', 'maize', 'soybean')
  }else{
      crop_seq <- c('maize', 'soybean', 'nil', 'nil', 'nil', 'nil', 'nil', 'nil', 'nil',  'nil',  'nil')
    } 
    
  for(crop_n in 1:11){
   # crop_n = 1
   node <- xml_find_all(base_doc, paste0('//manager/ui/crop', crop_n))
   xml_text(node) <- crop_seq[crop_n]
   }
  
  #--- extract soil data from the soil data base ---#
  # soils_database <- xmlParse('./Trial_crct_DIFM/Data/APssurgo_master/APSIM_soils/Soils_DIFM_bysoil.soils')
  soils_database <- xml2::read_xml(paste(directory, 'soils_vr_value.soils', sep = '/'))
  
  soil_name2 <- paste('.//Soil[@name="', tolower(instructions_tmp$mukey), '"]', sep = '')
  
  #--- replace soil ---#
  soil_temp2 <- xml_find_all(soils_database, soil_name2)
  base_doc_soil <- xml_find_all(base_doc, "//Soil")

  xml_replace(base_doc_soil, soil_temp2, .copy = TRUE)
  
  rm(soils_database, soil_temp2,base_doc_soil)
  
  if(instructions_tmp$type == 'YC'){
    source('./vr_value_v2/Codes/update_ic_May25.R')
    base_doc <- update_ic(base_doc, instructions_tmp)
  }
  
  #--- Set the rate for the stab period ---#
  x <- xml_find_all(base_doc, ".//manager/ui/fert_amount_stab")
  xml_text(x) <- "150"

  #--- CREATE A FOLDER TO SAVE FILES ---#
  folder_name <- paste0(directory, '/', sim_name)
  
  if(file.exists(folder_name)){unlink(folder_name ,recursive=TRUE) }
  dir.create(folder_name, recursive = TRUE)
  
  #--- Set the rate for the YC period ---#
  if(instructions_tmp$type == 'YC'){
    N_rates <- seq(0, 320, 10)
    for(N_n in N_rates){
      # N_n = 30
      sim_name_n <- paste(sim_name, N_n, sep = '_')
      
      #----------------------------------
      #Change the name of the simulation
      x <-  xml_find_all(base_doc, ".//simulation")
      xml2::xml_attrs(x, 'name')[[1]] <-  c(name = sim_name_n)
    
      #----------------------------------
      #Change the name of the outputfile
      x <- xml_find_all(base_doc, ".//outputfile/filename")
      xml_text(x) <- as.character(paste(sim_name_n, '.out', sep = ''))
      
      #----------------------------------
      #Change the rate
      x <- xml_find_all(base_doc, ".//manager/ui/fert_amount_yc")
      xml_text(x) <- as.character(N_n)
      
   
      #--- save as a temporary xml file ---#
      folder_name_n <- paste(folder_name, N_n, sep = '/')
      dir.create(folder_name_n, recursive = TRUE)
      filename <- paste0(sim_name_n, '.apsim')
      
      xml2::write_xml(base_doc, paste(folder_name_n,'/',filename,sep=''))
    }
  }else{
    #----------------------------------
    #Change the name of the simulation
    x <-  xml_find_all(base_doc, ".//simulation")
    xml2::xml_attrs(x, 'name')[[1]] <-  c(name = sim_name)
    
    #----------------------------------
    #Change the name of the outputfile
    x <- xml_find_all(base_doc, ".//outputfile/filename")
    xml_text(x) <- as.character(paste(sim_name, '.out', sep = ''))
    
    #--- save as a temporary xml file ---#
    filename <- paste0(sim_name, '.apsim')
    xml2::write_xml(base_doc, paste(folder_name,'/',filename,sep=''))
    
  }
  # apsimExe <- 'C:/Program Files (x86)/APSIM710-r4171/Model/Apsim.exe'
  # simulation <- apsimr::apsim(exe=apsimExe, wd=folder_name, files=filename)
  # simulation_dt <- data.table(simulation)
  # simulation <- apsim(exe=apsimExe, wd=folder_name, files=filename)
  
  # apsimWd <- folder_name2
  # apsimExe <- '/opt/apsim_dev/trunk/Model/ApsimModel.sh'
  # 
  # simulation <- suppressWarnings(apsimr::apsim(exe=apsimExe, wd=apsimWd, files=filename))
  # 
  
  instructions_tmp[,dir_path := folder_name]
  # initial_conditions[mukey == instructions_tmp$mukey & z == instructions_tmp$z & year == 2010, Y_dry]
	#return(one_treat.tmp)
  return(instructions_tmp)
}


keep <- c('keep', 'apsim_create_files', 'instructions', 'directory')
# if(unique(instructions$type) == 'YC'){ keep <- append(keep, 'initial_conditions' )}
# # #rm(list = ls()[!ls() %in% keep])

clusterExport(cl, varlist = keep, envir=environment())


results.list <- parallel::parLapply(cl, 1:nrow(instructions), function(x) apsim_create_files(x))

instructions <- rbindlist(results.list, fill = TRUE)

stopCluster(cl)


# apsim_create_files(1)
# results.list <- list()
# 
# for(i in 1:nrow(instructions)){
#   # print(i)
#   results.list[[i]] <- apsim_create_files(i)
# }

# instructions <- rbindlist(results.list, fill = TRUE)

# 

# Delete temporary folders created during simulations
# for(N_n in N_seq){
#   folder.name <- paste('C:/apsim_temp/', Sys.info()["nodename"],'/EONR_irr/loc_id_',LOC_id_n ,'_z_', z_n, '_N_',N_n, sep = '')
#   unlink(folder.name ,recursive=TRUE)}

       

