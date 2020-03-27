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
#i =   10

apsim_create_files <- function(i){
  #i = 10
  #--------------------------
	# preparation
	#--------------------------
	#--- load libraries ---#
  library(xml2)
  library(data.table)
  library(dplyr)
  
  instructions_tmp <- instructions[i]
  
 	#--- load the base apsim file ---# 
 
  base_doc <- xml2::read_xml("./vr_value/Data/apsim_files/vr_value_v6.apsim")
 
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
  year_start <- ifelse(instructions_tmp$type == 'stab', 2001, 2009)
  year_end <- ifelse(instructions_tmp$type == 'stab', 2010, 2010) #should be 2009. This is to compare cont vs seq
                   
  node <- xml_find_all(base_doc,'//clock/start_date')
  xml_text(node) <- paste('01/01/',year_start, sep = '')
  
  node <-  xml_find_all(base_doc,'//clock/end_date')
  xml_text(node) <- paste('31/12/',year_end, sep = '')
  
  #--------------------------
  # ROTATION
  #--------------------------
  if(instructions_tmp$type == 'stab'){
    if(instructions_tmp$rotation == 'SMM'){ 
      crop_seq <- c('maize', 'soybean', 'maize', 'maize', 'soybean', 'maize', 'maize', 'soybean', 'maize', 'maize')
      } else { 
      crop_seq <- c('maize', 'maize', 'soybean', 'maize', 'maize', 'soybean', 'maize', 'maize', 'soybean', 'maize')}
  }else{
    if(instructions_tmp$rotation == 'SMM'){ 
      crop_seq <- c('maize', 'maize', 'nil', 'nil', 'nil', 'nil', 'nil', 'nil', 'nil', 'nil')
    } else { 
      crop_seq <- c('soybean', 'maize', 'nil', 'nil', 'nil', 'nil', 'nil', 'nil', 'nil', 'nil')}
  }
    
  for(crop_n in 1:10){
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
    source('./vr_value/Codes/update_ic_May20.R')
    base_doc <- update_ic(base_doc, instructions_tmp)
  }
  
  #--- Set the rate for the stab period ---#
    x <- xml_find_all(base_doc, ".//manager/ui/fert_amount_stab")
    xml_text(x) <- "150"

  
  
  #--- Add nodes for different NRates ---#
  if(instructions_tmp$type == 'YC'){
  
    base_doc_sim_node <-  xml_find_all(base_doc, ".//simulation[@name=\"MS\"]")
    
    N_rates <- seq(0,250,10)
    for(N_n in N_rates){
      # N_n = 60
      xml_add_sibling(base_doc_sim_node, base_doc_sim_node, where = "before")
      new_node <- xml_find_all(base_doc, ".//simulation[@name=\"MS\"]")[2]
      
      #----------------------------------
      #Change the name of the simulation
      sim_name <- paste('N', N_n, sep = '')
      xml_attr(new_node, 'name') <- sim_name
      
      #----------------------------------
      #Change the name of the outputfile
      x <- xml_find_all(new_node, ".//outputfile/filename")
      xml_text(x) <- as.character(paste(sim_name, '.out', sep = ''))
      
      #----------------------------------
      #Change the rate
      x <- xml_find_all(base_doc, ".//manager/ui/fert_amount_yc")[2]
      xml_text(x) <- as.character(N_n)
      
    }#end N_n loop  
    
    xml2::xml_remove(base_doc_sim_node)

  }
  #--- save as a temporary xml file ---#
  folder_name <- paste0(directory, '/',paste(instructions_tmp, collapse = '_'))
  
  if(file.exists(folder_name)){unlink(folder_name ,recursive=TRUE) }
  dir.create(folder_name, recursive = TRUE)
  
  filename <- 'temp.apsim'
  
  xml2::write_xml(base_doc, paste(folder_name,'/',filename,sep=''))
  
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

       

