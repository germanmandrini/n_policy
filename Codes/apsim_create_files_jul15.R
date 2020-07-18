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
#i =   1

apsim_create_files <- function(i){
  # i = 1
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
  if(instructions_tmp$type == 'stab'){
    base_doc <- xml2::read_xml("./n_policy_box/Data/apsim_files/vr_value_v8.apsim")
    }else{
    base_doc <- xml2::read_xml(instructions_tmp$path)
    # unlink(dirname(instructions_tmp$path), recursive = TRUE)
  }
 
  #Clean the plots
  # for(x in xml2::xml_find_all(base_doc, '//Graph')[2:10]){xml2::xml_remove(x)}

  #--- edit the met directory ---#
  if(instructions_tmp$type == 'stab'){
    met_dir <- paste(directory, '/met_files/z_',instructions_tmp$z,'.met', sep = '')
    met_dir <- gsub("/", "\\", met_dir, fixed=TRUE)
    
    node <-  xml_find_all(base_doc,'//metfile/filename')
    xml_text(node) <- met_dir
  }
  
  #--- edit modules directory ---#
  if(instructions_tmp$type == 'stab'){
    #SurgaceOM Module
    module_dir <- paste0(getwd(), '/n_policy_box/Data/apsim_files/modules_edited/SurfaceOM_v711.xml')
    module_dir <- gsub("/", "\\", module_dir, fixed=TRUE)
    node <-  xml_find_all(base_doc,'//surfaceom/ini/filename')
    xml_text(node) <- module_dir
    
    #Maize Module
    module_dir <- paste0(getwd(), '/n_policy_box/Data/apsim_files/modules_edited/Maize_v711.xml')
    module_dir <- gsub("/", "\\", module_dir, fixed=TRUE)
    node <-  xml_find_all(base_doc,'//maize/ini/filename')
    xml_text(node) <- module_dir
    
    #Soybean Module
    module_dir <- paste0(getwd(), '/n_policy_box/Data/apsim_files/modules_edited/Soybean_v711.xml')
    module_dir <- gsub("/", "\\", module_dir, fixed=TRUE)
    node <-  xml_find_all(base_doc,'//soybean/ini/filename')
    xml_text(node) <- module_dir
    
    #--------------------------
    # PLANTING DATE BY REGION
    #--------------------------
    # planting_start_corn <- c('01-Apr', '05-Apr', '10-Apr')[instructions_tmp$region]
    # planting_end_corn <- c('05-Apr', '10-Apr', '15-Apr')[instructions_tmp$region]
    
    # planting_start_corn <- c('20-Mar', '27-Mar', '05-Apr')[instructions_tmp$region]
    # planting_end_corn <- c('02-Apr', '08-Apr', '15-Apr')[instructions_tmp$region]
    
    # planting_start_corn <- c('01-Apr', '05-Apr', '20-Apr')[instructions_tmp$region]
    # planting_end_corn <- c('05-Apr', '10-Apr', '25-Apr')[instructions_tmp$region]
    
    # planting_start_corn <- c('01-Apr', '03-Apr', '20-Apr')[instructions_tmp$region]
    # planting_end_corn <- c('08-Apr', '10-Apr', '25-Apr')[instructions_tmp$region]
    
    #batch 20 
    # planting_start_corn <- c('05-Apr', '10-Apr', '15-Apr')[instructions_tmp$region]
    # planting_end_corn <- c('10-Apr', '15-Apr', '20-Apr')[instructions_tmp$region] 
    
    #batch 21 
    planting_start_corn <- c('05-Apr', '05-Apr', '15-Apr')[instructions_tmp$region]
    planting_end_corn <- c('10-Apr', '10-Apr', '20-Apr')[instructions_tmp$region]
    
    x <- xml_find_all(base_doc, ".//manager/ui/date1_corn")
    xml_text(x) <- as.character(planting_start_corn)
    
    x <- xml_find_all(base_doc, ".//manager/ui/date2_corn")
    xml_text(x) <- as.character(planting_end_corn)
    
    # planting_start_soy <- c('15-Apr', '20-Apr', '24-Apr')[instructions_tmp$region]
    # planting_end_soy <- c('20-Apr', '24-Apr', '30-Apr')[instructions_tmp$region]
    
    planting_start_soy <- c('15-Apr', '25-Apr', '01-May')[instructions_tmp$region]
    planting_end_soy <- c('30-Apr', '05-May', '15-May')[instructions_tmp$region]
    
    x <- xml_find_all(base_doc, ".//manager/ui/date1_soy")
    xml_text(x) <- as.character(planting_start_soy)
    
    x <- xml_find_all(base_doc, ".//manager/ui/date2_soy")
    xml_text(x) <- as.character(planting_end_soy)
    
    #--------------------------
    # CULTIVAR BY REGION
    #--------------------------
    cultivar_corn <- c('B_115', 'B_110_gm', 'B_105_gm')[instructions_tmp$region]
    x <- xml_find_all(base_doc, ".//manager/ui/cultivar_corn")
    xml_text(x) <- as.character(cultivar_corn)
    
    cultivar_soy <- c('MG_4', 'MG_3', 'MG_2')[instructions_tmp$region]
    x <- xml_find_all(base_doc, ".//manager/ui/cultivar_soy")
    xml_text(x) <- as.character(cultivar_soy)
    
  }#else if(instructions_tmp$type == 'yc' & instructions_tmp$batch == 19 ){

    #--------------------------
    # PLANTING DATE BY SOIL TEMPERATURE
    #--------------------------
    # plant_dates_dt2 <- readRDS("./n_policy_box/Data/files_rds/plant_dates_dt.rds") %>% .[region == instructions_tmp$region & z == instructions_tmp$z]
    # planting_start_corn <- plant_dates_dt2$Date
    # planting_end_corn <- plant_dates_dt2$Date
    # 
    # x <- xml_find_all(base_doc, ".//manager/ui/date1_corn")
    # xml_text(x) <- as.character(planting_start_corn)
    # 
    # x <- xml_find_all(base_doc, ".//manager/ui/date2_corn")
    # xml_text(x) <- as.character(planting_end_corn)
    # 
    # }
  if(instructions_tmp$batch > 36 ){
    plant_population <- c('8', '7.5', '9')[instructions_tmp$region]
    x <- xml_find_all(base_doc, ".//manager/ui/density_corn")
    xml_text(x) <- as.character(plant_population)
    
    }
  #--------------------------
  # CLOCK
  #--------------------------
  date_start <- ifelse(instructions_tmp$type == 'stab', '01/01/2001', '01/01/2010')
  date_end <- ifelse(instructions_tmp$type == 'stab', '31/12/2010', '30/04/2012') #should be 2009. This is to compare cont vs seq
                   
  node <- xml_find_all(base_doc,'//clock/start_date')
  xml_text(node) <- date_start
  
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
  
  if(instructions_tmp$type == 'stab'){
    #--- extract soil data from the soil data base ---#
    # soils_database <- xmlParse('./Trial_crct_DIFM/Data/APssurgo_master/APSIM_soils/Soils_DIFM_bysoil.soils')
    soils_database <- xml2::read_xml(paste(directory, 'soils_vr_value.soils', sep = '/'))
    
    soil_name2 <- paste('.//Soil[@name="', tolower(instructions_tmp$mukey), '"]', sep = '')
    
    #--- replace soil ---#
    soil_temp2 <- xml_find_all(soils_database, soil_name2)
    base_doc_soil <- xml_find_all(base_doc, "//Soil")
  
    xml_replace(base_doc_soil, soil_temp2, .copy = TRUE)
    
    rm(soils_database, soil_temp2,base_doc_soil)
  }
  
  #--------------------------
  # UPDATE INITIAL CONDITIONS
  #--------------------------
  if(instructions_tmp$type == 'yc'){
    
    # "C:/Users/germanm2/Documents/n_policy_git/Codes/update_ic_nov18.R"
    # source(paste0(codes_folder, '/n_policy_git/Codes/update_ic_nov18.R'))
    "C:/Users/germanm2/Documents/n_policy_git/Codes/update_ic_reduced_jul13.R"
    "./n_policy_git/Codes/update_ic_reduced_jul13.R"
    source(paste0(codes_folder, '/n_policy_git/Codes/update_ic_reduced_jul13.R')) #simplified version
    #The initial residue assumes an alternation. Can be improved for account for other types of rotations
    base_doc <- update_ic(base_doc, instructions_tmp, initial_residue = crop_seq[2]) 
  }
  
  #--- Set the rate for the stab period ---#
  x <- xml_find_all(base_doc, ".//manager/ui/fert_amount_stab")
  xml_text(x) <- "150"
  
  #--- Set the starter rate ---#
  x <- xml_find_all(base_doc, ".//manager/ui/fert_amount_sow")
  xml_text(x) <- "0"
  

  #--- CREATE A FOLDER TO SAVE FILES ---#
  folder_name <- paste0(directory, '/', sim_name)
  
  if(file.exists(folder_name)){unlink(folder_name ,recursive=TRUE) }
  dir.create(folder_name, recursive = TRUE)
  
  #--- Set the rate for the YC period ---#
  if(instructions_tmp$type == 'yc'){
    N_rates <- seq(0, 320, 10)
    # N_rates <- seq(0, 300, 25)
    # N_rates <- c(0,260)
    # N_rates <- c('Nminus', 'Nrich')
    # N_rates <- 150
    for(N_n in N_rates){
      # N_n = 'n_minus'
      # N_n = N_rates[1]
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
      if(N_n %in% c('Nminus', 'Nrich')){
        #Apply the treatment at sowing
        x <- xml_find_all(base_doc, ".//manager/ui/fert_amount_sow")
        N_n_sow <- ifelse(N_n == 'Nminus', '0', '80')
        xml_text(x) <- as.character(N_n_sow)
        
        #Make rate at v5 cero
        x <- xml_find_all(base_doc, ".//manager/ui/fert_amount_yc")
        xml_text(x) <- as.character(0)
      }else{
        #Apply the treatment at v5
        x <- xml_find_all(base_doc, ".//manager/ui/fert_amount_yc")
        xml_text(x) <- as.character(N_n)
      }
   
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
  
  # instructions_tmp[,dir_path := folder_name]
  # initial_conditions[mukey == instructions_tmp$mukey & z == instructions_tmp$z & year == 2010, Y_dry]
	#return(one_treat.tmp)
  # return(instructions_tmp)
}


keep <- c('keep', 'apsim_create_files', 'instructions', 'directory', 'codes_folder')
# if(unique(instructions$type) == 'YC'){ keep <- append(keep, 'initial_conditions' )}
# # #rm(list = ls()[!ls() %in% keep])

clusterExport(cl, varlist = keep, envir=environment())


results.list <- parallel::parLapply(cl, 1:nrow(instructions), function(x) apsim_create_files(x))

# instructions <- rbindlist(results.list, fill = TRUE)

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

       

