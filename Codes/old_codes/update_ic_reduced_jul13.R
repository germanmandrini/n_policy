update_ic <- function(base_doc, instructions_tmp, initial_conditions, initial_residue){  
  #Open the initial conditions file
  # sim_name <- paste(unlist(instructions_tmp[,.(id_10, mukey, type)]), collapse = '_')
  
  
  file_name <- paste0('./n_policy_box/Data/initial_conditions_', instructions_tmp$batch,'/' , instructions_tmp$id_10,'_', instructions_tmp$mukey,'.rds')
  # file_name <- gsub(pattern = '_yc', replacement = '', file_name)
  # file_name <- gsub(pattern = paste0('_', instructions_tmp$z, '_'), replacement = '_1_', file_name) #only A1 is run as stabilization period
  
  initial_conditions_tmp <- readRDS(file_name)
  initial_conditions_tmp <- initial_conditions_tmp[z == instructions_tmp$z]
  # filter the right rows of the initial_condition (double check)
  # year_n <- max(initial_conditions_tmp$year)
  year_n = 2009
  initial_conditions_tmp <- initial_conditions_tmp[mukey == instructions_tmp$mukey &
                                                     year == year_n]
                                                   
  day_n <- max(initial_conditions_tmp$day)
  
  initial_conditions_tmp <- initial_conditions_tmp[day == day_n & month == 12]
    
  #variables_layers <- names(initial_conditions_tmp)[str_detect(names(initial_conditions_tmp), 'oc_[0-9]+$|biom_c_[0-9]+$|hum_c_[0-9]+$|inert_c_[0-9]+$|no3_[0-9]+$|nh4_[0-9]+$|sw_[0-9]+$')]
  
  #==============================================================================
  #Work with variables that are layers
  
  variables_layers <- names(initial_conditions_tmp)[grepl('_[0-9]+$', names(initial_conditions_tmp))]
  variables_layers <- variables_layers[!variables_layers == 'id_10']
  
  layers_numbers <- sort(as.numeric(unique(gsub(pattern = '_', replacement = '', 
                                                  stringr::str_extract_all(string = variables_layers, pattern = '_[0-9]+$', simplify = TRUE)))))
    
  variables_layers_unique <- unique(gsub(pattern = '_[0-9]+$', replacement = '', variables_layers))
  
  variables_layers_unique <- c("oc", "biom_c", "hum_c", "inert_c", "no3", "nh4", "sw")
    horizons_dt <- data.table(layer = layers_numbers)
    for( var_n in variables_layers_unique){
      var_n_seq <- paste(var_n, layers_numbers, sep = '_')
    
      horizons_dt <- cbind(horizons_dt, data.table(t(initial_conditions_tmp[, var_n_seq, with=FALSE]))) %>% 
      setnames(., 'V1', var_n)
    } #end var_n loop
    
    horizons_dt[,names(horizons_dt):= lapply(.SD, as.numeric), .SDcols = names(horizons_dt)]

    horizons_dt[,Finert := round(inert_c/(hum_c + biom_c),4)]
    #------------------------------------------------------------------------------------
    # Decrease FInert in South region (lower EONR while not changing Yr_zero)
    if(instructions_tmp$region == 1 & instructions_tmp$batch == 24){
      horizons_dt[,Finert := round(Finert*0.9,4)]
    }
    #------------------------------------------------------------------------------------
    horizons_dt[,Fbiom := round(biom_c/(hum_c-inert_c),4)]
    horizons_dt[,Finert := ifelse(oc == 0 & Finert == 0, 1, Finert)]  #correction for deep layers
    horizons_dt[,Fbiom := ifelse(Fbiom > 1 | Finert == 1, 0, Fbiom)]  #correction for deep layers
    
    horizons_dt <- horizons_dt[,.(layer, oc, no3, nh4, sw, Finert, Fbiom)]
    
    #-------------------------
    # Correct the n deep    
    #batch 52
    # n_mean_target <- c(1, 30, 70)[instructions_tmp$region]
    #batch 53
    # if(instructions_tmp$batch >= 53){n_mean_target <- c(5, 30, 30)[instructions_tmp$region]}
    #batch 54
    #n_mean_target <- c(5, 45, 60)[instructions_tmp$region]
    #batch 55
    n_mean_target <- c(5, 45, 100)[instructions_tmp$region]

    no3_target <- n_mean_target * (sum(horizons_dt$no3) / (sum(horizons_dt$no3) + sum(horizons_dt$nh4))) 
    nh4_target <- n_mean_target * (sum(horizons_dt$nh4) / (sum(horizons_dt$no3) + sum(horizons_dt$nh4)))
    horizons_dt[,n_deep_frac := (no3 + nh4)/sum(no3 + nh4)] 
    horizons_dt[,no3 := no3_target * n_deep_frac]
    horizons_dt[,nh4 := nh4_target * n_deep_frac]
    horizons_dt <- horizons_dt[,-'n_deep_frac']

    horizons_dt[no3 <= 0, no3 := 1 ] # make sure all are positive
    horizons_dt[nh4 <= 0, nh4 := 1 ] # make sure all are positive
    
    #-------------------------
    # Correct the sw to be less than DUL
    
    sub_node <- xml_find_all(base_doc,'//Water/DUL')
    
    horizons_dt$DUL <- "0"
    counter = 1
    for(node_layer in xml_children(sub_node)){
      # node_layer = 1
      horizons_dt$DUL[counter] <- xml_text(node_layer)
      counter = counter + 1
    }
    horizons_dt[,sw := ifelse(sw > DUL, DUL, sw)]
    horizons_dt <- horizons_dt[,-'DUL']

    #----------------------------------------------------------------------------------------------------
    # UPDATE THE BASE FILE
    # edit_childs <- c(paste0("//SoilOrganicMatter/", c("OC","FBiom","FInert")),
    #                  paste0("//Sample/", c("NO3", "NH4", "SW")))
    # 
    # equivalent_colname <- c('oc', 'Fbiom', 'Finert', 'no3', 'nh4', 'sw')
    edit_childs <- c(paste0("//SoilOrganicMatter/", c("FBiom","FInert")),
                     paste0("//Sample/", c("NO3", "NH4", "SW")))

    equivalent_colname <- c('Fbiom', 'Finert', 'no3', 'nh4', 'sw')
    
    # if(instructions_tmp$batch %in% c(31, 32, 33)){
      
    for(var_n in 1:length(edit_childs)){
      # var_n <- 5
      child_n <- edit_childs[var_n]
      equivalent_colname_n <- equivalent_colname[var_n]
      sub_node <- xml_find_all(base_doc,child_n)
      vector <- as.character( horizons_dt[[equivalent_colname_n]] )
      
      counter = 1
      for(node_layer in xml_children(sub_node)){
        # node_layer = 1
        xml_text(node_layer) <- vector[counter]
        counter = counter + 1
      }# end node_layer
      
    }# end var_n
    # }
    #==============================================================================
    #Work with variables that are one value
    
    names(initial_conditions_tmp)[!names(initial_conditions_tmp) %in% variables_layers &
                                    !names(initial_conditions_tmp) %in% variables_layers_unique]
    
    initial_conditions_tmp[,surfaceom_cn := round(as.numeric(surfaceom_c) / as.numeric(surfaceom_n), 0)]
    
    variables_one_value_names <- c("z","mukey", 'surfaceom_cn', "surfaceom_wt", "root_wt","root_cn")
    
    variables_one_value_table <- initial_conditions_tmp[,variables_one_value_names, with = FALSE]
    
    #------------------------------------------------------------------------------------
    # Increase residue and root in South region (lower EONR while not changing Yr_zero)
    if(instructions_tmp$region == 1 & instructions_tmp$batch == 22){
      variables_one_value_table[,surfaceom_wt := surfaceom_wt*1.1]
      variables_one_value_table[,root_wt := root_wt*1.1]
    }
    
    if(instructions_tmp$region == 1 & instructions_tmp$batch == 23){
      variables_one_value_table[,surfaceom_wt := surfaceom_wt*1.3]
      variables_one_value_table[,root_wt := root_wt*1.3]
    }
    
    if(instructions_tmp$region == 1 & instructions_tmp$batch == 24){
      variables_one_value_table[,surfaceom_wt := surfaceom_wt*1.3]
      variables_one_value_table[,root_wt := root_wt*1.4]
    }
    
    if(instructions_tmp$region == 1 & instructions_tmp$batch == 25){
      variables_one_value_table[,surfaceom_wt := surfaceom_wt*1.4]
      variables_one_value_table[,root_wt := root_wt*1.4]
    }
    
    if(instructions_tmp$region == 1 & instructions_tmp$batch == 26){
      variables_one_value_table[,surfaceom_wt := surfaceom_wt*2]
      variables_one_value_table[,root_wt := root_wt*2]
    }
    
    if(instructions_tmp$region == 1 & instructions_tmp$batch == 27){
      variables_one_value_table[,surfaceom_wt := surfaceom_wt*1]
      variables_one_value_table[,root_wt := root_wt*2]
    }
    
    #------------------------------------------------------------------------------------
    
    #Residue weight
    node <- xml_find_all(base_doc, '//surfaceom/mass')
    xml_text(node) <- as.character(variables_one_value_table$surfaceom_wt)

    #Residue C:N ratio
    node <- xml_find_all(base_doc, '//surfaceom/cnr')
    xml_text(node) <- as.character(variables_one_value_table$surfaceom_cn)
    
    #Residue name
    node <- xml_find_all(base_doc, '//surfaceom/PoolName')
    xml_text(node) <- as.character(initial_residue) 
    
    node <- xml_find_all(base_doc, '//surfaceom/type')
    xml_text(node) <- as.character(initial_residue) 
    
    node <- xml_find_all(base_doc,'//SoilOrganicMatter/RootWt')
    xml_text(node) <- as.character(variables_one_value_table$root_wt)
    
    node <- xml_find_all(base_doc,'//SoilOrganicMatter/RootCN')
    xml_text(node) <- as.character(variables_one_value_table$root_cn)
    
    
    return(base_doc)
    }