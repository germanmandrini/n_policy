update_ic <- function(base_doc, instructions_tmp, initial_conditions){  
  # ar_n = 2009

  #Open the initial conditions file
  file_name <- paste0('./vr_value_v2/Data/initial_conditions/cell_', instructions_tmp$id_10, '/',paste(instructions_tmp, collapse = '_'),'.rds')
  file_name <- gsub(pattern = '_YC', replacement = '_stab', file_name)
  # file_name <- gsub(pattern = paste0('_', instructions_tmp$z, '_'), replacement = '_1_', file_name) #only A1 is run as stabilization period
  
  initial_conditions_tmp <- readRDS(file_name)
  
  # filter the right rows of the initial_condition (double check)
  # year_n <- max(initial_conditions_tmp$year)
  year_n = 2009
  initial_conditions_tmp <- initial_conditions_tmp[mukey == instructions_tmp$mukey &
                                                     year == year_n]
                                                   
  day_n <- max(initial_conditions_tmp$day)
  
  initial_conditions_tmp <- initial_conditions_tmp[day == 365 & month == 12]
    
  #variables_layers <- names(initial_conditions_tmp)[str_detect(names(initial_conditions_tmp), 'oc_[0-9]+$|biom_c_[0-9]+$|hum_c_[0-9]+$|inert_c_[0-9]+$|no3_[0-9]+$|nh4_[0-9]+$|sw_[0-9]+$')]
  
  
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
    sapply(horizons_dt,class)
    
    horizons_dt[,Finert := round(inert_c/(hum_c + biom_c),4)]
    horizons_dt[,Fbiom := round(biom_c/(hum_c-inert_c),4)]
    horizons_dt[,Finert := ifelse(oc == 0 & Finert == 0, 1, Finert)]  #correction for deep layers
    horizons_dt[,Fbiom := ifelse(Fbiom > 1 | Finert == 1, 0, Fbiom)]  #correction for deep layers
    
    horizons_dt <- horizons_dt[,.(layer, oc, no3, nh4, sw, Finert, Fbiom)]
    
    names(initial_conditions_tmp)[!names(initial_conditions_tmp) %in% variables_layers &
                                    !names(initial_conditions_tmp) %in% variables_layers_unique]
    
    initial_conditions_tmp[,surfaceom_cn := round(as.numeric(surfaceom_c) / as.numeric(surfaceom_n), 0)]
    
    variables_one_value_names <- c("z","mukey", 'surfaceom_cn', "surfaceom_wt", "root_wt","root_cn")
    
    variables_one_value_table <- initial_conditions_tmp[,variables_one_value_names, with = FALSE]
    
    #----------------------------------------------------------------------------------------------------
    # UPDATE THE BASE FILE
    edit_childs <- c(paste0("//SoilOrganicMatter/", c("OC","FBiom","FInert")),
                     paste0("//Sample/", c("NO3", "NH4", "SW")))
    
    equivalent_colname <- c('oc', 'Fbiom', 'Finert', 'no3', 'nh4', 'sw')
    
    for(var_n in 1:length(edit_childs)){
      # var_n <- 1
      child_n <- edit_childs[var_n]
      equivalent_colname_n <- equivalent_colname[var_n]
      sub_node <- xml_find_all(base_doc,child_n)
      vector <- as.character( horizons_dt[[equivalent_colname_n]] )
      
      counter = 1
      for(node_layer in xml_children(sub_node)){
        # node_layer = 1
        xml_text(node_layer) <- vector[counter]
        counter = counter + 1
      }
      
    }
    
    node <- xml_find_all(base_doc, '//surfaceom/mass')
    xml_text(node) <- as.character(variables_one_value_table$surfaceom_wt)

    # surfaceom_cn ---
    node <- xml_find_all(base_doc, '//surfaceom/cnr')
    xml_text(node) <- as.character(variables_one_value_table$surfaceom_cn)
    
    node <- xml_find_all(base_doc,'//SoilOrganicMatter/RootWt')
    xml_text(node) <- as.character(variables_one_value_table$root_wt)
    
    node <- xml_find_all(base_doc,'//SoilOrganicMatter/RootCN')
    xml_text(node) <- as.character(variables_one_value_table$root_cn)
    
    return(base_doc)
    }