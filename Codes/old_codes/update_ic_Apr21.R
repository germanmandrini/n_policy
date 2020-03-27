update_ic <- function(base_doc, instructions_tmp, initial_conditions){  
  year_n = 2009
  # year_n <- max(initial_conditions$year)-1
  z1_n <- gsub(pattern = '[0-9]+$', replacement = '', instructions_tmp$z)

  # filter the right rows of the initial_condition
  initial_conditions_tmp <- initial_conditions[mukey == instructions_tmp$mukey & year == year_n]
  initial_conditions_tmp[,z1 := gsub(pattern = '[0-9]+$', replacement = '', z)]
  initial_conditions_tmp <- initial_conditions_tmp[z1 == z1_n ]
    
  variables_layers <- names(initial_conditions_tmp)[grepl('_[0-9]+$', names(initial_conditions_tmp))]
  variables_layers <- variables_layers[!variables_layers == 'id_10']
  
  layers_numbers <- sort(as.numeric(unique(gsub(pattern = '_', replacement = '', 
                                                  stringr::str_extract_all(string = variables_layers, pattern = '_[0-9]+$', simplify = TRUE)))))
    
  variables_layers_unique <- unique(gsub(pattern = '_[0-9]+$', replacement = '', variables_layers))
  
    
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