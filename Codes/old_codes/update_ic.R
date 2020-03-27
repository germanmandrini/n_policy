horizons_cell_dt

horizons_cell2_dt

initial_conditions

new_toolbox <- read_xml("<folder></folder>") %>% 
xml2::xml_attr(new_toolbox, 'version') <- '36'
xml2::xml_attr(new_toolbox, 'creator') <- 'Apsim 7.10'
xml2::xml_attr(new_toolbox, 'name') <- 'soils_vr_value_updated'

sapply(initial_conditions, class)
for(mukey_n in unique(initial_conditions$mukey)){
  # mukey_n  <-  unique(initial_conditions$mukey)[1]
  initial_conditions_tmp <- initial_conditions[mukey == mukey_n]
  for(z_n in unique(initial_conditions_tmp$z)){
    # z_n <- unique(initial_conditions_tmp$z)[1]
    year_n <- max(initial_conditions_tmp$year)
    
    initial_conditions_tmp <- initial_conditions_tmp[z == z_n & year == year_n]
    
    variables_layers <- names(initial_conditions_tmp)[grepl('_[0-9]+$', names(initial_conditions_tmp))]
    
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
    
    horizons_dt[,Finert := round(inert_c/(hum_c + biom_c),3)]
    horizons_dt[,Finert := ifelse(oc == 0 & Finert == 0, 1, Finert)]  #correction for deep layers
    horizons_dt[,Fbiom := round(biom_c/(hum_c-inert_c),3)]
    
    horizons_dt <- horizons_dt[,.(layer, oc, no3, nh4, sw, Finert, Fbiom)]
    
    names(initial_conditions_tmp)[!names(initial_conditions_tmp) %in% variables_layers &
                                    !names(initial_conditions_tmp) %in% variables_layers_unique]
    
    initial_conditions_tmp[,surfaceom_cn := round(as.numeric(surfaceom_c) / as.numeric(surfaceom_n), 0)]
    
    variables_one_value_names <- c("z","mukey", "fom_c", "fom_n", 'surfaceom_cn', "surfaceom_wt", "root_wt","root_cn")
    
    variables_one_value_table <- initial_conditions_tmp[,variables_one_value_names, with = FALSE]
    variables_one_value_table
    
    
    
    #----------------------------------------------------------------------------------------------------
    # UPDATE THE TOOLBOX AND CREATE A NEW ONE
    soils_database <- xml2::read_xml(paste(directory, 'soils_vr_value.soils', sep = '/'))
    
    soils_database_childrens <- xml_children(soils_database)
    soil_temp <- soils_database_childrens[[which(xml_attr(soils_database_childrens, 'name') == mukey_n)]]
    
    xml_add_child(new_toolbox, soil_temp)
    
    rm(soils_database,soils_database_childrens, soil_temp)
    
    new_node <- xml_find_all(new_toolbox, paste(".//Soil[@name=\"", mukey_n,"\"]", sep = ''))
    
    edit_childs <- c(paste0("//SoilOrganicMatter/", c("OC","FBiom","FInert")),
                     paste0("//Sample/", c("NO3", "NH4", "SW")))
    
    equivalent_colname <- c('oc', 'Fbiom', 'Finert', 'no3', 'nh4', 'sw')
    
    for(var_n in 1:length(edit_childs)){
      # var_n <- 1
      child_n <- edit_childs[var_n]
      equivalent_colname_n <- equivalent_colname[var_n]
      sub_node <- xml_find_first(new_node,child_n)
      vector <- as.character( horizons_dt[[equivalent_colname_n]] )
      
      counter = 1
      for(node_layer in xml_children(sub_node)){
        # layer_n = 1
        xml_text(node_layer) <- vector[counter]
        counter = counter + 1
      }
      
    }
    
    node <- xml_find_all(new_node, '//surfaceom')
    xml_text(node) <- one_value$surfaceom_wt
    
    # surfaceom_cn ---
    node <- xml_find_all(base_doc, '//surfaceom/cnr')
    xml_text(node) <- one_value$surfaceom_cn
    
    node <- xml_find_all(new_soil,'//SoilOrganicMatter/RootWt')
    xml_text(node) <- one_value$root_wt
    
    node <- xml_find_all(new_soil,'//SoilOrganicMatter/RootCN')
    xml_text(node) <- one_value$root_cn
    sub_node <- xml_find_all(new_node,child_n)
    variables_one_value_table
    
    xml2::xml_attr(new_node, 'name') <- paste(mukey_n, gsub(pattern = '[0-9]', replacement = '', z_n), sep = "_")
  }#end z_n loop
  }#end mukey_n loop
