update_ic <- function(base_doc, instructions_tmp, initial_conditions, initial_residue){  
  
  #-------------------------
  # Set initial N
  horizons_dt <- data.table(layer = 1:10, 
                        Fbiom = c(0.12, 0.08, 0.04, 0.04, 0.03, 0.01,0.01, 0.01, 0.01,0.01),
                        Finert = c(0.4, 0.40, 0.420, 0.46, 0.56, 0.72, 0.80,0.80, 0.92, 0.98), 
                        no3_frac = c(0.4,0.2,0.1,0.1,0.1, 0.05, 0.05, 0, 0, 0))
  
  #-------------------------
  if(TRUE){#this was used in batch 88
  # Correct the n deep to target    
  # n_target <- c(sample(10:20, 1), sample(1:10, 1), sample(10:30, 1))[instructions_tmp$region] #add some randomness
    n_target <- c(sample(25:30, 1), sample(1:1, 1), sample(40:50, 1))[instructions_tmp$region] #add some randomness
    horizons_dt[,no3 := n_target * no3_frac]
    horizons_dt[,nh4 := 0]
    
    horizons_dt[no3 <= 0 | is.na(no3), no3 := 0.00 ] # make sure all are positive
    horizons_dt[nh4 <= 0 | is.na(nh4), nh4 := 0.00 ] # make sure all are positive
    
    horizons_dt <- horizons_dt[,-c('no3_frac')]
  }
  
  #----------------------------------------------------------------------------------------------------
  # UPDATE THE BASE FILE
  # edit_childs <- c(paste0("//SoilOrganicMatter/", c("OC","FBiom","FInert")),
  #                  paste0("//Sample/", c("NO3", "NH4", "SW")))
  # 
  # equivalent_colname <- c('oc', 'Fbiom', 'Finert', 'no3', 'nh4', 'sw')
  edit_childs <- c(paste0("//SoilOrganicMatter/", c("FBiom","FInert")),
                 paste0("//Sample/", c("NO3", "NH4")))
  
  equivalent_colname <- c('Fbiom', 'Finert', 'no3', 'nh4')
  
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
  
  return(base_doc)
  }