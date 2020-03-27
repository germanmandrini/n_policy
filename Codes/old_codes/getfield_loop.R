######################################
# Parallelized Simulations
######################################

#===================================
# prepare clusters
#===================================



#===================================
# parallelized simulations 
#===================================
# 
# data_dt = ic_dt2
# column_sets = 'id10_mukey_z'

get_field_loop <- function(interpolated_dt, grid10_soils_sf4){
  
  # no_cores <- detectCores() * 7/8
  # 
  # cl <- makeCluster(no_cores,type='SOCK')
    
  get_field <- function(field_n){
    # set_n = '99_937031_E5'
    library(dplyr)
    library(data.table)
    library(sf)
    # field_n = 99
    # print(field_n)
    fields_seq_tmp <- fields_seq[field_n,]
    one_field_sf <- grid10_soils_sf4[grid10_soils_sf4$id_10 == fields_seq_tmp$id_10 &
                                       grid10_soils_sf4$id_field == fields_seq_tmp$id_field,]
    
    area_dt <- data.table(one_field_sf) %>% .[,.(area_ha = sum(area_ha)), by = mukey]
    
    # LOAD OUTPUT
    ic_field_dt <- interpolated_dt[id_10 == fields_seq_tmp$id_10 & mukey %in% unique(one_field_sf$mukey)]
    
    ic_field_dt <- merge(ic_field_dt, area_dt, by = 'mukey')
    ic_field_dt <- cbind(fields_seq_tmp, ic_field_dt)
    
    
    summary(ic_field_dt[,.(area_ha = sum(area_ha)),  by = .(id_10, z, NRate)]$area_ha)
    
    #---------------------------------------------------------------------------
   
    
    return(ic_field_dt)
  }

  # keep <- c('keep', 'get_field','interpolated_dt', 'grid10_soils_sf4')
  
  fields_seq <- data.table(grid10_soils_sf4) %>% .[id_10 %in% unique(interpolated_dt$id_10), c('id_10', 'id_field')] %>% unique()
# get_field(1)

  # clusterExport(cl, varlist = keep, envir=environment())
  # 
  # results_list <- parallel::parLapply(cl, 1:nrow(fields_seq), function(x) interpolate(x))
  
  results_list <- list()
  for(i in 1:nrow(fields_seq)){
    print(i)
    results_list[[length(results_list)+1]] <- get_field(i)
  }
  # saveRDS(results_list, './vr_value/Data/files_rds/interpolated_ls.rds')
  results_list <- readRDS('./vr_value/Data/files_rds/interpolated_ls.rds')
  interpolated_dt <- rbindlist(results_list, fill = TRUE)
  
  return(interpolated_dt)
}