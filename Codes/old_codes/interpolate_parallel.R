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

interpolate_paralell <- function(data_dt, column_sets){
  
  no_cores <- detectCores() * 7/8
  
  cl <- makeCluster(no_cores,type='SOCK')
    
  names(data_dt)[(which(names(data_dt) == column_sets))]  <- 'sets'
    
  interpolate <- function(set_n){
    # set_n = '99_937031_E5'
    library(dplyr)
    library(data.table)
    library(stats)
    # set_n = unique(sets$sets)[1]
    training <- data_dt[sets == set_n, ] %>% .[order(NRate)]
    model_y <- loess(Y ~ NRate, span = 0.5, data=training)
    model_n <- loess(leach_no3_ss ~ NRate, span = 0.5, data=training)
    
    if(FALSE){
      plot(training$Y, x=training$NRate, 
           type="p", main="Loess Smoothing and Prediction", xlab="NRate", ylab="Y")
      lines(predict(model) , x= training$NRate, col="red")
    }
    
    interpolated_tmp <- data.table(training[1, .(id_10, z, mukey, sets)], 
                                   NRate = seq(from = min(training$NRate), to = max(training$NRate), by = 5), 
                                   stringsAsFactors = F) %>% .[order(NRate)]
    
    interpolated_tmp$Y <- stats::predict(model_y, interpolated_tmp)
    interpolated_tmp$leach_no3_ss <- stats::predict(model_n, interpolated_tmp)
    
    setnames(interpolated_tmp, 'sets', column_sets)
    
    return(interpolated_tmp)
  }

  keep <- c('keep', 'interpolate', 'data_dt')

# interpolate(unique(data_dt$sets)[1])

  clusterExport(cl, varlist = keep, envir=environment())

  results_list <- parallel::parLapply(cl, unique(data_dt$sets), function(x) interpolate(x))
  
  # results_list <- list()
  # for(set_n in unique(data_dt$sets)){
  #   print(set_n)
  #   results_list[[length(results_list)+1]] <- interpolate(set_n)
  #   
  # }

  interpolated_dt <- rbindlist(results_list, fill = TRUE)

  stopCluster(cl)
  
  return(interpolated_dt)
}