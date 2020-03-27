#COLLECT AND MERGE THE DATA
# library(lubridate)
library(tools)

apsim_merge_data <- function(instructions, directory_output){
  # directory_output= paste0('./vr_value/Data/initial_conditions/cell_', id10_n)
  # directory_output= paste0('./vr_value/Data/yc_output/cell_', id10_n)
 
  for(i in 1:nrow(instructions)){
    # i = 1
    instructions_tmp <- instructions[i]
    
    out_files <- list.files(instructions_tmp$dir_path, pattern = '.out', full.names = T, recursive = T)
    
    results_collection_ls <- list()
    for(out_file_n in out_files){
      # out_file_n <- out_files[1]
      name_sim <- gsub(".out$", "", basename(out_file_n))
      res <- try(fread(out_file_n, header = T), TRUE)
      
      res <- res[-1, ]
        
      if ("Date" %in% colnames(res)) {
        res$Date <- as.Date(res$Date, format = c("%d/%m/%Y"))
      }
      
      exclude <- c('Date', 'stage', 'stage_name')
      res_col_names <- names(res)[!names(res) %in% exclude]
      
      res[, (res_col_names) := lapply(.SD, as.numeric), .SDcols = res_col_names]
      
      names(res) <- gsub('(\\()([0-9]+)(\\))$', '_\\2', names(res))
      names(res) <- gsub('\\()', '', names(res))
      
      res <- cbind(data.table(instructions_tmp[,-'dir_path'],
                              sim_name = basename(file_path_sans_ext(out_file_n))),
                   res)
      
      results_collection_ls[[length(results_collection_ls)+1]] <- res
    }# end of out_file loop
    
    #SAVE THE OUTPUT
    
      if(!file.exists(directory_output)){ dir.create(directory_output, recursive = TRUE) }
        saveRDS(rbindlist(results_collection_ls, fill = TRUE), paste0(directory_output,'/', basename(instructions_tmp$dir_path) , '.rds'))
      } #end of the 1:nrow(instructions loop
}