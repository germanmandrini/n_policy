#CV = CROSS VALIDATION: The idea is to iterate and do a cv by sampling different z,
# training and then testing the models. Results should be the average of training and testing over all the z. 

# source('./Codes_useful/R.libraries.R')
# source('./Codes_useful/gm_functions.R')

source('~/vr_value_v2/Codes/3b_regional_stations_cv.R')

# training_set_list <- list(cv1 = c(1:10), cv2=c(11:20), cv3=c(21:30))

training_set_list <- list(cv1=c(1:10), cv2=c(6:15), 
                          cv3=c(11:20), cv4=c(16:25),
                          cv5=c(21:30), cv6=c(1:5, 26:30))

for(set_n in names(training_set_list)){
  # set_n <- 'cv1'
  training_z = training_set_list[[set_n]]
  
  #------
  # Save temporal objects
  reg_model_stuff_tmp <- list()
  reg_model_stuff_tmp[['training_z']] <- training_z
  # keep <- c('training_set_list','set_n','reg_model_stuff_tmp')
  
  # Split the data and get the training set
  source('~/vr_value_v2/Codes/3c_make_training_cv.R')
  
  # Train regional models using the training data
  source('~/vr_value_v2/Codes/3d_regional_models_cv.R')
  
  #Clean the environment
  keep <- c('training_set_list', 'set_n', 'training_z', 'reg_model_stuff', 'reg_model_stuff_tmp')
  remove <- ls()[!ls() %in% keep]
  rm(list=remove)
  
  #Calculate performance of the differnet models
  source('~/vr_value_v2/Codes/4_economics_field_and_local_RF_cv.R')
  
  #Save the output
  reg_model_stuff[[set_n]] <- reg_model_stuff_tmp
  saveRDS(reg_model_stuff, "./vr_value_v2/Data/files_rds/reg_model_stuff.rds")
}

names(reg_model_stuff)
