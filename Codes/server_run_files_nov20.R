
if(server){

  # apsim_exe <- '/opt/apsim/Model/ApsimRun.sh'
  apsim_exe <- '/opt/apsim_dev/trunk/Model/ApsimRun.sh'
  
  flist = list.files(directory, full.names = TRUE, recursive = TRUE, pattern = '.apsim')
  
  apsim_file = file.path(directory, 'apsim.txt')
  write.table(flist, apsim_file, row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  # start <- Sys.time()
  tst = system(paste('parallel -j 28 --eta', apsim_exe, '{} <', apsim_file), ignore.stdout = T, ignore.stderr = T)
  # print(Sys.time() - start)

# -----------------------------------------------------------------------
# OLD COMMANDS
# apsim_exe = '/opt/apsim_dev/trunk/Model/ApsimModel.sh '
# apsimr::apsim
# 
# apsim_exe <- '/opt/apsim/Model/ApsimRun.sh'
# 
# apsim_exe = '/opt/apsim_dev/trunk/Model/ApsimModel.sh '
# 
# file <- "/home/germanm2/apsim_temp/vr_value/cell1094/z100_173430/temp.apsim"
# 
# res <- suppressWarnings(system(paste(apsim_exe, file, sep = " "), show.output.on.console = FALSE))

# -----------------------------------------------------------------------
}else if(cpsc){
  # WINDOWS
  library(doParallel)  
  library(stringr)
  no_cores <- detectCores() * 7/8
  registerDoParallel(cores=no_cores)  
  cl <- makeCluster(no_cores) 
  
  flist = list.files(directory, full.names = TRUE, recursive = TRUE, pattern = '.apsim')
  if(any(str_detect(string = flist, pattern = '_yc_'))) {
    flist <- flist[str_detect(string = flist, pattern = '_yc_150|Nminus|Nrich')] #OJO!!!!
  }
  result <- parLapply(cl, flist, function(x) system2( 'C:/Program Files (x86)/APSIM710-r4158/Model/Apsim.exe',  x ))  
  stopCluster(cl) 

  # -----------------------------------------------------------------------
}else if(cluster){
  #CLUSTER
  # system2(paste0('singularity exec /projects/aces/germanm2/apsim_nov16.simg /usr/bin/mono /usr/local/APSIMClassic/Model/Apsim.exe ', directory, '/**/*.apsim'))
  
  # apsim_exe <- '/usr/bin/mono /usr/local/APSIMClassic/Model/ApsimRun.sh'
  apsim_exe <- '/usr/bin/mono /usr/local/APSIMClassic/Model/Apsim.exe'

  
  flist = list.files(directory, full.names = TRUE, recursive = TRUE, pattern = '.apsim')
  if(any(str_detect(string = flist, pattern = '_yc_'))) {
    flist <- flist[str_detect(string = flist, pattern = '_yc_')] #OJO!!!!
  }

  file_name = file.path(directory, 'files_to_run.txt')
  write.table(flist, file_name, row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  tst = system(paste('parallel -j 28 --eta', apsim_exe, '{} <', file_name), ignore.stdout = T, ignore.stderr = T)

  
}

