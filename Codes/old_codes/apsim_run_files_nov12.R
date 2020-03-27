server <- ifelse(Sys.info()["nodename"] == "campodonico", TRUE, FALSE)

if(server){

  # apsim_exe <- '/opt/apsim/Model/ApsimRun.sh'
  apsim_exe <- '/opt/apsim_dev/trunk/Model/ApsimRun.sh'
  
  flist = list.files(directory, full.names = TRUE, recursive = TRUE, pattern = '.apsim')
  
  apsim_file = file.path(directory, 'apsim.txt')
  write.table(flist, apsim_file, row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  # start <- Sys.time()
  tst = system(paste('parallel -j 32 --eta', apsim_exe, '{} <', apsim_file), ignore.stdout = T, ignore.stderr = T)
  
  # system( 'singularity exec /opt/apsim.simg /usr/bin/mono /usr/local/APSIMClassic/Model/Apsim.exe /home/germanm2/apsim_temp/vr_value_v2/cell152/152*')
 
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
}else{
# WINDOWS
  library(doParallel)  
  no_cores <- detectCores() * 7/8
  registerDoParallel(cores=no_cores)  
  cl <- makeCluster(no_cores)  
  flist = list.files(directory, full.names = TRUE, recursive = TRUE, pattern = '.apsim')
  result <- parLapply(cl, flist, function(x) system2( 'C:/Program Files (x86)/APSIM710-r4158/Model/Apsim.exe',  x ))  
  stopCluster(cl)         
}


