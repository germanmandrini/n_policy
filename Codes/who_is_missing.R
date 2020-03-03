

files_drive <- list.dirs('S:/Bioinformatics Lab/germanm2/vr_value/yc_output',full.names = F, recursive = F)
files_svr <- readRDS('S:/Bioinformatics Lab/germanm2/vr_value/yc_output/files_runned_svr.rds')

setdiff(files_svr, files_drive)
