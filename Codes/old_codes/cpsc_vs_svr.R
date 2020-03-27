#======================================YIELD CURVE ============================================================================#

files_cpsc <- list.files('C:/Users/germanm2/Box Sync/My_Documents/vr_value/Data/initial_conditions', recursive = T)
files_srv <- list.files('C:/Users/germanm2/Box Sync/My_Documents/vr_value/Data/initial_conditions_svr', recursive = T)

files_both <- dplyr::intersect(files_cpsc, files_srv)
compare_dt <- data.table()
for(file_n in files_both){
  # file_n <- files_both[10]
  print(file_n)
  
  file_cpsc <- readRDS(paste('C:/Users/germanm2/Box Sync/My_Documents/vr_value/Data/initial_conditions', file_n, sep = '/'))
  file_cpsc_sum <- file_cpsc[,.(fertiliser = sum(fertiliser),
               Y_cpsc = max(Y, na.rm = T)), by = year]
  
  file_svr <- readRDS(paste('C:/Users/germanm2/Box Sync/My_Documents/vr_value/Data/initial_conditions_svr', file_n, sep = '/'))
  file_svr_sum <- file_svr[,.(fertiliser = sum(fertiliser),
              Y_svr = max(Y, na.rm = T)), by = year]
  
  compare_tmp <- data.table(file=file_n, merge(file_cpsc_sum, file_svr_sum))[,Y_diff := abs(Y_cpsc - Y_svr)]
  compare_dt <- rbind(compare_dt, compare_tmp)
  
}
compare_dt[,Y_diff := abs(Y_cpsc - Y_svr)]

ggplot(data=compare_dt, aes(x = Y_cpsc, y = Y_svr, color = year)) +
  geom_point()+ theme(aspect.ratio=1) + coord_fixed() + geom_abline() + # ylim(0, 12000)+ xlim(1000, 12000) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  ggtitle('Sequential vs Continuous') +
  theme_bw()


ggplot(data = compare_dt, aes(x = fertiliser, y = Y_diff)) +
  geom_point()

#======================================INITIAL CONDITIONS==============================================================================#

files_cpsc <- list.files('C:/Users/germanm2/Box Sync/My_Documents/vr_value/Data/yc_output', recursive = T)
files_srv <- list.files('C:/Users/germanm2/Box Sync/My_Documents/vr_value/Data/yc_output_svr', recursive = T)

files_both <- dplyr::intersect(files_cpsc, files_srv)

for(file_n in files_both){
  # file_n <- files_both[989]
  print(file_n)
  
  file_cpsc <- readRDS(paste('C:/Users/germanm2/Box Sync/My_Documents/vr_value/Data/yc_output', file_n, sep = '/'))
  file_cpsc[,.(fertiliser = sum(fertiliser),
               Y = max(Y, na.rm = T)), by = year]
  
  file_svr <- readRDS(paste('C:/Users/germanm2/Box Sync/My_Documents/vr_value/Data/yc_output_svr', file_n, sep = '/'))
  file_svr[,.(fertiliser = sum(fertiliser),
              Y = max(Y, na.rm = T)), by = year]
  rbind(file_cpsc[2], file_svr[2])
  