library(data.table)
library(stringr)
library(ggplot2)
# files_list <- list.files('S:/Bioinformatics Lab/germanm2/n_policy/yc_output/', full.names = T) #CLUSTER
files_list <- list.files('S:/Bioinformatics Lab/germanm2/n_policy_tmp/yc_output/', full.names = T) #SERVER
files_list <- files_list[str_detect(string = files_list, pattern = '137_182564')]
print(files_list)

daily_dt <- data.table()

for (file_n in files_list){
  daily_tmp <- readRDS(file_n)
  daily_dt <- rbind(daily_dt, daily_tmp)
  
  
}

yearly_dt <- daily_dt[year < 2011,.(Yld = max(Y, na.rm = T)/0.85,
                       #leach_no3 = sum(leach_no3),
                       N_fert = sum(fertiliser),
                       dul_dep = max(dul_dep),
                       ll15_dep = max(ll15_dep),
                       whc = max(dul_dep) - max(ll15_dep)), by = .(id_10, mukey,  z, sim_name, year)]
library(ggplot2)

ggplot(data = yearly_dt, aes(x= N_fert, y = Yld, colour = z)) +
  geom_point()

weather_dt <- readRDS('C:/Users/germanm2/Box Sync/My_Documents/n_policy/Data/met_files/weather_z_cell37_dt.rds')
#---------------------------------------------------------------------------
#COMPARE SEQ VS CONT
files_list <- list.files('S:/Bioinformatics Lab/germanm2/n_policy_tmp/yc_output/', full.names = T) #SERVER
id10_n = '137'

#COMPARE THE CONTINUOUS SIMULATION VS THE SEQUENTIAL
# COMPARE THE YIELD
ic_files <- list.files('S:/Bioinformatics Lab/germanm2/n_policy_tmp/initial_conditions/', pattern = paste0(id10_n, '_'), full.names = TRUE)
ic_dt <- data.table()
for(file_n in ic_files){
  ic_dt <- rbind(ic_dt, readRDS(file_n))}

yc_files <- list.files('S:/Bioinformatics Lab/germanm2/n_policy_tmp/yc_output/', pattern = paste0(id10_n, '_'), full.names = TRUE)
yc_dt <- data.table()
for(file_n in yc_files){
  yc_dt <- rbind(yc_dt, readRDS(file_n))}

sim_name_n <- unique(yc_dt$sim_name)[str_detect(unique(yc_dt$sim_name), pattern = '_150')]
ic_dt2 <- ic_dt[year %in% c(2010), .(z, mukey, year, day, sim_name, fertiliser, Y_ic = Y)]
yc_dt2 <- yc_dt[year %in% c(2010) & sim_name %in% sim_name_n,.(z, mukey, year, day, sim_name, fertiliser, Y_yc = Y)]

ic_dt2 <- ic_dt2[,.(Y_ic = max(Y_ic, na.rm = T), fertiliser = sum(fertiliser)), by = .(z, mukey, year, sim_name)]
yc_dt2 <- yc_dt2[,.(Y_yc = max(Y_yc, na.rm = T), fertiliser = sum(fertiliser)), by = .(z, mukey, year, sim_name)]

paired <- merge(ic_dt2[,-'sim_name'], yc_dt2[,-'sim_name'], by = c('z', 'mukey', 'year', 'fertiliser'), al.x = T)
res_col_names <- c('Y_yc', 'Y_ic')
paired[, (res_col_names) := lapply(.SD, as.numeric), .SDcols = res_col_names]

ggplot(data=paired, aes(x = Y_ic, y = Y_yc)) +
  geom_point()+ theme(aspect.ratio=1) + coord_fixed() + geom_abline() + ylim(0, 12000)+ xlim(0, 12000) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  ggtitle('Sequential vs Continuous') +
  theme_bw()

paired[,Y_diff := abs(Y_ic - Y_yc)][order(-Y_diff)]
# CHECK THE IC
fc <- ic_dt[year == 2009 & day == 365][1]
ic <- yc_dt[year == 2010 & day == 1][1]

rbind(fc, ic)

# SHOW YIELD CURVES
yc <- yc2[year == 2010]
yc[,mukey_rotation := paste(mukey, rotation, sep = '_')]

ggplot(data=yc, aes(x = fertiliser, y = Y_yc, color = mukey_rotation)) +
  geom_point(size = 3) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  ggtitle(paste('Summarized Response by mukey'))




#COMPARE CLUSTER VS SERVER