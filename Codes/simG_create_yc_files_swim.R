instructions_stab #commes from merge_results
instructions_stab[rows == max(rows)]
instructions <- data.table(path = list.files(directory, pattern = paste0('*_', water_n, '.apsim'), recursive = TRUE, full.names = TRUE)) #all out files in one folder
instructions[,basename_f := gsub(basename(path), pattern = 'stab.apsim', replacement = 'yc')]
instructions[,id_10 := sapply(strsplit(basename_f, split="_"), "[", 1) ]
instructions[,mukey := sapply(strsplit(basename_f, split="_"), "[", 2) ]
instructions[,z := as.integer(sapply(strsplit(basename_f, split="_"), "[", 3)) ]
instructions[,type := 'yc']
instructions[,region := instructions_stab$region[1]]
instructions[,water := water_n]
instructions[,batch := batch_n]
instructions[,watertable := instructions_stab$watertable[1]]
instructions %>% setcolorder(c('region','id_10',  'mukey', 'z', 'type'))

#Clean failed soils
instructions <- merge(instructions, instructions_stab[,.(mukey,  z, rows)], by = c('mukey', 'z'))
# instructions <- instructions[rows == max(rows)]

#CREATE ALL APSIM FILES
"C:/Users/germanm2/Documents/n_management_git/Codes/simD_apsim_create_files_swim.R"
"./n_management_git/Codes/simD_apsim_create_files_swim.R"
source(paste0(codes_folder, '/n_management_git/Codes/simD_apsim_create_files_swim.R'))

