instructions_stab <- instructions
instructions <- data.table(path = list.files(directory, pattern = '*stab.apsim', recursive = TRUE, full.names = TRUE)) #all out files in one folder
instructions[,basename_f := gsub(basename(path), pattern = 'stab.apsim', replacement = 'yc')]
instructions[,id_10 := lapply(strsplit(as.character(basename_f), split="_"), "[", 1) ]
instructions[,mukey := lapply(strsplit(as.character(basename_f), split="_"), "[", 2) ]
instructions[,z := lapply(strsplit(as.character(basename_f), split="_"), "[", 3) ]
instructions[,type := 'yc']
instructions[,region := instructions_stab$region]
instructions[,batch := batch_n]

instructions %>% setcolorder(c('region','id_10',  'mukey', 'z', 'type'))


#CREATE ALL APSIM FILES
"C:/Users/germanm2/Documents/n_policy_git/Codes/apsim_create_files_jul15.R"
"./n_policy_git/Codes/apsim_create_files_jul15.R"
source(paste0(codes_folder, '/n_policy_git/Codes/apsim_create_files_jul15.R'))

