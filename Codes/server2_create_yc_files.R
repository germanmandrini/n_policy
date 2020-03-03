instructions <- data.table(path = list.files(directory, pattern = '*stab.apsim', recursive = TRUE, full.names = TRUE)) #all out files in one folder
instructions[,basename_f := gsub(basename(path), pattern = 'stab.apsim', replacement = 'yc')]
instructions[,id_10 := lapply(strsplit(as.character(basename_f), split="_"), "[", 1) ]
instructions[,mukey := lapply(strsplit(as.character(basename_f), split="_"), "[", 2) ]
instructions[,z := lapply(strsplit(as.character(basename_f), split="_"), "[", 3) ]
instructions[,type := 'yc']
instructions %>% setcolorder(c('id_10',  'mukey', 'z', 'type'))

#CREATE ALL APSIM FILES
source('./vr_value_v2/Codes/apsim_create_files_nov18.R')
