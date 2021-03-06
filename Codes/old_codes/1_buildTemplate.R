# This script creates a template file for the factorial simulations ###################
# with an APSIM sim file with the four main soils at the Nashua, IA site ##############
# data downladed with the APssurgo tool ###############################################
# install.packages('tidyverse')
library(tidyverse)
library(apsimr)
setwd('C:/Users/germanm2/Box Sync/APSIM Workshop 2018/day 3/day 3/Advanced factorials with apsimr - Copy/')
# Read soils ##########################################################################

soils <- readLines("soils/ISU Nashua Farm.xml")
soils <- soils[c(-1,-length(soils))]
soils <- unname(split(soils, cumsum(seq_along(soils) %in% grep("Soil name=",soils))))
class(soils)

soils <- readLines('C:/Users/germanm2/Box Sync/My_Documents/vr_value/Data/APssurgo_master/APSIM_soils/Soils_vr_value.soils')
soils <- soils[c(-1,-length(soils))]
soils <- unname(split(soils, cumsum(seq_along(soils) %in% grep("Soil name=",soils))))
class(soils)
length(soils)


# Extract Name and extent 
soilNames <- gsub('  <Soil name=\"',"", 
                  gsub('% of AOI)\">',"",
                       gsub("\\(","",unlist(lapply(soils,function(x)head(x,1))))))

head(soils[[3]],1)

soilNames <- separate(data.frame(soilNames),soilNames,c("soilNames","AOI"), sep = " ")
saveRDS(soilNames,"soils.rds")

names(soils) <- soilNames[,1]

# Dump soils with small extent 
soils <- soils[which(soilNames$AOI > 10)]

# read template 
template <- readLines("template.apsim")
header <- template[1]
footer <- template[length(template)]

# Assemble template for factorials #####################################################

out <- c()
n <- length(template)

for(i in 1:length(soils)) { 
  
  simulation <- c(template[2:(n-3)],
                  soils[[i]],
                  template[(n-2):(n-1)])
  simulation <- gsub("myTemplate",paste0("myScenario_",names(soils)[i]), simulation)
  out <- c(out,simulation)
  
  }

out <- c(header,out,footer)

writeLines(out,"forFactorials.apsim")
