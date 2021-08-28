rm(list=ls())

# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# codes_folder <-'C:/Users/germa/Documents'#Dell
setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
codes_folder <-'C:/Users/germanm2/Documents'#CPSC
# setwd('~')#Server
# codes_folder <-'~' #Server


source('./Codes_useful/R.libraries.R')
# library(scales)
source('./Codes_useful/gm_functions.R')
source(paste0(codes_folder, '/n_policy_git/Codes/parameters.R'))
"~/n_policy_git/Codes/parameters.R"

#---------------------------------------------------------------------------------------------
#Go to the training script and train a policy for the level that leads to a 20% reduction
#Then each time come here and save the varimp matrix


varimp_list <- list()
varimp_mt <- varImpPlot(dynamic, type=2)
varimp_list[[1]] <- data.table(policy = 'ratio', variable = rownames(varimp_mt), varimp_mt)

varimp_mt <- varImpPlot(dynamic, type=2)
varimp_list[[2]] <- data.table(policy = 'leach', variable = rownames(varimp_mt), varimp_mt)

varimp_mt <- varImpPlot(dynamic, type=2)
varimp_list[[3]] <- data.table(policy = 'bal', variable = rownames(varimp_mt), varimp_mt)

varimp_dt <- rbindlist(varimp_list)
# saveRDS(varimp_dt, "./n_policy_box/Data/files_rds/varimp20percent_dt.rds")
#---------------------------------------------------------------------------------------------
varimp_dt <- readRDS( "./n_policy_box/Data/files_rds/varimp20percent_dt.rds")

varimp_dt[,policy_labels := factor(policy, levels = c('ratio', 'leach', 'bal', 'red', 'base'),
                                      labels = c("Price ratio", 
                                                 "Leaching fee",
                                                 "Balance fee",
                                                 "Vol. reduction",
                                                 "Baselevel"))]
varimp_dt[, variable  := gsub(pattern = 'corn', replacement = 'maize', x = variable)]

plots_list <- list()
pol_n <- 'ratio'

for(pol_n in c('ratio', 'leach', 'bal')){
  
  # varimp_top_dt <- varimp_dt[order(policy, -IncNodePurity)][, head(.SD, 8), by=policy][order(IncNodePurity)]
  varimp_tmp_dt <- varimp_dt[policy == pol_n][order(IncNodePurity)]
  varimp_tmp_dt[, variable := factor(variable, levels = varimp_tmp_dt$variable)]
  
  options(scipen=10000)
  (p1 <- ggplot(data = varimp_tmp_dt, aes(x=variable, y=IncNodePurity)) + 
      geom_point() +
      geom_segment(aes(x=variable,xend=variable,y=0,yend=IncNodePurity)) +
      # scale_color_discrete(name="Variable Group") +
      ylab("IncNodePurity") +
      xlab("Variable Name") +
      coord_flip()+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid = element_blank(), 
            strip.placement = "outside",
            panel.spacing = unit(1.5, "lines"),
            strip.text.x = element_text(size = 13, face = "bold"),
            strip.background =element_rect(fill="white"))+
      # scale_x_continuous(labels = scales::comma)+
      facet_wrap(policy_labels~., scales = 'free'))
  
  plots_list[[pol_n]] <- p1
}

p1 <- grid.arrange(plots_list[['ratio']], plots_list[['leach']], plots_list[['bal']], nrow = 1)

ggsave(plot = p1, 
       filename = "./n_policy_box/Data/figures/varimp20percent.pdf", width = 1276/100, height = 308/100, units = 'in')
