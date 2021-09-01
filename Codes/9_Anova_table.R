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
theme_set(theme_bw())  # pre-set the bw theme.
#--------------------------------------------------------------------------------
# Prepare data

field_perfomances_dt <- readRDS("./n_policy_box/Data/files_rds/field_perfomances_dt.rds")

#--------------------------------------------------------------------------------
# STATS method
# DATA AT FIELD X Z LEVEL CONSIDERING THE AREA
library(lme4)
# install.packages('emmeans')
library(emmeans)
# library(lsmeans)
library(car)
library(multcomp)

str(field_perfomances_dt)


#=====================================================================================================================
# Table of state results
field_perfomances_dt$NRT %>% unique()


state_dt <- field_perfomances_dt[, 
                                 .(Y_corn =  mean(Y_corn),
                                   L = mean(L),
                                   N_fert = mean(N_fert),
                                   N_fert_min = min(N_fert),
                                   N_fert_max = max(N_fert),
                                   P = mean(P),
                                   r2 = cor(N_fert, eonr_12)^2,
                                   # cor = cor(N_fert, eonr_12),
                                   ME = mean(N_fert-eonr_12),
                                   MAE = mean(abs(N_fert-eonr_12)),
                                   RMSE = mlr::measureRMSE(truth = eonr_12, response = N_fert),
                                   overpred = sum(overpred)/.N*100,
                                   subpred = sum(subpred)/.N*100,
                                   exact = sum(exact)/.N*100), by = .(NRT)][order(-P)]#[order(lag,-P)]

baselevel_L <- state_dt[ NRT == 'mrtn_peak', L]
state_dt[,L_change := round((L / baselevel_L) - 1,3)*100 ]
# state_dt[,P_kgL := P/L]
cols <- names(state_dt)[sapply(state_dt,is.numeric)]
cols <- cols[!cols %in% c('r2', 'L')]
state_dt[,(cols) := round(.SD,0), .SDcols=cols]
state_dt[,r2 := round(r2,2)]
state_dt[,L := round(L,1)]
state_dt[order(-P)]
state_plot_dt <- state_dt[order(-P)]

png("./n_policy_box/Data/figures/state_lag0_table.png", width = 900, height = 480,units = "px")
p<-tableGrob(state_plot_dt)
grid.arrange(p, top = textGrob("State summary",gp=gpar(fontsize=20,font=3)))
dev.off()
saveRDS(state_plot_dt, './n_policy_box/Data/files_rds/state_plot_dt.rds')
#=====================================================================================================================
#Stats

field_perfomances_dt <- readRDS("./n_policy_box/Data/files_rds/field_perfomances_dt.rds")

percent20_dt <- readRDS("./n_policy_box/Data/files_rds/percent20_dt.rds")

percent20_dt <- percent20_dt[NRT == 'dynamic' & policy_name %in% policies_paper,.(policy, NRT)]
percent20_dt <- rbind(percent20_dt, data.table(policy = 'ratio_5', NRT = 'dynamic')) #add baselevel

# perfomances_dt5[,region_eq := 'state']
# region20_dt <- rbind(perfomances_dt4, perfomances_dt5, fill = T)%>% 
#   .[policy_name %in% policies_paper & NRT %in% c('dynamic')]

region20_dt2 <- filter_dt_in_dt(field_perfomances_dt, filter_dt = percent20_dt, return_table = T)
region20_dt2[, c("policy_name", "policy_val") := tstrsplit(policy, "_", fixed=TRUE)]
region20_dt2[policy =='ratio_5', policy_name := 'base-level']
region20_dt2[policy =='ratio_5', policy_val := 0]


#=============================================================================================================================================
# ---------
# Add baselevel information
baselevel_dt <- region20_dt2[policy == 'ratio_5' & NRT == 'dynamic', 
                                      .(region_eq, id_10, id_field, z,
                                        L_base = L, 
                                        P_base = P
                                        # N_base = N_fert, 
                                        # Y_base = Y_corn
                                        )]

region20_dt3 <- merge(region20_dt2, 
                               baselevel_dt, by = c('region_eq','id_10', 'id_field', 'z'))



region20_dt3[,policy_cost := P_base - P  - G]
region20_dt3[,abatement_cost := policy_cost/(L_base -  L)]
region20_dt3[is.na(abatement_cost), abatement_cost := 0]


region20_dt3[,policy_name  := factor(policy_name )]
region20_dt3[,z := factor(z)]
region20_dt3[,id_10 := factor(id_10)]
region20_dt3[,id_field := factor(id_field)]



if(TRUE){
  NRTs_letters_list <- list() 
  region_n = 'state'
  var_n = 'policy_cost'
  for(region_n in c('state', '1-South', '2-Central', '3-North')){
    for(var_n in c("Y_corn", "L", "N_fert","P", "G","policy_cost", "abatement_cost")){ #c("Yld", "leach_n2", "N_fert","P")
      print(var_n)
      # formula_tmp <- paste(var_n,  '~ NRT * (1|id_10) * (1|z)')
      # formula_tmp <- paste(var_n,  '~ NRT + (1|id_10) + (1|z) + (1|id_10:id_field) + (1|id_10:z) + (1|id_10:id_field:z)')
      # formula_tmp <- paste(var_n,  '~ NRT + (1|id_10) + (1| id_10:id_field) + (1|id_10:z)')
      # formula_tmp <- paste(var_n,  '~ NRT + (1| z/id_10/id_field)')
      formula_tmp <- paste(var_n,  '~ policy_name + (1| z) + (1| z:id_10)+ (1| z:id_10:id_field)')
      if(region_n == 'state'){
        data_tmp = region20_dt3
      }else{
        data_tmp = region20_dt3[region_eq == region_n]
      }
      mixed_lmer <- lmer(as.formula(formula_tmp), data = data_tmp)
      summary(mixed_lmer)
      Anova(mixed_lmer)
      # confint(mixed_lmer)
      # 
      # install.packages('lmerTest')
      # library(lmerTest)
      # ranova(mixed_lmer)
      # VarCorr(mixed_lmer)
      
      #LSD comparisson
      
      ### specify all pair-wise comparisons among levels of variable "tension"
      
      tuk2 <- multcomp ::glht(mixed_lmer, linfct = mcp(policy_name = "Tukey")) #From: https://www.rdocumentation.org/packages/multcomp/versions/1.4-15/topics/cld
      
      ### extract information
      NRTs_letters <-  multcomp::cld(tuk2, #emmeans::CLD
                                     level =0.01, 
                                     decreasing = T,
                                     Letters=letters,      ### Use lower-case letters for .group
                                     adjust="tukey")       ### Tukey-adjusted comparisons 
      
      small_list <- list(NRT = mixed_lmer, 
                         letters = NRTs_letters)
      
      NRTs_letters_list[[var_n]][[region_n]] <- small_list
      
    }
  }
  
  saveRDS(NRTs_letters_list, './n_policy_box/Data/files_rds/NRTs_letters_list.rds')
}



NRTs_letters_list <- readRDS('./n_policy_box/Data/files_rds/NRTs_letters_list.rds')
names(NRTs_letters_list)

#Is the model significant for all variables?
Anova(NRTs_letters_list[['Y_corn']][['NRT']])
Anova(NRTs_letters_list[['L']][['NRT']])
Anova(NRTs_letters_list[['N_fert']][['NRT']])
Anova(NRTs_letters_list[['P']][['NRT']])
Anova(NRTs_letters_list[['G']][['NRT']])
Anova(NRTs_letters_list[['policy_cost']][['NRT']])



summary(NRTs_letters_list[['P']][['NRT']])
NRTs_letters_list[['P']]$letters

policies_list <- as.character(unique(region20_dt3$policy_name))
letters_all <- data.table(policy_name = policies_list)

for(var_n in c("Y_corn", "L", "N_fert","P", "G","policy_cost")){
  # var_n = 'Y_corn'
  letters_tmp <- NRTs_letters_list[[var_n]]$letters
  
  effects_tmp <- summary(NRTs_letters_list[[var_n]]$NRT)
  effects_values <- effects_tmp$coefficients[,1]
  
  #Add the intercept to the other effect
  intercept_val <- as.numeric(effects_values[1])
  
  for(i in 2:length(effects_values)){
    effects_values[i] <- effects_values[i]+intercept_val
  }
  
  # Fix the names
  effects_names <- gsub(names(effects_values), pattern = 'policy_name', replacement = '')
  effects_names[1] <-   policies_list[!policies_list %in% effects_names]
  names(effects_values) <- effects_names
  
  #Combine effects and letters
  effects_dt <- data.table(policy_name = names(effects_values), value = round(effects_values,1))
  letter_dt <- data.table(policy_name = names(letters_tmp$mcletters$monospacedLetters), 
                          letter = gsub(x = letters_tmp$mcletters$monospacedLetters, pattern = ' ',''))
  
  
  letters_tmp2 <- merge(effects_dt, letter_dt, by = 'policy_name')
  setnames(letters_tmp2, c('value', 'letter'), c(var_n, paste(var_n, 'group', sep = '_')))
  letters_all <- merge(letters_all, letters_tmp2, by = 'policy_name')
}

setnames(letters_all, c('Y_corn_group', 'L_group',  'N_fert_group', 'P_group', 'policy_cost_group'), 
         c('g1', 'g2', 'g3', 'g4', 'g5'))

#-------------------------------------------------------------------------------------
#Stats with info, tech, origin and model as factors





#-------------------------------------------------------------------------------------
# # RMSE
# ex_post_rate_dt <- perfomances_dt[NRT == 12, .(id_10, mukey, z, id_field, N_fert_12 = N_fert)]
# rmse_dt <- merge(perfomances_dt[,-'N_fert_12'], ex_post_rate_dt, by = c('id_10', 'mukey', 'z', 'id_field'))
# rmse_dt[,res := N_fert - N_fert_12]
# rmse_dt[,res := ifelse(res <0, res^2, res)]
# 
# rmse_dt2 <- rmse_dt[ ,.(cor = cor(N_fert_12, N_fert),
#                         RMSE = round(mlr::measureRMSE(truth = N_fert_12, response = N_fert),1),
#                         MAE = mlr::measureMAE(truth = N_fert_12, response = N_fert),
#                         RMSE_MAE = sqrt(mean(res)) ), by = .( NRT)][order(NRT)]
# 
# 
# rmse_dt[,NRT := as.integer(NRT)]
# str(rmse_dt)
#-------------------------------------------------------------------------------------
# Combine all in a table. It has results at state level aggregated by corn_ha, but letters are with a mixed models at field level and RMSE is with the N_fert_12
state_plot_dt <- readRDS('./n_policy_box/Data/files_rds/state_plot_dt.rds')

stats_table_dt <- merge(state_plot_dt, letters_all[,.(NRT, g1, g2, g3, g4)], by = 'NRT') %>% .[order(-P)]
setcolorder(stats_table_dt, neworder = c("NRT", "Y_corn", "g1", "L", "g2", "P", "g4" , "N_fert", "g3"))

setcolorder(region_dt, neworder = c("NRT", "Y_corn", "L", "P", "N_fert", 'N_fert_min', 'N_fert_max', 'ME', 'MAE', 'RMSE', 'r2','overpred', 'subpred'))



if(FALSE){ #ratio20
  state_plot_dt$L_change <- NULL
  setcolorder(stats_table_dt, neworder = c("NRT", "Y_corn", "g1", "L", "g2", "P", "g4" , "N_fert", "g3"))
  setnames(stats_table_dt, c('Y_corn', 'L_change', 'N_fert', 'P'),
           c('Yield (kg/ha)', 'N leaching (%change)', 'N rate (kg/ha)', 'Profits ($/ha)'), skip_absent=TRUE)
  
  
}

p<-tableGrob(stats_table_dt[order(-P)])
grid.arrange(p, top = textGrob("State summary",gp=gpar(fontsize=20,font=3)))

setnames(stats_table_dt, c('Y_corn', 'L_change', 'N_fert', 'P'),
         c('Yield (kg/ha)', 'N leaching (%change)', 'N rate (kg/ha)', 'Profits ($/ha)'))

write.csv(stats_table_dt, "./n_policy_box/Data/figures/stats_results.csv")

library(xtable)
print(xtable(stats_table_dt,
             type = "latex", auto = TRUE, label = 'tab:stats_results', 
             caption ='State averages of Yield, N leaching, N rate and Profits considering the area of corn on each cell (with eq. \ref(eq_I_units_ha)).
                      Different letters show significant differences for each variable for the mixed models ($p value < 0.01$). 
                      RMSE was calculated using eq. ref{eq_rmse}'
), file = "./n_policy_box/Data/figures/state_results.tex", include.rownames=FALSE, scalebox = 0.7)