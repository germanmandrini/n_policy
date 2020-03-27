# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
# setwd('~')

source('./Codes_useful/R.libraries.R')
source('./vr_value/Codes/parameters.R')
source('./vr_value/Codes/interpolate_parallel.R')
# source('./Codes_useful/gm_functions.R')

grid10_soils_sf4 <- readRDS("./vr_value/Data/Grid/grid10_soils_sf4.rds")  
interpolated_dt <- readRDS('./vr_value/Data/files_rds/interpolated_dt.rds')

#----------------------------------------------------------------------------
# REGIONAL RATES (SIMILAR TO MRTN)
#----------------------------------------------------------------------------
runned <- list.files('./vr_value/Data/yc_output/', pattern = 'ic_cell', full.names = T)
# runned <- sample(runned, 100, replace = F)

if(FALSE){
  #-----------------------------------------------------------------------------------------------
  # ECONOMICS
  interpolated_dt[, P := Y * Pc - NRate * Pn]
  saveRDS(interpolated_dt, './vr_value/Data/files_rds/interpolated_dt.rds')
  
  #-----------------------------------------------------------------------------------------------
  # NO SS APPROACH
  interpolated_dt[, P_total := P * area_ha]
  no_ss_rates_dt <- interpolated_dt[, .(P_ha = sum(P_total)/sum(area_ha)), by = .(region, NRate)]
  
  (p <- ggplot(data=no_ss_rates_dt, aes(x = NRate, y = P_ha, colour = as.factor(region))) +
      geom_point(size = 3) +
      stat_smooth(se = FALSE) +
      scale_color_discrete(name = "REGION") +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14))+
      ggtitle(paste('Regional Profits - No SS aproach'))+    
      theme_bw() +
      theme(legend.position='bottom',
            panel.grid = element_blank(),
            strip.background = element_blank(),
            legend.text.align = 0,
            #legend.title = element_blank(),
            strip.text = element_blank()))
  
  ggsave(p, filename = "./vr_value/Data/figures/regional_rates.jpg")
  
  rates_no_ss_dt <- no_ss_rates_dt[, .SD[ P_ha == max(P_ha)], by = region][,-'P_ha']
  
  #-----------------------------------------------------------------------------------------------
  # SS APPROACH
  eonr_byset_dt <- interpolated_dt[, .SD[ P == max( P)], by = .(id_10, mukey,  z)]
  setnames(eonr_byset_dt, 'NRate', 'eonr')
  
  # REGRESSIONS!!!
  #FIND THE LINEAR MODEL THAT RELATES EONR WITH n_40ss
  #Show one set
  eonr_byset_dt <- eonr_byset_dt[n_40cm < 80] #remove outliers
  
  ggplot(eonr_byset_dt[mukey == '1949587'], aes(x = n_40cm, y = eonr)) + 
    geom_point() + 
    stat_smooth(method = "lm", formula = y ~ x, size = 1, se = FALSE)
  
  ggplot(eonr_byset_dt[region == 2], aes(x = n_40cm, y = eonr)) + 
    geom_point(aes(colour = mukey)) + 
    stat_smooth(method = "lm", formula = y ~ x, size = 1, se = FALSE, aes(colour = mukey))+
    stat_smooth(method = "lm", formula = y ~ x, size = 2, se = FALSE) +
    theme(legend.position='none')
  
  reg_n40_1 <- lm(eonr ~ n_40cm + as.factor(mukey), data = eonr_byset_dt[region == 1])
  
  reg_n40_2 <- lm(eonr ~ n_40cm + as.factor(mukey), data = eonr_byset_dt[region == 2])
   
  reg_n40_3 <- lm(eonr ~ n_40cm + as.factor(mukey), data = eonr_byset_dt[region == 3])
  
  rates_yes_ss_dt <- data.table(rbind(reg_n40_2$coefficients[1:2], reg_n40_3$coefficients[1:2]), region = 2:3)
  
  rates_reg_ls <- list(no_ss = rates_no_ss_dt, ss = rates_yes_ss_dt)
  
  saveRDS(rates_reg_ls, './vr_value/Data/files_rds/rates_reg_ls.rds')
  
}
rates_reg_ls <- readRDS('./vr_value/Data/files_rds/rates_reg_ls.rds')


interpolated_dt$sets <- NULL

#----------------------------------------------------------------------------
#DO A MAP OF THE VARIABILITY OF EONR
id_10_runned <- as.numeric(unlist(str_extract_all(runned, pattern = '[0-9]+')))
fields_seq <- data.table(grid10_soils_sf4) %>% .[id_10 %in% id_10_runned, c('id_10', 'id_field')] %>% unique()

# ECONOMICS
interpolated_dt[, P := Y * Pc - NRate * Pn]
interpolated_dt[, P_gross := P * area_ha]
interpolated_dt[, n_40cm_gross := n_40cm * area_ha]
interpolated_dt[, leach_no3_ss_gross := leach_no3_ss * area_ha]
interpolated_dt[, Y_gross := Y * area_ha]

#FIND THE RATE THAT MAXIMIZES PROFITS FOR THE CELL
eonr_cell_dt <- interpolated_dt[,.(P = sum(P_gross),
                                   Y = sum(Y_gross),
                                 leach_no3_ss = sum(leach_no3_ss_gross),
                                 n_40cm = sum(n_40cm_gross),
                                 area_ha = sum(area_ha)), by =  .(id_10, z, NRate)]

summary(eonr_cell_dt$area_ha)

eonr_cell_dt2 <- eonr_cell_dt[, .SD[ P == max( P)], by = .(id_10, z)]
setnames(eonr_cell_dt2, 'NRate', 'eonr')

#Convert to ha
eonr_cell_dt2[,P := P/area_ha]
eonr_cell_dt2[,Y := Y/area_ha]
eonr_cell_dt2[,leach_no3_ss := leach_no3_ss/area_ha]
eonr_cell_dt2[,n_40cm := n_40cm/area_ha]

#Summarize variables of interest
eonr_map_dt <- eonr_cell_dt2[, .(eonr = mean(eonr),
                  eonr_sd = sd(eonr),
                  P = mean( P),
                  Y = mean( Y),
                  leach_no3_ss = mean(leach_no3_ss),
                  n_40cm = mean(n_40cm)), by = id_10]


# MAKE A MAP OF EX POST EONR AND SD
grid10_eonr_sf <- left_join(grid10_tiles_sf, eonr_map_dt, by = 'id_10')

(p <- tm_shape(grid10_eonr_sf) + tm_polygons(c('eonr','eonr_sd'))+
    tm_layout(legend.text.size = 0.7,
              main.title = paste('EX POST EONR AND SD'),
              main.title.position = "center",
              main.title.size = 1.2))

tmap_save(p, "./vr_value/Data/figures/eonr_expost.jpg")

# MAKE A MAP OF EX POST P
(p <- tm_shape(grid10_eonr_sf) + tm_polygons(c('Y', 'P'))+
    tm_layout(legend.text.size = 0.7,
              main.title = paste('Y-EONR AND P-EONR'),
              main.title.position = "center",
              main.title.size = 1.2))

tmap_save(p, "./vr_value/Data/figures/yeonr_expost.jpg")


#----------------------------------------------------------------------------
id_10_runned <- as.numeric(unlist(str_extract_all(runned, pattern = '[0-9]+')))
fields_seq <- data.table(grid10_soils_sf4) %>% .[id_10 %in% id_10_runned, c('id_10', 'id_field')] %>% unique()

economics_ls <- list()

for(j in 1:nrow(fields_seq)){
  # j = 99
  print(j)
  fields_seq_tmp <- fields_seq[j,]
  one_field_sf <- grid10_soils_sf4[grid10_soils_sf4$id_10 == fields_seq_tmp$id_10 &
                                     grid10_soils_sf4$id_field == fields_seq_tmp$id_field,]
  
  area_dt <- data.table(one_field_sf) %>% .[,.(area_ha = sum(area_ha)), by = mukey]
  
  #---------------------------------------------------------------------------
  #MAKE A MAP OF THE FIELD
  if(FALSE){
  (field <- tm_shape(one_field_sf) + tm_polygons("mukey") + 
    tm_layout(legend.text.size = 0.7,
              main.title = paste('ONE FIELD MAP -', round(sum(one_field_sf$area_ha),1),' ha'),
              main.title.position = "center",
              main.title.size = 1))
  tmap_save(field, filename = "./vr_value/Data/figures/field.jpg", scale = 2)  
  }
  #---------------------------------------------------------------------------
  # LOAD OUTPUT
  ic_field_dt <- interpolated_dt[id_10 == fields_seq_tmp$id_10 & mukey %in% unique(one_field_sf$mukey)]
  
  ic_field_dt <- merge(ic_field_dt[,-c('area_ha')], area_dt, by = 'mukey')
  
  summary(ic_field_dt[,.(area_ha = sum(area_ha)),  by = .(id_10, z, NRate)]$area_ha)
  
  #---------------------------------------------------------------------------
  # ECONOMICS
  ic_field_dt[, P := Y * Pc - NRate * Pn]
  ic_field_dt[, P_gross := P * area_ha]
  ic_field_dt[, n_40cm_gross := n_40cm * area_ha]
  ic_field_dt[, leach_no3_ss_gross := leach_no3_ss * area_ha]
  # SHOW YIELD CURVES
  if(FALSE){
    mukey_n = unique(ic_field_dt$mukey)[1]  
  (p <- ggplot(data=ic_field_dt[mukey == mukey_n], aes(x = NRate, y = Y, color = z)) +
    geom_point(size = 1) +
    #stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE) +
      theme_bw() +
      theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"),
          legend.position='bottom')+
    facet_grid(mukey~.) +  
    ggtitle(paste('Y Response for one mukey')))
  
  ggsave(grid.arrange(p), filename = "./vr_value/Data/figures/response_one_mukey1.jpg")
    
  (p1 <- ggplot(data=ic_field_dt[mukey == mukey_n], aes(x = NRate, y = Y)) +
      #geom_point(aes(color = z), size = 3) +
      #stat_smooth(aes(color = z), method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE) +
      geom_line(aes(color = z))+
      stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 3, se = FALSE) +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))+
      theme_bw() +
      facet_grid(mukey~.) +  
      theme(#legend.position='bottom',
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.text.align = 0,
        #legend.title = element_blank(),
        strip.text = element_blank())+
      ggtitle(paste('Y Response for one mukey')))
  
  (p2 <- ggplot(data=ic_field_dt[mukey == mukey_n], aes(x = NRate, y = P)) +
    # geom_point(aes(color = z), size = 3) +
    # stat_smooth(aes(color = z), method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE) +
      geom_line(aes(color = z))+
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 3, se = FALSE) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"))+
    theme_bw() +
    facet_grid(mukey~.) +  
    theme(#legend.position='bottom',
          panel.grid = element_blank(),
          strip.background = element_blank(),
          legend.text.align = 0,
          #legend.title = element_blank(),
          strip.text = element_blank())+
    ggtitle(paste('Profits Response for one mukey')))
  
  
  ggsave(grid.arrange(p1, p2), filename = "./vr_value/Data/figures/response_one_mukey.jpg")
  
  
  ggplot(data=ic_field_dt, aes(x = NRate, y = Y, color = mukey)) +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE) +
    theme_bw() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"),
          panel.grid = element_blank(),
          strip.background = element_blank(),
          legend.text.align = 0,
          legend.position='bottom',
          strip.text = element_blank())+
    ggtitle(paste('Summarized Y Response by mukey'))
  
  
  p3 <- ggplot(data=ic_field_dt, aes(x = NRate, y = Y, color = mukey)) +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE) +
    theme_bw() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"),
          panel.grid = element_blank(),
          strip.background = element_blank(),
          legend.text.align = 0,
          legend.position='bottom',
          strip.text = element_blank())+
    ggtitle(paste('Summarized Y Response by mukey'))
  
  ggsave(p3, filename = "./vr_value/Data/figures/response_all_mukey_averaged_for_z.jpg")
  }
  #---------------------------------------------------------------------------
  # PROFITS USING REGIONAL RECOMMENDATION W/O SS
  
  rates <- rates_reg_ls[['no_ss']][region == unique(ic_field_dt$region)]$NRate
  n_regional_noss_dt <- ic_field_dt[NRate == rates]
  n_regional_noss_dt2 <- n_regional_noss_dt[,.(P_reg_no_ss = sum(P_gross),
                                               nleach_reg_no_ss = sum(leach_no3_ss_gross)), by = .(z)] #total field by z
  mean(n_regional_noss_dt2$P_reg_no_ss)
  #---------------------------------------------------------------------------
  # PROFITS USING REGIONAL RECOMMENDATION W SS
  
  rates <- rates_reg_ls[['ss']][region == unique(ic_field_dt$region)]
  n_regional_ss_dt <- ic_field_dt[, .(P_reg_ss = sum(P_gross),
                                      nleach_reg_ss = sum(leach_no3_ss_gross),
                                      n_40cm = sum(n_40cm_gross) / sum(area_ha)), by = .(z, NRate)]
  
  
  n_regional_ss_dt[,N_rec := round(rates$`(Intercept)` + rates$n_40cm * n_40cm,0)]
  n_regional_ss_dt[,N_rec := ifelse(N_rec < min(ic_field_dt$NRate) , min(ic_field_dt$NRate), N_rec)]
  n_regional_ss_dt2 <- n_regional_ss_dt[NRate == N_rec, .(z, P_reg_ss, nleach_reg_ss)]#total field by z
  
  mean(n_regional_ss_dt2$P_reg_ss)
  
  #---------------------------------------------------------------------------
  # EX ANTE UR 
  rates <- ic_field_dt[,.(P = sum(P_gross)), by =  .(NRate)][, .SD[ P == max( P)]]
  setnames(rates, 'NRate', 'eonr')
  
  eonr_ur_dt <- cbind(ic_field_dt, rates[,-'P']) %>% .[NRate == eonr] %>% .[ ,-'NRate']
  sum(eonr_ur_dt$P_gross) == rates$P #check
  eonr_ur_dt2 <- eonr_ur_dt[,.(P_ur = sum(P_gross),
                            nleach_ur = sum(leach_no3_ss_gross)), by = .(z)] #total field by z
  mean(eonr_ur_dt2$P_ur)
  #---------------------------------------------------------------------------
  # EX ANTE UR W SS
  # 
  # eonr_dt <- ic_field_dt[,.(P = sum(P_mukey),
  #                         n_40cm = sum(n_40cm_gross)/sum(area_ha)), by =  .(z, NRate)][, .SD[ P == max( P)], by = .(z)]
  # setnames(eonr_dt, 'NRate', 'eonr')
  # eonr_dt <- eonr_dt[n_40cm < 80] #remove outliers
  # 
  # reg_n40_3 <- lm(eonr ~ n_40cm, data = eonr_dt)
  # 
  # rates_yes_ss_dt <- data.table(rbind(reg_n40_2$coefficients[1:2], reg_n40_3$coefficients[1:2]), region = 2:3) 
  # 
  # eonr_ur_dt <- cbind(ic_field_dt, rates[,-'P']) %>% .[NRate == eonr] %>% .[ ,-'NRate']
  # sum(eonr_ur_dt$P_mukey) == rates$P #check
  # eonr_ur_dt2 <- eonr_ur_dt[,.(P_ur = sum(P_mukey)), by = .(z)] #total field by z
  #  rates_yes_ss_dt <- data.table(rbind(reg_n40_2$coefficients[1:2], reg_n40_3$coefficients[1:2]), region = 2:3)
  
  #---------------------------------------------------------------------------
  # EX ANTE VR 
  rates <- ic_field_dt[,.(P = mean(P)), by = .(mukey, NRate)][, .SD[ P == max( P)], by = mukey] %>% 
    setnames('NRate', 'eonr') 
  eonr_vr_dt <- merge(ic_field_dt, rates[,-'P']) %>% .[NRate == eonr] %>% .[ ,-'NRate']
  eonr_vr_dt[,.(P = mean(P)), by = mukey]$P == rates$P #check
  eonr_vr_dt2 <- eonr_vr_dt[,.(P_vr = sum(P_gross),
                               nleach_vr = sum(leach_no3_ss_gross)), by = .(z)] #total field by z
  mean(eonr_vr_dt2$P_vr)
  #---------------------------------------------------------------------------
  # MAKE A DT
  economics_field_dt <- merge(n_regional_noss_dt2, n_regional_ss_dt2) %>% merge(eonr_ur_dt2) %>% merge(eonr_vr_dt2)
  economics_field_dt[,area_ha := sum(area_dt$area_ha)]
  economics_field_dt[,val_ss_ha := (P_reg_ss - P_reg_no_ss)/area_ha]
  economics_field_dt[,val_info_ha := (P_ur - P_reg_no_ss)/area_ha]
  economics_field_dt[,val_tech_ha := (P_vr - P_ur)/area_ha]
  
  economics_field_dt[,nval_ss_ha := (nleach_reg_ss - nleach_reg_no_ss)/area_ha]
  economics_field_dt[,nval_info_ha := (nleach_ur - nleach_reg_no_ss)/area_ha]
  economics_field_dt[,nval_tech_ha := (nleach_vr - nleach_ur)/area_ha]
  
  cols <- names(economics_field_dt)[sapply(economics_field_dt,is.numeric)]
  economics_field_dt[,(cols) := round(.SD,3), .SDcols=cols]
  
  
  economics_field_dt <- cbind(fields_seq_tmp, economics_field_dt)
  economics_field_dt[,mukey_count := nrow(area_dt)]
  
  economics_ls[[j]] <- economics_field_dt
}

economics_dt <- rbindlist(economics_ls)
saveRDS(economics_dt, './vr_value/Data/files_rds/economics_dt.rds')

#---------------------------------------------------------------------------
# MAKE A MAP OF VALUE OF I AND T
val_map_dt <- economics_dt[,.(val_ss_ha = mean(val_ss_ha), 
                              val_info_ha = mean(val_info_ha),
                              val_tech_ha = mean(val_tech_ha)), by = id_10]

grid10_value_sf <- left_join(grid10_tiles_sf, val_map_dt, by = 'id_10')

(p <- tm_shape(grid10_value_sf) + tm_polygons(c('val_ss_ha','val_info_ha', 'val_tech_ha'))+
  tm_layout(legend.text.size = 0.7,
            main.title = paste('VALUE OF TECHNOLOGY AND INFORMATION'),
            main.title.position = "center",
            main.title.size = 1.2))

tmap_save(p, "./vr_value/Data/figures/value_t_i.jpg")
#---------------------------------------------------------------------------
# MAKE A MAP OF VALUE OF I AND T FOR LEACHING
val_map_dt <- economics_dt[,.(nval_ss_ha = mean(nval_ss_ha), 
                              nval_info_ha = mean(nval_info_ha),
                              nval_tech_ha = mean(nval_tech_ha)), by = id_10]

grid10_value_sf <- left_join(grid10_tiles_sf, val_map_dt, by = 'id_10')

(p <- tm_shape(grid10_value_sf) + tm_polygons(c('nval_ss_ha','nval_info_ha', 'nval_tech_ha'))+
    tm_layout(legend.text.size = 0.7,
              main.title = paste('VALUE OF TECHNOLOGY AND INFORMATION'),
              main.title.position = "center",
              main.title.size = 1.2))

tmap_save(p, "./vr_value/Data/figures/nvalue_t_i.jpg")
#---------------------------------------------------------------------------
# MAKE A MAP OF PROFITS
economics_dt
prof_map_dt <- economics_dt[,P_ur_ha := P_ur / area_ha]
prof_map_dt <- prof_map_dt[,.(P_ur_ha = mean(P_ur_ha)), by = id_10]

grid10_value_sf <- left_join(grid10_value_sf, prof_map_dt, by = 'id_10')

(p <- tm_shape(grid10_value_sf) + tm_polygons('P_ur_ha')+
    tm_layout(legend.text.size = 0.7,
              main.title = paste('AVERAGE UR PROFITS $/HA'),
              main.title.position = "center",
              main.title.size = 1.2))

tmap_save(p, "./vr_value/Data/figures/profits.jpg")


#---------------------------------------------------------------------------
# MAKE BOXPLOT
dat.m <- melt(economics_dt,id.vars= c('id_10', 'id_field',  'z'), measure.vars=c('val_ss_ha','val_info_ha', 'val_tech_ha'))
(p <- ggplot(dat.m) +
    geom_boxplot(aes(x=variable, y=value, color=variable)) +
    # scale_color_discrete(name = "REGION") +
    ggtitle('Value of information and technology')+    
    theme_bw() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          #legend.position='bottom',
          panel.grid = element_blank(),
          strip.background = element_blank(),
          legend.text.align = 0,
          legend.position = "none",
          legend.title = element_blank(),
          strip.text = element_blank()))

ggsave(p, filename = "./vr_value/Data/figures/value_boxplot.jpg")
