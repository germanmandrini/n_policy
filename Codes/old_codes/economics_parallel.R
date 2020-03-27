######################################
# Parallelized Simulations
######################################

#===================================
# prepare clusters
#===================================

no_cores <- detectCores() * 7/8

cl <- makeCluster(no_cores,type='SOCK')

#===================================
# parallelized simulations 
#===================================
#i =   10

calculate_value <- function(j){
  # j = 45
  print(j)
  
  library(data.table)
  library(dplyr)
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
  sapply(ic_field_dt, class)
  
  ic_field_dt <- merge(ic_field_dt, area_dt, by = 'mukey')
  
  summary(ic_field_dt[,.(area_ha = sum(area_ha)),  by = .(id_10, z, NRate)]$area_ha)
  
  #---------------------------------------------------------------------------
  # ECONOMICS
  ic_field_dt[, P := Y * Pc - NRate * Pn]
  ic_field_dt[, P_mukey := P * area_ha]
  
  # SHOW YIELD CURVES
  if(FALSE){
    mukey_n = unique(ic_field_dt$mukey)[1]  
    ggplot(data=ic_field_dt[mukey == mukey_n], aes(x = NRate, y = Y, color = z)) +
      geom_point(size = 3) +
      stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE) +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))+
      # theme_bw() +
      facet_grid(mukey~.) +  
      ggtitle(paste('Y Response for one mukey'))
    
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
  # PROFITS USING REGIONAL RECOMMENDATION
  rates <- rates_reg_dt[region == unique(ic_field_dt$region)]$NRate
  n_regional_dt <- ic_field_dt[NRate == rates]
  n_regional_dt2 <- n_regional_dt[,.(P_reg = sum(P_mukey)), by = .(z)] #total field by z
  
  #---------------------------------------------------------------------------
  # EX ANTE UR 
  rates <- ic_field_dt[,.(P = sum(P_mukey)), by =  .(NRate)][, .SD[ P == max( P)]]
  setnames(rates, 'NRate', 'eonr')
  
  eonr_ur_dt <- cbind(ic_field_dt, rates[,-'P']) %>% .[NRate == eonr] %>% .[ ,-'NRate']
  sum(eonr_ur_dt$P_mukey) == rates$P #check
  eonr_ur_dt2 <- eonr_ur_dt[,.(P_ur = sum(P_mukey)), by = .(z)] #total field by z
  
  #---------------------------------------------------------------------------
  # EX ANTE VR 
  rates <- ic_field_dt[,.(P = mean(P)), by = .(mukey, NRate)][, .SD[ P == max( P)], by = mukey] %>% 
    setnames('NRate', 'eonr') 
  eonr_vr_dt <- merge(ic_field_dt, rates[,-'P']) %>% .[NRate == eonr] %>% .[ ,-'NRate']
  eonr_vr_dt[,.(P = mean(P)), by = mukey]$P == rates$P #check
  eonr_vr_dt2 <- eonr_vr_dt[,.(P_vr = sum(P_mukey)), by = .(z)] #total field by z
  
  #---------------------------------------------------------------------------
  # MAKE A DT
  economics_field_dt <- merge(n_regional_dt2, eonr_ur_dt2) %>% merge(eonr_vr_dt2)
  economics_field_dt <- cbind(fields_seq_tmp, economics_field_dt)
  economics_field_dt[,area_ha := sum(area_dt$area_ha)]
  economics_field_dt[,val_info := P_ur - P_reg]
  economics_field_dt[,val_tech := P_vr - P_ur]
  economics_field_dt[,val_info_ha := val_info/area_ha]
  economics_field_dt[,val_tech_ha := val_tech/area_ha]
  economics_field_dt[,mukey_count := nrow(area_dt)]
  
  #---------------------------------------------------------------------------
  # MAKE BOXPLOT
  dat.m <- melt(economics_field_dt,id.vars= c('id_10', 'id_field',  'z'), measure.vars=c('val_info_ha', 'val_tech_ha'))
  if(FALSE){
    (p <- ggplot(dat.m) +
       geom_boxplot(aes(x=variable, y=value, color=variable)) +
       # scale_color_discrete(name = "REGION") +
       theme(axis.text=element_text(size=12),
             axis.title=element_text(size=14))+
       ggtitle(paste('Value of information and technology - ', fields_seq_tmp$id_10))+    
       theme_bw() +
       theme(legend.position='bottom',
             panel.grid = element_blank(),
             strip.background = element_blank(),
             legend.text.align = 0,
             legend.title = element_blank(),
             strip.text = element_blank()))
    ggsave(p, filename = "./vr_value/Data/figures/value_boxplot.jpg")
  }
  
  return(economics_field_dt)
}

keep <- c('keep', 'calculate_value', 'grid10_soils_sf4', 'fields_seq', 'interpolated_dt')

clusterExport(cl, varlist = keep, envir=environment())


results.list <- parallel::parLapply(cl, 1:nrow(fields_seq), function(x) calculate_value(x))

economics_dt <- rbindlist(results.list, fill = TRUE)

stopCluster(cl)

saveRDS(economics_dt, './vr_value/Data/files_rds/economics_dt.rds')
