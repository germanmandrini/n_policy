# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
# setwd('~')

source('./Codes_useful/R.libraries.R')
source('./vr_value/Codes/parameters.R')

grid10_soils_sf5 <- readRDS("./vr_value/Data/Grid/grid10_soils_sf5.rds") 
#----------------------------------------------------------------------------
# PROCESS BY BATCH
runned <- list.files('./vr_value/Data/yc_output_yearly/', pattern = 'ic_cell', full.names = T)
id_10_runned <- as.numeric(unlist(str_extract_all(runned, pattern = '[0-9]+')))

economics_ls <- list()

for(reg_n in 1:3){
  # reg_n = 1
  print(reg_n)
  # LOAD CLEANED DATA
  
  interpolated_dt <- readRDS(paste0('./vr_value/Data/files_rds/interpolated_', reg_n, '_dt.rds'))
  
  fields_seq <- data.table(grid10_soils_sf5) %>% 
    .[id_10 %in% unique(interpolated_dt$id_10), c('id_10', 'id_field')] %>% unique()
  
  for(j in 1:nrow(fields_seq)){
    # j = 150
    print(j)
    fields_seq_tmp <- fields_seq[j,]
    one_field_sf <- grid10_soils_sf5[grid10_soils_sf5$id_10 == fields_seq_tmp$id_10 &
                                       grid10_soils_sf5$id_field == fields_seq_tmp$id_field,]
    
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
    # FILTER FIELD DATA
    ic_field_dt <- interpolated_dt[id_10 == fields_seq_tmp$id_10 & mukey %in% unique(one_field_sf$mukey)]
    
    ic_field_dt <- merge(ic_field_dt[,-c('area_ha', 'P_gross',  'Y_gross', 'n_40cm_gross', 'leach_no3_ss_gross')], area_dt, by = 'mukey')
    
    summary(ic_field_dt[,.(area_ha = sum(area_ha)),  by = .(id_10, z, NRate)]$area_ha)
    
    #---------------------------------------------------------------------------
    # RECALCULTATE ECONOMICS (AREAS ARE DIFFERENT)
    
    ic_field_dt[, Y_gross := Y * area_ha]
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
      
      
      (p3 <- ggplot(data=ic_field_dt, aes(x = NRate, y = Y, color = mukey)) +
        stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE) +
        theme_bw() +
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=14,face="bold"),
              panel.grid = element_blank(),
              strip.background = element_blank(),
              legend.text.align = 0,
              legend.position='bottom',
              strip.text = element_blank())+
        ggtitle(paste('Summarized Y Response by mukey')))
      
      ggsave(p3, filename = "./vr_value/Data/figures/response_all_mukey_averaged_for_z.jpg")
    }
    #---------------------------------------------------------------------------
    # LOAD THE REGRESSIONS
    rates_reg_dt <- readRDS(paste0('./vr_value/Data/files_rds/rates_reg', reg_n,'_ls.rds'))
    
    #---------------------------------------------------------------------------
    # PROFITS USING REGIONAL RECOMMENDATION W/O SS
    
    rates <- rates_reg_dt[method == 'no_ss']$`(Intercept)`
    n_regional_noss_ur_dt <- ic_field_dt[NRate == rates]
    n_regional_noss_ur_dt2 <- n_regional_noss_dt[,.(P_reg_noss_ur = sum(P_gross),
                                                 nleach_reg_noss_ur = sum(leach_no3_ss_gross)), by = .(z)] #total field by z
    mean(n_regional_noss_ur_dt2$P_reg_noss_ur)
    #---------------------------------------------------------------------------
    # PROFITS USING REGIONAL RECOMMENDATION W SS
    
    rates <- rates_reg_dt[method == 'ss']
    field_averaged_dt <- ic_field_dt[, .(P_reg_ss_ur = sum(P_gross),
                                        nleach_reg_ss_ur = sum(leach_no3_ss_gross),
                                        n_40cm = sum(n_40cm_gross) / sum(area_ha)), by = .(z, NRate)]
    
    
    field_averaged_dt[,N_rec := round((rates$`(Intercept)` + rates$n_40cm * n_40cm)/5,0)*5]
    field_averaged_dt[,N_rec := ifelse(N_rec < min(ic_field_dt$NRate) , min(ic_field_dt$NRate), N_rec)]
    n_regional_ss_dt2 <- field_averaged_dt[NRate == N_rec, .(z, P_reg_ss_ur, nleach_reg_ss_ur)]#total field by z
    
    mean(n_regional_ss_dt2$P_reg_ss_ur)
    
    #---------------------------------------------------------------------------
    # # PROFITS USING LOCAL NOSS UR 
    rates <- ic_field_dt[,.(P = sum(P_gross)), by =  .(NRate)][, .SD[ P == max( P)]]
    setnames(rates, 'NRate', 'eonr')
    
    eonr_ur_dt <- cbind(ic_field_dt, rates[,-'P']) %>% .[NRate == eonr] %>% .[ ,-'NRate']
    sum(eonr_ur_dt$P_gross) == rates$P #check
    eonr_loc_noss_ur_dt2 <- eonr_ur_dt[,.(P_loc_noss_ur = sum(P_gross),
                                 nleach_loc_noss_ur = sum(leach_no3_ss_gross)), by = .(z)] #total field by z
    mean(eonr_ur_dt2$P_ur)
    
    
    #---------------------------------------------------------------------------
    # PROFITS USING LOCAL SS UR   BINNING: it is better than a regression by soil, because it will weight more soils with largest area. 
    # in the regression, each soil is a rep, with the same weight. Values obtained are smaller than expected.
    
    
    ic_field_dt[, n_40cm_bin := .bincode(ic_field_dt$n_40cm, 
                                             breaks=seq(from = -1, to = (max(interpolated_dt$n_40cm)+10), by = 5))]
    
    profits_bybin_dt <- ic_field_dt[, .(P_ha = sum(P_gross)/sum(area_ha)), by = .(NRate, n_40cm_bin)]
    
    #Create a local regression
    eonr_bybin_dt <- profits_bybin_dt[, .SD[ P_ha == max( P_ha)], by = .(n_40cm_bin)][,n_40cm := n_40cm_bin *10][order(n_40cm_bin)][,-'n_40cm_bin']
    
    setnames(eonr_bybin_dt, 'NRate', 'eonr')
    
    (p <- ggplot(eonr_bybin_dt, aes(x = n_40cm, y = eonr)) +
        geom_point(size = 1) +
        stat_smooth(method = "lm", formula = y ~ x, size = 1, se = FALSE)+
        theme_bw() +
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=14,face="bold"),
              legend.position='bottom')+
        # facet_grid(mukey~.) +  
        ggtitle(paste('SS model - Region ', reg_n)))
    
    regression_local_ss <- lm(eonr ~ n_40cm, data = eonr_bybin_dt) 
    summary(regression_local_ss)
    
    #Create a local recomendation based on the model
    
    field_averaged_dt <- ic_field_dt[, .(P_loc_ss_ur = sum(P_gross),
                                       leach_no3_loc_ss_ur = sum(leach_no3_ss_gross),
                                       n_40cm = sum(n_40cm_gross) / sum(area_ha)), by = .(z, NRate)]
    
    field_averaged_dt[,N_rec := round((regression_local_ss$coefficients[1] + regression_local_ss$coefficients[2] * n_40cm)/5,0)*5]
    field_averaged_dt[,N_rec := ifelse(N_rec < min(field_averaged_dt$NRate) , min(field_averaged_dt$NRate), N_rec)]
    
    #Calculate profits using that recomendation
    eonr_ss_ur_dt <- field_averaged_dt[NRate == N_rec, .(z, P_loc_ss_ur, leach_no3_loc_ss_ur)]#total field by z
    
    mean(eonr_ss_ur_dt$P_loc_ss_ur)

    #---------------------------------------------------------------------------
    # PROFITS USING LOCAL NOSS VR 
    rates <- ic_field_dt[,.(P = mean(P)), by = .(mukey, NRate)][, .SD[ P == max( P)], by = mukey] %>% 
      setnames('NRate', 'eonr') 
    eonr_noss_vr_dt <- merge(ic_field_dt, rates[,-'P']) %>% .[NRate == eonr] %>% .[ ,-'NRate']
    eonr_noss_vr_dt[,.(P = mean(P)), by = mukey]$P == rates$P #check
    eonr_noss_vr_dt2 <- eonr_noss_vr_dt[,.(P_loc_noss_vr = sum(P_gross),
                                           leach_no3_loc_noss_vr = sum(leach_no3_ss_gross)), by = .(z)] #total field by z
    mean(eonr_noss_vr_dt2$P_loc_noss_vr)
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