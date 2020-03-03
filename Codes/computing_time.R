time_track <- readRDS('./vr_value/Data/yc_output/time_track.rds')

grid10_soils_sf4 <- readRDS("./vr_value/Data/Grid/grid10_soils_sf4.rds")  

grid10_soils_dt <- data.table(grid10_soils_sf4)
mukey_cnt <- grid10_soils_dt[,.N, by = .(id_10, mukey)][,.N, by = id_10]

time_track2 <- merge(time_track, mukey_cnt, by = 'id_10')
      
time_track2[,rate := cell/N]
mean(time_track2$rate)




fwrite(time_track2, './vr_value/Data/yc_output/time_track.csv')
