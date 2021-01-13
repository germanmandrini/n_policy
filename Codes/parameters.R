#--------------------------
# Corn and N price
#--------------------------
Pc = 0.14#3.5/25.4 #$/bu to $/kg https://markets.businessinsider.com/commodities/corn-price
Pc*25.4 #$/bu
# Pc = 3.3/25.4#3.5/25.4 #$/bu to $/kg https://markets.businessinsider.com/commodities/corn-price
# Pn = 0.87 #400/460 #  0.3944 $\lb N   https://www.ams.usda.gov/mnreports/gx_gr210.txt
Ps = 8.3/27.2155 #https://farmdoc.illinois.edu/handbook/2020-budgets-for-all-regions
Pn = 5*Pc
Pn/2.20462 #$\lb N
# https://farmdocdaily.illinois.edu/2018/09/fertilizer-prices-higher-for-2019-crop.html
# https://www.ams.usda.gov/mnreports/gx_gr210.txt

print(paste('Pn/Pc', Pn/Pc))


Yld_response_threshold <- 200 

low_var <- c("rain_30", "rain_60", "rain_90",
             "t_max_30", "t_max_60", "t_max_90", "t_min_30", "t_min_60",
             "t_min_90", 'Y_corn_lt_avg', "day_sow", "day_v5", "lai_v5")#'Y_corn_lt_min', 'Y_corn_lt_max', 

high_var <- c("whc",  "oc_20cm_v5", "sw_dep_v5", "n_0_60cm_v5",  
              "surfaceom_wt_v5", "sand_40cm", "clay_40cm") #"root_wt_v5",, "n_deep_v5", "esw_pct_v5", 

NRS_paper <- c('staticmrtn', 'staticmean', "extra20", "forecast", "rfhigh", "rflow")