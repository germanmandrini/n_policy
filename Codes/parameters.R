#--------------------------
# Corn and N price
#--------------------------
Pc = 0.14#3.5/25.4 #$/bu to $/kg https://markets.businessinsider.com/commodities/corn-price
Pn = 0.87 #400/460 #  0.3944 $\lb N   https://www.ams.usda.gov/mnreports/gx_gr210.txt

# Pn = 6*Pc
Pn/Pc

870*0.46

400.2/450
(400.2/450)/2.20462

400/450 # $/kg N
400/450/2.20462 # $/lb N

400/(2000*0.45)

Pn /2.2046*2000*0.45

Pe_min = 8.88 # $/kg N #    cost of the externality for each kg of N leached
Pe_med = 18.54 # $/kg N #    cost of the externality for each kg of N leached
Pe_max = 27.08 # $/kg N #    cost of the externality for each kg of N leached

#Urea 340/1000/0.46=0.739$/kgN https://www.ams.usda.gov/mnreports/gx_gr210.txt
#Liquid Nitrogen 28% spread 240/1000/0.28 = 0.857$/kgN https://www.ams.usda.gov/mnreports/gx_gr210.txt

#(p = 3.5/25.4)#$3.3/bu 180/1000
#(w = 0.3/0.453592)#$0.3/pound of N

#p = 0.1534 # ($/kg) = ($3.9/bu) Taro
#w = 0.5*2.20462 #($/kg) = $($100/100kg) Taro


#--------------------------
# Fixed cost 
#--------------------------
#--- costs excluding Nitrogen fertilizer ---#
# see http://www.farmdoc.illinois.edu/manage/2018_crop_budgets.pdf by Gary for this information
# c_fixed_ha <- 442/0.404686 # 600 ($/acre)
# 
# 356-130+131+83

#--------------------------
# Cost of N application 
#--------------------------
c_vra_ha <- 9*2.471 #($/ha) 9 ($/acre)
c_ura_ha <- 7*2.471 #($/ha) 7 ($/acre) 

#--------------------------
# Oportunity cost 
#--------------------------
# r_a <- 0.05 #annual return rate
# r_m <- (r.a +1) ^ (1/12) - 1 #monthly return rate

#--------------------------
# Grid Soil sampling cost
#--------------------------
#c_sampling_ha <- 8*2.471 #($/ha) 8 ($/acre) http://ocj.com/2012/02/intensive-soil-sampling-makes-dollars-and-sense/
#8$/sample
# c_N_sample <- 3
# c_OM_sample <- 3

#--------------------------
# Cost of trial implementation 
#--------------------------
# c_trial <- c(700,400,200,rep(100,7)) 