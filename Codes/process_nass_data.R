# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
# setwd('~')
rm(list=ls())

source('./Codes_useful/R.libraries.R')
# source('./Codes_useful/gm_functions.R')
# source('./n_policy_git/Codes/parameters.R')

#GOAL: add the county polygon to the nass data
#Load inputs

###=========================================================================================================###
#OBTAIN THE COUNTY AND STATE BOUNDARIES
# us_counties_sf <- us_counties() 
# us_counties_sf <- us_counties_sf %>% dplyr::select(state_name, county_name = name) 
# us_counties_sf <- mutate(us_counties_sf, state_name = tolower(state_name), county_name := tolower(county_name))

us_counties_sf <- sf::read_sf('./n_policy_box/Data/validation/StCoGenAll17_WGS84WMAS/CoUS17_WGS84WMAS.shp') %>% 
        st_transform(4326) %>% dplyr::select(county_name = atlas_name)

us_states_sf <- sf::read_sf("./n_policy_box/Data/validation/StCoGenAll17_WGS84WMAS/StUS17_WGS84WMAS.shp") %>% 
  st_sf() %>% st_transform(4326) %>% dplyr::select(state_name = atlas_name)

us_counties_sf <- st_join(us_counties_sf, us_states_sf, join = st_intersects, left = TRUE, largest = TRUE)

us_counties_sf <- us_counties_sf %>% group_by(county_name, state_name ) %>% summarize() %>% ungroup()

us_counties_sf <- dplyr::mutate(us_counties_sf, county_name = tolower(county_name),
                                                state_name = tolower(state_name))


us_counties_sf$county_name <- gsub("\\s", "", us_counties_sf$county_name)
us_counties_sf$state_name <- gsub("\\s", "", us_counties_sf$state_name)
us_counties_sf$county_name <- gsub("\\.", "", us_counties_sf$county_name)
us_counties_sf$state_name <- gsub("\\.", "", us_counties_sf$state_name)
us_counties_sf$county_name <- gsub("\\'", "", us_counties_sf$county_name)
us_counties_sf$state_name <- gsub("\\'", "", us_counties_sf$state_name)
illinois_counties_sf <- us_counties_sf[us_counties_sf$state_name == 'illinois',]

write_sf(illinois_counties_sf, "./n_policy_box/Data/validation/illinois_counties_sf.gpkg")
# tm_shape(illinois_counties_sf) + tm_polygons('county_name')

###=========================================================================================================###
#PROCESS THE TABLE DATA
# https://quickstats.nass.usda.gov/#CED9AC45-21BA-3EC5-8717-A4D5BE127BC2https://quickstats.nass.usda.gov/#CED9AC45-21BA-3EC5-8717-A4D5BE127BC2
# nass_county_yied_dt <- fread('C:/Users/germanm2/Box Sync/TCC2019_windows/seed_advisor_value/data/nass_county_yield.csv')
nass_county_yied_dt <- fread('./n_policy_box/Data/validation/nass_county_yields.csv')
nass_county_yied_dt <- nass_county_yied_dt[,c("Year", "State", "County" ,  "Value")]
nass_county_yied_dt[,county_name := tolower(County)]
nass_county_yied_dt[,state_name := tolower(State)]
# nass_county_yied_dt <- nass_county_yied_dt[state_name == 'illinois']

nass_county_yied_dt[,year := paste0('yield_', Year)]
nass_yield_wide_dt <- dcast(nass_county_yied_dt, state_name + county_name ~ year, value.var = "Value", fun.agg = function(x) mean(x))

nass_yield_wide_dt$county_name <- gsub("\\s", "", nass_yield_wide_dt$county_name)
nass_yield_wide_dt$state_name <- gsub("\\s", "", nass_yield_wide_dt$state_name)
nass_yield_wide_dt$county_name <- gsub("\\.", "", nass_yield_wide_dt$county_name)
nass_yield_wide_dt$state_name <- gsub("\\.", "", nass_yield_wide_dt$state_name)
nass_yield_wide_dt$county_name <- gsub("\\'", "", nass_yield_wide_dt$county_name)
nass_yield_wide_dt$state_name <- gsub("\\'", "", nass_yield_wide_dt$state_name)
nass_yield_wide_dt$county_name <- gsub("saint", "st", nass_yield_wide_dt$county_name)
nass_yield_wide_dt$county_name <- gsub("saint", "st", nass_yield_wide_dt$county_name)

# nass_yield_wide_dt[state_name == 'virginia' & county_name %in% c("chesapeakecity","suffolkcity","virginiabeachcity"), county_name := gsub("city", "", county_name)]

nass_county_yied_sf <- merge(illinois_counties_sf, nass_yield_wide_dt, by = c('state_name', 'county_name'), all.x = T)

# Create a state_county column
nass_yield_wide_dt[,state_county := paste(state_name, county_name, sep = '_')]
nass_county_yied_sf <- mutate(nass_county_yied_sf, state_county = paste(state_name, county_name, sep = '_'))

#Test if any county is missing
nass_yield_wide_dt[!(nass_yield_wide_dt$state_county %in% nass_county_yied_sf$state_county) & county_name != 'other(combined)counties',]

tm_shape(nass_county_yied_sf[nass_county_yied_sf$state_name == 'illinois',]) + tm_polygons('yield_2019')

saveRDS(nass_county_yied_sf, "./n_policy_box/Data/validation/nass_county_yied_sf.rds")

