#compare PUMA and metro area boundaries
library(sf)
library(tigris)
library(tmap)
library(here)
load(here("analysis","output","alternates.RData"))

#going to use the tigris library to get shapefiles because they are 
#much less memory intensive there

#need to get PUMAs by states and then rbind
us_states <- unique(fips_codes$state)[1:51]
pumas_list <- lapply(us_states, function(x) {
  pumas(state = x, cb = FALSE, year=2018)
})
pumas <- rbind_tigris(pumas_list)

cbsa <- core_based_statistical_areas(cb=TRUE, year=2018)
#find the metro areas in my data
met_in_data <- unique(alternates_acs$metarea)[-1]
#apparently filter doesn't work on these - weird
#test <- cbsa %>%
#  filter(GEOID %in% met_in_data)
met_area <- cbsa[cbsa$GEOID %in% met_in_data,]

#Make the map
tmap_mode("view")
tm_shape(pumas)+
  tm_borders("grey", lwd=4)+
  tm_shape(met_area)+
  tm_borders("red")+
  tm_text("NAME")
