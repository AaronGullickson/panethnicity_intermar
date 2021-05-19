#compare PUMA and metro area boundaries
library(sf)
library(tigris)
library(tmap)
library(tmaptools)
library(here)
library(USAboundaries)
library(grid)
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
cbsa_in_data <- unique(alternates_acs$metarea)[-1]
#apparently filter doesn't work on these - weird
#test <- cbsa %>%
#  filter(GEOID %in% met_in_data)
met_acs <- cbsa[cbsa$GEOID %in% cbsa_in_data,]

#1980 Standard MSA data from NHGIS
smsa <- st_read(here("analysis","input","nhgis0029_shape",
                     "nhgis0029_shapefile_tl2000_us_smsa_1980",
                     "US_smsa_1980.shp"), as_tibble=TRUE)
smsa$SMSAA <- as.numeric(as.character(smsa$SMSAA))/10
smsa_in_data <- sort(unique(alternates_census$metarea))[-1]
met_census <- smsa[smsa$SMSAA %in% smsa_in_data,]
#tibble issue
met_census <- st_as_sf(as.data.frame(met_census))


states <- us_states()
states <- states[states$stusps %in% us_states,]
continental_states <- states[!states$stusps %in% c("AK", "HI"),]
alaska <- states[states$stusps=="AK",]
hawaii <- states[states$stusps=="HI",]

#Make the maps
map_cn <- tm_shape(continental_states, projection = 2163)+
  tm_borders("grey20", lwd=2)+
  tm_fill("grey70")+
  tm_shape(met_census)+
  tm_borders("#7b3294", alpha=0.75)+
  tm_fill("#7b3294", alpha=0.4)+
  tm_shape(met_acs)+
  tm_borders("#008837", alpha=0.75)+
  tm_fill("#008837", alpha=0.4)+
  tm_layout(frame = FALSE, 
            inner.margins = c(0.1, 0.1, 0.05, 0.05),
            legend.position = c("right", "bottom"))+
  tm_add_legend(type="fill", 
                col = c("#7b3294","#008837"),
                alpha=0.5,
                labels = c("Census 1980",
                           "ACS 2014-2018"))


map_ak <- tm_shape(alaska, projection = 3338)+
  #tm_borders("grey20", lwd=1)+
  tm_fill("grey70")+
  tm_shape(met_census)+
  tm_borders("#7b3294", alpha=0.75, lwd=0.5)+
  tm_fill("#7b3294", alpha=0.4)+
  tm_shape(met_acs)+
  tm_borders("#008837", alpha=0.75, lwd=0.5)+
  tm_fill("#008837", alpha=0.4)+
  tm_layout("Alaska", frame=FALSE, title.size = 0.8)

map_hi <- tm_shape(hawaii, projection = 3759)+
  #tm_borders("grey20", lwd=1)+
  tm_fill("grey70")+
  tm_shape(met_census)+
  tm_borders("#7b3294", alpha=0.75, lwd=0.5)+
  tm_fill("#7b3294", alpha=0.4)+
  tm_shape(met_acs)+
  tm_borders("#008837", alpha=0.75, lwd=0.5)+
  tm_fill("#008837", alpha=0.4)+
  tm_layout("Hawaii", frame=FALSE, title.position = c("LEFT", "BOTTOM"),
            title.size = 0.8)

vp_ak <- viewport(x = 0.14, y = 0.14, width = 0.28, height = 0.28)
vp_hi <- viewport(x = 0.38, y = 0.1, width = 0.2, height = 0.1)

X11(type = "cairo")
tmap_mode("plot")
tmap_save(map_cn, 
          filename=here("analysis","output","msa_map.png"), 
          scale = 0.7, width = 6.125, outer.margins = 0,
          insets_tm = list(map_ak, map_hi), 
          insets_vp = list(vp_ak, vp_hi))
