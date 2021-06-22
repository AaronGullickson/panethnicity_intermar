#compare PUMA and metro area boundaries

load(here("analysis","output","alternates.RData"))
source(here("analysis","check_packages.R"))

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
#did we get everything?
length(cbsa_in_data)==nrow(met_acs)


#1980 Standard MSA data from NHGIS
smsa <- st_read(here("analysis","input","nhgis0029_shape",
                     "nhgis0029_shapefile_tl2000_us_smsa_1980",
                     "US_smsa_1980.shp"), as_tibble=TRUE)
smsa <- st_as_sf(as.data.frame(smsa))
smsa$SMSAA <- as.numeric(as.character(smsa$SMSAA))
#multiple metarea in IPUMS data by 10 to be consistent with NHGIS shapefile
smsa_in_data <- sort(unique(alternates_census$metarea))[-1]*10
#Corrections 
# I initially had ten missing links here. I can fix seven of them by the 
# notes below, but three MSAs appear to be missing from the NHGIS shapefile.
### Codes needing correction ###
## EL Paso Fixes ##
#2330 (Elmira, NY) should be 2335
#2320 (Elkhart IN) should be 2330
#2310 (El Paso) should be 2320
smsa_in_data[smsa_in_data==2330] <- 2335
smsa_in_data[smsa_in_data==2330] <- 2330
smsa_in_data[smsa_in_data==2310] <- 2320
#2660 (Florence, SC) should be 2655
smsa_in_data[smsa_in_data==2660] <- 2655
#2970 (Glenn Falls, NY) should be 2975
smsa_in_data[smsa_in_data==2970] <- 2975
## Jacksonville fixes ##
#3600 (Jacksonville, NC) should be 3605
#3590 (Jacksonville FL) should be 3600
smsa_in_data[smsa_in_data==3600] <- 3605
smsa_in_data[smsa_in_data==3590] <- 3600
## Paname City fixes
#6010 (Panama City, FL) should be 6015 
#6030 (Pascagoula, MS) should be 6025
smsa_in_data[smsa_in_data==6010] <- 6015
smsa_in_data[smsa_in_data==6030] <- 6025
## Santa Barbara Fixes ##
#7480 (Santa Cruz, CA) should be 7485
#7470 (Santa Barbara, CA) should be 7480
smsa_in_data[smsa_in_data==7480] <- 7485
smsa_in_data[smsa_in_data==7470] <- 7480
#8730 (Ventura, CA) should be 6000
smsa_in_data[smsa_in_data==8730] <- 6000
### Missing Cases ###
#460 is Appleton-Oshkosh-Neenah, WI.
#4760 is Manchester, NH
#7560 is Scranton PA
met_census <- smsa[smsa$SMSAA %in% smsa_in_data,]
#tibble issue
met_census <- st_as_sf(as.data.frame(met_census))
#what are we missing?
smsa_in_data[!(smsa_in_data %in% met_census$SMSAA)]
#the expected cases

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
                alpha=0.75,
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
          filename=here("analysis","output","msa_map.pdf"), 
          scale = 0.7, 
          width = 6.5, height = 3.5, 
          outer.margins = 0,
          insets_tm = list(map_ak, map_hi), 
          insets_vp = list(vp_ak, vp_hi))
