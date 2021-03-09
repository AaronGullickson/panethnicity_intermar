#' ---
#' title: "organize_data.R"
#' author: ""
#' ---

# Introduction ------------------------------------------------------------

# This script will read in raw data from the input directory, clean it up to
# produce the analytical dataset, and then write the analytical data to the
# output directory.

# For the couples dataset, we only want couples who were married in the last
# year as determined by age of marriage and current age. We also want to remove
# individuals who immigrated after the date of marriage.
 
# For the alternate partner datasets, we want individuals who have been in the
# US at least one year and were either married in the previous year or are
# currently single.

library(here)
source(here("analysis","check_packages.R"))
source(here("analysis","useful_functions.R"))

set.seed(39)

# Read in the Data ---------------------------------------------------------


census1980 <- read_fwf(here("analysis","input","census1980","usa_00105.dat.gz"), 
                       col_positions = fwf_positions(start = c(1, 9,19,21,28,32,42,43,46,47,48,51,55,61,66,72,78,81,85,88,89,92,95,100,106,110),
                                                     end   = c(8,18,20,23,31,41,42,45,46,47,49,53,57,65,69,75,80,84,87,88,91,94,99,103,109,112),
                                                     col_names = c("serial","hhwt","statefip","metarea",
                                                                   "pernum","perwt","sex","age","marst","marrno",
                                                                   "agemarr","raced","hispand","bpld","yrimmig",
                                                                   "languaged","educd","pernum_sp","age_sp","marrno_sp",
                                                                   "raced_sp","hispand_sp","bpld_sp","yrimmig_sp",
                                                                   "languaged_sp","educd_sp")),
                       col_types = cols(.default = "i"),
                       progress = FALSE)
#I had to cut out year for size reasons
census1980$year <- 1980

#ACS data is split into three separate files for size, but the indices are 
#identical
acs_start <- c(1, 5,13,23,25,30,34,44,45,48,49,50,51,56,60,66,71,77,83,86,90,93,94,97,100,105,111,115)
acs_end   <- c(4,12,22,24,29,33,43,44,47,48,49,50,54,58,62,70,74,80,85,89,92,93,96,99,104,108,114,117)
acs_names <- c("year","serial","hhwt","statefip","metarea","pernum","perwt",
               "sex","age","marst","marrno","marrinyr","yrmarr","raced",
               "hispand","bpld","yrimmig","languaged","educd","pernum_sp",
               "age_sp","marrno_sp","raced_sp","hispand_sp","bpld_sp",
               "yrimmig_sp","languaged_sp","educd_sp")

acs <- rbind(read_fwf(here("analysis","input","acs1418","usa_00106.dat.gz"),
                      col_positions = fwf_positions(start = acs_start,
                                                    end   = acs_end,
                                                    col_names = acs_names),
                      col_types = cols(.default = "i"), 
                      progress = FALSE),
             read_fwf(here("analysis","input","acs1418","usa_00107.dat.gz"),
                      col_positions = fwf_positions(start = acs_start,
                                                    end   = acs_end,
                                                    col_names = acs_names),
                      col_types = cols(.default = "i"), 
                      progress = FALSE),
             read_fwf(here("analysis","input","acs1418","usa_00108.dat.gz"),
                      col_positions = fwf_positions(start = acs_start,
                                                    end   = acs_end,
                                                    col_names = acs_names),
                      col_types = cols(.default = "i"), 
                      progress = FALSE))


# Code Variables -------------------------------------------------------------

census1980 <- code_census_variables(census1980)
acs <- code_census_variables(acs)

#Check Yourself Before You Wreck Yourself
table(census1980$race, census1980$raced, exclude=NULL)
table(census1980$race, census1980$hispand, exclude=NULL)
options(max.print=10000)
table(acs$race, acs$raced, exclude=NULL)
table(acs$race, acs$hispand, exclude=NULL)
options(max.print=99999)

table(census1980$educd, census1980$educ, exclude=NULL)
table(census1980$educd_sp, census1980$educ_sp, exclude=NULL)
table(acs$educd, acs$educ, exclude=NULL)
table(acs$educd_sp, acs$educ_sp, exclude=NULL)

#check whether is_single is working correctly
table(census1980$marst, is_single(census1980$marst), exclude=NULL)
table(acs$marst, is_single(acs$marst), exclude=NULL)

# Duration of Marriage ----------------------------------------------------

#This variable is defined differently in the two datasets so I need to 
#create it separately
census1980$dur_mar <- ifelse(census1980$agemarr==0, 
                             NA, census1980$age-census1980$agemarr)
tapply(census1980$dur_mar, census1980$marst, mean)

acs$dur_mar <- ifelse(acs$yrmarr==0,
                      NA, acs$year - acs$yrmarr)
tapply(acs$dur_mar, acs$marst, mean)


# Code Age at Migration ---------------------------------------------------

# Because of the intervalling of yrimmig in the 1980 data, I need to handle
# this variable differently by dataset. 

# These variables cannot be NA or the cases will be dropped when markets are
# created, so I will assign a zero to all missing values (which are native-born)
# individuals

#Census 1980
#Calculate years in USA based on midpoints of yrimmig intervals
#The open ended category before 1949 is tricky. Going to assume 1940, but 
#that will produce some negative values for age at entry
census1980$yrimmig_mid <- case_when(
  census1980$yrimmig==0 ~ NA_real_,
  census1980$yrimmig==1980 ~ 1977.5,
  census1980$yrimmig<1980 & census1980$yrimmig>1949 ~ census1980$yrimmig-2,
  TRUE ~ 1940
)
table(census1980$yrimmig, census1980$yrimmig_mid, exclude=NULL)

census1980$yrimmig_mid_sp <- case_when(
  is.na(census1980$yrimmig_sp) | census1980$yrimmig_sp==0 ~ NA_real_,
  census1980$yrimmig_sp==1980 ~ 1977.5,
  census1980$yrimmig_sp<1980 & census1980$yrimmig_sp>1949 ~ census1980$yrimmig_sp-2,
  TRUE ~ 1940
)

census1980$age_usa <- census1980$age-(1980-census1980$yrimmig_mid)
census1980$age_usa <- ifelse(is.na(census1980$age_usa) | census1980$age_usa<0, 
                             0, census1980$age_usa)
summary(census1980$age_usa)
census1980$age_usa_sp <- census1980$age_sp-(1980-census1980$yrimmig_mid_sp)
census1980$age_usa_sp <- ifelse(is.na(census1980$age_usa_sp) | 
                                  census1980$age_usa_sp<0, 
                                0, census1980$age_usa_sp)
summary(census1980$age_usa_sp)

#ACS data
#straightforward in ACS, but still get some negative values. Most of these are
#just -1 values due to calendar year/birth year issues, but a few other cases
#at older ages go back as far as -5. 
acs$age_usa <- acs$age-acs$yr_usa
acs$age_usa <- ifelse(is.na(acs$age_usa) | acs$age_usa<0, 0, acs$age_usa)
summary(acs$age_usa)
acs$age_usa_sp <- acs$age-acs$yr_usa_sp
acs$age_usa_sp <- ifelse(is.na(acs$age_usa_sp) | acs$age_usa_sp<0, 
                         0, acs$age_usa_sp)
summary(acs$age_usa_sp)

# Save alternate partner datasets -----------------------------------------

# Need to create datasets of possible alternate partners for later analysis.
# This will use the first bit of code in the create_unions function to
# determine who is eligible
years_mar <- 5

alternates_census <- subset(census1980, (is.na(yr_usa) | yr_usa>years_mar) & 
                              (is.na(yr_usa_sp) | yr_usa_sp>years_mar) &
                              (is_single(marst) | dur_mar<=years_mar),
                            select=c("statefip","metarea","sex","hhwt","perwt",
                                     "marst","age","race","educ","bpld","yr_usa",
                                     "lang","raced","hispand"))

alternates_acs <- subset(acs, (is.na(yr_usa) | yr_usa>years_mar) & 
                           (is.na(yr_usa_sp) | yr_usa_sp>years_mar) &
                           (is_single(marst) | dur_mar<=years_mar),
                         select=c("statefip","metarea","sex","hhwt","perwt",
                                  "marst","age","race","educ","bpld","yr_usa",
                                  "lang","raced","hispand"))

save(alternates_census, alternates_acs, 
     file=here("analysis","output","alternates.RData"))

# Create Counterfactual Unions -------------------------------------------------------

nreps <- 5

#important to use five year windows to fix intervalled year of migration
#data for census 1980
#remove Vietnamese from 1980 data due to poor fit
race_lvls1980 <- sort(c("White","Black","AIAN",
                        "Mexican","Puerto Rican","Cuban", 
                        "Chinese","Japanese","Filipino","Korean",
                        "Asian Indian"))
census1980$race <- factor(census1980$race,
                          levels=race_lvls1980)
census1980$race_sp <- factor(census1980$race_sp,
                             levels=race_lvls1980)
markets_census <- mclapply(1:nreps, function(i) {
  create_unions(census1980, years_mar, 25)
})

#for the ACS, first calculate one based on the full racial categories
markets_acs_full <- mclapply(1:nreps, function(i) {
  create_unions(acs, years_mar, 25)
})

#now do one that is restricted to groups with a sufficiently large population
#to do a full ethnicity-by-ethnicity breakout in ACS data
restricted_race <- sort(c("White","Black","AIAN",
                          "Mexican","Puerto Rican","Cuban", #basic Latino
                          "Guatemalan","Salvadorian","Colombian","Ecuadorian","Peruvian","Dominican", #extra
                          "Chinese","Japanese","Filipino","Korean",
                          "Vietnamese", #extra
                          "Asian Indian", #basic South Asian
                          "Pakistani")) #extra
acs$race <- factor(acs$race, levels=restricted_race)
acs$race_sp <- factor(acs$race_sp, levels=restricted_race)
markets_acs_restricted <- mclapply(1:nreps, function(i) {
  create_unions(acs, years_mar, 25)
})

#Calculate another one after removing the categories not available in 1980
#that will have groups consistent with Census 1980
acs$race <- factor(acs$race, levels=race_lvls1980)
acs$race_sp <- factor(acs$race_sp, levels=race_lvls1980)
markets_acs_1980basis <- mclapply(1:nreps, function(i) {
  create_unions(acs, years_mar, 25)
})


save(markets_census, 
     file=here("analysis","output","markets_census.RData"))
save(markets_acs_full, 
     file=here("analysis","output","markets_acs_full.RData"))
save(markets_acs_restricted, 
     file=here("analysis","output","markets_acs_restricted.RData"))
save(markets_acs_1980basis, 
     file=here("analysis","output","markets_acs_1980basis.RData"))