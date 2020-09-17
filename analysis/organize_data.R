## organize_data.R


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

# Read in the 1980 Data ---------------------------------------------------


census1980 <- read_fwf(here("analysis","input","census1980","usa_00097.dat.gz"), 
                       col_positions = fwf_positions(start = c(1, 9,19,21,28,32,42,43,46,47,48,51,55,58,66,70,78,81,85,88,89,92,95, 98,102,104),
                                                     end   = c(8,18,20,23,31,41,42,45,46,47,49,53,57,60,69,71,80,84,87,88,91,94,97,101,103,106),
                                                     col_names = c("serial","hhwt","statefip","metarea",
                                                                   "pernum","perwt","sex","age","marst","marrno",
                                                                   "agemarr","raced","hispand","bpl","yrimmig",
                                                                   "language","educd","pernum_sp","age_sp","marrno_sp",
                                                                   "raced_sp","hispand_sp","bpl_sp","yrimmig_sp",
                                                                   "language_sp","educd_sp")),
                       col_types = cols(.default = "i"),
                       progress = FALSE)
#we had to cut out year for size reasons
census1980$year <- 1980

#ACS data is split into three separate files for size, but the indices are 
#identical
acs_start <- c(1, 5,13,23,25,30,34,44,45,48,49,50,51,56,60,63,71,77,83,86,90,93,94,97,100,103,107,109)
acs_end   <- c(4,12,22,24,29,33,43,44,47,48,49,50,54,58,62,65,74,80,85,89,92,93,96,99,102,106,108,111)
acs_names <- c("year","serial","hhwt","statefip","metarea","pernum","perwt",
               "sex","age","marst","marrno","marrinyr","yrmarr","raced",
               "hispand","bpl","yrimmig","language","educd","pernum_sp",
               "age_sp","marrno_sp","raced_sp","hispand_sp","bpl_sp",
               "yrimmig_sp","language_sp","educd_sp")

acs <- rbind(read_fwf(here("analysis","input","acs1418","usa_00094.dat.gz"),
                      col_positions = fwf_positions(start = acs_start,
                                                    end   = acs_end,
                                                    col_names = acs_names),
                      col_types = cols(.default = "i"), 
                      progress = FALSE),
             read_fwf(here("analysis","input","acs1418","usa_00095.dat.gz"),
                      col_positions = fwf_positions(start = acs_start,
                                                    end   = acs_end,
                                                    col_names = acs_names),
                      col_types = cols(.default = "i"), 
                      progress = FALSE),
             read_fwf(here("analysis","input","acs1418","usa_00096.dat.gz"),
                      col_positions = fwf_positions(start = acs_start,
                                                    end   = acs_end,
                                                    col_names = acs_names),
                      col_types = cols(.default = "i"), 
                      progress = FALSE))


# Code Variables -------------------------------------------------------------

census1980 <- code_census_variables(census1980)
acs <- code_census_variables(acs)

#Check Yourself Before You Wreck Yourself
table(census1980$raced, census1980$race, exclude=NULL)
table(census1980$hispand, census1980$race, exclude=NULL)
table(acs$raced, acs$race, exclude=NULL)
table(acs$hispand, acs$race, exclude=NULL)

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

# Create Counterfactual Unions -------------------------------------------------------

#important to use five year windows to fix intervalled year of migration
#data for census 1980
markets_census <- create_unions(census1980, 5, 25)
markets_acs <- create_unions(acs, 5, 25)


save(markets_census, file=here("analysis","output","markets_census.RData"))
save(markets_acs, file=here("analysis","output","markets_acs.RData"))

