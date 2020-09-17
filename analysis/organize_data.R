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

# 1980 data is in a fixed-width format gzipped file. I am going to use the
# read_fwf function in the readr library rather than the vanilla read.fwf
# function because read_fwf allows more options to only read in certain
# variables from the full list and it is also much faster.
census1980 <- read_fwf(here("analysis","input","census1980","usa_00074.dat.gz"), 
                       col_positions = fwf_positions(start = c(1, 7,15,25,33,37,47,48,51,52,53,56,60,63,71,75,83,86,90,93,94,97,100,103,107,109),
                                                     end   = c(4,14,24,27,36,46,47,50,51,52,54,58,62,65,74,76,85,89,92,93,96,99,102,106,108,111),
                                                     col_names = c("year","hhid","hhwt","metarea",
                                                                   "pernum","perwt","sex",
                                                                   "age","marst","marrno",
                                                                   "agemarr","raced","hispand",
                                                                   "bpl","yrimmig","language",
                                                                   "educd","pernum_sp","age_sp",
                                                                   "marrno_sp","raced_sp","hispand_sp",
                                                                   "bpl_sp","yrimmig_sp","language_sp",
                                                                   "educd_sp")),
                       col_types = cols(.default = "i"), #ensure that all variables are read in as integers
                       progress = FALSE)

acs0816 <- read_fwf(here("analysis","input","acs0816","usa_00073.dat.gz"),
                    col_positions = fwf_positions(start = c(1, 7,15,25,31,35,45,46,49,50,51,52,57,61,64,72,76,84,87,91,94,95, 99,102,105,108,112,114),
                                                  end   = c(4,14,24,29,34,44,45,48,49,50,51,55,59,63,66,75,77,86,90,93,94,98,101,104,107,111,113,116),
                                                  col_names = c("year","hhid","hhwt","metarea",
                                                                "pernum","perwt","sex",
                                                                "age","marst","marrno","marrinyr",
                                                                "yrmarr","raced","hispand",
                                                                "bpl","yrimmig","language",
                                                                "educd","pernum_sp","age_sp",
                                                                "marrno_sp","yrmarr_sp","raced_sp","hispand_sp",
                                                                "bpl_sp","yrimmig_sp","language_sp",
                                                                "educd_sp")),
                    col_types = cols(.default = "i"), #ensure that all variables are read in as integers
                    progress = FALSE)



# Code Variables ----------------------------------------------------------------

census1980 <- code_census_variables(census1980)
acs0816 <- code_census_variables(acs0816)

#Check Yourself Before You Wreck Yourself
table(census1980$race, droplevels(as.factor(census1980$raced)), exclude=NULL)
table(census1980$race, census1980$hispand, exclude=NULL)

table(census1980$race_sp, droplevels(as.factor(census1980$raced_sp)), exclude=NULL)
table(census1980$race_sp, census1980$hispand_sp, exclude=NULL)

table(census1980$educd, census1980$educ, exclude=NULL)
table(census1980$educd_sp, census1980$educ_sp, exclude=NULL)

# Duration of Marriage ----------------------------------------------------

#This variable is defined differently in the two datasets so I need to 
#create it separately

census1980$dur_mar <- ifelse(census1980$agemarr==0, 
                             NA, 
                             census1980$age-census1980$agemarr)

acs0816$dur_mar <- ifelse(acs0816$marst>3 | acs0816$marst==0,
                          NA,
                          acs0816$year - acs0816$yrmarr)

# Create Counterfactual Unions -------------------------------------------------------

markets1980 <- create_unions(census1980, 5, 25)
markets0816 <- create_unions(acs0816, 5, 25)


save(markets1980, file=here("analysis","output","markets1980.RData"))
save(markets0816, file=here("analysis","output","markets0816.RData"))

