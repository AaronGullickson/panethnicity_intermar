## useful_functions.R

#I will put all of the coding variable stuff into a function to ensure that we do the 
#same operations on both datasets when we make changes. I will then put each separate
#variable coding into a sub-function to ensure that we do the same thing for each 
#spouse. 
code_census_variables <- function(census) {
  
  # Sex of respondent
  # - Male (sex 1)
  # - Female (sex 2)
  census$sex <- ifelse(census$sex==1, "Male",
                 ifelse(census$sex==2, "Female", NA))
  
  # Marital status
  census$marst <- factor(census$marst,
                         levels=1:6,
                         labels=c("Married, spouse present",
                                  "Married, spouse absent",
                                  "Separated","Divorced","Widowed",
                                  "Never married"))
  
  # Race - see details in code_race below
  census$race <- code_race(census$raced, census$hispand)
  census$race_sp <- code_race(census$raced_sp, census$hispand_sp)
  
  # Education - see details in code_educ below
  census$educ <- code_educ(census$educd)
  census$educ_sp <- code_educ(census$educd_sp)
  
  # Language 
  # We don't need to code language as a factor variable with labels because
  # ultimately all we need to know is whether the two partners speak the same
  # language. We should however code in any missing values (0), although 
  # there do not appear to be any.
  census$language <- ifelse(census$language==0, NA, census$language)
  census$language_sp <- ifelse(census$language_sp==0, NA, census$language_sp)
  
  # Country of Birth 
  census$bpl <- code_bpl(census$bpl)
  census$bpl_sp <- code_bpl(census$bpl_sp)
  
  #Years living in USA
  #1n 1980 this is based on intervalled data with the actual value
  #representing the last year in the interval. In this case we have 
  #intervals of 1975-1980 (1980) and 1970-1974 (1974) that are relevant.
  #this means we will not be able to measure timing of years in smaller
  #than five year increments - which is a good reason for using the marriage
  #in last five years as a benchmark. 
  census$yr_usa <- census$year-ifelse(census$yrimmig==0,NA,census$yrimmig)
  census$yr_usa_sp <- census$year-ifelse(census$yrimmig_sp==0,NA,
                                         census$yrimmig_sp)
  
  # Assign Unique Person ID - we removed the sample number to cut
  #down on size but year should do the same trick
  census$id <- census$serial*1000000+census$pernum*10000+census$year
  if(sum(duplicated(census$id))>0) {
    stop("Duplicted ids in data")
  }
  census$id_sp <- ifelse(is.na(census$pernum_sp),NA,
                         census$serial*1000000+census$pernum_sp*10000+census$year)
  if(sum(duplicated(na.omit(census$id_sp)))>0) {
    stop("Duplicted spousal ids in data")
  }
  
  return(census)
}

code_race <- function(raced, hispand) {
  # We want to take the raced and hispand variables and code them into a combined
  # race variable. We want categories to be large enough to sustain an analysis.
  # We will use the following categories based on this criteria. I looked at
  # case counts on IPUMS to ascertain this. The general benchmarks is over 10,000
  # in a given ACS year, aalthough Japanese is a little lower than this.
  # - White (raced 100)
  # - Black (raced 200)
  # - Indigenous (raced 300 (American Indian) and raced 630 (Native Hawaiian))
  # - Chinese (raced 400 and 410 Chinese and Tawaiianese respectively)
  # - Japanese (raced 500)
  # - Korean (raced 620)
  # - Vietnamese (raced 640)
  # - Filipino (raced 600)
  # - Mexican (hispand 100)
  # - Puerto Rican (hispand 200)
  # - Cuban (hispand 300)
  # - Guatemalan (raced 412)
  # - Salvadorian (raced 416)
  # - Colombian (raced 423)
  # - Dominican (raced 460)
  # Any other cases should be treated as missing values.
  #Note that I cannot get Guatemalan, Salvadorian, Colombian, and Dominican in
  #1980 data so that when using comparisons I will need a separate ACS data which
  #leaves these categories out
  race <- case_when(
    hispand==100 ~ "Mexican",
    hispand==200 ~ "Puerto Rican",
    hispand==300 ~ "Cuban",
    hispand==412 ~ "Guatemalan",
    hispand==416 ~ "Salvadorian",
    hispand==423 ~ "Colombian",
    hispand==460 ~ "Dominican",
    #hispand>=400 ~ NA_character_,
    raced==100 ~ "White",
    raced==200 ~ "Black",
    (raced>=300 & raced<400) | raced==630 ~ "Indigenous",
    raced==400 | raced==410 ~ "Chinese",
    raced==500 ~ "Japanese",
    raced==620 ~ "Korean",
    raced==600 ~ "Filipino",
    raced==640 ~ "Vietnamese",
    raced==610 ~ "Asian Indian",
    TRUE ~ NA_character_
  )
  
  race <- factor(race, 
                 levels=c("White","Black","Indigenous","Chinese","Japanese",
                          "Korean","Filipino","Vietnamese","Asian Indian", 
                          "Mexican","Cuban","Puerto Rican","Dominican",
                          "Guatemalan","Salvadorian","Colombian"))
  
  table(raced, hispand, race)
  
  return(race)
}

code_educ <- function(educd) {
  # We want to re-code the educd variable into the following simple
  # categories:
  # - Less than high school diploma
  # - High school diploma
  # - Some college, but less than a four year degree
  # - Four year college degree or more
  educ <- ifelse(educd<60, "LHS",
                 ifelse(educd<=65, "HS",
                        ifelse(educd<=90, "SC",
                               ifelse(educd<=999, "C", 
                                      NA))))
  educ <- factor(educ,
                 levels=c("LHS","HS","SC","C"),
                 ordered=TRUE)
  return(educ)
}

code_bpl <- function(bpl) {
  #recode any one born in the US (99 or less) as a single number. Otherwise
  #we will be fitting state level endogamy. also code in missing values
  return(ifelse(bpl>=900, NA, 
                ifelse(bpl<100,1,bpl)))
}

#This function will take the given census data, separate out 
#couples and alternate spouses, and create the marriage market data 
#for analysis. the second argument gives the time span for consideration
#of who will be considered. Its important to keep in mind that intervalled
#nature of years in the USA for the census 1980 data. years_mar that is not
#consistent with those intervals can create problems
create_unions <- function(census, years_mar, n_fakes) {
  
  # eliminate individuals who:
  # a) have been in the USA less than the marriage window
  # b) are currently married longer than the marriage window
  # or who have a marriage duration longer than the current window, if any at all
  census <- subset(census, (is.na(yr_usa) | yr_usa>years_mar) & 
                     (is.na(yr_usa_sp) | yr_usa_sp>years_mar) &
                     (is_single(marst) | dur_mar<=years_mar),
                   select=c("statefip","metarea","sex","hhwt","perwt","dur_mar","marst",
                            "id","age","race","educ","bpl","language","marrno",
                            "id_sp","age_sp","race_sp","educ_sp","bpl_sp","language_sp","marrno_sp"))
  
  # Actual couples who are within the marriage duration window of years_mar
  # both must be in a first time marriage for consistency with 1980 marriage
  # duration calculation
  unions <- subset(census, sex=="Male" & 
                     !is.na(id_sp) & 
                     (marrno<2 & marrno_sp<2),
                   select=c("statefip","metarea","hhwt",
                            "id","age","race","educ","bpl","language",
                            "id_sp","age_sp","race_sp","educ_sp","bpl_sp","language_sp"))
  colnames(unions) <- c("statefip","metarea","hhwt",
                        "idh","ageh","raceh","educh","bplh","languageh",
                        "idw","agew","racew","educw","bplw","languagew")
  unions <- na.omit(as.data.frame(unions))
  unions$metarea <- factor(as.character(unions$metarea))
  unions$statefip <- factor(as.character(unions$statefip))
  
  # Alternate Male Partners
  male_alternates <- subset(census, sex=="Male",
                            select=c("statefip","metarea","id","perwt",
                                     "age","race","educ","bpl","language"))
  colnames(male_alternates) <- c("statefip","metarea","idh","perwt",
                                 "ageh","raceh","educh","bplh","languageh")
  male_alternates <- na.omit(as.data.frame(male_alternates))
  male_alternates$metarea <- factor(as.character(male_alternates$metarea))
  male_alternates$statefip <- factor(as.character(male_alternates$statefip))
  
  # Alternate Female Partners
  female_alternates <- subset(census, sex=="Female",
                              select=c("statefip","metarea","id","perwt",
                                       "age","race","educ","bpl","language",
                                       "perwt"))
  colnames(female_alternates) <- c("statefip","metarea","idw", "perwt",
                                   "agew","racew","educw","bplw","languagew")
  female_alternates <- na.omit(as.data.frame(female_alternates))
  female_alternates$metarea <- factor(as.character(female_alternates$metarea))
  female_alternates$statefip <- factor(as.character(female_alternates$statefip))
  
  # Sample Counterfactual Spouses 
  markets <- generateCouples(n_fakes, 
                             unions, male_alternates, female_alternates,
                             geo="statefip", weight="perwt", verbose=FALSE)
  return(markets)
}


is_single <- function(marst) {
  #TODO: what about separated people?
  return(marst!="Married, spouse present" & marst!="Married, spouse absent")
}

#code variables for the marriage market dataset
add_vars <- function(markets) {
  #age difference 
  markets$agediff <- markets$ageh-markets$agew
  
  #birthplace endogamy
  markets$bpl_endog <- markets$bplh==markets$bplw
  
  #language endogamy 
  markets$language_endog <- markets$languageh==markets$languagew
  
  #educational hypergamy/hypogamy
  markets$hypergamy <- markets$educh > markets$educw
  markets$hypogamy <- markets$educh < markets$educw
  
  #educational crossing
  markets$edcross_hs <- (markets$educh>="HS" & markets$educw<"HS") | 
    (markets$educw>="HS" & markets$educh<"HS")
  markets$edcross_sc <- (markets$educh>="SC" & markets$educw<"SC") | 
    (markets$educw>="SC" & markets$educh<"SC")
  markets$edcross_c <- (markets$educh>="C" & markets$educw<"C") | 
    (markets$educw>="C" & markets$educh<"C")
  
  #create racial exogamy terms
  
  #full racial exogamy blocks
  markets$race_exog_full <- createExogamyTerms(markets$raceh, 
                                               markets$racew, 
                                               symmetric=TRUE)
  
  
  #restricted to pentagon plus Asian/Asian and Hispanic/Hispanic intermarriage
  markets$raceh_pent <- code_race_pentagon(markets$raceh)
  markets$racew_pent <- code_race_pentagon(markets$racew)
  markets$race_exog_pent <- createExogamyTerms(markets$raceh_pent, 
                                          markets$racew_pent, 
                                          symmetric=TRUE)
  #now replace endogamy with ethnic intermarriage for Asian and Hispanic groups
  lvls_tmp <- levels(markets$race_exog_pent)
  markets$race_exog_pent <- as.character(markets$race_exog_pent)
  markets$race_exog_pent <- ifelse(markets$raceh_pent=="Asian" & markets$racew_pent=="Asian" &
                                markets$raceh!=markets$racew, "Asian.Asian", 
                              ifelse(markets$raceh_pent=="Hispanic" & markets$racew_pent=="Hispanic" &
                                       markets$raceh!=markets$racew, "Hispanic.Hispanic",
                                     markets$race_exog_pent))
  markets$race_exog_pent <- factor(markets$race_exog_pent,
                              levels=c(lvls_tmp,
                                       "Asian.Asian","Hispanic.Hispanic"))
  #test
  #table(markets$raceh, markets$racew, markets$race.exog)
  
  return(markets)
}

#code race back into the racial pentagon, plus Asian Indian
code_race_pentagon <- function(race) {
  race_pent <- ifelse(is.na(race), NA, 
                      ifelse(race=="Chinese" | race=="Japanese" | 
                               race=="Korean" | race=="Filipino" | 
                               race=="Vietnamese", "Asian", 
                             ifelse(race=="Mexican" | race=="Cuban" | 
                                      race=="Puerto Rican" | race=="Guatemalan" |
                                      race=="Salvadorian" | race=="Dominican" |
                                      race=="Colombian", "Hispanic",
                                    as.character(race))))
  race_pent <- factor(race_pent, 
                      levels=c("White","Black","Indigenous","Asian","Hispanic",
                               "Asian Indian"))
  return(race_pent)
}

