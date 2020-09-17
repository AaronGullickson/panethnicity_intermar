## useful_functions.R

#I will put all of the coding variable stuff into a function to ensure that we do the 
#same operations on both datasets when we make changes. I will then put each separate
#variable coding into a sub-function to ensure that we do the same thing for each 
#spouse. 
code_census_variables <- function(census) {
  
  # Sex of respondent
  # - Male (sex 1)
  # - Female (sex 2)
  temp <- ifelse(census$sex==1, "Male",
                 ifelse(census$sex==2, "Female", NA))
  table(temp, census$sex, exclude=NULL)
  census$sex <- temp
  
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
  census$yr_usa <- census$year-ifelse(census$yrimmig==0,NA,census$yrimmig)
  census$yr_usa_sp <- census$year-ifelse(census$yrimmig_sp==0,NA,census$yrimmig_sp)
  
  # Assign Unique Person ID
  census$id <- census$hhid*10000+census$pernum
  sum(duplicated(census$id))
  census$id_sp <- ifelse(is.na(census$pernum_sp),NA,
                         census$hhid*10000+census$pernum_sp)
  
  return(census)
}

code_race <- function(raced, hispand) {
  # We want to take the raced and hispand variables and code them into a combined
  # race variable with the following categories:
  # - White (raced 100)
  # - Black (raced 200)
  # - American Indian (raced 300)
  # - Chinese (raced 400)
  # - Japanese (raced 500)
  # - Korean (raced 620)
  # - Vietnamese (raced 640) REMOVED BECAUSE OF SAMPLE SIZE, ALSO CONCERNED ABOUT WAR BRIDE EFFECT
  # - Filipino (raced 600)
  # - Native Hawaiian (raced 630)
  # - Mexican (hispand 100)
  # - Cuban (hispand 300)
  # - Puerto Rican (hispand 200)
  # Any other cases should be treated as missing values
  race <- ifelse(hispand==498, NA, 
                 ifelse(hispand==100, "Mexican",
                        ifelse(hispand==300, "Cuban",
                               ifelse(hispand==200, "Puerto Rican",
                                      ifelse(raced==100, "White",
                                             ifelse(raced==200, "Black",
                                                    ifelse(raced==400, "Chinese",
                                                           ifelse(raced==500, "Japanese",
                                                                  ifelse(raced==620, "Korean",
                                                                         ifelse(raced==600, "Filipino",
                                                                                ifelse(raced==610, "Asian Indian", NA)))))))))))
  race <- factor(race, 
                 levels=c("White","Black","Chinese",
                          "Japanese","Korean","Filipino",
                          "Asian Indian", "Mexican","Cuban","Puerto Rican"))
  return(race)
}

code_educ <- function(educd) {
  # We want to re-code the educd variable into the following simple
  # categories:
  # - Less than high school diploma
  # - High school diploma
  # - Some college, but less than a four year degree
  # - Four year college degree or more
  educ <- ifelse(educd<60, "no hs diploma",
                 ifelse(educd<=65, "hs diploma",
                        ifelse(educd<=90, "some college",
                               ifelse(educd<=999, "four-year degree", 
                                      NA))))
  educ <- factor(educ,
                 levels=c("no hs diploma","hs diploma",
                          "some college","four-year degree"),
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
#of who will be considered. This is a bit tricky because for the ACS
#data its just a boolean for being married in the last year, but I want
#to still put this in a function, so I may just need to adjust that elsewhere

create_unions <- function(census, years_mar, n_fakes) {
  
  # eliminate individuals who have been in the USA less than the marriage window. 
  #Also get rid of second marriages
  census <- subset(census, (is.na(yr_usa) | yr_usa>years_mar) & 
                     (is.na(yr_usa_sp) | yr_usa_sp>years_mar) &
                     (marrno<2 | marrno_sp<2),
                   select=c("metarea","sex","dur_mar","id","id_sp",
                            "age","race","educ","bpl","language",
                            "age_sp","race_sp","educ_sp","bpl_sp",
                            "language_sp"))
  
  # Actual couples who are within the marriage duration window of years_mar
  unions <- subset(census, sex=="Male" & !is.na(id_sp) & 
                     (!is.na(dur_mar) & dur_mar<=years_mar),
                   select=c("metarea",
                            "id","age","race","educ","bpl","language",
                            "id_sp","age_sp","race_sp","educ_sp","bpl_sp",
                            "language_sp"))
  colnames(unions) <- c("metarea",
                        "idh","ageh","raceh","educh","bplh","languageh",
                        "idw","agew","racew","educw","bplw","languagew")
  unions <- na.omit(as.data.frame(unions))
  unions$metarea <- factor(as.character(unions$metarea))
  
  # Alternate Male Partners
  male_alternates <- subset(census, sex=="Male" &
                              (is.na(dur_mar) | dur_mar<=years_mar),
                            select=c("metarea","id",
                                     "age","race","educ","bpl","language"))
  colnames(male_alternates) <- c("metarea","idh",
                                 "ageh","raceh","educh","bplh","languageh")
  male_alternates <- na.omit(as.data.frame(male_alternates))
  male_alternates$metarea <- factor(as.character(male_alternates$metarea))
  
  # Alternate Female Partners
  female_alternates <- subset(census, sex=="Female" &
                                (is.na(dur_mar) | dur_mar<=years_mar),
                              select=c("metarea","id",
                                       "age","race","educ","bpl","language"))
  colnames(female_alternates) <- c("metarea","idw",
                                   "agew","racew","educw","bplw","languagew")
  female_alternates <- na.omit(as.data.frame(female_alternates))
  female_alternates$metarea <- factor(as.character(female_alternates$metarea))
  
  # Sample Counterfactual Spouses 
  markets <- generateCouples(n_fakes, unions,
                             male_alternates,
                             female_alternates,
                             geo="metarea", 
                             verbose=FALSE)
  
  return(markets)
}

#code variables for the marriage market dataset
add_vars <- function(markets) {
  #age difference 
  markets$agediff <- markets$ageh-markets$agew
  
  #birthplace endogamy
  markets$bpl.endog <- markets$bplh==markets$bplw
  
  #language endogamy 
  markets$language.endog <- markets$languageh==markets$languagew
  
  #educational hypergamy/hypogamy
  markets$hypergamy <- markets$educh > markets$educw
  markets$hypogamy <- markets$educh < markets$educw
  
  #create racial exogamy terms
  markets$raceh_pent <- code_race_pentagon(markets$raceh)
  markets$racew_pent <- code_race_pentagon(markets$racew)
  markets$race.exog <- createExogamyTerms(markets$raceh_pent, 
                                          markets$racew_pent, 
                                          symmetric=TRUE)
  #now replace endogamy with ethnic intermarriage for Asian and Hispanic groups
  lvls_tmp <- levels(markets$race.exog)
  markets$race.exog <- as.character(markets$race.exog)
  markets$race.exog <- ifelse(markets$raceh_pent=="Asian" & markets$racew_pent=="Asian" &
                                markets$raceh!=markets$racew, "Asian.Asian", 
                              ifelse(markets$raceh_pent=="Hispanic" & markets$racew_pent=="Hispanic" &
                                       markets$raceh!=markets$racew, "Hispanic.Hispanic",
                                     markets$race.exog))
  markets$race.exog <- factor(markets$race.exog,
                              levels=c(lvls_tmp,
                                       "Asian.Asian","Hispanic.Hispanic"))
  #test
  #table(markets$raceh, markets$racew, markets$race.exog)
  
  return(markets)
}


code_race_pentagon <- function(race) {
  race_pent <- ifelse(is.na(race), NA, 
                      ifelse(race=="Chinese" | race=="Japanese" | race=="Korean" |
                               race=="Filipino", "Asian", 
                             ifelse(race=="Mexican" | race=="Cuban" | 
                                      race=="Puerto Rican", "Hispanic",
                                    as.character(race))))
  race_pent <- factor(race_pent, 
                      levels=c("White","Black","Asian","Hispanic",
                               "Asian Indian"))
  return(race_pent)
}

