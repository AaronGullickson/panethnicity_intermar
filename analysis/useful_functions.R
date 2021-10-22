## useful_functions.R


# Functions to code raw data ----------------------------------------------

#I will put all of the coding variable stuff into a function to ensure that we
#do the same operations on both datasets when we make changes. I will then put
#each separate variable coding into a sub-function to ensure that we do the same
#thing for each spouse.

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
  census$lang <- code_language(census$languaged)
  census$lang_sp <- code_language(census$languaged_sp)
  
  # Country of Birth 
  # The general codes are way to general. The detailed codes have some
  # overspecifity in the codes, but not in the actual data used here.
  census$bpld <- code_bpl(census$bpld)
  census$bpld_sp <- code_bpl(census$bpld_sp)
  
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
  
  #combined state and metro area id to get marriage market. Replace state id
  #with metro id for cases where it is not zero. Multiply by 100 to avoid id
  #collision
  census$mar_market <- census$statefip
  
  return(census)
}

code_race <- function(raced, hispand) {
  # We want to take the raced and hispand variables and code them into a combined
  # race variable. I am going to use the fullest possible coding here although
  # only a few of these cases will show up in the 1980 data. I am also going 
  # to leave out the indigenous population for the moment, due to some issues
  # with measurement across the two time periods.
  race <- case_when(
    hispand==100 ~ "Mexican",
    hispand==200 ~ "Puerto Rican",
    hispand==300 ~ "Cuban",
    hispand==411 ~ "Costa Rican",
    hispand==412 ~ "Guatemalan",
    hispand==413 ~ "Honduran",
    hispand==414 ~ "Nicaraguan",
    hispand==415 ~ "Panamanian",
    hispand==416 ~ "Salvadorian",
    hispand==420 ~ "Argentinian",
    hispand==421 ~ "Bolivian",
    hispand==422 ~ "Chilean",
    hispand==423 ~ "Colombian",
    hispand==424 ~ "Ecuadorian",
    hispand==425 ~ "Paraguayan",
    hispand==426 ~ "Peruvian",
    hispand==427 ~ "Uruguayan",
    hispand==428 ~ "Venezuelan",
    hispand==460 ~ "Dominican",
    hispand>=400 & hispand!=450 ~ NA_character_,
    raced==100 ~ "White",
    raced==200 ~ "Black",
    (raced>=300 & raced<400) ~ "AIAN",
    raced==400 | raced==410 ~ "Chinese",
    raced==500 ~ "Japanese",
    raced==600 ~ "Filipino",
    raced==610 ~ "Asian Indian",
    raced==620 ~ "Korean",
    raced==640 ~ "Vietnamese",
#    raced==641 ~ "Bhutanese",
#    raced==642 ~ "Mongolian",
#    raced==643 ~ "Nepalese",
    raced==660 ~ "Cambodian",
    raced==661 ~ "Hmong",
    raced==662 ~ "Laotian",
    raced==663 ~ "Thai",
    raced==664 ~ "Bangladeshi",
    raced==665 ~ "Burmese",
    raced==666 ~ "Indonesian",
    raced==667 ~ "Malaysian",
    raced==669 ~ "Pakistani",
    raced==670 ~ "Sri Lankan",
    TRUE ~ NA_character_
  )
  
  race <- factor(race)

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

code_bpl <- function(bpld) {
  #recode any one born in the US (99 or less) as a single number. Otherwise
  #we will be fitting state level endogamy. also code in missing values
  return(ifelse(bpld>=95000, NA, 
                ifelse(bpld<10000,1,bpld)))
}

code_language <- function(language) {
  #I need to use the detailed language codes as the general language codes 
  #are too general. However the detailed language codes are too detailed in 
  #some places, particularly in translating between the two time periods. Thus
  #I make some corrections to the detailed codes for consistency between the
  #two time periods.
  
  lang_recode <- language
  
  #the following cases will be collapsed to the their top two digit codes
  #(starting at the 100 levels)
  #1:27 - European language groups (e.g. English, French, German)
  #35: Uralic
  #37: Other Altaic
  #43: Chinese
  #47: Thai/Siamese/Lao (not separable in 1980)
  #52: Indonesian
  #53: Other Malay
  #58:  Near East Arabic Dialect
  collapsed_cases <- c(1:24,35,37,43,47,52,53,58)
  collapsed_language <- floor(language/100)
  lang_recode <- ifelse(collapsed_language %in% collapsed_cases,
                        collapsed_language*100, lang_recode)
  
  #A couple of cases need to be put back into there other categories
  #420: Afrikaans
  #1140: Haitian Creole
  #1150: Cajun
  #2310: Croatian
  #2320: Serbian
  uncollapsed_cases <- c(420,1140,1150,2310,2320)
  lang_recode <- ifelse(language %in% uncollapsed_cases,
                        language, lang_recode)
  
  #put malay and other malay together
  lang_recode <- ifelse(language==5270, 5300, lang_recode)
  
  #collapse Hindi and Urdu into 3101 (Hindustani)
  lang_recode <- ifelse(language>3101 & language<=3104, 3101, lang_recode)
  
  #For 1980 consistency put all American Indian languages in one group
  lang_recode <- ifelse(lang_recode>7000 & lang_recode<=9300, 7000, 
                        lang_recode)
  
  #A few cases are "other" or "nec". These will be recoded as -1 and
  #not treated as endogamy with each other
  nec_codes <- c(3140,3150,3190,5290,5590,6200,6390,6400,9400,9410,9420,9500,
                 9600,9601,9602,9999)
  lang_recode <- ifelse(lang_recode %in% nec_codes, -1, lang_recode)
  
  #code any missing values 
  lang_recode <- ifelse(lang_recode==0, NA, lang_recode)
  
  return(lang_recode)
  
}

is_single <- function(marst) {
  #I am going to consider separated people as single here
  return(marst!="Married, spouse present" & marst!="Married, spouse absent")
}


# Functions for marriage market creation ----------------------------------

#This function will take the given census data, separate out 
#couples and alternate spouses, and create the marriage market data 
#for analysis. the second argument gives the time span for consideration
#of who will be considered. Its important to keep in mind that intervalled
#nature of years in the USA for the census 1980 data. years_mar that is not
#consistent with those intervals can create problems
create_unions <- function(census, years_mar, n_fakes, n_samples=3) {
  
  # eliminate individuals who:
  # a) have been in the USA less than the marriage window
  # b) are currently married longer than the marriage window
  # or who have a marriage duration longer than the current window, if any at all
  census <- subset(census, (is.na(yr_usa) | yr_usa>years_mar) & 
                     (is.na(yr_usa_sp) | yr_usa_sp>years_mar) &
                     (is_single(marst) | dur_mar<=years_mar),
                   select=c("mar_market","sex","hhwt","perwt","dur_mar","marst",
                            "id","age","race","educ","bpld","lang","marrno","age_usa",
                            "id_sp","age_sp","race_sp","educ_sp","bpld_sp","lang_sp",
                            "marrno_sp","age_usa_sp"))
  
  # Actual couples who are within the marriage duration window of years_mar
  # both must be in a first time marriage for consistency with 1980 marriage
  # duration calculation
  unions <- subset(census, sex=="Male" & 
                     !is.na(id_sp) & 
                     (marrno<2 & marrno_sp<2),
                   select=c("mar_market","hhwt",
                            "id","age","race","educ","bpld","lang","age_usa",
                            "id_sp","age_sp","race_sp","educ_sp","bpld_sp",
                            "lang_sp","age_usa_sp"))
  colnames(unions) <- c("mar_market","hhwt",
                        "idh","ageh","raceh","educh","bplh","languageh","age_usah",
                        "idw","agew","racew","educw","bplw","languagew","age_usaw")
  unions <- na.omit(as.data.frame(unions))
  unions$mar_market <- factor(as.character(unions$mar_market))

  # Alternate Male Partners
  male_alternates <- subset(census, sex=="Male",
                            select=c("mar_market","id","perwt",
                                     "age","race","educ","bpld","lang","age_usa"))
  colnames(male_alternates) <- c("mar_market","idh","perwt",
                                 "ageh","raceh","educh","bplh","languageh","age_usah")
  male_alternates <- na.omit(as.data.frame(male_alternates))
  male_alternates$mar_market <- factor(as.character(male_alternates$mar_market))

  # Alternate Female Partners
  female_alternates <- subset(census, sex=="Female",
                              select=c("mar_market","id","perwt",
                                       "age","race","educ","bpld","lang","age_usa"))
  colnames(female_alternates) <- c("mar_market","idw", "perwt",
                                   "agew","racew","educw","bplw","languagew","age_usaw")
  female_alternates <- na.omit(as.data.frame(female_alternates))
  female_alternates$mar_market <- factor(as.character(female_alternates$mar_market))

  # Sample Counterfactual Spouses 
  markets <- replicate(n_samples,
                       generateCouples(n_fakes, unions, male_alternates, 
                                       female_alternates, geo="mar_market", 
                                       weight="perwt", verbose=FALSE),
                       simplify=FALSE)
  
  return(markets)
}

#code variables for the marriage market dataset
add_vars <- function(markets) {
  #age difference 
  markets$agediff <- markets$ageh-markets$agew
  
  #birthplace endogamy - will produce several alternate specifications
  markets <- code_birthplace_endog(markets)
  
  #language endogamy 
  # The -1 cases are NEC languages, so we assume non-endogamous
  markets$language_endog <- ifelse(markets$languageh<0 | markets$languagew<0, 
                                   FALSE, markets$languageh==markets$languagew)
  
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
  
  
  #pentagon based
  markets$raceh_pent <- code_race_pentagon(markets$raceh)
  markets$racew_pent <- code_race_pentagon(markets$racew)
  markets$race_exog_pent <- createExogamyTerms(markets$raceh_pent, 
                                               markets$racew_pent, 
                                               symmetric=TRUE)
  
  #now replace pentagon with Asian and Latino exogamy and for extended version
  #replace with specific ethnic combinations
  markets <- markets %>% 
    mutate(
      race_exog_pent=case_when(
        raceh_pent=="E&SE Asian" & racew_pent=="E&SE Asian" & raceh!=racew ~ "E&SE Asian.E&SE Asian",
        raceh_pent=="South Asian" & racew_pent=="South Asian" & 
          raceh!=racew ~ "South Asian.South Asian",
        raceh_pent=="Latino" & racew_pent=="Latino" & raceh!=racew ~ "Latino.Latino",
        TRUE ~ as.character(race_exog_pent)
      ),
      race_exog_extended=case_when(
        race_exog_pent=="E&SE Asian.E&SE Asian" | 
          race_exog_pent=="South Asian.South Asian" |
          race_exog_pent=="Latino.Latino" |
          race_exog_pent=="Black.Latino" |
          race_exog_pent=="Black.E&SE Asian" |
          race_exog_pent=="White.Latino" |
          race_exog_pent=="White.E&SE Asian"~ as.character(race_exog_full),
        TRUE ~ as.character(race_exog_pent)
      )
    )

  #turn into factors and set Endog as the reference
  markets$race_exog_pent <- relevel(factor(markets$race_exog_pent), "Endog")
  markets$race_exog_extended <- relevel(factor(markets$race_exog_extended), 
                                        "Endog")
  
  #test
  #table(markets$race_exog_extended, markets$race_exog_pent)
  
  #dummy variable for Filipino/Latino
  markets$race_filipino_latino <- markets$race_exog_extended=="Latino.E&SE Asian" & 
    (markets$raceh=="Filipino" | markets$racew=="Filipino") 
  
  markets$race_asianindian_aian <- markets$race_exog_extended=="AIAN.South Asian" & 
    (markets$raceh=="Asian Indian" | markets$racew=="Asian Indian") 
  
  #test
  #with(subset(markets, race_filipino_latino),
  #     table(raceh, racew))
  
  return(markets)
}

#code race back into the racial pentagon, plus Asian Indian
code_race_pentagon <- function(race) {
  race_pent <- ifelse(is.na(race), NA, 
                      ifelse(race=="Chinese" | 
                               race=="Japanese" | 
                               race=="Korean" | 
                               race=="Filipino" | 
                               race=="Vietnamese" |
                               race=="Cambodian" |
                               race=="Hmong" |
                               race=="Laotian" |
                               race=="Thai" |
                               race=="Burmese"  | 
                               race=="Indonesian" | 
                               race=="Malaysian" |
                               race=="Bhutanese" |
                               race=="Nepalese",
                             "E&SE Asian", 
                             ifelse(race=="Asian Indian" | 
                                      race=="Pakistani" | 
                                      race=="Bangladeshi" | 
                                      race=="Sri Lankan", 
                                    "South Asian",
                                    ifelse(race=="Mexican" | 
                                             race=="Cuban" | 
                                             race=="Puerto Rican" | 
                                             race=="Guatemalan" |
                                             race=="Salvadorian" | 
                                             race=="Dominican" |
                                             race=="Colombian" |
                                             race=="Costa Rican" |
                                             race=="Honduran" |
                                             race=="Nicaraguan" |
                                             race=="Panamanian" |
                                             race=="Argentinian" |
                                             race=="Bolivian" | 
                                             race=="Chilean" | 
                                             race=="Ecuadorian" | 
                                             race=="Peruvian" | 
                                             race=="Venezuelan" | 
                                             race=="Paraguayan" | 
                                             race=="Uruguayan", 
                                           "Latino",
                                           as.character(race)))))
  
  race_pent <- factor(race_pent,
                      levels=c("White","Black","AIAN","Latino","E&SE Asian",
                               "South Asian"))
  return(race_pent)
  
}

#code several different variables that indicate birthplace endogamy
code_birthplace_endog <- function(markets) {
  # I want to think carefully about how being a member of the 1.75 generation
  # (0-5 at entry to US), 1.5 generation (6-12 at entry), and 1.25 generation
  # (13-17 at entry) affect endogamy. I do this by how I consider endogamy, with
  # three choices:
  #
  # USA - this group is considered to be US-born only for purposes of endogamy
  # Both - this group is considered to be endogamous both with US and birthplace
  # Birthplace - this group is considered to be endogamous with birthplace only
  # 
  # Given these different options, I can construct 10 different possible codings
  # if we force the codings to be consistent so that a lower generation is never
  # given a more "assimilated" coding than a higher generation
  
  #first create booleans for generations each spouse
  is_h_1.75 <- markets$bplh!=1 & markets$age_usah<6
  is_h_1.5  <- markets$bplh!=1 & markets$age_usah>5 & markets$age_usah<13
  is_h_1.25 <- markets$bplh!=1 & markets$age_usah>12 & markets$age_usah<18
  
  is_w_1.75 <- markets$bplw!=1 & markets$age_usaw<6
  is_w_1.5  <- markets$bplw!=1 & markets$age_usaw>5 & markets$age_usaw<13
  is_w_1.25 <- markets$bplw!=1 & markets$age_usaw>12 & markets$age_usaw<18
  
  #now booleans for birthplace endog
  birthplace_endog <- markets$bplh==markets$bplw
  
  #create switched birthplaces for USA endog, each one inclusive of laters
  bplh_1.75 <- ifelse(is_h_1.75, 1, markets$bplh)
  bplh_1.5  <- ifelse(is_h_1.75 | is_h_1.5, 1, markets$bplh)
  bplh_1.25 <- ifelse(is_h_1.75 | is_h_1.5 | is_h_1.25, 1, markets$bplh)
  
  bplw_1.75 <- ifelse(is_w_1.75, 1, markets$bplw)
  bplw_1.5  <- ifelse(is_w_1.75 | is_w_1.5, 1, markets$bplw)
  bplw_1.25 <- ifelse(is_w_1.75 | is_w_1.5 | is_w_1.25, 1, markets$bplw)
  
  ## Create Variables ##
  
  ## All First Gen
  #strictest coding treats all three cases the same as second gen (birthplace)
  markets$bendog_all_first <- birthplace_endog
  
  ## All Second Gen
  ## all are treated as born in the US
  markets$bendog_all_second <- bplh_1.25==bplw_1.25
  
  ## All Flex
  ## either birthplace or US is treated as endogamous for all
  markets$bendog_all_flex <- bplh_1.25==bplw_1.25 | birthplace_endog
  
  ## Steep Grade (1.75): 1.75: USA, 1.5: Birthplace, 1.25: Birthplace
  markets$bendog_steep_grade1.75 <- bplh_1.75==bplw_1.75

  ## Steep Grade (1.5): 1.75: USA, 1.5: USA, 1.25: Birthplace
  markets$bendog_steep_grade1.5 <- bplh_1.5==bplw_1.5
  
  ## Slight Grade (1.75): 1.75: USA, 1.5: Both, 1.25: Both
  markets$bendog_slight_grade1.75 <- markets$bendog_steep_grade1.75 | 
    markets$bendog_all_second
 
  ## Slight Grade (1.5): 1.75: USA, 1.5: USA, 1.25: Both
  markets$bendog_slight_grade1.5 <- markets$bendog_steep_grade1.5 | 
    markets$bendog_all_second
  
  ## Full Grade: 1.75: USA, 1.5: Both, 1.25: Birthplace
  markets$bendog_full_grade <-  markets$bendog_steep_grade1.5 | 
    markets$bendog_steep_grade1.75
  
  ## Partial Flex (1.5): 1.75: Both, 1.5: Birthplace, 1.25: Birthplace
  markets$bendog_partial_flex1.75 <- bplh_1.75==bplw_1.75 | birthplace_endog
  
  ## Partial Flex (1.75): 1.75: Both, 1.5: Both, 1.25: Birthplace
  markets$bendog_partial_flex1.5 <- bplh_1.5==bplw_1.5 | birthplace_endog
  
  return(markets)
}

# Functions for model output summaries ------------------------------------

#create a distance matrix from model output
calc_or_matrix <- function(coefs) {
  temp <- strsplit(as.character(coefs$variable), "/")
  race1 <- sapply(temp, function(x) {x[1]})
  race2 <- sapply(temp, function(x) {x[2]})
  groups <- unique(c(race1,race2))
  tab <- matrix(0, length(groups), length(groups))
  rownames(tab) <- colnames(tab) <- groups
  for(i in 1:length(race1)) {
    tab[race1[i], race2[i]] <- coefs$coef[i]
    tab[race2[i], race1[i]] <- coefs$coef[i]
  }
  tab <- exp(-1 * tab)
  return(tab)
}

#convert the intermarriage variable names to something pretty
convert_intermar_names <- function(var_names, prefix) {
  var_names <- sub("\\.","/", sub(prefix,"",var_names))
  temp <- strsplit(var_names, "/")
  ethnic_exog <- sapply(temp, function(x) {
    if(length(x)>=2) {
      return(x[1]==x[2])
    } else {
      return(TRUE)
    }
  })
  first_name <- sapply(temp, function(x) {x[1]})
  var_names <- ifelse(ethnic_exog, 
                      paste(first_name, "ethnic exogamy"), var_names)
  return(var_names)
}

#order the variable variable (hah!) from smallest to largest based on a given
#year
order_variables <- function(coefs, data_choice, model_choice) {
  ord <- coefs %>% 
    filter(data==data_choice & model==model_choice) %>%
    select(variable, coef)
  lvls <- levels(reorder(factor(ord$variable), ord$coef, max))
  #check for missing cases in lvls and add to end if so
  all_vars <- unique(coefs$variable)
  missing_vars <- all_vars[!(all_vars %in% lvls)]
  coefs$variable <- factor(coefs$variable, levels=c(missing_vars,lvls))
  return(coefs)
}

#A function to convert model summaries to something knitreg will understand
convertModel <- function(model) {
  tr <- createTexreg(
    coef.names = rownames(model$coef), 
    coef = model$coef[,"b.pool"], 
    se = model$coef[,"se.pool"], 
    pvalues = model$coef[,"pvalue.pool"],
    gof.names = c("Deviance","BIC (relative to null)"), 
    gof = c(mean(model$deviance), mean(model$bic)), 
    gof.decimal = c(FALSE, FALSE)
  )
}

# Miscellaneous -----------------------------------------------------------

sum_symmetric <- function(tab) {
  for(i in 1:nrow(tab)) {
    for(j in 1:ncol(tab)) {
      if(j>i) {
        tab[j,i] <- tab[i,j]+tab[j,i]
        tab[i,j] <- NA
      }
    }
  }
  return(tab)
}


