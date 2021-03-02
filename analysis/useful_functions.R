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
    #(raced>=300 & raced<400) ~ "Indigenous",
    raced==400 | raced==410 ~ "Chinese",
    raced==500 ~ "Japanese",
    raced==600 ~ "Filipino",
    raced==610 ~ "Asian Indian",
    raced==620 ~ "Korean",
    raced==640 ~ "Vietnamese",
    raced==641 ~ "Bhutanese",
    raced==642 ~ "Mongolian",
    raced==643 ~ "Nepalese",
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
                             "Asian", 
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
                                           "Hispanic",
                                           as.character(race)))))
  
  race_pent <- factor(race_pent,
                      levels=c("White","Black","Asian","Hispanic",
                               "South Asian"))
  return(race_pent)
  
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
        raceh_pent=="Asian" & racew_pent=="Asian" & raceh!=racew ~ "Asian.Asian",
        raceh_pent=="South Asian" & racew_pent=="South Asian" & 
          raceh!=racew ~ "South Asian.South Asian",
        raceh_pent=="Hispanic" & racew_pent=="Hispanic" & raceh!=racew ~ "Hispanic.Hispanic",
        TRUE ~ as.character(race_exog_pent)
      ),
      race_exog_extended=case_when(
        race_exog_pent=="Asian.Asian" | 
          race_exog_pent=="South Asian.South Asian" |
          race_exog_pent=="Hispanic.Hispanic" |
          race_exog_pent=="Black.Hispanic" ~ as.character(race_exog_full),
        TRUE ~ as.character(race_exog_pent)
      )
    )
                                   
  #tests
  # with(subset(markets, race_exog_pent=="Asian.Asian"), table(raceh, racew))
  # with(subset(markets, race_exog_pent=="Hispanic.Hispanic"), table(raceh, racew))
  # with(subset(markets, race_exog_pent=="Endog"), table(raceh, racew))
  # with(subset(markets, race_exog_pent=="Asian.Asian"), 
  #      table(race_exog_extended, exclude=NULL))
  # with(subset(markets, race_exog_pent=="Hispanic.Hispanic"), 
  #      table(race_exog_extended, exclude=NULL))
  # with(subset(markets, race_exog_pent=="Black.Hispanic"), 
  #           table(race_exog_extended, exclude=NULL))
  # with(subset(markets, race_exog_pent=="South Asian.South Asian"), 
  #     table(race_exog_extended, exclude=NULL))

  #turn into factors and set Endog as the reference
  markets$race_exog_pent <- relevel(factor(markets$race_exog_pent), "Endog")
  markets$race_exog_extended <- relevel(factor(markets$race_exog_extended), 
                                        "Endog")
  
  #test
  #table(markets$race_exog_extended, markets$race_exog_pent)
  
  #dummy variable for Filipino/Latino
  markets$race_filipino_hispanic <- markets$race_exog_extended=="Asian.Hispanic" & 
    (markets$raceh=="Filipino" | markets$racew=="Filipino") 
  
  #test
  #with(subset(markets, race_filipino_hispanic),
  #     table(raceh, racew))
  
  return(markets)
}

#create a distance matrix from model output
calc_or_matrix <- function(model_summary, var_name, selected) {
  coefs <- model_summary$coef[,"coef"]
  coefs <- coefs[grepl(var_name,names(coefs))]
  #get single racial categories from names
  temp <- gsub(var_name,"",names(coefs))
  temp <- strsplit(temp, "\\.")
  race1 <- sapply(temp, function(x) {x[1]})
  race2 <- sapply(temp, function(x) {x[2]})
  groups <- unique(c(race1,race2))
  tab <- matrix(0, length(groups), length(groups))
  rownames(tab) <- colnames(tab) <- groups
  for(i in 1:length(race1)) {
    tab[race1[i], race2[i]] <- coefs[i]
    tab[race2[i], race1[i]] <- coefs[i]
  }
  tab <- tab[rownames(tab) %in% selected,colnames(tab) %in% selected]
  tab <- exp(-1 * tab)
  #dist <- as.dist(tab)
  return(tab)
}

plot_dendrogram <- function(model) {
  calc_distance(model)
  hc <- hclust(dist, method="average")
  plot(as.dendrogram(hc), 
       las=1, xlab="", ylab="distance")
  abline(h=1, lty=3, col="red")
}


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

#convert the intermarriage variable names to something pretty
convert_intermar_names <- function(var_names, prefix) {
  var_names <- sub("\\.","/", sub(prefix,"",var_names))
  return(var_names)
}

#pull out coefs belonging to a certain group from model summary output
pull_group_coefs <- function(model_summary, groups) {
  temp <- model_summary$coefficients[,c(1,3)]
  group_coefs <- tibble(variable=rownames(temp), coef=temp[,1], se=temp[,2]) %>%
    filter(grepl("race_exog", variable) & 
             grepl(paste(groups,collapse="|"), variable))
  return(group_coefs)
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
