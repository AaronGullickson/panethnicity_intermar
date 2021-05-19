#' ---
#' title: "run_models.R"
#' author: ""
#' ---

# This script will run all of the models for the analysis. There are three
# different dimensions for running these models: dataset, race coding, and
# model type. All together each model will be classified by the name of 
# model_dataname_racecode_modeltype.
# 
# Dataset
# * "census" for Census 1980
# * "acs_1980basis" for ACS data that only includes comparable Latino and Asian
#   groups from 1980
# * "acs_restricted" for ACS that includes more groups than available in 1980, 
#   but only those large enough to sustain a specific ethnicity analysis
# * "acs_full" for ACS that includes all Asian and Latino groups regardless of 
#    size
# Race Coding
# * "pentagon" when using racial pentagon categories and simple dummies for 
#   Asian and Latino exogamy
# * "extended" when using specific ethnicity combination within Asian and Latino
#   exogamy and Black/Latino exogamy, plus Filipino/Latino dummy
# Model Type
# * "base" for a baseline model that controls for age and education
# * "bendog" for a model that adds birthplace endogamy to the baseline
# * "lendog" for a model that adds language endogamy to the baseline
# * "both" for a model that adds both birthplace and language endogamy to the 
#   baseline
# 
# The conditional logit models can be very slow, but the clogit function has 
# an option to use approximate estimation procedures. I have found that the 
# "efron" option runs much, much faster and the results are identical, so I am 
# using that for all modeling. 


# Load libraries, functions, and data -------------------------------------

library(here)
source(here("analysis","check_packages.R"))
source(here("analysis","useful_functions.R"))

#going to load and unload the data as I use it to save memory

# Run models --------------------------------------------------------------

#saving the model summary output should be fine

## Census 1980                                                 ##
#===============================================================#
load(here("analysis","output","markets_census.RData"))

#create formulas and put them into a list for faster processing of models

#Race pentagon formulas
formula_base <- formula(choice~agediff+I(agediff^2)+
                          hypergamy+hypogamy+edcross_hs+edcross_sc+edcross_c+
                          race_exog_pent+
                          strata(group))
formula_bendog <- update(formula_base, .~.+bendog_all_first)
formula_lendog <- update(formula_base, .~.+language_endog)
formula_both <- update(formula_bendog, .~.+language_endog)

formulas_pentagon <- list(base=formula_base,
                          bendog=formula_bendog,
                          lendog=formula_lendog,
                          both=formula_both)

#Race extended formulas
formula_base <- formula(choice~agediff+I(agediff^2)+
                          hypergamy+hypogamy+edcross_hs+edcross_sc+edcross_c+
                          race_exog_extended+race_filipino_latino+
                          strata(group))
formula_bendog <- update(formula_base, .~.+bendog_all_first)
formula_lendog <- update(formula_base, .~.+language_endog)
formula_both <- update(formula_bendog, .~.+language_endog)

formulas_extended <- list(base=formula_base,
                          bendog=formula_bendog,
                          lendog=formula_lendog,
                          both=formula_both)


markets_census <- lapply(markets_census, add_vars)

models_census_pentagon <- lapply(formulas_pentagon,
                                 function(formula) {
                                   poolChoiceModel(formula,
                                                   data=markets_census,
                                                   method="efron")
                                 })

models_census_extended <- lapply(formulas_extended,
                                 function(formula) {
                                   poolChoiceModel(formula, 
                                                   data=markets_census,
                                                   method="efron")
                                 })
rm(markets_census)


## ACS, 1980 Basis                                             ##
#===============================================================#
load(here("analysis","output","markets_acs_1980basis.RData"))
markets_acs_1980basis <- lapply(markets_acs_1980basis, add_vars)

#create formulas and put them into a list for faster processing of models

#Race pentagon formulas
formula_base <- formula(choice~agediff+I(agediff^2)+
                          hypergamy+hypogamy+edcross_hs+edcross_sc+edcross_c+
                          race_exog_pent+
                          strata(group))
formula_bendog <- update(formula_base, .~.+bendog_partial_flex1.5)
formula_lendog <- update(formula_base, .~.+language_endog)
formula_both <- update(formula_bendog, .~.+language_endog)

formulas_pentagon <- list(base=formula_base,
                          bendog=formula_bendog,
                          lendog=formula_lendog,
                          both=formula_both)

#Race extended formulas
formula_base <- formula(choice~agediff+I(agediff^2)+
                          hypergamy+hypogamy+edcross_hs+edcross_sc+edcross_c+
                          race_exog_extended+race_filipino_latino+
                          strata(group))
formula_bendog <- update(formula_base, .~.+bendog_partial_flex1.5)
formula_lendog <- update(formula_base, .~.+language_endog)
formula_both <- update(formula_bendog, .~.+language_endog)

formulas_extended <- list(base=formula_base,
                          bendog=formula_bendog,
                          lendog=formula_lendog,
                          both=formula_both)


models_acs1980_pentagon <- lapply(formulas_pentagon,
                                  function(formula) {
                                    poolChoiceModel(formula, 
                                                    data=markets_acs_1980basis,
                                                    method="efron")
                                  })

models_acs1980_extended <- lapply(formulas_extended,
                                  function(formula) {
                                    poolChoiceModel(formula, 
                                                    data=markets_acs_1980basis,
                                                    method="efron")
                                  })

rm(markets_acs_1980basis)

## ACS, Full                                                   ##
#===============================================================#
load(here("analysis","output","markets_acs_full.RData"))
markets_acs_full <- lapply(markets_acs_full, add_vars)

models_acsfull_pentagon <- lapply(formulas_pentagon,
                                  function(formula) {
                                    poolChoiceModel(formula, 
                                                    data=markets_acs_full,
                                                    method="efron")
                                  })

rm(markets_acs_full)

## ACS, Full                                                   ##
#===============================================================#
load(here("analysis","output","markets_acs_restricted.RData"))
markets_acs_restricted <- lapply(markets_acs_restricted, add_vars)

models_acsres_extended <- lapply(formulas_extended,
                                 function(formula) {
                                   poolChoiceModel(formula, 
                                                   data=markets_acs_restricted,
                                                   method="efron")
                                 })
rm(markets_acs_restricted)

save(models_census_pentagon, models_census_extended, models_acs1980_pentagon, 
     models_acs1980_extended, models_acsfull_pentagon, models_acsres_extended,
     file=here("analysis","output","models.RData"))
