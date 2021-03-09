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
load(here("analysis","output","markets_census.RData"))
load(here("analysis","output","markets_acs_full.RData"))
load(here("analysis","output","markets_acs_restricted.RData"))
load(here("analysis","output","markets_acs_1980basis.RData"))

#add variables to the market datasets
markets_census <- mclapply(markets_census, add_vars)
markets_acs_1980basis <- mclapply(markets_acs_1980basis, add_vars)
markets_acs_restricted <- mclapply(markets_acs_restricted, add_vars)
markets_acs_full <- mclapply(markets_acs_full, add_vars)

# Create formulas --------------------------------------------------------

#create formulas and put them into a list for faster processing of models

#Race pentagon formulas
formula_base <- formula(choice~agediff+I(agediff^2)+
                          hypergamy+hypogamy+edcross_hs+edcross_sc+edcross_c+
                          race_exog_pent+
                          strata(group))
formula_bendog <- update(formula_base, .~.+bendog_flex2)
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
formula_bendog <- update(formula_base, .~.+bendog_flex2)
formula_lendog <- update(formula_base, .~.+language_endog)
formula_both <- update(formula_bendog, .~.+language_endog)

formulas_extended <- list(base=formula_base,
                          bendog=formula_bendog,
                          lendog=formula_lendog,
                          both=formula_both)

# Run models --------------------------------------------------------------

#saving the model summary output should be fine

### First run comparable pentagon models for census and ACS
models_census_pentagon <- mclapply(formulas_pentagon,
                                   function(formula) {
                                     poolChoiceModel(formula,
                                                     data=markets_census,
                                                     method="efron")
                                   })

models_acs1980_pentagon <- mclapply(formulas_pentagon,
                                    function(formula) {
                                      poolChoiceModel(formula, 
                                                      data=markets_acs_1980basis,
                                                      method="efron")
                                    })

### Then run a pentagon model on the ACS data with full set of ethnic groups
models_acsfull_pentagon <- mclapply(formulas_pentagon,
                                    function(formula) {
                                      poolChoiceModel(formula, 
                                                      data=markets_acs_full,
                                                      method="efron")
                                    })

## Now try extended models for the 1980 basis data
models_census_extended <- mclapply(formulas_extended,
                                   function(formula) {
                                     poolChoiceModel(formula, 
                                                     data=markets_census,
                                                     method="efron")
                                   })

models_acs1980_extended <- mclapply(formulas_extended,
                                   function(formula) {
                                     poolChoiceModel(formula, 
                                                     data=markets_acs_1980basis,
                                                     method="efron")
                                   })

### Now try an extended model for the ACS data
models_acsres_extended <- mclapply(formulas_extended,
                                   function(formula) {
                                     poolChoiceModel(formula, 
                                                     data=markets_acs_restricted,
                                                     method="efron")
                                   })

save(models_census_pentagon, models_acs1980_pentagon, models_acsfull_pentagon, 
     models_census_extended, models_acs1980_extended, models_acsres_extended,
     file=here("analysis","output","models.RData"))


# Test Asian Indian/American Indian model ---------------------------------

# model_sa.ai <- clogit(update(formulas_pentagon$both, 
#                              .~.+race_asianindian_aian), 
#                       data=markets_acs_full,
#                       method="efron")

#Commenting out because it has model-fitting issues.
