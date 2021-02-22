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
# * "shortacs" for ACS data that only includes three comparable Latino groups
#   from 1980
# * "fullacs" for ACS that includes more Latino groups than available in 1980
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
load(here("analysis","output","markets_acs.RData"))

#add variables to the market datasets
markets_census <- add_vars(markets_census)
markets_acs_shortrace <- add_vars(markets_acs_shortrace)
markets_acs_fullrace <- add_vars(markets_acs_fullrace)

# Create functions --------------------------------------------------------

#create formulas and put them into a list for faster processing of models

#Race pentagon formulas
formula_base <- formula(choice~agediff+I(agediff^2)+
                          hypergamy+hypogamy+edcross_hs+edcross_sc+edcross_c+
                          race_exog_pent+
                          strata(group))
formula_bendog <- update(formula_base, .~.+bpl_endog)
formula_lendog <- update(formula_base, .~.+language_endog)
formula_both <- update(formula_bendog, .~.+language_endog)

formulas_pentagon <- list(base=formula_base,
                          bendog=formula_bendog,
                          lendog=formula_lendog,
                          both=formula_both)

#Race extended formulas
formula_base <- formula(choice~agediff+I(agediff^2)+
                          hypergamy+hypogamy+edcross_hs+edcross_sc+edcross_c+
                          race_exog_extended+race_filipino_hispanic+
                          strata(group))
formula_bendog <- update(formula_base, .~.+bpl_endog)
formula_lendog <- update(formula_base, .~.+language_endog)
formula_both <- update(formula_bendog, .~.+language_endog)

formulas_extended <- list(base=formula_base,
                          bendog=formula_bendog,
                          lendog=formula_lendog,
                          both=formula_both)


# Run models --------------------------------------------------------------

#saving the model summary output should be fine

models_census_pentagon <- lapply(formulas_pentagon,
                                 function(formula) {
                                   summary(clogit(formula, 
                                                  data=markets_census,
                                                  method="efron"))
                                 })

models_shortacs_pentagon <- lapply(formulas_pentagon,
                                   function(formula) {
                                     summary(clogit(formula, 
                                                    data=markets_acs_shortrace,
                                                    method="efron"))
                                   })

models_fullacs_pentagon <- lapply(formulas_pentagon,
                                  function(formula) {
                                    summary(clogit(formula, 
                                                   data=markets_acs_fullrace,
                                                   method="efron"))
                                  })

models_census_extended <- lapply(formulas_extended,
                                 function(formula) {
                                   summary(clogit(formula, 
                                                  data=markets_census,
                                                  method="efron"))
                                 })

models_shortacs_extended <- lapply(formulas_extended,
                                   function(formula) {
                                     summary(clogit(formula, 
                                                    data=markets_acs_shortrace,
                                                    method="efron"))
                                   })

models_fullacs_extended <- lapply(formulas_extended,
                                  function(formula) {
                                    summary(clogit(formula, 
                                                   data=markets_acs_fullrace,
                                                   method="efron"))
                                  })


save(models_census_pentagon, models_shortacs_pentagon, models_fullacs_pentagon, 
     models_census_extended, models_shortacs_extended, models_fullacs_extended,
     file=here("analysis","output","models.RData"))

