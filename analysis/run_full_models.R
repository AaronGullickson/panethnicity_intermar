#This script will run the full models in which I allow every single pairwise
#comparison at the lower level of aggregation to be a dummy. Its a lot of terms
#so this model takes a long time to run and will not run on the 1980 data
#due to too many zero cells

library(here)
source(here("analysis","check_packages.R"))
source(here("analysis","useful_functions.R"))
load(here("analysis","output","markets_census.RData"))
load(here("analysis","output","markets_acs.RData"))

with(subset(markets_acs_fullrace, choice),
     table(raceh, racew))
with(subset(markets_acs_shortrace, choice),
     table(raceh, racew))

#add variables
markets_acs_shortrace <- add_vars(markets_acs_shortrace)

#run the model
model_acs_full <- clogit(choice~agediff+I(agediff^2)+ #husband-wife age difference
                           bpl_endog+language_endog+ #language and birthplace endogamy
                           hypergamy+hypogamy+ #education parameters
                           edcross_hs+edcross_sc+edcross_c+
                           race_exog_full+ #gender-symmetric racial exogamy
                           strata(group), data=markets_acs_shortrace)
