library(here)
source(here("analysis","check_packages.R"))
source(here("analysis","useful_functions.R"))
load(here("analysis","output","markets_census.RData"))
load(here("analysis","output","markets_acs.RData"))

with(subset(markets_census, choice),
     table(raceh, racew))
with(subset(markets_acs_shortrace, choice),
     table(raceh, racew))
with(subset(markets_acs_fullrace, choice),
     table(raceh, racew))

#add variables
markets_census <- add_vars(markets_census)
markets_acs_shortrace <- add_vars(markets_acs_shortrace)

# Run Models --------------------------------------------------------------

#baseline model

#estimate model without birthplace and language endogamy
formula_baseline <- formula(choice~agediff+I(agediff^2)+
                              hypergamy+hypogamy+
                              edcross_hs+edcross_sc+edcross_c+
                              race_exog_pent+ 
                              strata(group))
formula_bpl_endog <- update(formula_baseline, .~.+bpl_endog)
formula_lang_endog <- update(formula_baseline, .~.+language_endog)
formula_full <- update(formula_bpl_endog, .~.+language_endog)


model_census1 <- clogit(formula_baseline, data=markets_census)
model_census2 <- clogit(formula_bpl_endog, data=markets_census)
model_census3 <- clogit(formula_lang_endog, data=markets_census)
model_census4 <- clogit(formula_full, data=markets_census)

model_acs1 <- clogit(formula_baseline, data=markets_acs_shortrace)
model_acs2 <- clogit(formula_bpl_endog, data=markets_acs_shortrace)
model_acs3 <- clogit(formula_lang_endog, data=markets_acs_shortrace)
model_acs4 <- clogit(formula_full, data=markets_acs_shortrace)

#saving the model summary output should be fine
models_census_summary <- lapply(list(model_census1, model_census2, 
                                    model_census3, model_census4),
                               summary)
models_acs_summary <- lapply(list(model_acs1, model_acs2, 
                                  model_acs3, model_acs4),
                             summary)

save(models_census_summary, models_acs_summary,
     file=here("analysis","output","models_restricted.RData"))

