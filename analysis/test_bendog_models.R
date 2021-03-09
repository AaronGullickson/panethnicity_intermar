#' ---
#' title: "test_bendog_models.R"
#' author: ""
#' ---

# This script will test out different variations of coding birthplace endogamy
# to account for the 1.25 , 1.5, and 1.75 generations. I will try different
# versions of the variable on models of ACS data and then look at how well each
# fits the data.

#|  Gen |  All 2nd    |Gradation 1 |Gradation 2 |Gradation 3 |Gradation 4 |Gradation 5 | Full USA   |
#|------|-------------|------------|------------|------------|------------|------------|------------|
#| 1.75 |  Birthplace | USA        | USA        | USA        | USA        | USA        | USA        |
#| 1.5  |  Birthplace | Birthplace | Both       | Both       | USA        | USA        | USA        |
#| 1.25 |  Birthplace | Birthplace | Birthplace | Both       | Both       | Birthplace | USA        |
  
#|  Gen |  Full Flexible | Partial Flex 1 | Partial Flex 2 | 
#|------|----------------|----------------|----------------|
#| 1.75 | Both           | Both           | Both           | 
#| 1.5  | Both           | Birthplace     | Both           |
#| 1.25 | Both           | Birthplace     | Birthplace     | 

# I will use the "full" ACS data to capture as much heterogeneity as possible
# in the groups that make up these generations.

# Load libraries, functions, and data -------------------------------------

library(here)
source(here("analysis","check_packages.R"))
source(here("analysis","useful_functions.R"))
load(here("analysis","output","markets_acs_full.RData"))

#add variables to the market datasets
markets_acs_full <- mclapply(markets_acs_full, add_vars)


# Some sanity checks ------------------------------------------------------

#look at all husbands not born in the US and under 18 years of age when
#migrated
temp <- markets_acs_full[[1]] %>%
  filter(bplh!=1) %>%
  mutate(genh=cut(age_usah, 
                  breaks=c(-1,5,12,17,100), labels=c("gen1.75","gen1.5","gen1.25","gen2")),
         genw=cut(age_usaw, 
                  breaks=c(-1,5,12,17,100), labels=c("gen1.75","gen1.5","gen1.25","gen2")),
         genw=ifelse(bplw==1, "native", as.character(genw))) %>%
  select(group, idh, bplh,bplw,genh,genw,starts_with("bendog"))

#where do we end up on endogamy parameter when wife is native?
temp %>%
  filter(genw=="native") %>%
  group_by(genh) %>%
  select(genh, starts_with("bendog")) %>%
  summarize_all(mean) %>%
  t()

##Those all look correct

#where do we end up on endogamy parameter when wife is the same birthplace?
test <- temp %>%
  filter(bplh==bplw) %>%
  group_by(genh, genw) %>%
  select(genh, genw, starts_with("bendog")) %>%
  summarize_all(mean)

#all look correct

#lets do the same thing for wives to be sure
temp <- markets_acs_full[[1]] %>%
  filter(bplw!=1) %>%
  mutate(genw=cut(age_usaw, 
                  breaks=c(-1,5,12,17,100), labels=c("gen1.75","gen1.5","gen1.25","gen2")),
         genh=cut(age_usah, 
                  breaks=c(-1,5,12,17,100), labels=c("gen1.75","gen1.5","gen1.25","gen2")),
         genh=ifelse(bplh==1, "native", as.character(genh))) %>%
  select(group, idh, bplh,bplw,genh,genw,starts_with("bendog"))

#where do we end up on endogamy parameter when husband is native?
temp %>%
  filter(genh=="native") %>%
  group_by(genw) %>%
  select(genw, starts_with("bendog")) %>%
  summarize_all(mean) %>%
  t()

##Those all look correct

#where do we end up on endogamy parameter when wife is the same birthplace?
test <- temp %>%
  filter(bplh==bplw) %>%
  group_by(genh, genw) %>%
  select(genh, genw, starts_with("bendog")) %>%
  summarize_all(mean)

#looks correct

#lets just make sure that everything works fine for natives
markets_acs_full[[1]] %>%
  filter(bplw==bplh & bplw==1) %>%
  select(starts_with("bendog")) %>%
  summarize_all(mean)

#everything looks good!

# Create formulas --------------------------------------------------------

#create formulas and put them into a list for faster processing of models

#Race pentagon formulas
formula_base <- formula(choice~agediff+I(agediff^2)+
                          hypergamy+hypogamy+edcross_hs+edcross_sc+edcross_c+
                          race_exog_pent+language_endog+
                          strata(group))
model_formulae <- list(all_second=update(formula_base, .~.+bendog_all_second),
                       all_usa=update(formula_base, .~.+bendog_all_usa),
                       all_flex=update(formula_base, .~.+bendog_all_flex),
                       grad1=update(formula_base, .~.+bendog_grad1),
                       grad2=update(formula_base, .~.+bendog_grad2),
                       grad3=update(formula_base, .~.+bendog_grad3),
                       grad4=update(formula_base, .~.+bendog_grad4),
                       grad5=update(formula_base, .~.+bendog_grad5),
                       flex1=update(formula_base, .~.+bendog_flex1),
                       flex2=update(formula_base, .~.+bendog_flex2))

models_bendog <- mclapply(model_formulae,
                          function(formula) {
                            poolChoiceModel(formula, 
                                            data=markets_acs_full,
                                            method="efron")
                          })

save(models_bendog, file=here("analysis","output","models_bendog.RData"))
