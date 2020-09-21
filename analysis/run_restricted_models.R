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

formula_full <- formula(choice~agediff+I(agediff^2)+ #husband-wife age difference
                          bpl_endog+language_endog+ #language and birthplace endogamy
                          hypergamy+hypogamy+ #education parameters
                          edcross_hs+edcross_sc+edcross_c+
                          race_exog_pent+ #gender-symmetric racial exogamy
                          strata(group))

model_census <- clogit(formula_full, data=markets_census)
model_acs <- clogit(formula_full, data=markets_acs_shortrace)

coefs <- rbind(tibble(variable=names(coef(model_census)),
                      coef=coef(model_census),
                      year=1980),
               tibble(variable=names(coef(model_acs)),
                      coef=coef(model_acs),
                      year=2016))
ggplot(coefs, aes(x=reorder(variable, coef, max), 
                  y=coef, 
                  color=as.factor(year)))+
  geom_point()+
  coord_flip()+
  theme(legend.position = "bottom")
