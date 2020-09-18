library(here)
source(here("analysis","check_packages.R"))
source(here("analysis","useful_functions.R"))
load(here("analysis","output","markets_census.RData"))
load(here("analysis","output","markets_acs.RData"))

with(subset(markets_census, choice),
     table(raceh, racew))
with(subset(markets_acs, choice),
     table(raceh, racew))

#add variables
markets_census <- add_vars(markets_census)
markets_acs <- add_vars(markets_acs)

# Run Models --------------------------------------------------------------

#baseline model

formula_full <- formula(choice~agediff+I(agediff^2)+ #husband-wife age difference
                          bpl.endog+language.endog+ #language and birthplace endogamy
                          hypergamy+hypogamy+ #education parameters
                          race.exog+ #gender-symmetric racial exogamy
                          strata(group))

model_census <- clogit(formula_full, data=markets_census)
model_acs <- clogit(formula_full, data=markets_acs)

library(tibble)
coefs <- rbind(tibble(variable=names(coef(model_census)),
                      coef=coef(model_census),
                      year=1980),
               tibble(variable=names(coef(model_acs)),
                      coef=coef(model_acs),
                      year=2016))
library(ggplot2)
library(ggrepel)
ggplot(coefs, aes(x=reorder(variable, coef, max), 
                  y=coef, 
                  color=as.factor(year)))+
  geom_point()+
  coord_flip()+
  theme(legend.position = "bottom")

coefs <- tibble(variable=names(coef(model_census)), 
                coef80=coef(model_census),
                coef16=coef(model_acs))
ggplot(coefs, aes(x=coef80, y=coef16))+
  geom_abline(slope=1, linetype=2)+
  geom_point()+
  geom_text_repel(aes(label=variable))+
  theme_bw()
