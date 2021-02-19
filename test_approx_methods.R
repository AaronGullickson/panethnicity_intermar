library(here)
source(here("analysis","check_packages.R"))
source(here("analysis","useful_functions.R"))
load(here("analysis","output","markets_census.RData"))
load(here("analysis","output","markets_acs.RData"))

#add variables
markets_census <- add_vars(markets_census)
markets_acs_shortrace <- add_vars(markets_acs_shortrace)
markets_acs_fullrace <- add_vars(markets_acs_fullrace)

#estimate model without birthplace and language endogamy
formula_baseline <- formula(choice~agediff+I(agediff^2)+
                              hypergamy+hypogamy+
                              edcross_hs+edcross_sc+edcross_c+
                              race_exog_pent+ 
                              strata(group))
formula_bpl_endog <- update(formula_baseline, .~.+bpl_endog)
formula_lang_endog <- update(formula_baseline, .~.+language_endog)
formula_full <- update(formula_bpl_endog, .~.+language_endog)

system.time(model_acs_efron <- clogit(formula_full, 
                                      data=markets_acs_fullrace,
                                      method="efron"))
#user  system elapsed 
#139.652  17.199 159.155 

system.time(model_acs_approx <- clogit(formula_full, 
                                   data=markets_acs_fullrace,
                                   method="approximate"))
#user  system elapsed 
#130.660  17.015 149.784 

system.time(model_acs_exact <- clogit(formula_full, 
                                   data=markets_acs_fullrace,
                                   method="exact"))


results <- data.frame(variable=rep(names(coef(model_acs_efron)),3),
                      coef=c(coef(model_acs_efron),
                             coef(model_acs_approx),
                             coef(model_acs_exact)),
                      model=rep(c("efront","approx","exact"), 
                                each=length(coef(model_acs_efron))))

ggplot(results, 
       aes(x=variable, y=coef, color=model))+
  geom_point(alpha=0.3)+
  scale_color_manual(values = c("blue","red","yellow"))+
  coord_flip()+
  theme_bw()

cor(cbind(coef(model_acs_efron), coef(model_acs_approx), coef(model_acs_exact)))

