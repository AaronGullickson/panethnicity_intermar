library(here)
source(here("analysis","check_packages.R"))
source(here("analysis","useful_functions.R"))
load(here("analysis","output","markets_acs.RData"))
load(here("analysis","output","models.RData"))

markets_acs_restricted <- add_vars(markets_acs_restricted)

asian_groups <- c("Chinese","Korean","Japanese","Vietnamese","Filipino")
latino_groups <- c("Mexican","Puerto Rican","Cuban","Guatemalan",
                   "Salvadorian","Colombian","Ecuadorian","Peruvian",
                   "Dominican")

groups <- c(asian_groups, latino_groups)

test <- markets_acs_restricted %>%
  filter(raceh %in% groups & racew %in% groups)

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

model_test <- summary(clogit(formulas_extended$both, 
                             data=test,
                             method="efron"))

compare <- tibble(full_coef=models_acsres_extended$both$coefficients[,1],
                  lim_coef=model_test$coefficients[,1],
                  coef_names=names(models_acsres_extended$both$coefficients[,1]))
       
ggplot(subset(compare, grepl("race_exog",coef_names)), 
       aes(x=full_coef, y=lim_coef))+
  geom_point()+
  geom_abline(intercept=0, slope=1)

#TODO: try comparison of models with just race_exog terms to see if those fit more
#closely. Differences may be driven by differences in effets of control variables
#in models of subsets of the data.

#try removing all groups that have no true value
test <- droplevels(test)
temp <- tapply(test$choice, test$group, max)
keep <- names(temp)[which(temp==1)]
test <- test %>%
  filter(group %in% keep) %>%
  droplevels()

pr <- normal(location=coef(models_acsres_extended$both)[,1],
             scale=coef(models_acsres_extended$both)[,2],
             autoscale = FALSE)

library(rstanarm)
stan_test<- stan_clogit(choice~agediff+I(agediff^2)+
                          hypergamy+hypogamy+edcross_hs+edcross_sc+edcross_c+
                          race_exog_extended+race_filipino_latino+bendog_flex2+
                          language_endog,
                        strata = group,
                        data = markets_acs_restricted,
                        QR = TRUE,
                        chains = 1, iter = 200) # for speed only


#iIt took about 37 minutes per chain with 500 iterations. So for 2000 iterations, 
#that would probably be in the area of 148 minutes per chain. I can 
#probably speed it up by running multi.core properly, so that I have one chain
#per core. 

# So I think this might work. Currently, not 
# setting prior, but tighter priors will also speed things up apparently. 

# I could also try a James-Stein estimator