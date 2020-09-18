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

#add variables
markets_acs_fullrace <- add_vars(markets_acs_fullrace)

#run the model
model_acs_full <- clogit(choice~agediff+I(agediff^2)+ #husband-wife age difference
                           bpl_endog+language_endog+ #language and birthplace endogamy
                           hypergamy+hypogamy+ #education parameters
                           edcross_hs+edcross_sc+edcross_c+
                           race_exog_full+ #gender-symmetric racial exogamy
                           strata(group), data=markets_acs_fullrace)

#summarize as distances
coefs <- coef(model_acs_full)
coefs <- coefs[grepl("race_exog_full",names(coefs))]
temp <- gsub("race_exog_full","",names(coefs))
temp <- strsplit(temp, "\\.")
race1 <- sapply(temp, function(x) {x[1]})
race2 <- sapply(temp, function(x) {x[2]})
groups <- unique(c(race1,race2))
tab <- matrix(0, length(groups), length(groups))
rownames(tab) <- colnames(tab) <- groups
for(i in 1:length(race1)) {
  tab[race1[i], race2[i]] <- coefs[i]
  tab[race2[i], race1[i]] <- coefs[i]
}
tab <- exp(-1 * tab)
dist <- as.dist(tab)


hc <- hclust(dist, method="average")
plot(as.dendrogram(hc), 
     las=1, xlab="", ylab="distance")
abline(h=1, lty=3, col="red")

