
library(here)
source(here("analysis","check_packages.R"))
source(here("analysis","useful_functions.R"))
load(here("analysis","output","markets1980.RData"))
load(here("analysis","output","markets0816.RData"))

#add variables
markets1980 <- add_vars(markets1980)
markets0816 <- add_vars(markets0816)

# Run Models --------------------------------------------------------------

#baseline model
formula.base <- formula(choice~agediff+I(agediff^2)+
                          bpl.endog+language.endog+
                          hypergamy+hypogamy+strata(group))

formula.full <- formula(choice~agediff+I(agediff^2)+ #husband-wife age difference
                          bpl.endog+language.endog+ #language and birthplace endogamy
                          hypergamy+hypogamy+ #education parameters
                          race.exog+ #gender-symmetric racial exogamy
                          strata(group))

system.time(model.base.clogit <- clogit(formula.full, data=markets0816))
system.time(model.base.clogit.1980 <- clogit(formula.full, data=markets1980))

model.full.clogit.1980 <- clogit(formula.full, data=markets1980)
model.full.clogit.0816 <- clogit(formula.full, data=markets0816)

library(tibble)
coefs <- rbind(tibble(variable=names(coef(model.full.clogit.1980)),
                      coef=coef(model.full.clogit.1980),
                      year=1980),
               tibble(variable=names(coef(model.full.clogit.0816)),
                      coef=coef(model.full.clogit.0816),
                      year=2016))
library(ggplot2)
ggplot(coefs, aes(x=reorder(variable, coef, max), 
                  y=coef, 
                  color=as.factor(year)))+
  geom_point()+
  coord_flip()


createDistanceMatrix <- function(model, groups, varName="race.exog") {
  
  #TODO: for loop bad, figure out better way
  tab <- matrix(0, length(groups), length(groups))
  for(i in 1:length(groups)) {
    for(j in 1:length(groups)) {
      if(i>=j) {
        next
      }
      group1 <- groups[i]
      group2 <- groups[j]
      tab[i,j] <- summary(model)$coef[paste(varName,paste(group1,group2,sep="."),sep=""),"coef"]
      
    }
  }
  tab <- t(tab)
  rownames(tab) <- colnames(tab) <- groups
  
  #multiply by -1 aand exponentiate, to get it back to original scale
  #of distance
  tab <- exp(-1 * tab)
  return(as.dist(tab))
  
}

distance <- createDistanceMatrix(model.exog, groups=c("White","Black","Chinese","Japanese","Korean","Filipino","Mexican","Cuban","Puerto Rican"))
hc <- hclust(distance, method="average")
plot(as.dendrogram(hc), 
     las=1, xlab="", ylab="distance")
abline(h=1, lty=3, col="red")

distance <- createDistanceMatrix(model.exog, groups=c("White","Black","Chinese","Japanese","Korean","Filipino","Asian Indian","Mexican","Cuban","Puerto Rican"))
hc <- hclust(distance, method="average")
plot(as.dendrogram(hc), 
     las=1, xlab="", ylab="distance")
abline(h=1, lty=3, col="red")

  distance <- createDistanceMatrix(model.exog, groups=c("Chinese","Japanese","Korean","Filipino","Mexican"))
hc <- hclust(distance, method="average")
plot(as.dendrogram(hc), 
     las=1, xlab="", ylab="distance")
 abline(h=1, lty=3, col="red")
 
 distance <- createDistanceMatrix(model.exog, groups=c("White","Black","Mexican","Cuban","Puerto Rican"))
 hc <- hclust(distance, method="average")
 plot(as.dendrogram(hc), 
      las=1, xlab="", ylab="distance")
 abline(h=1, lty=3, col="red")
