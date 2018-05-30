#Data set: youngpeoplesurvey.csv
#Perform a principal component analysis using the first 31 variables in this data set (from
#Healthyeating to Charity). 
#Reduce these variables to a small number of relevant factors
#and try to give a "name" or a "label" to each factor, based on the variables that are
#correlated with it.

#Steps of the PCA procedure 
#1 Testing the overall correlation between the variables
#2 Extract the principal comonents
#3 Determone the significant or relevant factors that can be retainned. 
#4 Find the final solution
#5 Interpret the final solution and report the results. 

ad <- read.csv('advert.csv', stringsAsFactors = F)

# We find the optimal number of factors 
## Build factor analysis model 

Survey <-Survey[complete.cases(Survey),]
na.omit(Survey)
pcamodel <- princomp(ad, cor = T)
pcamodel

summary(pcamodel)

#Eigenvalues are the squared standard deviations of the components

pcamodel$sdev

eigenv <- pcamodel$sdev^2
eigenv

#Top 3 values are greater than 1 and are retained

factor_model <- factanal(ad, factors = 3, rotation = "varimax")
print(factor_model, digits=2, cutoff=.3, sort=T)  
  
###Adequacy test for correlation 

cormatrix <- cor(ad)
View(cormatrix)  
  
library(psych)

KMO(cormatrix)
#.93 is over .90 is excellent. 

KMO(ad) #can be DF as well 

cortest.bartlett(cormatrix, 369)
#
#P-value is lower than .05 

cortest.bartlett(ad) #is DF no sample size is needed 
