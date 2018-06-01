# load required libraries 
library(knitr)
library(plyr)
library(multcomp)
library(ResourceSelection)
library(glmmBUGS)
library(boot)

# read data, extract required predictors and response, and name the variables
creditData = read.table("german.data.txt",header=F)
creditData = creditData[,c(1,2,3,4,9,21)];
names(creditData) = c("checking","duration","history","purpose","genderStatus","quality")

# change response variables to {0,1} instead of {1,2} 
creditData$quality[creditData$quality == 2] = 0;


# rename categories for convenience
creditData$checking = revalue(creditData$checking, c("A11"="_<0",
                                                     "A12"="_0to200",
                                                     "A13"="_>200",
                                                     "A14"="_none"))

creditData$history = revalue(creditData$history, c("A30"="_allPaid",
                                                   "A31"="_atBankPaid",                                                   
                                                   "A32"="_existingPaid",
                                                   "A33"="_pastDelay",
                                                   "A34"="_critical"))

creditData$purpose = revalue(creditData$purpose, c("A40"="_newCar",
                                                   "A41"="_usedCar",
                                                   "A42"="_furniture",
                                                   "A43"="_radio_TV",
                                                   "A44"="_appliances",
                                                   "A45"="_repairs",
                                                   "A46"="_education",
                                                   "A47"="_vacation",
                                                   "A48"="_retraining",
                                                   "A49"="_business",
                                                   "A410"="_other"))
creditData$genderStatus = 
  revalue(creditData$genderStatus, c("A91"="_divorcedMale",
                                     "A92"="_divorcedMarriedFemale",
                                     "A93"="_singleMale",
                                     "A94"="_marriedMale",
                                     "A95"="_singleFemale"))

# fit base logistic regression model
fullModel = glm(quality ~ .,data=creditData,family=binomial(link="logit"))

# fit logistic regression model with alternative link functions
probitModel  = glm(quality ~ .,data=creditData,family=binomial(link="probit"))
cloglogModel = glm(quality ~ .,data=creditData,family=binomial(link="cloglog"))

# run non-additivity test
etaHatSq         = fullModel$linear.predictors^2;
fullModel_nonadd = glm(quality ~ . + etaHatSq,data=creditData,family=binomial(link="logit"))
devRed           = fullModel$deviance-fullModel_nonadd$deviance
nonAddPval       = 1-pchisq(devRed,1)

# run hosmer and lemeshow goodness-of-fit tests
HLtestStat = hoslem.test(fullModel$y, fitted(fullModel),10)$statistic
HLpval     = hoslem.test(fullModel$y, fitted(fullModel),10)$p.value

b = NULL
k = 1;
for(i in 3:100)
{b[k] = hoslem.test(fullModel$y, fitted(fullModel),i)$p.value; k=k+1}

# estimate model with outliers removed
fullModel_outlier = glm(quality ~ .,data=creditData[-c(106,204,736),],family=binomial(link="logit"))
oldBeta           = fullModel$coefficients["purpose_retraining"]
largeBeta         = fullModel_outlier$coefficients["purpose_retraining"]

# plot Cook statistic
plot(cooks.distance(fullModel),xlab = "Case",ylab="Cook Statistic")
abline(h=0.02,col=2,lty=2)

# look for main effects by removing one variable at a time and re-estimating model
y = drop1(fullModel, test = "LRT")

# run multiple comparisons
a = summary(confint(glht(fullModel, mcp(genderStatus="Tukey"))))$confint
b = summary(confint(glht(fullModel, mcp(checking="Tukey"))))$confint
c = summary(confint(glht(fullModel, mcp(history="Tukey"))))$confint
d = summary(confint(glht(fullModel, mcp(purpose="Tukey"))))$confint

durBeta = fullModel$coefficients[5]
se = sqrt(vcov(summary(fullModel))[5,5])

durL = durBeta-2*se
durU = durBeta+2*se

# create aggregated data by grouping variables into bins and rename variables
creditData_agg          = creditData
creditData_agg$gender   = creditData_agg$genderStatus
creditData_agg$duration = 
  cut(creditData_agg$duration,breaks=c(0,24,48,72),labels=c("0-2yr","2-4yr","4-6yr"))

creditData_agg$history  = revalue(creditData_agg$history, c("_allPaid"="_good",
                                                           "_atBankPaid"="_good",
                                                           "_existingPaid"="_good",
                                                           "_pastDelay"="_bad",
                                                           "_critical"="_bad"))

creditData_agg$genderStatus = 
  revalue(creditData_agg$genderStatus, c("_divorcedMale"="_divorcedMarried",
                                         "_divorcedMarriedFemale"="_divorcedMarried",
                                         "_singleMale"="_single",
                                         "_marriedMale"="_divorcedMarried",
                                         "_singleFemale"="_single"))

creditData_agg$gender = 
  revalue(creditData_agg$gender, c("_divorcedMale"="_male",
                                   "_divorcedMarriedFemale"="_female",
                                   "_singleMale"="_male",
                                   "_marriedMale"="_male",
                                   "_singleFemale"="_female"))

creditData_agg$purpose = revalue(creditData_agg$purpose, c("_newCar"="_car",
                                                           "_usedCar"="_car",
                                                           "_furniture"="_home",
                                                           "_radio_TV"="_home",
                                                           "_appliances"="_home",
                                                           "_repairs"="_home",
                                                           "_education"="_other",
                                                           "_vacation"="_other",
                                                           "_retraining"="_other",
                                                           "_business"="_other",
                                                           "_other"="_other"))

names(creditData_agg)[names(creditData_agg) == "genderStatus"] = "status"
creditData_agg = 
  binToBinom(creditData_agg$quality, creditData_agg[,c("checking","duration","history","purpose","status","gender")])
pctAbove5 = length(creditData_agg[creditData_agg$N<5,1])/length(creditData_agg[,1])
creditData_agg=creditData_agg[c(1:84,86:134),]

# estimate model using aggregate data, run non-addivity test, multiple comparisons
aggModel        = glm(cbind(y,N-y) ~ .,data=creditData_agg,family=binomial(link="logit"))
etaHatSq_agg    = aggModel$linear.predictors^2;
aggModel_nonadd = glm(cbind(y,N-y) ~ . + etaHatSq_agg,data=creditData_agg,family=binomial(link="logit"))
devRed_agg      = aggModel$deviance-aggModel_nonadd$deviance
nonAddPval_agg  = 1-pchisq(devRed_agg,1)

y = drop1(aggModel, test="LRT")
mcp1=confint(glht(aggModel, mcp(checking="Tukey")))
mcp2=confint(glht(aggModel, mcp(duration="Tukey")))
mcp3=confint(glht(aggModel, mcp(history="Tukey")))
mcp4=confint(glht(aggModel, mcp(purpose="Tukey")))
mcp5=confint(glht(aggModel, mcp(gender="Tukey")))
mcp6=confint(glht(aggModel, mcp(status="Tukey")))

par(mar=c(4,4,.1,.1),cex.lab=.9,cex.axis=.7,mgp=c(2,.7,0),tcl=-.3,las=1)
glm.diag.plots(aggModel)