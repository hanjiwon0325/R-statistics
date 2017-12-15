#로지스틱 회귀분석

install.packages("rchallenge")
library(rchallenge)
data(german)
head(german,5)
str(german)
summary(german) #연속형: Min,lst Qu,median,mean,3rd Qu,Max 

german.log = glm(Class~., data = german, family = "binomial") 
german.log #951 Residual, 1222 Null Deviance, 895.8 Residual Deviance, 993.8 AIC 

#로지스틱 선형함수 요약:
#Call, Deviance Residuals, Coefficients, Signif.codes,
#Null deviance, Residual deviance, AIC, Numver of Fisher Scoring iterations
summary(german.log) 
german.log$coefficients

good_ind = which(german$Class=='Good') #700개
bad_ind = which(german$Class=='Bad')   #300개
 
set.seed(123) 
sam_good_ind = sample(good_ind , 300 , replace=F) 
cc_german = rbind(german[bad_ind,] , german[sam_good_ind,]) #total 600 sample

cc_german.log = glm(Class~., data = cc_german, family = "binomial")
cc_german.log #551 Residual, 831.8 Null Deviance, 580.7 Residual Deviance, 687.7 AIC 
