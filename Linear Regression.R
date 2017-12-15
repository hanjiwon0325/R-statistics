#선형회귀분석

str(mtcars) #변수확인
fit = lm(mpg ~ factor(am) + wt + hp , data = mtcars) #선형회귀식, am은 범주형범수 
summary(fit) #call, residuals, coefficient

#예측모형
new_x = data.frame(am=0, wt=3, hp=150) # =coef(fit)%*%c(1,0,3,150)
predict(fit, newdata = new_x) # y = 34 + 2.084*0 + -2.879*3 + -0.037*150

new_x= data.frame(am=c(0,1), wt=c(3,4), hp=c(150,200))
predict(fit, newdata = new_x)

# file read and write
betahat = summary(fit)$coef
write.csv(betahat,"C:\\Users\\한지원\\Desktop\\SNU UDSL\\R statistics\\betahat.csv")
betahat2 = read.csv("C:\\Users\\한지원\\Desktop\\SNU UDSL\\R statistics\\betahat.csv")
betahat2 #summary(fit)$coef



library(MASS) # for Boston housing dataset 
head(Boston)
Boston$chas = factor(Boston$chas)
str(Boston)
summary(Boston)

#상관계수(cor()), 변수제거법
cor(Boston[ ,-4]) # chas가 범주형이므로,
#= cor(Boston[ , setdiff(colnames(Boston),"chas")])

#변수변환
Boston.lm1 = lm(medv~., data = Boston) 
summary(boston.lm1)
boston.lm2 = lm(log(medv) ~ I(rm^2) + log(dis) + log(rad) + I(nox^2) + .-rm-dis-rad-nox, Boston)
summary(boston.lm2)

#신뢰구간(confint())
confint(boston.lm1, level = 0.95) 
confint(boston.lm2, level = 0.95) 

#잔차분석(rstudent())
head(boston.lm1$residuals)
boston.rstudent1 = rstudent(boston.lm1) #스튜던트화 잔차
boston.rstudent2 = rstudent(boston.lm2)
par(mfrow=c(1,2))
plot(predict(boston.lm1, newdata=Boston), boston.rstudent1) #잔차도표
plot(predict(boston.lm2, newdata=Boston), boston.rstudent2)

#이상점(influence.measures())
influence_values = influence.measures(boston.lm1)$is.inf[ ,c("dffit", "cov.r", "cook.d", "hat")]
head(influence_values)                

#이상점 제거
boston.outliers = influence_values[, "cov.r"] 
boston.lm1.rm = lm(medv~., Boston[!boston.outliers, ])

yhat.rm = predict(boston.lm1.rm, newdata = Boston[!boston.outliers,]) 
tstudent.rm = rstudent(boston.lm1.rm)
plot(yhat.rm, tstudent.rm)

par(mfrow = c(1,2)) 
plot(rstudent(boston.lm1))
abline(h=0, col=2) #color="red" 
plot(rstudent(boston.lm1.rm))
abline(h=0, col=2)

#잔차의 정규성 
qqnorm(rstudent(boston.lm1.rm)) 
abline(0, 1, col = "red", lwd = 2)


#변수선택
install.packages("leaps")
library(leaps) 

#1.best subset selection
bos_bs = regsubsets(medv ~., data=Boston, nvmax = 13) #13개 변수 SSE비교
bs_summary = summary(bos_bs) 
names(bs_summary)
bs_summary$cp
bs_summary$adjr2


par(mfrow=c(2,2))
#R^2
plot(bs_summary$rsq, xlab="Number of Variables", ylab="R2", type="l")
nvar_rsq = which.max(bs_summary$rsq) #최대값
points(nvar_rsq, bs_summary$rsq[nvar_adjr2], col="red",cex=2,pch=20)

#Adjusted R^2 
plot(bs_summary$adjr2, xlab="Number of Variables", ylab="Adjusted R2", type="l") 
nvar_adjr2 = which.max(bs_summary$adjr2) #최대값
points(nvar_adjr2, bs_summary$adjr2[nvar_adjr2], col="red",cex=2,pch=20)

#cp 
plot(bs_summary$cp, xlab="Number of Variables", ylab="cp", type='l') 
nvar_cp = which.min(bs_summary$cp) #최소값
points(nvar_cp, bs_summary$cp[nvar_cp],col="red",cex=2,pch=20)

#BIC 
plot(bs_summary$bic, xlab="Number of Variables", ylab="BIC", type='l') 
nvar_bic = which.min(bs_summary$bic) #최소값
points(nvar_bic, bs_summary$bic[nvar_bic], col="red", cex=2, pch=20)

#coefs of the best model(cp) 
plot(bos_bs, scale="Cp")
nvar_cp = which.min(bs_summary$cp) 
coef(bos_bs, nvar_cp) #bos_bs 13개, nvar_cp 11개(indus, age 삭제)


#2.Forward & Backward stagewise  
lm_null = lm(medv~ 1, data=Boston) 
lm_full = lm(medv~ ., data=Boston) 
step_forward = step(lm_null, scope=list(lower=lm_null, upper=lm_full), direction='forward')
step_back = step(lm_full, scope=list(lower=lm_full, upper=lm_null), direction='backward')