#벌점화 회귀(glmnet)

install.packages("glmnet")
library(glmnet)
set.seed(1)
X = matrix(data= rnorm(1000*100), 1000 , 100) #1000*100
B = c(rnorm(10), rep(0,90)) #10까지 랜덤한 수, 이후 100까지 0.000000 
e = rnorm(1000)
y = X %*% B + e
y

#ridge
ridge = glmnet(x=X, y=y, alpha=0)
par(mfrow = c(1,2))
plot(ridge, xvar="norm")   #default(L1 Norm)
plot(ridge, xvar="lambda") #Log Lambda 

set.seed(3)
cv.ridge = cv.glmnet(x=X, y=y, alpha=0) #cross validation(교차검증)
bestlam = cv.ridge$lambda.min 
ridge.fit = glmnet(X, y, alpha=0, lambda=bestlam)
head(ridge.fit$beta)
predict(cv.ridge, newx=X[1:5, ], s=bestlam)
plot(cv.ridge)

#lasso
lasso = glmnet(x=X, y=y, alpha=1)
par(mfrow = c(1,2))
plot(lasso, xvar ="norm")   #L1 Norm
plot(lasso, xvar ="lambda") #Log Lambda 

set.seed(3)
cv.lasso = cv.glmnet(x=X, y=y, alpha=1) #cross validation(교차검증)
bestlam = cv.lasso$lambda.min #cv error 최소화
lasso.fit = glmnet(X, y, alpha=1, lambda=bestlam)
head(lasso.fit$beta)
length(which(lasso.fit$beta !=0))
predict(cv.lasso, newx=X[1:5, ], s=bestlam)
plot(cv.lasso)


lm.fit = lm(y ~ X)
beta = cbind(ridge.fit$beta, lasso.fit$beta, lm.fit$coefficient[-1])
colnames(beta) = c("ridge", "lasso", "linear model")
beta #v1 ~ v100


#모형나누기
n = nrow(Boston) #506
training_ratio = 0.7
set.seed(2)
train = sample(1:n, n*training_ratio)
test = setdiff(1:n, train)
X = model.matrix(medv~., Boston)[,-1]
y = Boston$medv

#best subset selection
bs_train = regsubsets(medv~., Boston[train,], nvmax=13) #FALSE

#ridge
cv_out_ridge = cv.glmnet(X[train,], y[train], alpha=0)
ridge_train = glmnet(X[train,], y[train], alpha=0, lambda=cv_out_ridge$lambda.min)

#lasso
cv_out_lasso = cv.glmnet(X[train,], y[train], alpha=1)
lasso_train = glmnet(X[train,], y[train], alpha=1, lambda=cv_out_lasso$lambda.min)

nvar_cp = which.min(summary(bs_train)$cp) #11
selvar = names(coef(bs_train, nvar_cp))[-1] #"crim"~"lstat"(indus,age제외)
bs_formula = as.formula(paste("medv ~", paste(selvar, collapse="+")))
lm_bs = lm(bs_formula, data=Boston[train,])

#test 예측
y_pred_bs = predict(lm_bs, newdata = Boston[test,])
y_pred_ridge = predict(ridge_train, newx =X[test,])
y_pred_lasso = predict(lasso_train, newx =X[test,])

sum(y_pred_bs -y[test]^2)
sum(y_pred_ridge -y[test]^2)
sum(y_pred_lasso -y[test]^2)