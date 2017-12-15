#오분류표

#7:3으로 자료 분할
set.seed(123) 
train_sam = sample(nrow(german), 700, replace=F) # 1000개 중 700개 샘플링 
train = german[train_sam, ] #700개
test = german[-train_sam, ] #300개
table(train$Class) #Bad 209, Good 491 (700개)
table(test$Class)  #Bad 91,  Good 209 (300개)

#훈련자료를 이용하여 로지스틱 분류모형을 적합 
logistic = glm(Class~. , data=train , family='binomial') #Residual Deviance 622.3, AIC 720.3

#시험자료의 사후확률을 예측 
pred_prob = predict(logistic, newdata = test , type='response') 
head(pred_prob, 10)

#다양한 분류기준값에 따른 예측값
cutoff = 0.5 
ifelse(pred_prob[1:10] > cutoff,'Good','Bad')
cutoff = 1/6 
ifelse(pred_prob[1:10] > cutoff,'Good','Bad')

#오분류표 함수 만들기
#1
classification = function(model, newdata, cutoff){
  prob = predict(model, newdata, 'response') 
  ifelse(prob > cutoff, "Good", "Bad")
}
table(test$Class, classification(logistic, test, 0.5)) #300개 

#2
crosstable = function(model, newdata, cutoff){ 
  table(test$Class, classification(model,newdata,cutoff)) 
}
crosstable(logistic, test, 3/4)

#3
class.table = function(model, newdata, cutoff){ 
  M = table(test$Class, classification(model, newdata, cutoff)) 
  acc = (M[1,1] + M[2,2]) / sum(M) #정확도
  sen = M[2,2] / (M[2,1] + M[2,2]) #민감도
  spe = M[1,1] / (M[1,1] + M[1,2]) #특이도
  cat(paste('정확도 : ', round(acc,4), ', 민감도 : ', round(sen,4), ', 특이도 : ', round(spe,4), sep = " "),"\n") 
}
class.table(logistic, test, 0.5)
class.table(logistic, test, 3/4)

#특이도를 고정시켰을 때 오분류표와 민감도 계산하는 함수 
fixed_spe=function(y, score, spe){
  y_score = data.frame(y = y, score = score) 
  y_eq_0 = y_score[which(y_score$y == 0), ] 
  cut_off = quantile(y_eq_0$score, spe) #분류기준값
  yhat = as.integer(y_score$score > cut_off) #분류기준값 보다 큰 예측값
  y_score$yhat = yhat 
  rm(yhat)
  y_table = table(y_score$y, y_score$yhat) #오분류표
  sen = y_table[2,2] / sum(y_table[2,]) #민감도
  list(tab = y_table, sen = sen) 
}
int_y = as.integer(test$Class=='Good') #Good 1, Bad 0
spe = 0.8 
fixed_spe(int_y, pred_prob, spe)
