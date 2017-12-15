#���з�ǥ

#7:3���� �ڷ� ����
set.seed(123) 
train_sam = sample(nrow(german), 700, replace=F) # 1000�� �� 700�� ���ø� 
train = german[train_sam, ] #700��
test = german[-train_sam, ] #300��
table(train$Class) #Bad 209, Good 491 (700��)
table(test$Class)  #Bad 91,  Good 209 (300��)

#�Ʒ��ڷḦ �̿��Ͽ� ������ƽ �з������� ���� 
logistic = glm(Class~. , data=train , family='binomial') #Residual Deviance 622.3, AIC 720.3

#�����ڷ��� ����Ȯ���� ���� 
pred_prob = predict(logistic, newdata = test , type='response') 
head(pred_prob, 10)

#�پ��� �з����ذ��� ���� ������
cutoff = 0.5 
ifelse(pred_prob[1:10] > cutoff,'Good','Bad')
cutoff = 1/6 
ifelse(pred_prob[1:10] > cutoff,'Good','Bad')

#���з�ǥ �Լ� �����
#1
classification = function(model, newdata, cutoff){
  prob = predict(model, newdata, 'response') 
  ifelse(prob > cutoff, "Good", "Bad")
}
table(test$Class, classification(logistic, test, 0.5)) #300�� 

#2
crosstable = function(model, newdata, cutoff){ 
  table(test$Class, classification(model,newdata,cutoff)) 
}
crosstable(logistic, test, 3/4)

#3
class.table = function(model, newdata, cutoff){ 
  M = table(test$Class, classification(model, newdata, cutoff)) 
  acc = (M[1,1] + M[2,2]) / sum(M) #��Ȯ��
  sen = M[2,2] / (M[2,1] + M[2,2]) #�ΰ���
  spe = M[1,1] / (M[1,1] + M[1,2]) #Ư�̵�
  cat(paste('��Ȯ�� : ', round(acc,4), ', �ΰ��� : ', round(sen,4), ', Ư�̵� : ', round(spe,4), sep = " "),"\n") 
}
class.table(logistic, test, 0.5)
class.table(logistic, test, 3/4)

#Ư�̵��� ���������� �� ���з�ǥ�� �ΰ��� ����ϴ� �Լ� 
fixed_spe=function(y, score, spe){
  y_score = data.frame(y = y, score = score) 
  y_eq_0 = y_score[which(y_score$y == 0), ] 
  cut_off = quantile(y_eq_0$score, spe) #�з����ذ�
  yhat = as.integer(y_score$score > cut_off) #�з����ذ� ���� ū ������
  y_score$yhat = yhat 
  rm(yhat)
  y_table = table(y_score$y, y_score$yhat) #���з�ǥ
  sen = y_table[2,2] / sum(y_table[2,]) #�ΰ���
  list(tab = y_table, sen = sen) 
}
int_y = as.integer(test$Class=='Good') #Good 1, Bad 0
spe = 0.8 
fixed_spe(int_y, pred_prob, spe)