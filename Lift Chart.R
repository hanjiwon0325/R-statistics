# AUC 
#x.name:"None:, y.name:"Area under the ROC curve", alpha.name:"none", 
#x.values:list(), y.values:0.8091908, alpha.values:list() 
install.packages("ROCR")
library(ROCR)

#performance(prediction(), measure, x.measure)
AUC = performance(prediction(pred_prob, int_y), "auc") 
?performance
AUC@y.values


#ROC curve
#x.name:"False positive rate", y.name:"True positive rate", alpha.name:"Cutoff",
#x.values, y.values
ROC = performance(prediction(pred_prob ,int_y), "tpr", "fpr") # x축:1-특이도, y축:민감도
plot(ROC, main = "ROC curve for test data")

#Lift chart(이익도표) 함수
lift_chart = function(prob, y, K=10){
  n=length(y)
  count=capture=response=lift=rep(0,K) 
  baselinelift = sum(y) / n * 100 
  n_group = round(n/K) 
  ind_vec = c(0, n_group*1:K) 
  sort_prob = sort(prob, decreasing=T) #내림차순 정렬 
  sort_y = y[match(sort_prob, prob)] 
  for(i in 1:K){ 
    start = ind_vec[i]+1 
    end = ind_vec[i+1]
    count[i] = sum(sort_y[start:end])    #각 등급의 1의 빈도
    capture[i] = count[i]/sum(y) * 100   #captured response 
    response[i] = count[i]/n_group * 100 #response 
    lift[i] = response[i]/baselinelift   #lift
  } 
  lc = data.frame(capture=capture, response=response, lift=lift) 
  return(lc) 
}
lc_df = lift_chart(pred_prob, int_y, 10) #10개 행
lc_df

plot(lc_df$lift, type='b', lty=1, pch=20, cex=1.5, col='orange', xlab='Level', ylab='Lift', main='Lift graph')

