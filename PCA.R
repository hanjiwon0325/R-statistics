#주성분분석

head(USArrests)    #Murder, Assault, UrbanPop, Rape 
apply(USArrests,2, mean) #열 별 평균 
apply(USArrests,2, sd)   #열 별 표준편차
summary(USArrests) #Min, lst Qu, Median, Mean, 3rd Qu, Max 
plot(USArrests)

pr.out = prcomp(USArrests, scale = TRUE) #scale=TRUE:변수표준화, #standard deviations, Rotations  

str(pr.out)   #sdev, rotation, center, scale, x, 
pr.out$sdev   #주성분의 표준편차
pr.out$rotation #네 변수의 선형결합계수
pr.out$center #각 변수의 평균
pr.out$scale  #각 변수의 미표본 표준편차
pr.out$x      #국가별  주성분 
biplot(pr.out,scale=0)
?prcomp

pr.out$rotation = -pr.out$rotation 
pr.out$x = -pr.out$x 
biplot(pr.out, scale=0)
?biplot

pr.out$sdev
pr.var=pr.out$sdev^2
pve = pr.var / sum(pr.var) #주성분의 분산의 크기=설명력 

#scree plot
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1), type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1), type='b')


##실습
install.packages("ElemStatLearn")
library(ElemStatLearn)
head(spam)
spam[ ,1:57]
pr.out = prcomp(spam[ ,1:57], scale=TRUE)
pr.out$sdev
pr.var = pr.out$sdev^2
pr.var
pve = pr.var/sum(pr.var) #설명력 
pve 
cumsum(pve) #총합=1 

sum=0
for(i in 1:57){
  sum = sum + pve[i] #0.8063
  if(sum > 0.8)      #원 데이터의 0.8정도를 설명할 수 있는 변수라고 해석
    break
}
i
