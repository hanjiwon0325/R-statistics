#�ּ��км�

head(USArrests)    #Murder, Assault, UrbanPop, Rape 
apply(USArrests,2, mean) #�� �� ��� 
apply(USArrests,2, sd)   #�� �� ǥ������
summary(USArrests) #Min, lst Qu, Median, Mean, 3rd Qu, Max 
plot(USArrests)

pr.out = prcomp(USArrests, scale = TRUE) #scale=TRUE:����ǥ��ȭ, #standard deviations, Rotations  

str(pr.out)   #sdev, rotation, center, scale, x, 
pr.out$sdev   #�ּ����� ǥ������
pr.out$rotation #�� ������ �������հ��
pr.out$center #�� ������ ���
pr.out$scale  #�� ������ ��ǥ�� ǥ������
pr.out$x      #������  �ּ��� 
biplot(pr.out,scale=0)
?prcomp

pr.out$rotation = -pr.out$rotation 
pr.out$x = -pr.out$x 
biplot(pr.out, scale=0)
?biplot

pr.out$sdev
pr.var=pr.out$sdev^2
pve = pr.var / sum(pr.var) #�ּ����� �л��� ũ��=������ 

#scree plot
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1), type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1), type='b')


##�ǽ�
install.packages("ElemStatLearn")
library(ElemStatLearn)
head(spam)
spam[ ,1:57]
pr.out = prcomp(spam[ ,1:57], scale=TRUE)
pr.out$sdev
pr.var = pr.out$sdev^2
pr.var
pve = pr.var/sum(pr.var) #������ 
pve 
cumsum(pve) #����=1 

sum=0
for(i in 1:57){
  sum = sum + pve[i] #0.8063
  if(sum > 0.8)      #�� �������� 0.8������ ������ �� �ִ� ������� �ؼ�
    break
}
i