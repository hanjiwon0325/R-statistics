#군집분석

#1)비계층적 군집분석(kmeans)
#Cluster means, Cluter vector, Within cluster sum of squares by cluster, Available components 
clustering.K3 = kmeans(USArrests, 3);
clustering.K3 #20,16,14
str(clustering.K3) #cluster, centers, totss, withinss, tot.withinss, betweenss, size, iter, ifault
head(clustering.K3)
plot(USArrests, col = clustering.K3$cluster) #color 3가지 

set.seed(1) 
clustering1 <- kmeans(USArrests, 4) 
clustering1 #10,10,16,14

set.seed(2) 
clustering2 <- kmeans(USArrests, 4) 
clustering2 #13,20,6,11 
table(clustering1$cluster, clustering2$cluster)

clustering3 <- kmeans(USArrests, 4, nstart = 25) #초기값 수 25개
clustering3 #16,10,14,10 

clustering1$tot.withinss #34728.63
clustering2$tot.withinss #37730.8
clustering3$tot.withinss #34728.63


#kmeans 실습
set.seed(2)
x = matrix(rnorm(50*2), ncol=2) #50행 2열 
x[1:25, 1] = x[1:25, 1] + 3 
x[1:25, 2] = x[1:25, 2] - 4
plot(x) #열끼리 비교 

set.seed(2)
km.out = kmeans(x, 2, nstart=20)
plot(x, col=(km.out$cluster + 1), main="k-means clustering result with k=2", xlab="", ylab="")
km.out
km.out$tot.withinss #128.6066


#2)계층적 군집분석(hclust)
#dist(x, method="euclidean/manhattan/canberra/maximum",diag=FALSE )
#hclust요소 : Call, Cluster method, Distance, Number of objects
#hclust(dist(), method="ward/single/complete/average/median/centroid등", member=NULL)
#rect.hclust(tree, k=NULL, which=NULL, x=NULL, h=NULL, border=, cluster=NULL)
#cutree(tree, k=NULL, h=NULL)

dist(USArrests) 
clustering.EC = hclust(dist(USArrests)) 
clustering.EC #complete, euclidean, 50
plot(clustering.EC) #main="Cluster Dendogram"(default)

clustering.EA = hclust(dist(USArrests),"average")
clustering.EA
plot(clustering.EA, main="평균연결법")

clustering.ES = hclust(dist(USArrests),"single")
clustering.ES
plot(clustering.ES, main="최단거리법")

cutree(clustering.EC, k=2:4) #cutree
cutree(clustering.EC, h=100)

rect.hclust(clustering.EC, k=3, border="red")
rect.hclust(clustering.EA, k=3, border="red")
rect.hclust(clustering.ES, k=3, border="red")

summary(USArrests[cutree(clustering.EA, 3)==1,])
summary(USArrests[cutree(clustering.EA, 3)==2,])
summary(USArrests[cutree(clustering.EA, 3)==3,])

complete = cutree(clustering.EC, 3) #K=3(1,2,3)
average = cutree(clustering.EA, 3)
single = cutree(clustering.ES, 3) 

table(complete, average)
table(complete, single)


#hclust 실습
head(iris)
x= iris[ ,1:4] #Species 빼기
y= iris[ ,5]   #Species
hc.complete= hclust(dist(x), method="complete")
hc.average= hclust(dist(x), method="average")
hc.single= hclust(dist(x), method="single")

par(mfrow=c(1,3))
plot(hc.complete, main="complete linkage", xlab="", sub="", cex=.9)
plot(hc.average, main="average linkage", xlab="", sub="", cex=.9)
plot(hc.single, main="single linkage", xlab="", sub="", cex=.9 )
dev.off()  #그림지우기

complete = cutree(hc.complete, 3)
average = cutree(hc.average, 3)
single = cutree(hc.single, 3)

table(complete, average)
table(complete, single)

table(y, complete)
table(y, average)
table(y, single)