#의사결정나무

#tree 패키지:이진반복분할 방법론 
#rpart패키지:CART 방법론, 가지치기 필요
#party패키지:ctree()
#prune(tree, cp=) #split, n, deviance, yval
#printcp()        #CP, nsplit, rel, error, xerror, xstd 

install.packages(c("ISLR","ggplot2"))
install.packages("rpart")
library(ISLR) 
library(ggplot2) 
library(rpart) 

ggplot(data=Hitters, aes(x=Years, y=Hits, col=Salary))+ geom_point() + 
  theme_bw() + scale_colour_gradientn(colours=terrain.colors(10))
data(Hitters)

#rpart()
tree_full = rpart(Salary ~ Years + Hits, data = Hitters) 
tree_pruned = prune(tree_full, cp = 0.03) #split, n, deviance, yval 
plot(tree_pruned, margin=0.1) 
text(tree_pruned, cex = 1)

tree_full = rpart(Salary ~ ., data = Hitters, control=rpart.control(minsplit = 15)) 
tree_full
plot(tree_full, margin=0.1) 
text(tree_full, cex=1, use.n=T)

cptable = printcp(tree_full)  #CP, nsplit, rel, error, xerror, xstd 
cp_optimal = cptable[which.min(cptable[ ,"xerror"]), "CP"] # xerror열 중 최소값의 CP: 0.04477

tree_pruned = prune(tree_full, cp=cp_optimal) 
plot(tree_pruned, margin=0.1) 
text(tree_pruned, cex=1)


#german data(rchallenge패키지) 실습 
install.packages("rchallenge") 
library(rchallenge) 
data(german) 
summary(german)
str(german)

tree_full = rpart(Class~., data=german) 
plot(tree_full) 
text(tree_full, cex = 0.7, use.n =T) #use.n =T

cptable = printcp(tree_full)
cp_optimal = cptable[which.min(cptable[, "xerror"]), "CP"] 
tree_pruned = prune(tree_full, cp=cp_optimal) 
plot(tree_pruned)
text(tree_pruned, cex = 0.7, use.n =T)