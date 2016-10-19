library(caret)
library(e1071)
filename <- "/Users/sumit/Documents/major_work/TestingAndTrainingDataSet.csv"
testing_0 <- read.csv(filename, header=TRUE)
#dataset
testing_0
#graph plotted
plot(pH~Aval.N,testing_0)
with(testing_0,text(pH~Aval.N,testing_0,labels=name,pos=4,cex=0.5))

#3d scattered plot pending.

testing_1 = testing_0[-1]
testing_1

#normalise the dataset.
m <- apply(testing_1,2,mean)
s <- apply(testing_1,2,sd)
testing_1 <- scale(testing_1,m,s)
testing_1
print(testing_1,digits = 3)

#elbow curve
wssplot <- function(data,nc=15,seed=1234){
wss <- (nrow(data)-1)*sum(apply(data,2,var))
for(i in 2:nc){
  set.seed(seed)
  wss[i] <- sum(kmeans(data,centers=i)$withinss)}
plot(1:nc,wss,type="b",xlab="Number of clusters",
ylab="Within Groups Sum of Squares")}
wssplot(testing_1,nc=19,seed=1234)


#kmeans
testing_kmeans <- kmeans(testing_1,3)
testing_kmeans


library("scatterplot3d")
scatterplot3d(testing_1[,2:4],color = testing_kmeans$cluster)


distance <- dist(testing_1)
print(distance, digits = 3)

#cluster dendogram
#complete
hc.c <- hclust(distance)
plot(hc.c,labels=testing_0$name)

#average
hc.a <- hclust(distance,method = "average")
plot(hc.a,labels=testing_0$name)


#cluster membership
member.c <- cutree(hc.c,3)
member.a <- cutree(hc.a,3)
#plot(member.c,member.a)
table(member.c, member.a)

#summary(testing_1)

#cluster means
aggregate(testing_1,list(member.c),mean)

library(cluster)
#silhoute plot
plot(silhouette(cutree(hc.c,3),distance))


