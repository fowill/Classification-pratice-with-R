#设置工作路径
setwd('/Users/fowillwly/Dev/R_homework_classification')

#__________DATASET ONE_______________________________#

#载入相关程序包
library(ggplot2)
library(cluster)
library(fpc)
library(mclust)

#载入Iris数据集，并修改表头
iris <- read.csv('./data/iris.data',header = FALSE)
names(iris) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")

#画图，对数据有初步了解
#----按照花萼的长宽分布
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point(aes(color = Species))
#----按照花瓣的长宽分布
ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) + geom_point(aes(color = Species))

#方法1，kmeans，聚为3个簇
fit_km1 = kmeans(iris[,-5],center=3)
print(fit_km1)

#输出本次聚类的总平方和、组间平方和的总和、组间平方和
fit_km1$totss;fit_km1$tot.withinss;fit_km1$betweenss

#迭代k，计算最合适的聚类簇数
fit_km = 0
result = rep(0,80)
for (k in 1:80)
{
fit_km = kmeans(iris[,-5],center=k)
result[k] = fit_km$betweenss/fit_km$totss
}
round(result,2)

plot(1:80,result,type="b",main="choosing the optimal number of cluster",xlab="number of cluster:1 to 80",ylab="betweenss/totss")
points(66,result[66],pch=16)     
legend(66,result[66],paste("(66,",sprintf("%.1f%%",result[66]*100),")",sep=""),bty="n",xjust=0.3,cex=0.8)
legend(10,result[10],paste("(10,",sprintf("%.1f%%",result[10]*100),")",sep=""),bty="n",xjust=0.3,cex=0.8)

#方法2，k中心点
fit_pam = pam(iris[,-5],3)
print(fit_pam)

#方法3，系谱聚类
fit_hc = hclust(dist(iris[,-5]))
print(fit_hc)

#作出系谱图
plot(fit_hc)

#控制输出类别和系谱高度
group_k3 = cutree(fit_hc,k=3)
table(group_k3)
group_h18 = cutree(fit_hc,h=6)
table(group_h18)

#方法4，密度聚类

ds1 =dbscan(iris[,-5],eps=1,MinPts=5)
ds1
ds2 = dbscan(iris[,-5],eps=4,MinPts=5)
ds2
ds3 = dbscan(iris[,-5],eps=4,MinPts=2)
ds3
ds4 = dbscan(iris[,-5],eps=0.2,MinPts=5)
ds4

d = dist(iris[,-5])
max(d);min(d)
interval = cut_interval(d,4)
table(interval)

for (i in 2:11)
  {for (j in 1:5)
  {
    ds = dbscan(iris[,-5],eps=i*0.1,MinPts=j)
    print(ds)
  }}

#方法5，期望最大化聚类

fit_EM = Mclust(iris[,-5])
summary(fit_EM)
plot(fit_EM)

Iris_BIC = mclustBIC(iris[,-5])
Iris_BIC_sum = summary(Iris_BIC,data=iris[,-5])
Iris_BIC_sum
plot(Iris_BIC,G=1:7,col="black")

#画出2维和3维密度图
Iris_Dens = densityMclust(iris[,-5])
plot(Iris_Dens,iris[,-5],col="grey",nlevels=55)
plot(Iris_Dens,type="persp",col=grey(0.8))

#__________DATASET TWO_______________________________#
library(tidyverse)
library(class)
library(dplyr)


mushroom = read.csv('./data/agaricus-lepiota.data',header=FALSE)
mushroom = mushroom[,-12]
names(mushroom) <- c("edibility", "cap_shape", "cap_surface", 
                     "cap_color", "bruises", "odor", 
                     "gill_attachement", "gill_spacing", "gill_size", 
                     "gill_color", "stalk_shape", 
                     "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring", 
                     "stalk_color_below_ring", "veil_type", "veil_color", 
                     "ring_number", "ring_type", "spore_print_color", 
                     "population", "habitat")

mushroom <- mushroom %>% map_df(function(.x) as.factor(.x))


#重命名每个变量
levels(mushroom$edibility) <- c(0, 1)
levels(mushroom$cap_shape) <- c(0,1,2,3,4,5)
levels(mushroom$cap_color) <- c(0,1,2,3,4,5,6,7,8,9)
levels(mushroom$cap_surface) <- c(0,1,2,3)
levels(mushroom$bruises) <- c(0,1)
levels(mushroom$odor) <- c(0,1,2,3,4,5,6,7,8)
levels(mushroom$gill_attachement) <- c(0,1)
levels(mushroom$gill_spacing) <- c(0,1)
levels(mushroom$gill_size) <- c(0,1)
levels(mushroom$gill_color) <- c(0,1,2,3,4,5,6,7,8,9,10,11)
levels(mushroom$stalk_shape) <- c(0,1)
#levels(mushroom$stalk_root) <- c(0,1,2,3,4)
levels(mushroom$stalk_surface_above_ring) <- c(0,1,2,3)
levels(mushroom$stalk_surface_below_ring) <- c(0,1,2,3)
levels(mushroom$stalk_color_above_ring) <- c(0,1,2,3,4,5,6,7,8,9)
levels(mushroom$stalk_color_below_ring) <- c(0,1,2,3,4,5,6,7,8,9)
levels(mushroom$veil_type) <- 0
levels(mushroom$veil_color) <- c(0,1,2,3)
levels(mushroom$ring_number) <- c(0,1,2)
levels(mushroom$ring_type) <- c(0,1,2,3,4)
levels(mushroom$spore_print_color) <- c(0,1,2,3,4,5,6,7,8)
levels(mushroom$population) <- c(0,1,2,3,4,5)
levels(mushroom$habitat) <- c(0,1,2,3,4,5,6)

#veil_type的值是一致的，所以删掉
mushroom <- mushroom %>% select(- veil_type)
map_dbl(mushroom, function(.x) {sum(is.na(.x))})


#方法1，kmeans
mushroom_km = kmeans(mushroom[,-1],center=2)
print(mushroom_km)
#计算精度
acc = 0
for (i in 1:8124)
{
  if (mushroom_km$cluster[i] == mushroom[i,1])
  {
    acc <- acc + 1
  }
}

acc <- acc/8124
print(acc)

#迭代k，计算最合适的聚类簇数
mushroom_km = 0
result = rep(0,80)

for (k in 1:80)
{
  mushroom_km = kmeans(mushroom[,-1],center=k)
  result[k] = mushroom_km$betweenss/mushroom_km$totss
}
round(result,2)

plot(1:80,result,type="b",main="choosing the optimal number of cluster",xlab="number of cluster",ylab="betweenss/totss")
points(60,result[60],pch=16)     
legend(60,result[60],paste("(60,",sprintf("%.1f%%",result[60]*100),")",sep=""),bty="n",xjust=0.3,cex=0.8)


#方法2，K中心点
mushroom_pam = pam(mushroom[,-1],2)
print(mushroom_pam)

#计算精度
acc = 0
for (i in 1:8124)
{
  if (mushroom_pam$clustering[i] == mushroom[i,1])
  {
    acc <- acc + 1
  }
}

acc <- acc/8124
print(acc)



#__________DATASET THREE_______________________________#

#载入需要的程序包
library(NbClust)
library(corrplot)
library("car")

#载入wine数据集
wine <- read.csv('./data/wine.data',header=FALSE)
head(wine)

#重命名
#names(wine) <- c("Class","Alcohol", "Malic acid", "Ash", "Alcalinity of ash", "Magnesium","Total phenols","Flavanoids","Nonflavanoid phenols","Proanthocyanins","Color intensity","Hue","OD280/OD315 of diluted wines","Proline")

#计算并绘制相关矩阵，观察不同属性之间的联系
wine_corr <- cor(wine)
corrplot(wine_corr,method="number",tl.cex=0.5)

#画一部分相关图
scatterplotMatrix(wine[2:6])

#"Ash"和"Alcalinity of ash"属性相关分析
plot(wine$V4, wine$V5)
#标记上种类，更适合观察
text(wine$V4, wine$V5, wine$V1, cex=0.7, pos=4, col="red")

#做归一化处理
sapply(wine[2:14],mean)
sapply(wine[2:14],sd)
table(wine$V1)

#对这14个属性做探究
wine1=wine[wine$V1=="1",]
wine2=wine[wine$V1=="2",]
wine3=wine[wine$V1=="3",]

sapply(wine1[,2:14],mean)
sapply(wine1[,2:14],sd)

sapply(wine2[,2:14],mean)
sapply(wine2[,2:14],sd)


sapply(wine3[,2:14],mean)
sapply(wine3[,2:14],sd)

cor(wine[,2:14])

#太过繁杂，因此尝试使用PCA

W.pca <- princomp(wine[,-1], cor = TRUE, scores = TRUE, covmat = NULL)

summary(W.pca)
plot(W.pca)


stdwine=as.data.frame(scale(wine[,2:14]))

set.seed(123)

wine.pca <- prcomp(wine,scale=TRUE)
summary(wine.pca)
wine.pca$sdev

screeplot(wine.pca, type="lines")
abline(h=1)

#Another way of deciding how many components to retain is to use Kaiser's criterion: that we should only retain
#principal components for which the variance is above 1 (when principal component analysis was applied to
#standardised data).

(wine.pca$sdev)^2



#Loadings/scores for the Principal Components


#The loadings for the principal components are stored in a named element "rotation" of the variable returned by
# "prcomp()".

#This contains a matrix with the loadings of each principal component, where the first column in the
#matrix contains the loadings for the first principal component, the second column contains the loadings for the
#second principal component, and so on.


wine.pca$rotation[,1:3]



#    values of the first principal component are stored in the variable wine.pca$x[,1] that was returned by
#    the "prcomp()" function


wine.pca$x[,1]


#The first principal component has highest (in absolute value) loadings for V1(0.393), V7 (-0.359), V8 (-0.390),
#V13 (-0.350), V10 (-0.279), V9(0.2267),V12(-0.27), V14 (-0.269), V3 (0.222), and V5 (0.224). The loadings for V8, V7,
#V13, V10, V12 and V14 are negative, while those for V9, V3, and V5 are positive. Therefore, an interpretation
#of the first principal component is that it represents a contrast between the concentrations of V8, V7, V13, V10,
#V12, and V14, and the concentrations of V9, V3 and V5.



#Scatterplots of the Principal Components


#scatterplot of the first two principal components

plot(wine.pca$x[,1],wine.pca$x[,2]) # make a scatterplot

text(wine.pca$x[,1],wine.pca$x[,2], wine$V1, cex=0.7, pos=4, col="red") # add labels


#The scatterplot shows the first principal component on the x-axis, and the second principal component on the yaxis.
#We can see from the scatterplot that wine samples of cultivar 1 have much lower values of the first principal
#component than wine samples of cultivar 3. Therefore, the first principal component separates wine samples of
#cultivars 1 from those of cultivar 3.
#We can also see that wine samples of cultivar 2 have much higher values of the second principal component than
#wine samples of cultivars 1 and 3. Therefore, the second principal component separates samples of cultivar 2 from
#samples of cultivars 1 and 3.
#Therefore, the first two principal components are reasonably useful for distinguishing wine samples of the three
#different cultivars.

wineDF = data.frame(wine.pca$x[,1],wine.pca$x[,2])
wine_km = kmeans(wineDF,centers = 3)
for (k in 0:178)
{
  wine_km$cluster[k] = toString(wine_km$cluster[k])
}
wineDF = data.frame(wine.pca$x[,1],wine.pca$x[,2],wine_km$cluster)
names(wineDF) <- c("c1","c2","Species")
ggplot(wineDF,aes(x=c1,y=c2,colour = Species))+stat_density2d ()
wine_km$cluster












