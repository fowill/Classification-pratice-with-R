#--------开始导入数据----------#

#设置工作路径
setwd('/Users/fowillwly/Dev/R_homework_classification')

#载入Iris数据集
iris <- read.csv('./data/iris.data',header = FALSE)
head(iris)
names(iris) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
#画图，对数据有初步了解
library(ggplot2)
#按照花萼的长宽分布
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point(aes(color = Species))
#按照花瓣的长宽分布
ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) + geom_point(aes(color = Species))

fit_km1 = kmeans(iris[,-5],center=3)
print(fit_km1)

#输出本次聚类的总平方和、组间平方和的总和、组间平方和
fit_km1$totss;fit_km1$tot.withinss;fit_km1$betweenss

fit_km = 0
result = rep(0,150)
for (k in 1:150)
+{
+ fit_km = kmeans(iris[,-5],center=k)
+ result[k] = fit_km$betweenss/fit_km$totss
}
round(result,2)
