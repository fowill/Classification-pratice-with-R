#--------开始导入数据----------#

#载入Iris数据集
iris <- read.csv('/Users/fowillwly/Downloads/iris.data',header = FALSE)
head(iris)
names(iris) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")

#画图，对数据有初步了解
library(ggplot2)
#按照花萼的长宽分布
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point(aes(color = Species))
#按照花瓣的长宽分布
ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) + geom_point(aes(color = Species))