# Part1 Introduction

在本次报告中，我们使用了K-均值聚类，K-中间点聚类，系谱聚类，密度聚类， 期望最大化聚类等经典聚类方法，在多个不同的UCI数据集上进行聚类测试，并试图评估方法之间的优缺点以及其在运用中可能会碰到的实际问题。

## 一、数据集介绍

本次报告使用了三个数据集，均来自UCI Dataset。

第一个数据集为Iris鸢尾花数据集，样本大小为150条，参数数目为4个，分别为花萼的长宽度，花瓣的长宽度，均为数字格式。数据集中的鸢尾花共有三个种类：Serosa , Versicolor , Virginica。
作为最简单的数据集，在实验一开始使用Iris数据集的意义在于将所有聚类方法都尽可能地尝试一遍，掌握其基本的使用。

第二个数据集为Mushroom蘑菇数据集，样本大小为8124条，参数数目为
22个，主要包括蘑菇的伞柄、根茎的大小、形状、颜色等指标，均为字符串格式，因此需要做一定的预处理，操作起来相对复杂。数据集中蘑菇共有两个种类：有毒与无毒。
选择Mushroom数据集的目的有二：第一，它是一个稍大的数据集，更多的数据条数有利于我们得到更普遍的聚类效果。第二，Mushroom数据集的特征值相对较多，事实上，我在对其进行聚类时，也发现了数据维度高而导致的难以展示、分析的问题，从而引发了我对这些问题的思考，并在第三个数据集中作出了尝试。

第三个数据集为Wine红酒数据集，样本大小为178条，参数数目为15个，主要包括红酒的种类（共三种），酒中苹果酸含量、镁元素含量、色泽等。
在第三个数据集中，经过前两次的实践与思考，我选择使用PCA主成分分析法，对原本的高维数据进行降维，从而得到了更好的可视化效果，也方便我们聚类结果进行解析。

## 二、算法介绍

本次报告使用了K-均值聚类，K-中间点聚类，系谱聚类，密度聚类，期望最大化聚类这几种经典聚类方法。

K-均值聚类（K-Means）是一种迭代求解的聚类分析算法，其核心思想在于随机选取几个簇中心，通过距离计算将新的点不断加入簇中，每完成一轮迭代，更新一次簇中心，从而达到聚类的效果。是最经典有效的聚类算法之一。

K-中间点聚类（K-Medoids），是一种类似于K-Means的算法。其为了防止K-Means迭代中“平均”过程带来的对孤立点的敏感性，每次更新簇中心时采用选取最近点的方法。虽然做了改进，但仍无法摆脱此类聚类对边界把握较为模糊的问题。

系谱聚类（Hierarchical Clustering），也是一类迭代类算法，其每次迭代都会将两个距离最近的簇/样本点合并成一个新簇。因此对于同一个数据集，系谱聚类能够同时给出k在不同取值下的聚类结果。

密度聚类（DBSCAN），是一个代表性的基于密度的聚类算法，它将簇定义为“密度相连的点的最大集合”。DBSCAN的最大优点是其能够在有噪声的情况下发现任意形状的簇。由于其的这个优点，DBSCAN在文本聚类领域发挥了很大作用，也受到学术界的青睐。

期望最大化聚类（EM）的思路则不一样，它在聚类时将数据集看作一个含有隐性变量的概率模型，通过“反复估计”模型参数找出符合模型的最优解，并同时给出相应的最优簇数k。




# Part2 Experiment

在报告的第二部分中，与第一份报告不同，我们将按照数据集的顺序，依次介绍每个数据集的预处理、聚类操作以及探索性分析。并结合每一步的结果提出思考。

## 一、Iris数据集

### 1.1 预处理与探索
Iris数据集的特征值不多，仅仅包括花萼和花瓣的长宽度，但仍然包括4维特征值，难以进行可视化，因此我们选取花萼的长宽来对数据集做一个初步的认识，可以看到不同的鸢尾花被分为三类，且有着较为明晰的分类界限。

 

### 1.2 部署聚类算法
第一步，我们采用K-Means算法进行聚类，为了和鸢尾花的种类保持一致，将簇的个数k限定为3个。计算得到组间平方和占总平方和百分比为88.4%，说明在三个簇的情况下，聚类结果尚可。
我们接着采取穷举实验的方法来寻找最佳簇数k，以组间平方和占总平方和百分比作为评估指标，得到结果如下所示：
 

可以看出，在簇数k取66的情况下，组间平方和占总平方和达到了99.5%，此后基本保持不变。然而，猜簇数k仅仅取10的情况下，百分比便可达到95.4%。说明对于鸢尾花数据集这种较小数据集，一味增加簇的个数是不可取的，并不有利于我们进行聚类。

第二步，我们尝试使用K- Medoids方法进行聚类。由于方法与K-Means基本一致，因此这里不过多介绍。

第三步，我们使用系谱聚类方法。使用欧式距离来生成Iris数据集的距离矩阵，在展开系谱聚类，生成如下的系谱图：
 
由于系谱聚类会优先生成偶数个簇，正如图上表明，最终聚类的结果有1、2、4、8、16……个簇。但如果我们仔细观察，可以发现在划分成4个簇时，产生了“3个大簇+1个小簇”的状况，在一定程度上反应了“一共有三种鸢尾花”的事实。

系谱聚类的另一个特点是其可以进行剪枝，更好地符合我们聚类的需求。我们通过设定高度H和簇数K来完成剪枝。如下表所示：

簇1 | 簇2 | 簇3
-- | -- | ---
H=6，K不做限制 | 78 | 72 | None
K=3，H不做限制 | 50 | 72 | 28

至于K和H值的选定，往往是通过实验结果结合自己需求所得出。

第四步，使用密度聚类。密度聚类需要设定半径EPS和阈值MinPts两个核心参数。一般来说，半径与阈值的取值差别越大，所得的簇数越小。

因此，在选取这两个核心参数时，我们有必要针对数据本身做一些观察，例如，我们需要考察大多数样本之间的距离，将这个距离作为半径参数的取值，以此来保证大部分样本不会被算法考虑成噪声。经过计算，我们发现大部分样本间的距离在0.2-1.1之间。因此，我们循环列举这两个参数，得到下表：

EPS | 0.2 | 0.2 | 0.5 | 0.5 | 0.8 | 0.8 | 1.1 | 1.1
阈值MinPts	| 类别数k	| 噪声点数 |	类别数k | 噪声点数 |	类别数k |	噪声点数 |	类别数k |	噪声点数
------- | ------ | ----- | ------ | ------ | ------ | ------| ------ | ------ 
1	| 108 |	0 |	12 |	49 |	3 |	0 |	2 |	0
2	| 19 |	89 |	6 |	6 | 3 |	0 |	2 |	0
3	| 6	| 115 |	4 |	10 |	2 |	2 |	2 |	0
4 |	3 |	128 |	3 |	13 |	2 |	2 |	2 |	0
5 |	2 |	132 |	2 |	17 |	2 |	2 |	2 |	0


从表中可以看出，实验结果符合预期：当EPS与MinPts的差值变大时，得到的类别数k即簇数也越小。

第五步，使用EM最大化聚类。使用Mclust程序包，根据BIC指标，各大模型的表现如图所示：

 

由图可以看出，算法根据BIC选出的最佳模型类型为VEV，最佳簇数为2。

在此顺便给出Mclust聚类结果的概率图：

 

至此，完成了对于Iris数据集的聚类练习。


## 二、Mushroom数据集

### 2.1 数据预处理
相比Iris，Mushroom数据集最大的不同是有相对复杂的预处理过程。
首先，数据集本身存在缺失，例如stalk root属性中出现了部分“？”值，应当删除这部分样本，并重新因子化。
其次，大部分属性为字母，指代某种特性，是典型的离散型变量，需要我们进行重新编码，用数字一一进行代替。
最后，部分属性是完全一致的，为了避免影响，应当删除，例如Veil_type这个属性属于没有意义的重复属性。

### 2.2 探索和疑问
由于在Iris数据集中，我们已经实践过全部聚类算法，因此在这里我们并不对已经涉及过的算法进行重复实践，而是选择关注这样一个问题：对于Mushroom这种有22个属性的数据集，如此多的属性会不会对聚类效果造成影响呢？此外，这样多的属性也使得我们无法很好地对结果进行可视化，聚类和分析工作都会遇到一定的困难。

我们首先采用K-Means算法，采用迭代的方式寻找合适的簇数，结果显示，在簇数为60左右时，计算得到组间平方和占总平方和百分比为83.8%，此后增长缓慢。因此我们可以认为这8124个蘑菇样本适合被分为60个左右的不同种类。

事实上，Mushroom数据集的本意是让我们来找出哪些蘑菇是有毒的，哪些是无毒的。这可以被转化成簇数为2的聚类问题。这不由得引起了我们的兴趣：如果我们用聚类方法聚出两个类，可以精准的反应“有毒”和“无毒”这两大类吗？

我们采用K-Means算法和K-Medoids方法进行实验，可惜的是，这两种方法的聚类结果经过比对，分别只能达到31%和27%，结果并不能令人满意。这一定程度上也反映了聚类和分类本质上是为了解决不同的问题，在方法上也存在着较大的分隔，不能做到想象中的“互通有无”。

在与Mushroom数据集打交道的过程中，最令人头痛的是高维数据带来的麻烦：我很难对不同的属性进行一一比对，因为数量太多。我也不能很好地作图展示数据的概况，因为2维和3维的作图方法面对22维的数据有些力不从心。
那么，该如何解决聚类过程中由维度引起的这类问题呢？这就引出了第三数据集的处理方法——属性筛选，主成分分析。

## 三、Wine数据集

对于Wine数据集，我们采用两种方法来解决刚刚提到的高维数据带来的问题。
（以下图表数据和结论均为归一化等预处理后得出，不再一一赘述数据处理过程）

### 3.1 方法一：筛选属性
Wine数据集希望我们使用提供的14个属性来对3个不同种类的红酒进行分析。面对这样多的属性，我们首先可以通过计算属性间的相关矩阵、来分析它们之间的联系，如下图所示：

 

通过相关矩阵中两两属性之间的相似度，我们可以筛选出相关度不高的彼此独立的属性，以它们作为聚类的重要指标。相对应地，弱化那些相关度高的属性，因为它们包含了相近的信息。

同时，我们也可以绘制样本点在部分属性轴上的投影，更好地观察哪几对属性能够对于样本整体起到更好的划分作用：

 

通过上图分别观察各对属性下样本的分布后，我们可以缩小需要观察属性的范围，以便进一步搜索。例如，假设我们对于“Ash”和“Alcalinity of ash”这一对属性（即图中红圈部分）非常感兴趣，我们可以针对这对属性单独绘制一张样本分布情况图：

 

可以看到，在“Ash”和“Alcalinity of ash”这对属性的维度下，不同类别的样本点能够呈现较为清晰的区隔分布，因此这一对属性对于聚类是有比较重要的参考价值的。
依次类推，按照这个流程寻找，就可以得到一批适合做聚类的属性，事实上，对于本数据集，用两个质量较高的属性作出的聚类效果就已经很可观，并且天然适合可视化等操作。

### 3.2 方法二：主成分分析（PCA）

虽然方法一在本数据集表现不错，但是依然有着两个问题：第一，只保留若干属性显然会损失大量原有信息，这显然不合适数据挖掘“物尽其用”的思想。第二，面对更高维度的数据集，即使做过筛选，依然有较多属性剩下，又会陷入维度过高的困扰。

因此，为了在充分利用所有信息的前提下，尽可能的降低数据的维度，我们使用最流行的降维方法：主成分分析。

主成分分析是一种经典的统计学方法，它通过正交变换将一组可能存在相关性的变量转化成一线性不相关的变量，每个变量代表了数据在这个方向上的信息。

在聚类问题中，主成分分析往往用来将多个属性“融合”成若干个精简后的新属性，每个新属性都是之前几个原属性的线性表示，相当于对众多属性执行了“压缩”操作。这样既尽可能保留了原有信息，又做到了降维。

在R中，我们采用princomp函数进行PCA操作。

 
由图可知，PCA操作将原有的属性“压缩”成了10个成分，其中，第一和第二成分包含的信息最多。事实上，如果选用前三个成分，我们就可以得到绝大部分原有属性包含的信息用来执行聚类。

我们不妨取第一成分和第二成分为依照进行聚类：

 

令人惊喜的是，样本数据在这两个主成分方向上的投影变得非常有规律，仅凭肉眼都可以给三类样本画出清晰的边界。

 
通过这样的实验，我们初步领会了PCA降维给聚类带来的便利性，这样的便利性不仅是体现在结果的准确上，也是体现在可视化的方便和分析的容易入手上。

以上即为本次实验的全部内容。

# Part3 Conclusion

本次实验完成了以下工作：
首先，对于最简单的Iris数据集，我们尝试了课本介绍的所有聚类方法，对这些方法的使用有了清晰的了解。
其次，对于稍显复杂的Mushroom数据集，我们练习了聚类前的预处理流程，并发现了高维数据带来的两个问题：1.属性过于繁杂，难以发现内在联系。2.高维数据不利于可视化。
最后，在Wine数据集上，我们实践了两个解决高维数据给聚类带来的困难的方法。1.利用相关矩阵和二维投影的方式，挑选有代表性的属性，删除其他的。2.使用主成分分析，对属性进行“压缩”，尽可能多保留原有信息。前一个方法有着较大的局限性，且较为繁琐。后者则更为实用，并且在数据集上取得了不错的效果。


# Part4 Reference

黄文、王正林：《数据挖掘实战》 
Pang-Ning Tan：《Introduction to Data Mining》 
Brett Lantz：《Machine Learning with R》 
Jared Lander：《R for Everyone》 
Cookbook for R： http://www.cookbook-r.com/ 



