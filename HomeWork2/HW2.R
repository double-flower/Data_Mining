library(arules)
library("arulesViz")

path = "D:/BIT/课程/2017-2018下/数据挖掘2018/作业/HomeWork2/"
setwd(path)

#读取并转换数据
trans_data <- read.transactions("Building_Permits.csv")
inspect(trans_data[1:3])

#找出频繁项集
freq_sets <- apriori(trans_data, parameter=list
	(support=0.1, maxlen=10, minlen=2, target="frequent itemsets"))
inspect(sort(freq_sets, by="support")[1:10])

#freq_sets <- eclat(trans_data, parameter=list(support=0.01, maxlen=10, minlen=2))

#导出关联规则，计算其支持度和置信度
rules = apriori(trans_data, parameter=list(support=0.1, minlen=2))
inspect(sort(rules, by="support")[1:10])
plot(rules)

#对规则进行评价
sub.rules = subset(rules, subset=rhs%in%"frame"&lift>1)
inspect(sort(sub.rules, by="lift")[1:10])

#画图
plot(rules)

#文件保存
df.rules = as(rules, "data.frame")
write.csv(df.rules, "Rules_BP.csv")