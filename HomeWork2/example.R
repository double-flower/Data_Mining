path = "D:/BIT/课程/2017-2018下/数据挖掘2018/作业/HomeWork2/"
setwd(path)

#读取并转换数据
data <- read.csv("Building_Permits.csv")
data1 <- data.frame(
	permittype = as.factor(data$Permit.Type),
	streetsuffix = as.factor(data$Street.Suffix),
	currentstatus = as.factor(data$Current.Status)
)
trans_data1 <- as(data1, "transactions")
inspect(trans_data1[1:3])

#频繁项集
freq1 <- apriori(trans_data1, parameter=list
	(support=0.1, maxlen=10, minlen=2, target="frequent itemsets"))
inspect(sort(freq1, by="support")[1:10])

#关联规则
rules = apriori(trans_data1, parameter=list(support=0.1, minlen=2))
inspect(sort(rules, by="support")[1:7])

plot(rules)


#对规则进行评价
sub.rules = subset(rules, subset=rhs%in%"permittype=8"&lift>1)
inspect(sort(sub.rules, by="lift")[1:7])