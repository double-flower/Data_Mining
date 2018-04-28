path = "D:/BIT/�γ�/2017-2018��/�����ھ�2018/��ҵ/HomeWork2/"
setwd(path)

#��ȡ��ת������
data <- read.csv("Building_Permits.csv")
data1 <- data.frame(
	permittype = as.factor(data$Permit.Type),
	streetsuffix = as.factor(data$Street.Suffix),
	currentstatus = as.factor(data$Current.Status)
)
trans_data1 <- as(data1, "transactions")
inspect(trans_data1[1:3])

#Ƶ���
freq1 <- apriori(trans_data1, parameter=list
	(support=0.1, maxlen=10, minlen=2, target="frequent itemsets"))
inspect(sort(freq1, by="support")[1:10])

#��������
rules = apriori(trans_data1, parameter=list(support=0.1, minlen=2))
inspect(sort(rules, by="support")[1:7])

plot(rules)


#�Թ����������
sub.rules = subset(rules, subset=rhs%in%"permittype=8"&lift>1)
inspect(sort(sub.rules, by="lift")[1:7])