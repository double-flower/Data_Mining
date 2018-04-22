library(arules)
library("arulesViz")

path = "D:/BIT/�γ�/2017-2018��/�����ھ�2018/��ҵ/HomeWork2/"
setwd(path)

#��ȡ��ת������
trans_data <- read.transactions("Building_Permits.csv")
inspect(trans_data[1:3])

#�ҳ�Ƶ���
freq_sets <- apriori(trans_data, parameter=list
	(support=0.1, maxlen=10, minlen=2, target="frequent itemsets"))
inspect(sort(freq_sets, by="support")[1:10])

#freq_sets <- eclat(trans_data, parameter=list(support=0.01, maxlen=10, minlen=2))

#�����������򣬼�����֧�ֶȺ����Ŷ�
rules = apriori(trans_data, parameter=list(support=0.1, minlen=2))
inspect(sort(rules, by="support")[1:10])
plot(rules)

#�Թ����������
sub.rules = subset(rules, subset=rhs%in%"frame"&lift>1)
inspect(sort(sub.rules, by="lift")[1:10])

#��ͼ
plot(rules)

#�ļ�����
df.rules = as(rules, "data.frame")
write.csv(df.rules, "Rules_BP.csv")