library(car)
library(DMwR)

path = "D:/BIT/�γ�/2017-2018��/�����ھ�2018/��ҵ/��ҵ1/"

#���ݼ�1��̽���Է�����Ԥ����
#��ȡ����
base_path = paste(path, "NFL/", sep="")
setwd(base_path)
data <- read.csv("NFL Play by Play 2009-2017 (v4).csv")

#����ժҪ
summary(data)

#����ֱ��ͼ
setwd(base_path)
dir.create("hist")
hist_path = paste(base_path, "hist/", sep="")
setwd(hist_path)

for (i in 2:ncol(data))
	if (class(data[, i]) != "factor")
	{
		hist(data[[i]], col=rainbow(7), xlab=names(data[i]), main=paste("Histogram of", names(data[i])))
		savePlot(paste("hist_", gsub(".", "-", names(data[i]), fixed=TRUE), sep=""), type=c("jpg"))
	}

#����QQͼ
setwd(base_path)
dir.create("qqFigure")
qqPlot_path = paste(base_path, "qqFigure/", sep="")
setwd(qqPlot_path)

for (i in 2:ncol(data))
	if (class(data[, i]) != "factor")
	{
		qqPlot(data[[i]], col=rainbow(7), main=paste('Norm QQ Plot of', names(data[i])), ylab=names(data[i]))
		savePlot(paste("qqPlot_", gsub(".", "-", names(data[i]), fixed=TRUE), sep=""), type=c("jpg"))
	}

#���ƺ�ͼ
setwd(base_path)
dir.create("boxPlot")
boxPlot_path = paste(base_path, "boxPlot/", sep="")
setwd(boxPlot_path)

#GameID��ֵ�ϴ����⴦��
boxplot(data$GameID/1000000000, main="Box Figure of GameID", ylab="GameID/e+09")
rug(data$GameID/1e+09, side=4)
abline(h=mean(data$GameID/1e+09, na.rm=T), lty=2)
savePlot("boxPlot_GameID", type=c("jpg"))

for (i in 3:ncol(data))
	if (class(data[, i]) != "factor")
	{
		boxplot(data[[i]], main=paste('Box Figure of', names(data[i])), ylab=names(data[i]))
		rug(data[[i]], side=4)
		abline(h=mean(data[[i]], na.rm=T), lty=2)
		savePlot(paste("boxPlot_", gsub(".", "-", names(data[i]), fixed=TRUE), sep=""), type=c("jpg"))
	}

#��ȱʧ���ݴ���
setwd(base_path)
dir.create("comp")
comp_path = paste(base_path, "comp/", sep="")
setwd(comp_path)


#��ȱʧ�����޳�
data_omit1 = na.omit(data_omit1)

#�����Ƶ�����ȱʧֵ
data_omit2 = data[-manyNAs(data),]
data_omit2 = centralImputation(data_omit2)
#����
#ֱ��ͼ�Ա�
jpeg(filename="freq_Home-WP-pre_hist.jpg")
split.screen(c(1,2))
screen(1)
hist(data$Home_WP_pre, col=rainbow(7), xlab="original Home_WP_pre", main="")
screen(2)
hist(data_omit2$Home_WP_pre, col=rainbow(7), xlab="modified Home_WP_pre", main="")
dev.off()

#qqͼ�Ա�
jpeg(filename="freq_Home-WP-pre_qqFig.jpg", width=600, height=480)
split.screen(c(1,2))
screen(1)
qqPlot(data$Home_WP_pre, col=rainbow(7), xlab="original Home_WP_pre", main="", ylab="")
screen(2)
qqPlot(data_omit2$Home_WP_pre, col=rainbow(7), xlab="modified Home_WP_pre", main="", ylab="")
dev.off()

#��ͼ�Ա�
jpeg(filename="freq_Home-WP-pre_boxFig.jpg", width=600, height=480)
split.screen(c(1,2))
screen(1)
boxplot(data$Home_WP_pre, main="", ylab="original Home_WP_pre", xlab="")
screen(2)
boxplot(data_omit2$Home_WP_pre, main="", ylab="modified Home_WP_pre", xlab="")
dev.off()

#ͨ�����Ե���ع�ϵ���ȱʧֵ
symnum(cor(data[83:102],use="complete.obs"))
lm(formula = Home_WP_pre~Away_WP_pre, data = data)
data_omit3 = data[-manyNAs(data),]
fillHome_WP_pre <- function(Away_WP_pre){
	if(is.na(Away_WP_pre))
		return(NA)
	else
		return (0.9991-0.9970*Away_WP_pre)
}
data_omit3[is.na(data_omit3$Home_WP_pre), 'Home_WP_pre'] <- sapply(data_omit3[is.na(data_omit3$Home_WP_pre),'Away_WP_pre'],fillHome_WP_pre)
#����
#ֱ��ͼ�Ա�
jpeg(filename="relation_Home-WP-pre_hist.jpg")
split.screen(c(1,2))
screen(1)
hist(data$Home_WP_pre, col=rainbow(7), xlab="original Home_WP_pre", main="")
screen(2)
hist(data_omit3$Home_WP_pre, col=rainbow(7), xlab="modified Home_WP_pre", main="")
dev.off()

#qqͼ�Ա�
jpeg(filename="relation_Home-WP-pre_qqFig.jpg", width=600, height=480)
split.screen(c(1,2))
screen(1)
qqPlot(data$Home_WP_pre, col=rainbow(7), xlab="original Home_WP_pre", main="", ylab="")
screen(2)
qqPlot(data_omit3$Home_WP_pre, col=rainbow(7), xlab="modified Home_WP_pre", main="", ylab="")
dev.off()

#��ͼ�Ա�
jpeg(filename="relation_Home-WP-pre_boxFig.jpg", width=600, height=480)
split.screen(c(1,2))
screen(1)
boxplot(data$Home_WP_pre, main="", ylab="original Home_WP_pre", xlab="")
screen(2)
boxplot(data_omit3$Home_WP_pre, main="", ylab="modified Home_WP_pre", xlab="")
dev.off()


#ͨ�����ݶ���֮������������ȱʧֵ
data_1 = data[1:1000, 94:95]
data_2 = data[1:5000, 94:95]
data_omit4_1 = knnImputation(data_1, 10)
data_omit4_2 = knnImputation(data_2, 10)

#����
#ֱ��ͼ�Ա�
jpeg(filename="simmilar_Home-WP-pre_hist_1.jpg")
split.screen(c(1,2))
screen(1)
hist(data_1$Home_WP_pre, col=rainbow(7), xlab="original Home_WP_pre(1000)", main="")
screen(2)
hist(data_omit4_1$Home_WP_pre, col=rainbow(7), xlab="modified Home_WP_pre(1000)", main="")
dev.off()

jpeg(filename="simmilar_Home-WP-pre_hist_2.jpg")
split.screen(c(1,2))
screen(1)
hist(data_2$Home_WP_pre, col=rainbow(7), xlab="original Home_WP_pre(5000)", main="")
screen(2)
hist(data_omit4_2$Home_WP_pre, col=rainbow(7), xlab="modified Home_WP_pre(5000)", main="")
dev.off()


#qqͼ�Ա�
jpeg(filename="simmilar_Home-WP-pre_qqFig_1.jpg", width=600, height=480)
split.screen(c(1,2))
screen(1)
qqPlot(data_1$Home_WP_pre, col=rainbow(7), xlab="original Home_WP_pre(1000)", main="", ylab="")
screen(2)
qqPlot(data_omit4_1$Home_WP_pre, col=rainbow(7), xlab="modified Home_WP_pre(1000)", main="", ylab="")
dev.off()

jpeg(filename="simmilar_Home-WP-pre_qqFig_2.jpg", width=600, height=480)
split.screen(c(1,2))
screen(1)
qqPlot(data_2$Home_WP_pre, col=rainbow(7), xlab="original Home_WP_pre(5000)", main="", ylab="")
screen(2)
qqPlot(data_omit4_2$Home_WP_pre, col=rainbow(7), xlab="modified Home_WP_pre(5000)", main="", ylab="")
dev.off()

#��ͼ�Ա�
jpeg(filename="simmilar_Home-WP-pre_boxFig_1.jpg", width=600, height=480)
split.screen(c(1,2))
screen(1)
boxplot(data_1$Home_WP_pre, main="", ylab="original Home_WP_pre(1000)", xlab="")
screen(2)
boxplot(data_omit4_1$Home_WP_pre, main="", ylab="modified Home_WP_pre(1000)", xlab="")
dev.off()

jpeg(filename="simmilar_Home-WP-pre_boxFig_2.jpg", width=600, height=480)
split.screen(c(1,2))
screen(1)
boxplot(data_2$Home_WP_pre, main="", ylab="original Home_WP_pre(5000)", xlab="")
screen(2)
boxplot(data_omit4_2$Home_WP_pre, main="", ylab="modified Home_WP_pre(5000)", xlab="")
dev.off()



