library(car)
library(DMwR)

path = "D:/BIT/课程/2017-2018下/数据挖掘2018/作业/作业1/"

#数据集2的探索性分析与预处理
#读取数据
base_path = paste(path, "BP/", sep="")
setwd(base_path)
data <- read.csv("Building_Permits.csv")

#数据摘要
summary(data)

#绘制直方图
setwd(base_path)
dir.create("hist")
hist_path = paste(base_path, "hist/", sep="")
setwd(hist_path)

for (i in 1:ncol(data))
	if (class(data[, i]) != "factor")
	{
		hist(data[[i]], col=rainbow(7), xlab=names(data[i]), 
			main=paste("Histogram of", names(data[i])))
		savePlot(paste("hist_", gsub(".", "-", names(data[i]), fixed=TRUE), sep=""), type=c("jpg"))
	}

#绘制QQ图
setwd(base_path)
dir.create("qqFigure")
qqPlot_path = paste(base_path, "qqFigure/", sep="")
setwd(qqPlot_path)

for (i in 1:ncol(data))
	if (class(data[, i]) != "factor")
	{
		qqPlot(data[[i]], main=paste('Norm QQ Plot of', names(data[i])), ylab=names(data[i]))
		savePlot(paste("qqPlot_", gsub(".", "-", names(data[i]), fixed=TRUE), sep=""), type=c("jpg"))
	}

#绘制盒图
setwd(base_path)
dir.create("boxPlot")
boxPlot_path = paste(base_path, "boxPlot/", sep="")
setwd(boxPlot_path)

for (i in 1:ncol(data))
	if (class(data[, i]) != "factor")
	{
		boxplot(data[[i]], main=paste('Box Figure of', names(data[i])), ylab=names(data[i]))
		rug(data[[i]], side=4)
		abline(h=mean(data[[i]], na.rm=T), lty=2)
		savePlot(paste("boxPlot_", gsub(".", "-", names(data[i]), fixed=TRUE), sep=""), type=c("jpg"))
	}

#将缺失数据处理
setwd(base_path)
dir.create("comp")
comp_path = paste(base_path, "comp/", sep="")
setwd(comp_path)


#将缺失部分剔除
data_omit1 = na.omit(data)
#举例
#直方图对比
jpeg(filename="del_Number-of-Existing-Stories_hist.jpg")
split.screen(c(1,2))
screen(1)
hist(data$Number.of.Existing.Stories, col=rainbow(7), xlab="original Number.of.Existing.Stories", main="")
screen(2)
hist(data_omit1$Number.of.Existing.Stories, col=rainbow(7), xlab="modified Number.of.Existing.Stories", main="")
dev.off()

#qq图对比
jpeg(filename="del_Number-of-Existing-Stories_qqFig.jpg", width=600, height=480)
split.screen(c(1,2))
screen(1)
qqPlot(data$Number.of.Existing.Stories, col=rainbow(7), xlab="original Number.of.Existing.Stories", main="", ylab="")
screen(2)
qqPlot(data_omit1$Number.of.Existing.Stories, col=rainbow(7), xlab="modified Number.of.Existing.Stories", main="", ylab="")
dev.off()

#盒图对比
jpeg(filename="del_Number-of-Existing-Stories_boxFig.jpg", width=600, height=480)
split.screen(c(1,2))
screen(1)
boxplot(data$Number.of.Existing.Stories, main="", ylab="original Number.of.Existing.Stories", xlab="")
screen(2)
boxplot(data_omit1$Number.of.Existing.Stories, main="", ylab="modified Number.of.Existing.Stories", xlab="")
dev.off()



#用最高频率来填补缺失值
data_omit2 = data[-manyNAs(data),]
data_omit2 = centralImputation(data_omit2)
#举例
#直方图对比
jpeg(filename="freq_Number-of-Existing-Stories_hist.jpg")
split.screen(c(1,2))
screen(1)
hist(data$Number.of.Existing.Stories, col=rainbow(7), xlab="original Number.of.Existing.Stories", main="")
screen(2)
hist(data_omit2$Number.of.Existing.Stories, col=rainbow(7), xlab="modified Number.of.Existing.Stories", main="")
dev.off()

#qq图对比
jpeg(filename="freq_Number-of-Existing-Stories_qqFig.jpg", width=600, height=480)
split.screen(c(1,2))
screen(1)
qqPlot(data$Number.of.Existing.Stories, col=rainbow(7), xlab="original Number.of.Existing.Stories", main="", ylab="")
screen(2)
qqPlot(data_omit2$Number.of.Existing.Stories, col=rainbow(7), xlab="modified Number.of.Existing.Stories", main="", ylab="")
dev.off()

#盒图对比
jpeg(filename="freq_Number-of-Existing-Stories_boxFig.jpg", width=600, height=480)
split.screen(c(1,2))
screen(1)
boxplot(data$Number.of.Existing.Stories, main="", ylab="original Number.of.Existing.Stories", xlab="")
screen(2)
boxplot(data_omit2$Number.of.Existing.Stories, main="", ylab="modified Number.of.Existing.Stories", xlab="")
dev.off()

#通过属性的相关关系来填补缺失值
x<-vector(mode="numeric",length=0)
j = 1
for(i in 1:ncol(data))
	if(class(data[,i]) != "factor")
	{
		x[j] <- i
		j <- j+1
	}

symnum(cor(data[, c(x)],use="complete.obs"))
lm(formula = Number.of.Existing.Stories~Number.of.Proposed.Stories, data = data)
data_omit3 = data[-manyNAs(data),]
fillNE <- function(NP){
	if(is.na(NP))
		return(NA)
	else
		return (-0.0171+0.9968*NP)
}
data_omit3[is.na(data_omit3$Number.of.Existing.Stories), 'Number.of.Existing.Stories'] <- 
		sapply(data_omit3[is.na(data_omit3$Number.of.Existing.Stories),'Number.of.Proposed.Stories'],fillNE)
#举例
#直方图对比
jpeg(filename="relation_Number-of-Existing-Stories_hist.jpg")
split.screen(c(1,2))
screen(1)
hist(data$Number.of.Existing.Stories, col=rainbow(7), xlab="original Number.of.Existing.Stories", main="")
screen(2)
hist(data_omit3$Number.of.Existing.Stories, col=rainbow(7), xlab="modified Number.of.Existing.Stories", main="")
dev.off()

#qq图对比
jpeg(filename="relation_Number-of-Existing-Stories_qqFig.jpg", width=600, height=480)
split.screen(c(1,2))
screen(1)
qqPlot(data$Number.of.Existing.Stories, col=rainbow(7), xlab="original Number.of.Existing.Stories", main="", ylab="")
screen(2)
qqPlot(data_omit3$Number.of.Existing.Stories, col=rainbow(7), xlab="modified Number.of.Existing.Stories", main="", ylab="")
dev.off()

#盒图对比
jpeg(filename="relation_Number-of-Existing-Stories_boxFig.jpg", width=600, height=480)
split.screen(c(1,2))
screen(1)
boxplot(data$Number.of.Existing.Stories, main="", ylab="original Number.of.Existing.Stories", xlab="")
screen(2)
boxplot(data_omit3$Number.of.Existing.Stories, main="", ylab="modified Number.of.Existing.Stories", xlab="")
dev.off()


#通过数据对象之间的相似性来填补缺失值
data_1 = data[1:500, 1:25]
data_2 = data[1:5000, 1:25]
data_omit4_1 = knnImputation(data_1, 10)
data_omit4_2 = knnImputation(data_2, 10)

#举例
#直方图对比
jpeg(filename="simmilar_Number-of-Existing-Stories_hist_1.jpg")
split.screen(c(1,2))
screen(1)
hist(data_1$Number.of.Existing.Stories, col=rainbow(7), xlab="original Number.of.Existing.Stories(500)", main="")
screen(2)
hist(data_omit4_1$Number.of.Existing.Stories, col=rainbow(7), xlab="modified Number.of.Existing.Stories(500)", main="")
dev.off()

jpeg(filename="simmilar_Number-of-Existing-Stories_hist_2.jpg")
split.screen(c(1,2))
screen(1)
hist(data_2$Number.of.Existing.Stories, col=rainbow(7), xlab="original Number.of.Existing.Stories(5000)", main="")
screen(2)
hist(data_omit4_2$Number.of.Existing.Stories, col=rainbow(7), xlab="modified Number.of.Existing.Stories(5000)", main="")
dev.off()


#qq图对比
jpeg(filename="simmilar_Number-of-Existing-Stories_qqFig_1.jpg", width=600, height=480)
split.screen(c(1,2))
screen(1)
qqPlot(data_1$Number.of.Existing.Stories, col=rainbow(7), xlab="original Number.of.Existing.Stories(500)", main="", ylab="")
screen(2)
qqPlot(data_omit4_1$Number.of.Existing.Stories, col=rainbow(7), xlab="modified Number.of.Existing.Stories(500)", main="", ylab="")
dev.off()

jpeg(filename="simmilar_Number-of-Existing-Stories_qqFig_2.jpg", width=600, height=480)
split.screen(c(1,2))
screen(1)
qqPlot(data_2$Number.of.Existing.Stories, col=rainbow(7), xlab="original Number.of.Existing.Stories(5000)", main="", ylab="")
screen(2)
qqPlot(data_omit4_2$Number.of.Existing.Stories, col=rainbow(7), xlab="modified Number.of.Existing.Stories(5000)", main="", ylab="")
dev.off()

#盒图对比
jpeg(filename="simmilar_Number-of-Existing-Stories_boxFig_1.jpg", width=600, height=480)
split.screen(c(1,2))
screen(1)
boxplot(data_1$Number.of.Existing.Stories, main="", ylab="original Number.of.Existing.Stories(500)", xlab="")
screen(2)
boxplot(data_omit4_1$Number.of.Existing.Stories, main="", ylab="modified Number.of.Existing.Stories(500)", xlab="")
dev.off()

jpeg(filename="simmilar_Number-of-Existing-Stories_boxFig_2.jpg", width=600, height=480)
split.screen(c(1,2))
screen(1)
boxplot(data_2$Number.of.Existing.Stories, main="", ylab="original Number.of.Existing.Stories(5000)", xlab="")
screen(2)
boxplot(data_omit4_2$Number.of.Existing.Stories, main="", ylab="modified Number.of.Existing.Stories(5000)", xlab="")
dev.off()

