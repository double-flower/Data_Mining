library("randomForest")

path = "D:/BIT/课程/2017-2018下/数据挖掘2018/作业/Data_Mining/HomeWork3"
setwd(path)


#读取数据
train_data <- read.csv("./dataset/train.csv")
test_data <- read.csv("./dataset/test.csv")
test_label <- read.csv("./dataset/gender_submission.csv")


#output.forest <- randomForest(train_data$Survived ~ 
#					train_data$Pclass + 
#					train_data$Name + 
#					train_data$Sex + 
#					train_data$Age + 
#					train_data$SibSp + 
#					train_data$Parch + 
#					train_data$Ticket + 
#					train_data$Fare + 
#					train_data$Cabin + 
#					train_data$Embarked, 
#					data=train_data)


output.forest <- randomForest(train_data$Survived ~ ., data=train_data[1:30,], na.action=na.exclude)

