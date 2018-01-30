# Kaggle:
#   Titanic

library(psych)

setwd("d:/kaggle/titanic")

d_train <- read.csv("train.csv")
d_test <- read.csv("test.csv")

# TODO - 結合
d <- rbind(train, test)
d <- train

str(d)
summary(d)
head(d)

# TODO - 空白の欠損値 Cabin
apply(is.na(d), 2, sum)

# 散布図行列
pairs(d, panel=panel.smooth)
pairs.panels(d)

# TODO - 相関
cor(d)
hist(d$Age)
plot(d$Sex)

# ----- Decision Tree -----
library(rpart)
library(rpart.plot)

d <- read.csv("train.csv")
test <- read.csv("test.csv")

d$Name <- NULL
d$Ticket <- NULL
d$Cabin <- NULL

m <- rpart(Survived~., d)
rpart.plot(m)

pred <- predict(m, test)
a <- data.frame(PassengerId=test$PassengerId, Survived=round(pred))
write.csv(a, "answer.csv", quote=FALSE, row.names=FALSE)

# ----- Random Forest -----
library(randomForest)

d <- read.csv("train.csv")
test <- read.csv("test.csv")

d$PassengerId <- NULL
d$Name <- NULL
d$Ticket <- NULL
d$Cabin <- NULL

d <- na.omit(d)
m <- randomForest(as.factor(Survived)~., d)
m
varImpPlot(m)

d["Age"].fillna(d.Age.median(), inplace=True)

pred <- predict(m, test) 

