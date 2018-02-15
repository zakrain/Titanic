# Kaggle:
#   Titanic
#
# - DATA
# PassengerID:　乗客ID
# Survived: 　　生存結果(0:No, 1:Yes)　
# Pclass: 　　　乗客の階級(1:1st, 2:2nd, 3:3rc)
# Name: 　　  　乗客の名前
# Sex: 　　　　 性別
# Age: 　　　　 年齢
# SibSp 　　 　 兄弟、配偶者の数
# Parch 　　　　両親、子供の数
# Ticket 　　　 チケット番号
# Fare 　　　　 乗船料金
# Cabin 　　　　部屋番号
# Embarked 　　 乗船した港(c:Cherbourg, Q:Queenstown, S:Southampton)

library(psych)

setwd("d:/kaggle/titanic")
train <- read.csv("train.csv", stringsAsFactors=F, na.strings = c("NA",""))
test <- read.csv("test.csv", stringsAsFactors=F, na.strings = c("NA",""))

# 結合
test$Survived <- NA
d <- rbind(train, test)

str(d)
summary(d)
head(d)

# 欠損値 Cabin
apply(is.na(d), 2, sum)
sapply(d, function(x) {sum(is.na(x))})

# 称号(title)
d$Title <- sapply(d$Name, function(x) {strsplit(x, split='[,.]')[[1]][2]})
d$Title <- trimws(d$Title, "both")

# クロス集計
table(d$Sex)
table(d$Pclass)
table(d$Embarked)
table(d$Title)

d$Sex <- as.factor(d$Sex)
d$Pclass <- as.factor(d$Pclass)
d$Embarked <- as.factor(d$Embarked)
d$Title <- as.factor(d$Title)

# 欠損値を補間
a <- tapply(d$Age, d$Title, function(x) {mean(x, na.rm=T)})
b <- d[is.na(d$Survived) & is.na(d$Age), "Title"]
table(b)
d[is.na(d$Survived) & is.na(d$Age) & d$Title=="Master", "Age"] <- a["Master"]
d[is.na(d$Survived) & is.na(d$Age) & d$Title=="Miss", "Age"] <- a["Miss"]
d[is.na(d$Survived) & is.na(d$Age) & d$Title=="Mr", "Age"] <- a["Mr"]
d[is.na(d$Survived) & is.na(d$Age) & d$Title=="Mrs", "Age"] <- a["Mrs"]
d[is.na(d$Survived) & is.na(d$Age) & d$Title=="Ms", "Age"] <- a["Ms"]
d[is.na(d$Fare),"Fare"] <- median(d$Fare, na.rm=T)
apply(is.na(d[is.na(d$Survived),]), 2, sum)

# 生存率
table(d$Survived)
addmargins(round(prop.table(table(d$Survived))*100,2))
addmargins(table(d$Sex,d$Survived))
addmargins(round(prop.table(table(d$Sex, d$Survived))*100,2))
addmargins(table(d$Pclass,d$Survived))
addmargins(round(prop.table(table(d$Pclass, d$Survived))*100,2))

# TODO - グラフ化
plot(as.factor(train$Survived))
hist(d$Fare)

# TODO - 相関
cor(d)
hist(d$Age)
plot(d$Sex)

# 散布図行列
pairs(d, panel=panel.smooth)
pairs.panels(d)

# ----- Decision Tree -----
library(rpart)
library(rpart.plot)

train <- d[!is.na(d$Survived),]
test <- d[is.na(d$Survived),]

train$Name <- NULL
train$Ticket <- NULL
train$Cabin <- NULL

m <- rpart(Survived~., train)
rpart.plot(m)

pred <- predict(m, test)
a <- data.frame(PassengerId=test$PassengerId, Survived=round(pred))
write.csv(a, "answer.csv", quote=FALSE, row.names=FALSE)

# ----- Random Forest -----
library(randomForest)

train <- d[!is.na(d$Survived),]
test <- d[is.na(d$Survived),]

train$PassengerId <- NULL
train$Name <- NULL
train$Ticket <- NULL
train$Cabin <- NULL

train <- na.omit(train)
m <- randomForest(as.factor(Survived)~., d=train)
m
varImpPlot(m)

pred <- predict(m, test)
pred
a <- data.frame(PassengerId=test$PassengerId, Survived=pred)
write.csv(a, "answer.csv", quote=FALSE, row.names=FALSE)

# ----- svM -----


