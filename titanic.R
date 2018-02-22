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

# ----- データ読込み -----

setwd("d:/kaggle/titanic")
train <- read.csv("train.csv", stringsAsFactors=F, na.strings = c("NA",""))
test <- read.csv("test.csv", stringsAsFactors=F, na.strings = c("NA",""))

# 結合
test$Survived <- NA
d <- rbind(train, test)
str(d)

# ----- 前処理 -----

# 称号(Title)
d$Title <- sapply(d$Name, function(x) {trimws(strsplit(x, split='[,.]')[[1]][2], "both")})
d[d$Title=="Mlle", "Title"] <- "Miss"
d[d$Title=="Mme", "Title"] <- "Mrs"
table(d$Title)

# 欠損値の補完(Fare)
a <- aggregate(Fare~Pclass, train, median)
d[(is.na(d$Fare) | d$Fare==0) & d$Pclass==1, "Fare"] <- a[a$Pclass==1, "Fare"]
d[(is.na(d$Fare) | d$Fare==0) & d$Pclass==2, "Fare"] <- a[a$Pclass==2, "Fare"]
d[(is.na(d$Fare) | d$Fare==0) & d$Pclass==3, "Fare"] <- a[a$Pclass==3, "Fare"]

# 欠損値の補完(Fare)
d[is.na(d$Embarked), "Embarked"] <- "S"

# 欠損値を補間(Age)
m <- glm(Age ~ Title + Fare + SibSp + Sex -1, data = d)

m <- glm(Age~Title+Fare+SibSp+Parch+Sex, data = d[!is.na(d$Age),])
m <- randomForest(Age~Title+Fare+SibSp+Sex, data = d[!is.na(d$Age),], action=na.ommit)

pred <- predict(m, d[is.na(d$Age),])
pred
d[is.na(d$Age),"Age"] <- pred
d[d$Age < 0, "Age"] <- 0

sapply(d, function(x) {sum(is.na(x))})

# 欠損値を補間(Age)
a <- tapply(d$Age, d$Title, function(x) {mean(x, na.rm=T)})
b <- d[is.na(d$Survived) & is.na(d$Age), "Title"]
table(b)
#d[is.na(d$Survived) & is.na(d$Age) & d$Title=="Master", "Age"] <- a["Master"]
#d[is.na(d$Survived) & is.na(d$Age) & d$Title=="Miss", "Age"] <- a["Miss"]
#d[is.na(d$Survived) & is.na(d$Age) & d$Title=="Mr", "Age"] <- a["Mr"]
#d[is.na(d$Survived) & is.na(d$Age) & d$Title=="Mrs", "Age"] <- a["Mrs"]
#d[is.na(d$Survived) & is.na(d$Age) & d$Title=="Ms", "Age"] <- a["Ms"]

d[is.na(d$Age) & d$Title=="Master", "Age"] <- a["Master"]
d[is.na(d$Age) & d$Title=="Miss", "Age"] <- a["Miss"]
d[is.na(d$Age) & d$Title=="Mr", "Age"] <- a["Mr"]
d[is.na(d$Age) & d$Title=="Mrs", "Age"] <- a["Mrs"]
d[is.na(d$Age) & d$Title=="Ms", "Age"] <- a["Ms"]
d[is.na(d$Age) & d$Title=="Dr", "Age"] <- a["Dr"]

# ファクター型変換
lapply(d, function(x) length(unique(x)))
d$Sex <- as.factor(d$Sex)
d$Pclass <- as.factor(d$Pclass)
d$Embarked <- as.factor(d$Embarked)
d$Title <- as.factor(d$Title)

# データ分離
d$Name <- NULL
d$Ticket <- NULL
d$Cabin <- NULL
d$Embarked <- NULL

train <- d[!is.na(d$Survived),]
test <- d[is.na(d$Survived),]

train$PassengerId <- NULL

#train <- na.omit(train)
#train <- train[!train$Fare==0,]
str(train)

# ----- データ可視化 -----

# all
table(train$Survived)
addmargins(table(train$Survived))
addmargins(round(prop.table(table(d$Survived))*100,2))
plot(as.factor(train$Survived))

# Sex
addmargins(table(d$Sex,d$Survived))
addmargins(round(prop.table(table(d$Sex, d$Survived))*100,2))
plot(as.factor(train$Sex), as.factor(train$Survived))

# Pclass
addmargins(table(d$Pclass,d$Survived))
addmargins(round(prop.table(table(d$Pclass, d$Survived))*100,2))
plot(train$Pclass, as.factor(train$Survived))

# Age
hist(d$Age)

# Fare
aggregate(Fare~Pclass,train,median)
hist(d$Fare)

#train$Survived <- as.factor(train$Survived)

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

m <- rpart(Survived~., train)
rpart.plot(m)

pred <- predict(m, test)
a <- data.frame(PassengerId=test$PassengerId, Survived=round(pred))
write.csv(a, "answer.csv", quote=FALSE, row.names=FALSE)

# ----- Random Forest -----
library(randomForest)

set.seed(1234)
m <- randomForest(as.factor(Survived)~., d=train, mtry=4, ntree=500)
m
varImpPlot(m)
plot(m)

pred <- predict(m, test)
pred
a <- data.frame(PassengerId=test$PassengerId, Survived=pred)
write.csv(a, "answer.csv", quote=FALSE, row.names=FALSE)

# tuneRF
tuneRF(train[,-1], as.factor(train[,1]), doBest=T, ntreeTry=500)

# ----- svM -----
library(e1071)
library(kernlab)

m <- ksvm(as.factor(Survived)~., d=train)
pred <- predict(m, test)
pred
a <- data.frame(PassengerId=test$PassengerId, Survived=pred)
write.csv(a, "answer.csv", quote=FALSE, row.names=FALSE)

