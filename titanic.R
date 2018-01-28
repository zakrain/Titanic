# Kaggle:
#   Titanic

library(ggplot2)

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

d <- na.omit(d)
d$Survived <- as.factor(d$Survived)
m <- randomForest(Survived~., d)

d["Age"].fillna(d.Age.median(), inplace=True)

pred <- predict(m, test) 

