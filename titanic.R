# Kaggle:
#   Titanic

library(randomForest)
library(ggplot2)

setwd("d:/kaggle/titanic")

train <- read.csv("train.csv")
test <- read.csv("test.csv")
str(test)
summary(d)


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

sapply(test, function(y) sum(is.na(y)))
apply(is.na(test), 2, sum)

d["Age"].fillna(d.Age.median(), inplace=True)

pred <- predict(m, test) 

