# Kaggle:
#   Titanic

library(randomForest)
library(ggplot2)

setwd("d:/kaggle/titanic")

d <- read.csv("train.csv")
test <- read.csv("test.csv")
str(test)
summary(d)

# ----- decision tree -----
library(rpart)
library(rpart.plot)

d$Name <- NULL
d$Ticket <- NULL
d$Cabin <- NULL

m <- rpart(Survived~., d)
rpart.plot(m)

pred <- predict(m, test)
a <- data.frame(PassengerId=test$PassengerId, Survived=round(pred))
write.csv(a, "answer.csv", quote=FALSE, row.names=FALSE)
