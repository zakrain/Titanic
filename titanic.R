# Kaggle:
#   Titanic

library(randomForest)
library(ggplot2)

setwd("d:/kaggle/titanic")

d <- read.csv("train.csv")
str(d)
summary(d)

test <- read.csv("test.csv")
