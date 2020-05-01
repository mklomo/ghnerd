#K Nearest Neighbours
library(ISLR)
library(dplyr)
library(tidyverse)
library(tidyr)
names(Smarket)
dim(Smarket)

#Selecting lag1, lag2, Year and Direction 
knn_data <- data.frame(Smarket$Direction, Smarket$Lag1, Smarket$Lag2, Smarket$Year)
head(knn_data)

#Randomly Sample which rows go into training set
Train <- knn_data$Smarket.Year<2005
train_data <- knn_data[Train, ]
Smarket_2005 <- knn_data[!Train, ]
dim(Smarket_2005)

#Actual Train Predictors
Train_X <- subset(train_data, select = c(Smarket.Lag1, Smarket.Lag2))
Test_X <- subset(Smarket_2005, select = c(Smarket.Lag1, Smarket.Lag2))
Train_Direction <- train_data$Smarket.Direction[Train]
Test_Direction <- Smarket_2005$Smarket.Direction

#lets loop through and see what the missclassification rate is for different k-values
set.seed(1)
library(class)
for(k in 1:30){ 
  print(k)
  predicted_labels <- knn(Train_X, Test_X, Train_Direction, k)
  #We are using the R function knn()
  num_incorrect_labels <- sum(predicted_labels != Smarket_2005$Smarket.Direction)
  misclassification_rate <- num_incorrect_labels/nrow(Smarket_2005)
  Correct_Classification_rate <- 1-misclassification_rate
  print(Correct_Classification_rate)
  }














