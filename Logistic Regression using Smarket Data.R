library(ISLR)


#About the data
names(Smarket)
dim(Smarket)
summary(Smarket)


#structure of the data set
str(Smarket)

#Scatterplot Matrix
pairs(Smarket)

#Correlation Matrix
cor(Smarket[,-9])

#Boxplot of Volume against Years
Smarket$Year <- as.factor(Smarket$Year)
library(ggplot2)
ggplot(Smarket, aes(x = Year, y = Volume))+
  geom_boxplot()+
  xlab("Year")+
  ylab("Volume")+
  ggtitle("Correlation between Volume and Year in Smarket Data")


#Fitting a Logistic Regression
attach(Smarket)
glm_fits <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial)
summary(glm_fits)
coef(glm_fits)

#Predicting probabilities
glm_probs <- predict(glm_fits, type = "response")
glm_probs[1:20]
contrasts(Direction)

#Creating a Confusion Matrix
glm_pred <- rep("Down", 1250)
glm_pred[glm_probs > 0.5] <- "Up"
table(glm_pred, Direction)

#Training Error Rate
Correct_Classification_rate <- mean(glm_pred == Direction)
Training_Error_Rate <- 1 - Correct_Classification_rate


#Splitting Data into Pre-2004 and 2--15
attach(Smarket)
train <- (Year < 2005)
Smarket_2005 <- Smarket[!train,]
Direction_2005 <- Direction[!train]

#Fitting a Logistic regression model for training data
glm_fits_2 <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial, subset = train)

#Using Fitted Model on Testing Data
glm_probs_2 <- predict(glm_fits_2, Smarket_2005, type = "response")  

##Reuslts
glm_pred <- rep("Down", nrow(Smarket_2005))
glm_pred[glm_probs_2 > 0.5] <- "Up"

#Confusion Matrix
table(glm_pred, Direction_2005)
Correct_Classification_rate <- mean(glm_pred == Direction_2005)
Training_Error_Rate <- 1 - Correct_Classification_rate


##Fitting a better Logistics Reg Model using lag1 and lag2 only
glm_fits_3 <- glm(Direction ~ Lag1 + Lag2, family = binomial, subset = train)
glm_probs_2 <- predict(glm_fits_3, Smarket_2005, type = "response")
glm_pred_2 <- rep("Down", 252)
glm_pred_2[glm_probs_2 > 0.5] <- "Up"
table(Direction_2005, glm_pred_2)
Correct_Classification_rate_2 <- mean(glm_pred_2 == Direction_2005)
Test_Error_Rate <- 1-Correct_Classification_rate_2 


##Fitting a better Logistics Reg Model using lag1, lag2 and Volume only
glm_fits_4 <- glm(Direction ~ Lag1 + Lag2 + Volume, family = binomial, subset = train)
glm_probs_3 <- predict(glm_fits_4, Smarket_2005, type = "response")
glm_pred_3 <- rep("Down", 252)
glm_pred_3[glm_probs_3 > 0.5] <- "Up"
table(Direction_2005, glm_pred_3)
Correct_Classification_rate_3 <- mean(glm_pred_3 == Direction_2005)
Test_Error_Rate_2 <- 1-Correct_Classification_rate_3 

#Note: Model with Lag1 and Lag 2 only outperforms model with lag1, lag2 and Volume

#Predicting a direction given values of lag1 and lag2
predict(glm_fits_3, newdata = data.frame(Lag1 = c(1.2, 1.5, 1.35), Lag2 = c(1.1, -0.8, 1.2)), type = "response")





