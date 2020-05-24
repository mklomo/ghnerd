#Linear Discriminant Analysis
library(MASS)
library(ISLR)
attach(Smarket)
train <- Year < 2005
lda_fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda_fit
plot(lda_fit)


#Prediction
lda_predict <- predict(lda_fit, newdata = Smarket_2005)
names(lda_predict)
lda_class <- lda_predict$class
table(lda_predict$class, Direction_2005)
Test_Error_Rate_1 <- 1-mean(lda_predict$class == Direction_2005)


#A look at the Posterior Probabilities
lda_predict$posterior
sum(lda_predict$posterior[,1] >= 0.5)
sum(lda_predict$posterior[,1] < 0.5)
lda_class[1:20]

#Another Criterion
sum(lda_predict$posterior[,1] >= 0.9)




#Quadratic Discriminant Analysis
qda_fit <- qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda_fit

#Prediction with QDA
qda_pred <- predict(qda_fit, newdata = Smarket_2005)
qda_class <- qda_pred$class
table(qda_class, Direction_2005)
Test_Error_Rate <- 1 - mean(qda_class == Direction_2005)
Test_Error_Rate_2








