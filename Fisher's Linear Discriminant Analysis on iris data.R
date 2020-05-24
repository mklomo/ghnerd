#lda on Fishers iris Dataset
data("iris")
attach(iris)
head(iris)


#Summary of iris dataset
summary(iris)

#using lda to predict species type based on properties
train <- sample(1:nrow(iris), nrow(iris) * 0.75, replace = FALSE)
iris_test <- iris[-train,]
species_test <- iris_test$Species

lda_fit <- lda(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris, subset = train)
lda_fit
plot(lda_fit)
lda_pred <- predict(lda_fit, newdata = iris_test)
lda_class <- lda_pred$class
table(lda_class, species_test)
mean(lda_class == species_test)








