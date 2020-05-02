#Random Forest Algorithm

#Import Dataset Iris

install.packages("caret", dependencies = TRUE)
install.packages("randomForest")
library(randomForest)

model <- randomForest(iris$Species~., data = iris, ntree = 1000)

#View the Forest Result.
print(model)

#Importance of the variable - Lower Gini
print(importance(model))

#Prediction
pred<- predict(model, iris[,-5])
table(pred, iris$Species)
