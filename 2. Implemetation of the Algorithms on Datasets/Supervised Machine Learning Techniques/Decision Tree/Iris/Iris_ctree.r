#Decision Tree

#we Build a Decision Tree for the Iris Data using ctree() in "party" package

datasets::iris
data("iris")

library(caret)
library(party)

summary(iris)
#The column Species is already in the form of a categorical Variable

names(iris) #Gives the Column Names

dim(iris) #Gives the Number of Rows and Columns

str(iris) #Gives the Entire Structure of the Data

#Standard Deviation
sd(iris$Sepal.Length)
sd(iris$Sepal.Width)
sd(iris$Petal.Length)
sd(iris$Petal.Width)

#Variance
var(iris$Sepal.Length)
var(iris$Sepal.Width)
var(iris$Petal.Length)
var(iris$Petal.Width)

boxplot(iris) #Displays the Boxplot for every column in the Dataset

pairs(iris) #Using Pairs Function we get plot for each and every Column

#Lets Divide the Data for building the model
inTraininglocal <- createDataPartition(iris$Species,p=.70,list = F) #.70 means 70% Partition
training<- iris[inTraininglocal,]
testing<- iris[-inTraininglocal,]
attach(iris)

#Lets build the model
model <- ctree(Species~. , data = training) #ctree() is available in "party" library
summary(model)

#Lets Predict the data for the test Dataset
pred <- predict(model,testing[,-5])
pred
iris_tab <- table(testing$Species, pred)

acc <- sum(diag(iris_tab))/sum(iris_tab) #Gives the Accuracy of the Model
acc

CrossTable(testing$Species, pred)

plot(model) #This Plots the Decision Tree
#The Decision Tree says that the left group has 35 flowers whose Petal.Length is less than equal to 1.9
#The second group consists of the flowers with Petal.Length greater than 1.9 and Petal.Width less than equal to 1.7 and Petal.Length less than equal to 4.8
#The Third group consists of the flowers with Petal.Length greater than 1.9 and Petal.Width less than equal to 1.7 and Petal.Length greater than 4.8 
#The Fourth group consists of the flowers with Petal.Length greater than 1.9 and Petal.Width Greater than 1.7.

