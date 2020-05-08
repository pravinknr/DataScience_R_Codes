#Decision Tree
#Irisdataset is available in the R dataset
datasets::iris
data("iris")

install.packages("caret")
install.packages("C50")

library(caret)
library(C50)

inTraininglocal <- createDataPartition(iris$Species,p=.70,list = F) #.70 means 70% Partition
training<- iris[inTraininglocal,]
testing<- iris[-inTraininglocal,]

#Model Building
#Model should be built on Training Dataset
model <- C5.0(training$Species~., data = training)

#Generate the Model Summary
summary(model)

#Predict for test data

pred<- predict.C5.0(model,testing[,-5])
a<- table(testing$Species,pred) #Crating a table with the testing$species values and predicted values
sum(diag(a))/sum(a) #to find the Accuracy of the Model
plot(model)
