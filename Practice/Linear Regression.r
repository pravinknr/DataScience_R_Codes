#Linear Regression in R'

#Prediction using Linear Regression in R


#Create a model for the dataset

#The parameter on Y axis is dependent on the prameter of X axis

# modelname <- lm(y-Axis~X-Axis, data = DatasetName)

#predictionName <- predict(modelname, newdata = data.frame(X-Axis = Value we want to predict for))

#In the Below Example i have imported the Dataset named NewspaperData
model <- lm(sunday~daily, data = NewspaperData) # here lm stands for linear model, Sunday is Y Axis data and Daily is X Axis data , data is the dataset that we want to use
summary(model) #To get the summary of the data

pred <- predict(model, newdata = data.frame(daily=200)) #Create a prediction object using predict function where model is the linear model name, newdata is the data that we want to add in the data model and predict for by using data.frame function by specifying the column name in the brackets
pred

#Predict sunday circulations for 300 Daily circulations

pred1 <- predict(model, newdata = data.frame(daily=300))
pred1

#Predict sunday circulations for 500 daily circulations
pred2 <- predict(model, newdata = data.frame(daily=500))
pred2

#Lets predict the data for some more values

library("lattice")
#Visualization
dotplot(NewspaperData$daily, main = "DotPlot for Daily circulations")

dotplot(NewspaperData$sunday, main = "DotPlot for Sunday Circulations")

boxplot(NewspaperData$daily, col = "blue")

boxplot(NewspaperData$sunday, col = "cyan")

#Lets predict the sunday circulations for (500,460,290,320) Daily Circulations

newdata1 <- data.frame(daily = c(500,460,290,320))
pred3 <- predict(model, newdata = newdata1)
pred3

pred4<- predict(model) #This will predict the Sunday Values for all the Daily values of the model

finaldata1 <- data.frame(NewspaperData,pred4, "Errors" = NewspaperData$sunday - pred4) #this will subtract the predicted value with the Actual values of the dataset
finaldata1
