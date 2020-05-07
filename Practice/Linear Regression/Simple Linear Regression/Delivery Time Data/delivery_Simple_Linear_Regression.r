#Delivery Time

#Delivery_time -> Predict delivery time using sorting time 

#Do the necessary transformations for input variables for getting better R^2 value for the model prepared.

#Lets Import the data
delivery <- read.csv("E:\\Data Science\\Assignment\\Simple Linear Regression\\Delivery Time Data\\delivery_time.csv")
attach(delivery)
head(delivery)
tail(delivery)
summary(delivery) #Gives the Summary

sd(Delivery.Time) #Standard Deviation
var(Delivery.Time) #Variance
sd(Sorting.Time)
var(Sorting.Time)

cor(delivery) #Gives the Correlation Coefficients as a matrix

plot(delivery)

#Lets build a Linear Model on the data
model <- lm(Delivery.Time~., data = delivery)

plot(model)

summary(model)
#Here we can see that the R-Squared value is 0.6823 and the P-value is less than 0.05 and Residual Standard Error as 2.935

#Lets increase the R-Square value by data Transformation
#Squareroot Transformation
sqrt_model<-lm(sqrt(Delivery.Time)~.,delivery)
summary(sqrt_model)
#Here we got the R-sqaure value as 0.6958 with Residual Standard Error as 2.872


#Log Transformation
log_model<-lm(log(Delivery.Time)~.,data=delivery)
summary(log_model)
#Here we get R-Squared value as 0.7109 and Residual Standard Error as 0.1755 

#Inorder to find the points that affect the model, use library mvinfluence
install.packages("mvinfluence")
library(mvinfluence)

influenceIndexPlot(model)
#Using the plots we can see that the points 5,9,21,19 are causing problems, lets remove these points and build the model
delivery1 <- delivery[-c(5,9,21),]

#Lets Build a linear Model on the new data
model1 <- lm(Delivery.Time~., data = delivery1)
plot(model1)
summary(model1)
#Now we got the R-Squarred value as 0.8332 which means this will give the output 83.32% time correct
#with Residual Standard Error as 1.839

#Here we conclude that Log Model has better R-square value as 0.7109 and Residual Standard Error as 0.1755 but we
#get Even better R-Square value by removing the Points that Influence the model which reduces the Accuracy.
#The best Model we got is model1 with R-Square value as0.8332 with Residual Standard Error as 1.839