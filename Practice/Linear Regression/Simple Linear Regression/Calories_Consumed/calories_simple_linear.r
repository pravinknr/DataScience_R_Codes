#Calories Consumed

#Calories_consumed-> predict weight gained using calories consumed.

#Do the necessary transformations for input variables for getting better R^2 value for the model prepared.


install.packages("DataExplorer")
library(DataExplorer)

#Lets Import the Data
calories <- read.csv("E:\\Data Science\\Assignment\\Simple Linear Regression\\Calories_Consumed\\calories_consumed.csv")
attach(calories)
#Lets Perfrom EDA (Exploratory Data analysis)
head(calories) #Returns the first 6 rows of the dataset

tail(calories) #Returns the last 6 rows of the dataset

summary(calories) #Gives Summary of the dataset

sd(Weight.gained..grams.) #Gives the Standard DEviation of the Column
sd(Calories.Consumed)

var(Weight.gained..grams.) #Gives the Variance of the Column
var(Calories.Consumed)

cor(calories) #Returns the Correlation Coefficient between the columns in  matrix format

plot(calories) #Plots the Data as a simple dotplot

#Lets Build a Linear Model
calorie_model <- lm(Weight.gained..grams.~. , data = calories)
summary(calorie_model)
#The R-Sqaured value for the above model is 0.8968 and the p-value is less than 0.05 that means this model will predict the output 89.68% time correct.
#with Residual Standard Error as 111.6

#Lets do some Data Trnsformation and build the model to get better R-Square Value

calorie_sqrt_model <- lm(sqrt(Weight.gained..grams.)~. , data = calories)
summary(calorie_sqrt_model)
#Here the R-Square value is 0.8567 and the p-value is less than 0.05 with Residual standard Error as 131.5

#log transformation
calorie_log_model <-lm(log(Weight.gained..grams.)~.,data=calories)
summary(calorie_log_model)
#Here the R-square value obtained is 0.8776 amd the p-value is less than 0.05 with Residual Standard Error as 0.3314

#So Comapring all these models, we Conclude that the log Transformation model has the highest R-square value as 0.8776
#and lowest Residual Standard error as 0.3314

predict(calorie_log_model,interval="predict")
