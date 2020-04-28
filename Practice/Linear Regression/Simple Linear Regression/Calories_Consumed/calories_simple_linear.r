#Calories Consumed

calories <- read.csv("C:\\Users\\pravi\\Desktop\\Assignment\\Simple Linear Regression\\calories_consumed.csv")
summary(calories)

sd(calories$Weight.gained..grams.)
sd(calories$Calories.Consumed)

var(calories$Weight.gained..grams.)
var(calories$Calories.Consumed)

cor(calories)


plot(calories)

calorie_model <- lm(calories$Weight.gained..grams.~. , data = calories)
summary(calorie_model)
#The R-Sqaured value for the above model is 0.8968 and the p-value is less than 0.05 that means this model will predict the output 89.68% time correct.
