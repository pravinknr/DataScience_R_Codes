#Multi Linar regression

#Prepare a prediction model for profit of 50_startups data.
#Do transformations for getting better predictions of profit and
#make a table containing R^2 value for each prepared model.

library(ggplot2)

#Lets Import the data
startup <- read.csv(file.choose())
attach(startup)
summary(startup) #Summary of the Dataset

sum(is.na(startup)) #Gives the Total of NA values in the Dataset

cor(startup[,-4]) #Gives the Correlation Matrix of the Dataset. Accepts only Numeric variables

boxplot(startup)

pairs(startup)

names(startup)

#Standard Deviation
sd(R.D.Spend)
sd(Administration)
sd(Marketing.Spend)
sd(Profit)

#Variance
var(R.D.Spend)
var(Administration)
var(Marketing.Spend)
var(Profit)

#Plots

ggplot(startup) +geom_histogram(aes(R.D.Spend), binwidth = 6000, fill = "purple")

ggplot(startup) + geom_bar(aes(Administration),binwidth = 6000 ,fill = "black") + xlab("Administration")

plot(Marketing.Spend, xlab = "Marketing Expense", type = "p")

plot(Profit, type = "l")

#Lets Build the Model
model <- lm(Profit~. , data = startup)
summary(model)
#The R-Square value for the above model is 0.9508

pred1 <- predict(model)
model_rmse <- sqrt(mean((startup$Profit - model$fit)^2, na.rm = T))
model_rmse

plot(model)
install.packages("car")
library(car)

#Lets Find Outliers and Remove them
car::vif(model) #variation influence factor

residualPlots(model)

qqPlot(model)

influenceIndexPlot(model)
#Here we got observation number 49 and 50 as outliers so we will remove them

#Iteration 1
startup <- startup[-c(49,50),]
#Now lets make another model

model1 <- lm(Profit~. , data = startup)
summary(model1)
#The R-square value is 0.9628
car::vif(model1)
plot(model1)
residualPlots(model1)
qqPlot(model1)

#Iteration 2

startup <- startup[-c(15,16),]
model2 <- lm(Profit~. , data = startup)
summary(model2)
#The R-square value is 0.9712
car::vif(model2)
plot(model2)
residualPlots(model2)
qqPlot(model2)

#Iteration 3

startup <- startup[-c(46,44,37,35),]
model3 <- lm(Profit~. , data = startup)
summary(model3)
#The R-square value is 0.9767

r_square <- data.frame("model"=c("0 Iteration","Iteration 1", "Iteration 2","Iteration 3"),"R-square" = c(0.9508,0.9628, 0.9712, 0.9767))

#Above we have removed the Outliers and built a Multi Linear model
#Now lets Do Data Transformation and build different models nd compare their RMSE values

# 1.Log Transformation (Exponential model)

log_model <- lm(log(Profit)~., data = startup)
summary(log_model) #0.7652
log_model_rmse <- sqrt(mean((Profit - exp(log_model$fit))^2, na.rm = T))
log_model_rmse

# 2. Sqrt model
sqrt_model <- lm(sqrt(Profit)~., data = startup)
summary(sqrt_model) #0.8962
sqrt_model_rmse <- sqrt(mean((Profit - (sqrt_model$fitted.values)^2)))
sqrt_model_rmse

#Lets Build the RMSE table

rmse_table <- data.frame("Model Name" = c("Multi Linear Model", "Log_Model","sqrt_model"), "RMSE" = c(model_rmse, log_model_rmse,sqrt_model_rmse))
rmse_table

#By Comparing the Models, we Conclude that the Sqrt_model has the lowest RMSE value so it is a best Model