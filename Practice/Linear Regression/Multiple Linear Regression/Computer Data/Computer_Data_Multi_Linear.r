#Multi Linear Regression

#Predict Price of the computer

library(ggplot2)

#Lets Import the Dataset
pc <- read.csv(file.choose())

pc <- pc[,-1] #Here we Remove the 1st Column as it contains Id values
attach(pc)
head(pc)

str(pc)

cor(pc[,-c(6,7,8,10)]) #Accepts Only Numeric Variable

tail(pc)

dim(pc) #Returns the total Number of rows and Column

names(pc) #Gives the Column names of the Dataset

summary(pc) #Gives the Summary of the Dataset

#Standard Deviation
sd(price)
sd(speed)
sd(hd)
sd(ram)
sd(screen)

#Variance
var(price)
var(speed)
var(hd)
var(ram)
var(screen)

windows()
pairs(pc)

#We have to convert the columns cd, multi, premium as factors
cd <- as.factor(cd)
multi<- as.factor(multi)
premium <- as.factor(premium)

#Plots
ggplot(pc) +geom_histogram(aes(hd), binwidth =50,  fill = "purple") #the data in this column is right skewed

ggplot(pc) + geom_bar(aes(trend),binwidth = 0.5 ,fill = "black") + xlab("Trend") #This data is Normally Distributed

ggplot(pc) + geom_bar(aes(ram),binwidth = 0.9 ,fill = "black") + xlab("ram")

ggplot(pc) + geom_histogram(aes(price), fill = "green", binwidth = 50) + xlab("Price")

#Lets Build a Multi Linear Model
model <- lm(price~. , data = pc)
summary(model)
#The r-Square value of this model is 0.7778 with Residual Standard Error as 275.3
model_rmse <- sqrt(mean((Profit - model$fit)^2, na.rm = T))
model_rmse

#Lets do some Data Transformation and build a better Model

#Log Transformation
log_model <- lm(log(price)~. , data = pc)
summary(log_model)
#Here we got the R-Square value as 0.7832 with the least Residual Standard Error as 0.1202
log_model_rmse <- sqrt(mean((price - exp(log_model$fit))^2, na.rm = T))
log_model_rmse

# SQRT model
sqrt_model <- lm(price~. , data = pc)
summary(sqrt_model)
sqrt_model_rmse <- sqrt(mean((price - sqrt_model$fitted.values)^2))
sqrt_model_rmse

#Lets make RMSE table
rmse_table <- data.frame("Model Name" = c("Multi Linear Model", "Log_Model","sqrt_model"), "RMSE" = c(model_rmse, log_model_rmse,sqrt_model_rmse))
rmse_table

#Here we Conclude that Log_model has the Least RMSE value so it can be used for Prediction

