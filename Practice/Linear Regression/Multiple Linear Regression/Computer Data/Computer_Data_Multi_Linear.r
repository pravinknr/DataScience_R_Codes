#Multi Linear Regression

#Predict Price of the computer

#Lets Import the Dataset
pc <- read.csv(file.choose())
pc <- pc[,-1]
head(pc)
tail(pc)
summary(pc) #Gives the Summary of the Dataset

#Standard Deviation
sd(pc$price)
sd(pc$speed)
sd(pc$hd)
sd(pc$ram)
sd(pc$screen)

#Variance
var(pc$price)
var(pc$speed)
var(pc$hd)
var(pc$ram)
var(pc$screen)

windows()
pairs(pc)

#We have to convert the columns cd, multi, premium as factors
pc$cd <- as.factor(pc$cd)
pc$multi<- as.factor(pc$multi)
pc$premium <- as.factor(pc$premium)

#Lets Build a Multi Linear Model
model <- lm(pc$price~. , data = pc)
summary(model)
#The r-Square value of this model is 0.7778 with Residual Standard Error as 275.3

#Lets do some Data Transformation and build a better Model

#Log Transformation
log_model <- lm(log(pc$price)~. , data = pc)
summary(log_model)
#Here we got the R-Square value as 0.7832 with the least Residual Standard Error as 0.1202