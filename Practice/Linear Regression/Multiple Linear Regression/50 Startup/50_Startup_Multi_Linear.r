#Multi Linar regression

#Prepare a prediction model for profit of 50_startups data.
#Do transformations for getting better predictions of profit and
#make a table containing R^2 value for each prepared model.

#Lets Import the data
startup <- read.csv(file.choose())

summary(startup)

cor(startup[,-4])

boxplot(startup)

pairs(startup)

model <- lm(startup$Profit~. , data = startup)
summary(model)
#The R-Square value for the above model is 0.9508

plot(model)
install.packages("car")
library(car)

car::vif(model) #variation influence factor

residualPlots(model)

qqPlot(model)

influenceIndexPlot(model)
#Here we got observation number 49 and 50 as outliers so we will remove them

#Iteration 1
startup <- startup[c(-49,-50),]
#Now lets make another model

model1 <- lm(startup$Profit~. , data = startup)
summary(model1)
#The R-square value is 0.9628
car::vif(model1)
plot(model1)
residualPlots(model1)
qqPlot(model1)

#Iteration 2

startup <- startup[c(-15,-16),]
model2 <- lm(startup$Profit~. , data = startup)
summary(model2)
#The R-square value is 0.9712
car::vif(model2)
plot(model2)
residualPlots(model2)
qqPlot(model2)

#Iteration 3

startup <- startup[c(-46,-44,-37,-35),]
model3 <- lm(startup$Profit~. , data = startup)
summary(model3)
#The R-square value is 0.9767
car::vif(model3)
plot(model3)
residualPlots(model3)
qqPlot(model3)

r_square <- data.frame("R-square" = c(0.9508,0.9628, 0.9712, 0.9767))