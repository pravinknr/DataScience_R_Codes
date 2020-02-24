#Multiple Linear Regression
#Here i have a dataset of toyota

#import the dataset
toyota <- Toyoto_Corrola[,c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Weight")]

#Create a model Using the dataset
model1 <- lm(Price~., data = toyota)
summary(model1)

library(car)
car::vif(model1)
plot(model1)
residualPlots(model1)
qqPlot(model1)
influenceIndexPlot(model1)

###Iteration 1

toyota1 <- toyota[c(-222,-602),]
model2 <- lm(Price~., data = toyota1)
summary(model2)
car::vif(model2)
plot(model2)
residualPlots(model2)
qqPlot(model2)
influenceIndexPlot(model2)

###Iteration 2

toyota1 <- toyota[-c(81,961,959,222,602),]
model2 <- lm(Price~., data = toyota1)
summary(model2)
car::vif(model2)
plot(model2)
residualPlots(model2)
qqPlot(model2)
influenceIndexPlot(model2)

###Iteration 3

toyota1 <- toyota[-c(81,961,959,222,602,655,652),]
model2 <- lm(Price~., data = toyota1)
summary(model2)
car::vif(model2)
plot(model2)
residualPlots(model2)
qqPlot(model2)
influenceIndexPlot(model2)

###Iteration 4

toyota1 <- toyota[-c(81,961,959,222,602,655,652,524,522),]
model2 <- lm(Price~., data = toyota1)
summary(model2)
car::vif(model2)
plot(model2)
residualPlots(model2)
qqPlot(model2)
influenceIndexPlot(model2)

