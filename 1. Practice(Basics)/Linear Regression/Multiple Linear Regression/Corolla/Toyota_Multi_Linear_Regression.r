#Multiple Linear Regression
#Here i have a dataset of toyota
#In the below Example, we do Iterations to get better R-Square Value. Each Iteration will give better R-Square Results

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

### Iteration 5

toyota1 <- toyota[-c(193,192,81,961,959,222,602,655,652,524,522),]
model2 <- lm(Price~., data = toyota1)
summary(model2)
car::vif(model2)
plot(model2)
residualPlots(model2)
qqPlot(model2)
influenceIndexPlot(model2)

### Iteration 6

toyota1 <- toyota[-c(172,171,193,192,81,961,959,222,602,655,652,524,522),]
model2 <- lm(Price~., data = toyota1)
summary(model2)
car::vif(model2)
plot(model2)
residualPlots(model2)
qqPlot(model2)
influenceIndexPlot(model2)


###Iteration 7

toyota1 <- toyota[-c(394,388,172,171,193,192,81,961,959,222,602,655,652,524,522),]
model2 <- lm(Price~., data = toyota1)
summary(model2)
car::vif(model2)
plot(model2)
residualPlots(model2)
qqPlot(model2)
influenceIndexPlot(model2)

###Iteration 8

toyota1 <- toyota[-c(403,395,394,388,172,171,193,192,81,961,959,222,602,655,652,524,522),]
model2 <- lm(Price~., data = toyota1)
summary(model2)
car::vif(model2)
plot(model2)
residualPlots(model2)
qqPlot(model2)
influenceIndexPlot(model2)

###Iteration 9

toyota1 <- toyota[-c(1436,1419,403,395,394,388,172,171,193,192,81,961,959,222,602,655,652,524,522),]
model2 <- lm(Price~., data = toyota1)
summary(model2)
car::vif(model2)
plot(model2)
residualPlots(model2)
qqPlot(model2)
influenceIndexPlot(model2)

###Iteration 10

toyota1 <- toyota[-c(1059,1042,1436,1419,403,395,394,388,172,171,193,192,81,961,959,222,602,655,652,524,522),]
model2 <- lm(Price~., data = toyota1)
summary(model2)
car::vif(model2)
plot(model2)
residualPlots(model2)
qqPlot(model2)
influenceIndexPlot(model2)

###Iteration 11

toyota1 <- toyota[-c(148,147,1059,1042,1436,1419,403,395,394,388,172,171,193,192,81,961,959,222,602,655,652,524,522),]
model2 <- lm(Price~., data = toyota1)
summary(model2)
car::vif(model2)
plot(model2)
residualPlots(model2)
qqPlot(model2)
influenceIndexPlot(model2)



