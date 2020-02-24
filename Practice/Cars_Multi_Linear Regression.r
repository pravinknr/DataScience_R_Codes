#Linear Regression model with Multiple parameters
#I have a dataset named Cars


model1 <- lm(MPG~., data = Cars) #Create a linear model
summary(model1) #Summary of the model

boxplot(Cars)

#Scatter Plot Matrix
pairs(Cars)

#Corelation Mtrix
cor(Cars) 

#Multi-Collinearity
#Install Package car
install.packages("car")
library(car)
car::vif(model1) #Variance Inflation Factor. #IF the value of a parameter is greater than 20 then that parameter has a Multi-Collinearity Problem

#Diagnostic Plots: Residual Plot, QQ-Plot, Std. Residuals vs Fitted
plot(model1)

#Residual vs Regressors
residualPlots(model1)

#QQ Plots of studentized residuals
qqPlot(model1) #Quartile-Quartile Plot. #This will give the Outliers

#Deletion Diagnostics
influenceIndexPlot(model1) #Index plots of the Influence Measures. #This will give the Outliers that we should remove for better results

###Iteration 1
#Remove the 77th Observation
cars1 <- Cars[-77,]

#Create another model with new dataset i.e cars1 and execute all the above functions
model2 <- lm(MPG~., data = cars1)
car::vif(model2)
plot(model2)
residualPlots(model2)
qqPlot(model2)
influenceIndexPlot(model2)

###Iteration 2
#Remove the observation number you get in the qqPlots
cars2 <- Cars[c(-66,-77,-79,-80),]

model3 <- lm(MPG~., data = cars2)
car::vif(model3)
plot(model3)
residualPlots(model3)
qqPlot(model3)
influenceIndexPlot(model3)

#Continue this till we get no outliers