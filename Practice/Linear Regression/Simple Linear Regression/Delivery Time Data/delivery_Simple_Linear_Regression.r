delivery <- read.csv("C:\\Users\\pravi\\Desktop\\Assignment\\Simple Linear Regression\\delivery_time.csv")

summary(delivery)

sd(delivery$Delivery.Time)
var(delivery$Delivery.Time)
sd(delivery$Sorting.Time)
var(delivery$Sorting.Time)
model <- lm(delivery$Delivery.Time~., data = delivery)

plot(delivery)
plot(model)

summary(model)
#Here we can see that the R-Squared value is 0.6823 and the P-value is less than 0.05

#Lets increase the R-Square value by data Transformation
#Inorder to find the points that affect the model, use library mvinfluence
install.packages("mvinfluence")
library(mvinfluence)

influenceIndexPlot(model)
#Using the plots we can see that the points 5,9,21,19 are causing problems, lets remove these points and build the model
delivery1 <- delivery[c(-5,-9,-21),]
model1 <- lm(delivery1$Delivery.Time~., data = delivery1)
plot(model1)
summary(model1)
#Now we got the R-Squarred value as 0.8332 which means this will give the output 83.32% time correct