#Support Vector Machine

#classify the Size_Categorie using SVM

library(kernlab)
library(ggplot2)

forest <- read.csv(file.choose())
summary(forest)
attach(forest)
forest1 <- forest[,1:10]
forest1 <- cbind(forest1,size_category)
attach(forest1)

str(forest1)
#Standard deviation
sd(FFMC)
sd(DMC)
sd(DC)
sd(ISI)
sd(temp)
sd(RH)
sd(wind)
sd(rain)

#Variance
var(FFMC)
var(DMC)
var(DC)
var(ISI)
var(temp)
var(RH)
var(wind)
var(rain)

names(forest1)

#Lets Convert the Character Variable into Categorical Variable
forest1$month <- as.factor(forest1$month)
forest1$day <- as.factor(forest1$day)
forest1$size_category <- as.factor(forest1$size_category)
rowsum(is.na(forest1[,3:10]))
#Plots

ggplot(forest1) + geom_density(aes(FFMC), stat = "density") + theme_classic()

ggplot(forest1) + geom_bar(aes(DC),binwidth = 10, fill = "blue")

ggplot(forest1) + geom_bar(aes(temp), binwidth = 0.50, fill = "yellow") + theme_classic()

plot(wind, rain, type = "p", xlab = "Wind", ylab = "rain")

#Lets Divide the Data in Training and Testing Set
library(caret)
partdata <- createDataPartition(forest1$size_category, p=.70, list = F)
train1 <- forest1[partdata,]
test1 <- forest1[-partdata,]

#Lets Build the Models
model1 <- ksvm(size_category~. , data = train1, kernel = "vanilladot") #Vanilladot - Linear kernel
model1 #Training Error - 0.247934
#Lets Evaluate the Model Performance
pred1 <- predict(model1, test1[,-11])
tab1 <- table(test1[,11], pred1)
equals <- test1[,11] == pred1
prop.table(table(equals))
#Here the Accuracy of the Model is 73.37%

#lets build Models using Different Kernels and Compare their Accuracy
model2 <- ksvm(size_category~. , data =train1, kernel = "rbfdot") # rbfdot - radial basis function kernel(Gaussian) #Sigma parametr can be used
model2 #Training Error - 0.245179
pred2 <- predict(model2, test1[-11])
tab2 <- table(test1[,11], pred2)
equals2 <-test1[,11] == pred2
prop.table(table(equals2))
#Here the Accuracy of the Model is 73.37%

model3 <- ksvm(size_category~. , data =train1, kernel = "polydot", kpar = list(degree = 2, scale = 2)) #polydot - Polynomial kernel
model3 #Training Error - 0.123967
pred3 <- predict(model3, test1[-11])
tab3 <- table(test1[,11], pred3)
equals3 <-test1[,11] == pred3
prop.table(table(equals3))
#Here the Accuracy of the Model is 66.23%

model4 <- ksvm(size_category~. , data = train1, kernel = "tanhdot") #tanhdot - Hyperbolic Tangent kernel
model4 #Training Error - 0.426997
pred4 <- predict(model4,test1[-11])
tab4 <- table(test1[,11], pred4)
equals4 <- test1[,11] == pred4
prop.table(table(equals4))
#Here the Accuracy of the Model is 63.63%

model5 <- ksvm(size_category~. , data = train1, kernel = "laplacedot", kpar = list(sigma =2), C = 8) #laplacedot - Laplacian kernel
model5 #Training Error - 0.0169529
pred5 <- predict(model5, test1[-11])
tab5 <- table(test1[,11], pred5)
equals5 <- test1[,11] == pred5
prop.table(table(equals5))
#Here the Accuracy of the Model is 71.42%

model6 <- ksvm(size_category~. , data = train1, kernel = "anovadot") #anovadot - ANOVA RBF kernel - parameters(Sigma, degree)
model6 #Training Error - 0.250689
pred6 <- predict(model6,test1[-11])
tab6 <- table(test1[,11], pred6)
equals6 <- test1[,11] == pred6
prop.table(table(equals6))
#Here the Accuracy of the Model is 73.37%

model7 <- ksvm(size_category~. , data = train1, kernel = "splinedot", C=10) #splinedot - spline kernel 
model7 #Training Error - 0.30854
pred7 <- predict(model7,test1[-11])
tab7 <- table(test1[,11], pred7)
equals7 <- test1[,11] == pred7
prop.table(table(equals7))
#Here the Accuracy of the Model is 61.03%

model8 <- ksvm(size_category~. , data = train1, kernel = "besseldot", kpar = list(degree = 2), C= 15) #besseldot - String kernel - parameters(degree, sigma,order)
model8 #Training Error - 0.07438
pred8 <- predict(model8, test1[-11])
tab8 <- table(test1[,11], pred8)
equals8 <- test1[,11] == pred8
prop.table(table(equals8))
#Here the Accuracy of the Model is 64.28%

#The Laplacedot Kernel Model has the Least Training Error with a Accuracy of 71.42%