# Support Vector Machine(SVM)

# Prepare a classification model using SVM for salary data 

library(kernlab)
library(ggplot2)
library(dplyr)

#Lets Import the Data
salary_train <- read.csv(file.choose()) #This data is used to train the Model
attach(salary_train)

salary_test <- read.csv(file.choose()) #This data is used to Test the Model
attach(salary_test)

#Lets First Analyze the Salary_train Data
summary(salary_train)
str(salary_train)
names(salary_train)
#Here there are many variables that we need to convert into an Ctaegorical Vriable. Lets Convert them

salary_train$workclass <- as.factor(salary_train$workclass)
salary_train$education <-as.factor(salary_train$education)
salary_train$maritalstatus <- as.factor(salary_train$maritalstatus)
salary_train$occupation <- as.factor(salary_train$occupation)
salary_train$relationship <- as.factor(salary_train$relationship)
salary_train$race <- as.factor(salary_train$race)
salary_train$sex <- as.factor(salary_train$sex)
salary_train$native <- as.factor(salary_train$native)
salary_train$Salary <- as.factor(salary_train$Salary)

summary(salary_test)
str(salary_test)
names(salary_test)
#Lets Apply the same Transformation to the salary_test Data
salary_test$workclass <- as.factor(salary_test$workclass)
salary_test$education <-as.factor(salary_test$education)
salary_test$maritalstatus <- as.factor(salary_test$maritalstatus)
salary_test$occupation <- as.factor(salary_test$occupation)
salary_test$relationship <- as.factor(salary_test$relationship)
salary_test$race <- as.factor(salary_test$race)
salary_test$sex <- as.factor(salary_test$sex)
salary_test$native <- as.factor(salary_test$native)
salary_test$Salary <- as.factor(salary_test$Salary)

#Plotting the Data
plot()

ggplot(salary_train, aes(x = sex,y = educationno)) + geom_bar(stat = "Identity") + theme_bw()

#Lets build the SVM model with Kernel vanilladot
model1 <- ksvm(Salary~. , data = salary_train, kernel = "vanilladot") #Vanilladot - Linear kernel
model1
#Lets Evaluate the Model Performance
pred1 <- predict(model1, salary_test[-14])
tab1 <- table(salary_test[,14], pred1)
equals <- salary_test[,14] == pred1
prop.table(table(equals))
#Here the Accuracy of the Model is 84.62%

#lets build Models using Different Kernels and Compare their Accuracy
model2 <- ksvm(Salary~. , data = salary_train, kernel = "rbfdot") # rbfdot - radial basis function kernel(Gaussian) #Sigma parametr can be used
model2 #Training Error - 0.1378
pred2 <- predict(model2, salary_test[-14])
tab2 <- table(salary_test[,14], pred2)
equals2 <- salary_test[,14] == pred2
prop.table(table(equals2))
#Here the Accuracy of the Model is 85.43%

model3 <- ksvm(Salary~. , data = salary_train, kernel = "polydot", kpar = list(degree = 2, scale = 1)) #polydot - Polynomial kernel
model3 #Training Error - 0.1319
pred3 <- predict(model3, salary_test[-14])
tab3 <- table(salary_test[,14], pred3)
equals3 <- salary_test[,14] == pred3
prop.table(table(equals3))
#Here the Accuracy of the Model is 84.44%

model4 <- ksvm(Salary~. , data = salary_train, kernel = "tanhdot") #tanhdot - Hyperbolic Tangent kernel
model4 #Training Error - 0.335566
pred4 <- predict(model4, salary_test[-14])
tab4 <- table(salary_test[,14], pred4)
equals4 <- salary_test[,14] == pred4
prop.table(table(equals4))
#Here the Accuracy of the Model is 66.38%

model5 <- ksvm(Salary~. , data = salary_train, kernel = "laplacedot", kpar = list(sigma =1.5), C = 1.5) #laplacedot - Laplacian kernel
model5 #Training Error - 0.053844
pred5 <- predict(model5, salary_test[-14])
tab5 <- table(salary_test[,14], pred5)
equals5 <- salary_test[,14] == pred5
prop.table(table(equals5))
#Here the Accuracy of the Model is 84.35%

model6 <- ksvm(Salary~. , data = salary_train, kernel = "anovadot") #anovadot - ANOVA RBF kernel - parameters(Sigma, degree)
model6 #Training Error - 0.218295
pred6 <- predict(model6, salary_test[-14])
tab6 <- table(salary_test[,14], pred6)
equals6 <- salary_test[,14] == pred6
prop.table(table(equals6))
#Here the Accuracy of the Model is 78.26%

model7 <- ksvm(Salary~. , data = salary_train, kernel = "splinedot", C=10) #splinedot - spline kernel 
model7 #Training Error - 0.383243
pred7 <- predict(model7, salary_test[-14])
tab7 <- table(salary_test[,14], pred7)
equals7 <- salary_test[,14] == pred7
prop.table(table(equals7))
#Here the Accuracy of the Model is 75.16%

model8 <- ksvm(Salary~. , data = salary_train, kernel = "besseldot", kpar = list(degree = 2), C= 15) #besseldot - String kernel - parameters(degree, sigma,order)
model8
pred8 <- predict(model8, salary_test[-14])
tab8 <- table(salary_test[,14], pred8)
equals8 <- salary_test[,14] == pred8
prop.table(table(equals8))
#Here the Accuracy of the Model is 77.22%

#So among all these Models, Laplacedot kernel Model has the Lowest Training Error with an Accuracy of 84.35%
#Hence, Laplacedot kernel Model is the best Model for Prediction for this Dataset.