#Naive Bayes classifier

#Prepare a classification model using Naive Bayes for salary data.

library(e1071)
library(caret)
library(pROC)
library(ggplot2)

#We have 2 datasets. the Salary_train data and Salary_Test data. 
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

salary_train$Salary <- as.factor(ifelse(salary_train$Salary == " >50K", "High", "Low"))
salary_test$Salary <- as.factor(ifelse(salary_test$Salary == " >50K", "High", "Low"))

str(salary_train)
str(salary_test)

salary_train$age <- scale(salary_train$age)
salary_train$educationno <- scale(salary_train$educationno)
salary_train$capitalgain <- scale(salary_train$capitalgain)
salary_train$capitalloss<- scale(salary_train$capitalloss)
salary_train$hoursperweek <- scale(salary_train$hoursperweek)

salary_test$age <- scale(salary_test$age)
salary_test$educationno <- scale(salary_test$educationno)
salary_test$capitalgain <- scale(salary_test$capitalgain)
salary_test$capitalloss<- scale(salary_test$capitalloss)
salary_test$hoursperweek <- scale(salary_test$hoursperweek)

#Plots
ggplot(salary_train) + geom_bar(aes(age)) + ggtitle("People COunt w.r.t Age") #The data is Skewed at the Right

ggplot(salary_train) + geom_bar(aes(sex), color = "blue") + ggtitle("Count of Male Female Population")

ggplot(salary_train) + geom_histogram(aes(race), stat = "count") #Insatance of White People is more than the Other Race people

ggplot(salary_train, aes(age,capitalgain)) + geom_line()

#Lets Build the Naive Bayes Classifier
salary_classifier <- naiveBayes(Salary~. , data = salary_train)
salary_classifier
summary(salary_classifier)
pred <- predict(salary_classifier, salary_test[,-14])
conf <- confusionMatrix(salary_test$Salary, pred, mode = "everything")
conf
acc <-  conf$overall[1]
precision <-  conf$byClass[5]
recall <-  conf$byClass[6]
f1score <-  conf$byClass[7]
area <- roc(salary_test$Salary,predictor = factor(pred, ordered = TRUE))
plot(area)
auc <- area$auc
auc

salary_classifier1 <- naiveBayes(Salary~. , data = salary_train, laplace = 1)
salary_classifier1
summary(salary_classifier1)
pred1 <- predict(salary_classifier1, salary_test[,-14])
conf1 <- confusionMatrix(salary_test$Salary, pred1, mode = "everything")
conf1
acc1 <-  conf1$overall[1]
precision1 <-  conf1$byClass[5]
recall1 <-  conf1$byClass[6]
f1score1 <-  conf1$byClass[7]
area1 <- roc(salary_test$Salary,predictor = factor(pred1, ordered = TRUE))
plot(area1)
auc1 <- area1$auc
auc1

#Lets Do some Feature Selection and build the model
salary_classifier2 <- naiveBayes(Salary~age + educationno + occupation + sex, data = salary_train )
salary_classifier2
summary(salary_classifier2)
pred2 <- predict(salary_classifier2, salary_test[,-14])
conf2 <- confusionMatrix(salary_test$Salary, pred2, mode = "everything")
conf2
acc2 <-  conf2$overall[1]
precision2 <-  conf2$byClass[5]
recall2 <-  conf2$byClass[6]
f1score2 <-  conf2$byClass[7]
area2 <- roc(salary_test$Salary,predictor = factor(pred2, ordered = TRUE))
plot(area2)
auc1 <- area2$auc
auc1

salary_classifier3 <- naiveBayes(Salary~age + educationno + occupation + sex + capitalgain, data = salary_train )
salary_classifier3
summary(salary_classifier3)
pred3 <- predict(salary_classifier3, salary_test[,-14])
conf3 <- confusionMatrix(salary_test$Salary, pred3, mode = "everything")
conf3
acc3 <-  conf3$overall[1]
precision3 <-  conf3$byClass[5]
recall3 <-  conf3$byClass[6]
f1score3 <-  conf3$byClass[7]
area3 <- roc(salary_test$Salary,predictor = factor(pred3, ordered = TRUE))
plot(area3)
auc3 <- area3$auc
auc3

salary_classifier4 <- naiveBayes(Salary~age + educationno + occupation + sex + capitalgain + capitalloss + hoursperweek, data = salary_train )
salary_classifier4
summary(salary_classifier4)
pred4 <- predict(salary_classifier4, salary_test[,-14])
conf4 <- confusionMatrix(salary_test$Salary, pred4, mode = "everything")
conf4
acc4 <-  conf4$overall[1]
precision4 <-  conf4$byClass[5]
recall4 <-  conf4$byClass[6]
f1score4 <-  conf4$byClass[7]
area4 <- roc(salary_test$Salary,predictor = factor(pred4, ordered = TRUE))
plot(area4)
auc4 <- area4$auc
auc4

salary_classifier5 <- naiveBayes(Salary~age + educationno + occupation + capitalgain+ hoursperweek, data = salary_train )
salary_classifier5
summary(salary_classifier5)
pred5 <- predict(salary_classifier5, salary_test[,-14])
conf5 <- confusionMatrix(salary_test$Salary, pred5, mode = "everything")
conf5
acc5 <-  conf5$overall[1]
precision5 <-  conf5$byClass[5]
recall5 <-  conf5$byClass[6]
f1score5 <-  conf5$byClass[7]
area5 <- roc(salary_test$Salary,predictor = factor(pred5, ordered = TRUE))
plot(area5)
auc5 <- area5$auc
auc5

salary_classifier6 <- naiveBayes(Salary~age + educationno + capitalgain+ hoursperweek, data = salary_train )
salary_classifier6
summary(salary_classifier6)
pred6 <- predict(salary_classifier6, salary_test[,-14])
conf6 <- confusionMatrix(salary_test$Salary, pred6, mode = "everything")
conf6
acc6 <-  conf6$overall[1]
precision6 <-  conf6$byClass[5]
recall6 <-  conf6$byClass[6]
f1score6 <-  conf6$byClass[7]
area6 <- roc(salary_test$Salary,predictor = factor(pred6, ordered = TRUE))
plot(area6)
auc6 <- area6$auc
auc6

salary_classifier7 <- naiveBayes(Salary~age + native + race+ hoursperweek, data = salary_train )
salary_classifier7
summary(salary_classifier7)
pred7 <- predict(salary_classifier7, salary_test[,-14])
conf7 <- confusionMatrix(salary_test$Salary, pred7, mode = "everything")
conf7
acc7 <-  conf7$overall[1]
precision7 <-  conf7$byClass[5]
recall7 <-  conf7$byClass[6]
f1score7 <-  conf7$byClass[7]
area7 <- roc(salary_test$Salary,predictor = factor(pred7, ordered = TRUE))
plot(area7)
auc7 <- area7$auc
auc7

#The salary_classifier model has the best Accuracy and f1score compared to rest of the Models.
