#Logistic Regression

#Classify whether application accepted or not using Logistic regression
#Import the creditcard.csv file

creditcard <- read.csv(file.choose())
summary(creditcard)

#We dont want the first column so we will remove it
creditcard1 <- creditcard[,-1]

#Standard Deviation
sd(creditcard1$reports)
sd(creditcard1$age)
sd(creditcard1$income)
sd(creditcard1$share)
sd(creditcard1$expenditure)
sd(creditcard1$dependents)
sd(creditcard1$months)
sd(creditcard1$majorcards)
sd(creditcard1$active)

#Variance
var(creditcard1$reports)
var(creditcard1$age)
var(creditcard1$income)
var(creditcard1$share)
var(creditcard1$expenditure)
var(creditcard1$dependents)
var(creditcard1$months)
var(creditcard1$majorcards)
var(creditcard1$active)

#Lets convert the Categorical columns into Factors
creditcard1$card <- as.factor(creditcard1$card)
creditcard1$owner <- as.factor(creditcard1$owner)
creditcard1$selfemp <- as.factor(creditcard1$selfemp)
summary(creditcard1)
sum(is.na(creditcard1)) #To see total Number of Null Value

#Lets build a Logistic Regression Model

credit_model <- glm(creditcard1$card~. , data = creditcard1, family = "binomial")
summary(credit_model)

pred <- predict(credit_model,type = "response")
pred

#lets make a confusion matrix
confusion <- table(pred>0.5, creditcard1$card)
confusion

#Lets find the Accuracy of the Model
accuracy <- sum(diag(confusion)/sum(confusion))
accuracy
#The Accuracy of the Model is 85.97

final <- data.frame(creditcard1$card, pred)

#Reciever Operating Characteristics
install.packages("pROC")
library(pROC)

creditcard_roc <- roc(creditcard1$card, pred)
creditcard_roc

#Lets see the Sensitivities
sens <-creditcard_roc$sensitivities
sens
#Lets see the Specificity
spec <-creditcard_roc$specificities

credit_roc_data <- data.frame(sens, spec)
credit_roc_data

library(ggplot2)
#Lets Plot the ROC curve with Specificity and Sensitivitty
ggplot(credit_roc_data, aes(x=spec, y=sens))+ geom_line()

#Here the Area Under Curve(AUC) is 0.6935