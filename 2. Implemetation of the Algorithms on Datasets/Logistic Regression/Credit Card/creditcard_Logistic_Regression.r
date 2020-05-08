#Logistic Regression

#Classify whether application accepted or not using Logistic regression
#Import the creditcard.csv file

creditcard <- read.csv(file.choose())
attach(creditcard)
summary(creditcard)

#We dont want the first column so we will remove it
creditcard1 <- creditcard[,-1]

#Standard Deviation
sd(reports)
sd(age)
sd(income)
sd(share)
sd(expenditure)
sd(dependents)
sd(months)
sd(majorcards)
sd(active)

#Variance
var(reports)
var(age)
var(income)
var(share)
var(expenditure)
var(dependents)
var(months)
var(majorcards)
var(active)

#Lets convert the Categorical columns into Factors
card <- as.factor(card)
owner <- as.factor(owner)
selfemp <- as.factor(selfemp)
summary(creditcard1)
sum(is.na(creditcard1)) #To see total Number of Null Value

#Lets build a Logistic Regression Model

credit_model <- glm(card~. , data = creditcard1, family = "binomial")
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