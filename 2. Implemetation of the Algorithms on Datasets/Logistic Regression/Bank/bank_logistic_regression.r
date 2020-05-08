#Logistic Regression
#Output variable -> y

#Lets Import the dataset

bank <- read.csv(file.choose(), sep = ";") #The dta in the file is seperated using Semi-Colons
attach(bank)
summary(bank)

#Standard Deviation
sd(age)
sd(balance)
sd(duration)
sd(campaign)
sd(pdays)
sd(previous)

#Variance
var(age)
var(balance)
var(duration)
var(campaign)
var(pdays)
var(previous)

#We see that the categorical Variables are defined as Character. Lets convert it into the Categorical variable using Factor function

job <- as.factor(job)
marital <- as.factor(marital)
education <- as.factor(education)
default <- as.factor(default)
housing <- as.factor(housing)
loan <- as.factor(loan)
contact <- as.factor(contact)
bank <- as.factor(month)
poutcome <- as.factor(poutcome)
y <- as.factor(y)

summary(bank)

windows()
pairs(bank)
#Lets Build a generalized linear regression model

bank_model <- glm(y~. , data = bank, family = "binomial")
summary(bank_model)

#Lets predict the values using the model

pred <- predict(bank_model, bank, type = "response")
#pred <- ifelse(pred>0.8,"yess","no")

#The Coefficients
round(exp(coef(bank_model)),3)

#Lets build the Confusion Matrix
confusion_matrix <- table(bank$y, pred >0.8)
confusion_matrix

#Lets compute the Accuracy of the Model
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
accuracy
#The Accuracy of the model we built is 90.18%

#Reciever Operating Characteristics
install.packages("pROC")
library(pROC)

bank_roc <- roc(bank$y, pred)
bank_roc

#Lets see the Sensitivities
sens <-bank_roc$sensitivities[2:45212]

#Lets see the Specificity
spec <-bank_roc$specificities[2:45212]

bnk_roc_data <- data.frame(sens, spec)
bnk_roc_data

library(ggplot2)
#Lets Plot the ROC curve with Specificity and Sensitivitty
ggplot(bnk_roc_data, aes(x=spec, y=sens)) + geom_line()

#Here the Area Under Curve(AUC) is 0.9079