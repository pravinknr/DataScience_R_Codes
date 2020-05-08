#Logistic Regression
#Output variable -> y

#Lets Import the dataset

bank <- read.csv(file.choose(), sep = ";") #The dta in the file is seperated using Semi-Colons
summary(bank)

#Standard Deviation
sd(bank$age)
sd(bank$balance)
sd(bank$duration)
sd(bank$campaign)
sd(bank$pdays)
sd(bank$previous)

#Variance
var(bank$age)
var(bank$balance)
var(bank$duration)
var(bank$campaign)
var(bank$pdays)
var(bank$previous)

#We see that the categorical Variables are defined as Character. Lets convert it into the Categorical variable using Factor function

bank$job <- as.factor(bank$job)
bank$marital <- as.factor(bank$marital)
bank$education <- as.factor(bank$education)
bank$default <- as.factor(bank$default)
bank$housing <- as.factor(bank$housing)
bank$loan <- as.factor(bank$loan)
bank$contact <- as.factor(bank$contact)
bank$month <- as.factor(bank$month)
bank$poutcome <- as.factor(bank$poutcome)
bank$y <- as.factor(bank$y)

summary(bank)

windows()
pairs(bank)
#Lets Build a generalized linear regression model

bank_model <- glm(bank$y~. , data = bank, family = "binomial")
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