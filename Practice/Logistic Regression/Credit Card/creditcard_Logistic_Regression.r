#Logistic Regression

#Classify whether application accepted or not using Logistic regression
#Import the creditcard.csv file

creditcard <- read.csv(file.choose())
summary(creditcard)

#We dont want the first column so we will remove it
creditcard1 <- creditcard[,-1]

creditcard1$card <- as.factor(creditcard1$card)
creditcard1$owner <- as.factor(creditcard1$owner)
creditcard1$selfemp <- as.factor(creditcard1$selfemp)
summary(creditcard1)
sum(is.na(creditcard1)) #To see total Number of Null Value

#Lets build a Logistic Regression Model

credit_model <- glm(creditcard1$card~. , data = creditcard1, family = "binomial")
summary(credit_model)

pred <- data.frame(predict(credit_model,creditcard1,type = "response"))
pred

#lets make a confusion matrix
confusion <- table(pred>0.5, creditcard1$card)
confusion

#Lets find the Accuracy of the Model
accuracy <- sum(diag(confusion)/sum(confusion))
accuracy
#The Accuracy of the Model is 85.97

final <- data.frame(creditcard1$card, pred)
