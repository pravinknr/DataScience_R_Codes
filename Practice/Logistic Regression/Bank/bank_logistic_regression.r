#Logistic Regression

#Lets Import the dataset

bank <- read.csv(file.choose(), sep = ";") #The dta in the file is seperated using Semi-Colons
summary(bank)
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

#Lets Build a generalized linear regression model

bank_model <- glm(bank$y~. , data = bank, family = "binomial")
summary(bank_model)

#Lets predict the values using the model

pred <- predict(bank_model, bank, type = "response")

#Lets build the Confusion Matrix
confusion_matrix <- table(bank$y, pred >0.5)
confusion_matrix

#Lets compute the Accuracy of the Model
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
accuracy
#The Accuracy of the model we built is 90.18%