#Support Vector Machine (SVM)

#Import the dataset named lettersdata.csv

library(kernlab)

letterdata <- read.csv("C:\\Users\\pravi\\Desktop\\R practice\\Supervised Machine Learning Techniques in R\\Support Vector Machine\\letterdata.csv")

#Divide into Train and Test data
letter_train <- letterdata[1:16000,]
letter_test <-letterdata[16001:20000,]

#Training a model on the data
#Begin by Training a simple linear SVM
letter_classifier <- ksvm(letter~., data=letter_train, kernel = "vanilladot")

#Evaluating Model Performance
#prediction on est data
letter_pred <- predict(letter_classifier,letter_test)
head(letter_pred)
table(letter_pred, letter_test$letter)
agreement <- letter_pred==letter_test$letter
agreement
prop.table(table(agreement))

#Improving the model Performance
letter_classifier_rbf <- ksvm(letter~., data = letter_train, kernel = "rbfdot")
letter_pred_rbf <- predict(letter_classifier_rbf, letter_test)
head(letter_pred_rbf)
agreement_rbf <- letter_pred_rbf==letter_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))
