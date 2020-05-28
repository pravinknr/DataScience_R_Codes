#KNN - K-Nearest Neighbour Algorithm
#Import the KNN.csv dataset

wbcd <- read.csv("C:\\Users\\pravi\\Desktop\\Datasets\\KNN.csv")
wbcd <- wbcd[,-1] #We will exclude the ID column from the dataset

#Table of Diagnosis
table(wbcd$diagnosis)

#Table of Proportions with more Informative Labels
round(prop.table(table(wbcd$diagnosis))*100, digits = 1)

#Create Normalization Function
normalize <- function(x){
  return((x - min(x))/(max(x) - min(x)))
}

#Normalize the wbcd data

wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
wbcd_nl<- cbind(wbcd_n,wbcd$diagnosis)

#Creating the Training and the Test data
wbcd_train <- wbcd_n[1:469,]
wbcd_test<- wbcd_n[470:569,]

#Create Labels for Training and Test Data
wbcd_train_labels<- wbcd[1:469,1]
wbcd_test_labels<- wbcd[470:569,1]

#Load the "Class" Library
library(class)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=20) 

##Evaluating Model Performance
#load the "gmodels" library
library(gmodels)

#Create the cross Tabulation of predicted vs Actual
CrossTable(x= wbcd_test_labels, y=wbcd_test_pred, prop.chisq = F, prop.c = F, prop.r = F) #to find the Accuracy of the Model

###Improving Model Performance

#Use Scale() function to Z-Score standardize a data frame
wbcd_z <- as.data.frame(scale(wbcd[-1]))

#Confirm that the transformation was done Successfully
#Create Training and Test data
wbcd_train <- wbcd_z[1:469,]
wbcd_test <- wbcd_z[470:569,]

#Re classify Test Cases
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
CrossTable(x= wbcd_test_labels, y=wbcd_test_pred, prop.chisq = F, prop.c = F, prop.r = F)

#Try for Various k values
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=9)
CrossTable(x= wbcd_test_labels, y=wbcd_test_pred, prop.chisq = F, prop.c = F, prop.r = F)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=11)
CrossTable(x= wbcd_test_labels, y=wbcd_test_pred, prop.chisq = F, prop.c = F, prop.r = F)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=25)
CrossTable(x= wbcd_test_labels, y=wbcd_test_pred, prop.chisq = F, prop.c = F, prop.r = F)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=51)
CrossTable(x= wbcd_test_labels, y=wbcd_test_pred, prop.chisq = F, prop.c = F, prop.r = F)
