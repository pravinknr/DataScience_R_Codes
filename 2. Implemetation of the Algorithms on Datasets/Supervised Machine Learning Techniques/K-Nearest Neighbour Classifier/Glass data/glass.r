#K-Nearest Neighbour Classifier

#Prepare a model for glass classification using KNN

library(ggplot2)
#Lets Import the Data
glass <- read.csv(file.choose())
attach(glass)
summary(glass)# to get the Summary of the Dataset

str(glass) #Here we can see that Type variable is recognized as Integer. We have to change it into Categorical Variable

glass$Type <- as.factor(glass$Type)

colnames(glass) #Gives the Column names of the Dataset

dim(glass) #Gives the Number of Rows and COlumns of the Dataset

#Standard Deviation
sd(RI)
sd(Na)
sd(Mg)
sd(Al)
sd(Si)
sd(K)
sd(Ca)
sd(Ba)
sd(Fe)

#Variance
var(Ri)
var(Na)
var(Mg)
var(Al)
var(Si)
var(K)
var(Ca)
var(Ba)
var(Fe)

#Plots
ggplot(glass, aes(x=Na)) +geom_histogram( binwidth=0.5, fill="#69b3a2", color="#e9ecef", alpha=0.9) + ggtitle("Plot for Na") +theme_gray()

ggplot(glass, aes(x=Mg)) +geom_histogram( binwidth=0.3, fill="#69b3a2", color="#e9ecef", alpha=0.9) + ggtitle("Plot for Mg") +theme_gray()

ggplot(glass, aes(x=RI)) +geom_histogram( binwidth=0.0005, fill="#69b3a2", color="#e9ecef", alpha=0.9) + ggtitle("Plot for Refractive Index") +theme_gray()

ggplot(glass, aes(x=Fe)) +geom_histogram( binwidth=0.045, fill="#69b3a2", color="#e9ecef", alpha=0.9) + ggtitle("Plot for Fe") +theme_gray()


#Lets derive the Normalization Function
normalise <- function(x) {
  return((x - min(x))/(max(x) - min(x)))
}

#Lets apply the Function on the Data
glas_n <- as.data.frame(lapply(glass[,-10], normalise))
glass_nl <- cbind(glas_n, glass$Type) #Combining the Normlized data and the Type Column


#Lets Divide the DataSet into Training and Testing Sets
library(caret)
indatapartition <- createDataPartition(glass_nl$`glass$Type`, p=.70, list = FALSE)
training <- glass_nl[indatapartition,]
testing <- glass_nl[-indatapartition,]

#Lets Build the KNN model
library(class)
class_identifier <- knn(train = training[,-10], test = testing[,-10], cl = training[,10], k=1) #cl stands for Classification
class_identifier

#Lets Evaluate the Accuracy of the Model
library(gmodels)
CrossTable(testing$`glass$Type`, class_identifier, prop.r = F, prop.c = F, prop.chisq = F)
tab <- table(testing$`glass$Type`, class_identifier)
Accuracy <- round(sum(diag(tab))/sum(tab)*100, digits = 2) #Here Digits attribute Specifies the Number of digits after Decimal Point
Accuracy

#Accuracy for k=1 is 65.57

#Lets see the Accuracy for Different K values

#For k= 3
class_identifier1 <- knn(train = training[,-10], test = testing[,-10], cl = training[,10], k=3)
CrossTable(testing$`glass$Type`, class_identifier1, prop.r = F, prop.c = F, prop.chisq = F)
tab <- table(testing$`glass$Type`, class_identifier1)
Accuracy <- round(sum(diag(tab))/sum(tab)*100, digits = 2)
Accuracy
#Accuracy for k= 3 is 62.3

#For k = 5
class_identifier2 <- knn(train = training[,-10], test = testing[,-10], cl = training[,10], k=5)
CrossTable(testing$`glass$Type`, class_identifier2, prop.r = F, prop.c = F, prop.chisq = F)
tab <- table(testing$`glass$Type`, class_identifier2)
Accuracy <- round(sum(diag(tab))/sum(tab)*100, digits = 2)
Accuracy
#Accuracy for k = 5 is 63.93

#For k=10
class_identifier3 <- knn(train = training[,-10], test = testing[,-10], cl = training[,10], k=11)
CrossTable(testing$`glass$Type`, class_identifier3, prop.r = F, prop.c = F, prop.chisq = F)
tab <- table(testing$`glass$Type`, class_identifier3)
Accuracy <- round(sum(diag(tab))/sum(tab)*100, digits = 2)
Accuracy
#Accuracy for k=10 is again 60.66

#So as we can see that the Accuracy is not Increasing for different k values
#Lets Improve the Model Performance

#Lets Scale the Values of the Dataset Using Scale() function

glass_sc <- as.data.frame(scale(glass[,-10]))
glass_scaled <- cbind(glass_sc, glass$Type)

#Lets Divide the Data in Training and Testing Sets
indatapartition1 <- createDataPartition(glass_scaled$`glass$Type`, p=.50, list = FALSE)
train_scaled <- glass_scaled[indatapartition1,]
test_scaled <- glass_scaled[-indatapartition1,]

#Lets Build the KNN Classifier Model for Scaled Values
class1 <- knn(train = train_scaled[,-10], test = test_scaled[,-10], cl = train_scaled[,10], k=2)
CrossTable(test_scaled$`glass$Type`, class1, prop.r = F, prop.c = F,prop.chisq = F)
tab1 <- table(test_scaled$`glass$Type`, class1)
Acc1 <- round(sum(diag(tab1))/sum(tab1)*100, digits = 3)
Acc1
#Accuracy for Scaled Model with k=2 is 71.429
#So here We can see that the Model has been Improved using Scale Function. Lets do it for Different k values

#For k=5
class2 <- knn(train = train_scaled[,-10], test = test_scaled[,-10], cl = train_scaled[,10], k=5)
CrossTable(test_scaled$`glass$Type`, class2, prop.r = F, prop.c = F,prop.chisq = F)
tab2 <- table(test_scaled$`glass$Type`, class2)
Acc2 <- round(sum(diag(tab2))/sum(tab2)*100, digits = 3)
Acc2
#For k=5 we got Accuracy as 64.762

#For k=6
class3 <- knn(train = train_scaled[,-10], test = test_scaled[,-10], cl = train_scaled[,10], k=6)
CrossTable(test_scaled$`glass$Type`, class3, prop.r = F, prop.c = F,prop.chisq = F)
tab3 <- table(test_scaled$`glass$Type`, class3)
Acc3 <- round(sum(diag(tab3))/sum(tab3)*100, digits = 3)
Acc3
#For k = 6 we got 70.476% Accuracy

#Lets see for k=11
class4 <- knn(train = train_scaled[,-10], test = test_scaled[,-10], cl = train_scaled[,10], k=11)
CrossTable(test_scaled$`glass$Type`, class4, prop.r = F, prop.c = F,prop.chisq = F)
tab4 <- table(test_scaled$`glass$Type`, class4)
Acc4 <- round(sum(diag(tab4))/sum(tab4)*100, digits = 3)
Acc4
#For k=11 we got Accuracy as 66.667%

#For k=16
class5 <- knn(train = train_scaled[,-10], test = test_scaled[,-10], cl = train_scaled[,10], k=16)
CrossTable(test_scaled$`glass$Type`, class5, prop.r = F, prop.c = F,prop.chisq = F)
tab5 <- table(test_scaled$`glass$Type`, class5)
Acc5 <- round(sum(diag(tab5))/sum(tab5)*100, digits = 3)
Acc5
#For k=16, the Accuracy is 69.524%

#for k=1
class6 <- knn(train = train_scaled[,-10], test = test_scaled[,-10], cl = train_scaled[,10], k=1)
CrossTable(test_scaled$`glass$Type`, class6, prop.r = F, prop.c = F,prop.chisq = F)
tab6 <- table(test_scaled$`glass$Type`, class6)
Acc6 <- round(sum(diag(tab6))/sum(tab6)*100, digits = 3)
Acc6
#The Accuracy for k=1 is 70.476%

#for k=21
class7 <- knn(train = train_scaled[,-10], test = test_scaled[,-10], cl = train_scaled[,10], k=21)
CrossTable(test_scaled$`glass$Type`, class7, prop.r = F, prop.c = F,prop.chisq = F)
tab7 <- table(test_scaled$`glass$Type`, class7)
Acc7 <- round(sum(diag(tab7))/sum(tab7)*100, digits = 3)
Acc7
#For k=21 the Accuracy is 63.81%
#Thus we Conclude that the model performs the Best for k= 1 and k=2