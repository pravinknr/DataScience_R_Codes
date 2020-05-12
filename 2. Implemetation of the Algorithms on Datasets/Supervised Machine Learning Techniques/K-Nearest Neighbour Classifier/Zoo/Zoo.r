#k-Nearest Neighbour Classifier

#Implement a KNN model to classify the animals in to categories

library(factoextra)
#Lets Import the Data
zoo <- read.csv(file.choose())
attach(zoo)
View(zoo)
summary(zoo)

str(zoo)#Get the Structure of the Dataset
#Now We want the Type column to be a Categorical Variable as we have to Predict the Type of the Animal Category, Lets Convert it Into Categorical Variable
zoo$type <- as.factor(zoo$type)

#We dont want the 1st column of the Dataset as it holds the Name of the Animals. We will Remove it
zoo <- zoo[,-1]

#Lets Normalize the Data
#Lets Derive a Function for Nomalize the Data

normalise <- function(x){
  return((x - min(x))/(max(x) - min(x)))
}

#Lets Apply the Function on the Data
zoo_norm <- as.data.frame(lapply(zoo[-17], normalise))
zoo_norm1 <- cbind(zoo_norm,zoo$type)

#Lets Create Data Partition for defining Training and Testing Sets
library(caret)
#I will Take 60% of the Total Dataset Randomly for Training set and the rest 40%in the Testing set
partition <- createDataPartition(zoo_norm1$`zoo$type`, p=.60, list = F)
training <- zoo_norm1[partition,]
testing <- zoo_norm1[-partition,]

#Now Lets Build the KNN Classifier Model
library(gmodels)
library(class)
class1 <-knn(train = training[,-17], test = testing[,-17], cl = training$`zoo$type`, k=1) 
CrossTable(testing$`zoo$type`, class1, prop.r = F, prop.c = F, prop.chisq = F)
tab1 <- table(testing$`zoo$type`, class1)
Acc1 <- round((sum(diag(tab1))/sum(tab1))*100, digits = 2)
Acc1
#For k=1 the Accuracy is 100%

#Lets See for Different K Values

#for K=3
class2 <- knn(train = training[,-17], test = testing[,-17], cl = training$`zoo$type`, k=3)
CrossTable(testing$`zoo$type`, class2)
tab2 <- table(testing$`zoo$type`, class2)
Acc2 <- round((sum(diag(tab2))/sum(tab2))*100, digits = 2)
Acc2
#For k=3 the Accuracy is 97.44%

#for k=10
class3 <- knn(train = training[,-17], test = testing[,-17], cl = training$`zoo$type`, k=10)
CrossTable(testing$`zoo$type`, class3)
tab3 <- table(testing$`zoo$type`, class3)
Acc3 <- round((sum(diag(tab3))/sum(tab3))*100, digits = 2)
Acc3
#for k=10 the Accuracy is 94.87%

#Lets try it for k=15
class4 <- knn(train = training[,-17], test = testing[,-17], cl = training$`zoo$type`, k=15)
CrossTable(testing$`zoo$type`, class4)
tab4 <- table(testing$`zoo$type`, class4)
Acc4 <- round((sum(diag(tab4))/sum(tab4))*100, digits = 2)
Acc4
#For k=15 the Accuracy is 82.05%

#Through this we can infer that this model can classify the Animals correctly for k=1

#Lets do it by Scaling the Datapoints using Scale() function.
zoo_scaled <- as.data.frame(scale(zoo[,-17]))
zoo_scaled_data <- cbind(zoo_scaled, zoo$type)

#Lets do Data Partition. This time i will be taking 50% of the Scaled dataset randomly in Training set and te rest 50% in testing set
partition1 <- createDataPartition(zoo_scaled_data$`zoo$type`, p=.50, list = F)
train_scaled <- zoo_scaled_data[partition1,]
test_scaled <- zoo_scaled_data[-partition1,]

#Now Lets Build the KNN Classifier Model
scaled_class1 <- knn(train = train_scaled[,-17], test = test_scaled[,-17], cl = train_scaled$`zoo$type`, k=1)
CrossTable(test_scaled$`zoo$type`, scaled_class1, prop.r = F,prop.c = F, prop.chisq = F)
Tab11 <- table(test_scaled$`zoo$type`, scaled_class1)
Acc11 <- round((sum(diag(Tab11))/sum(Tab11))*100, digits = 2)
Acc11
#The Accuracy for k=1 is 93.88%

#For k=3
scaled_class2 <- knn(train = train_scaled[,-17], test = test_scaled[,-17], cl = train_scaled$`zoo$type`, k=3)
CrossTable(test_scaled$`zoo$type`, scaled_class2, prop.r = F,prop.c = F, prop.chisq = F)
Tab12 <- table(test_scaled$`zoo$type`, scaled_class2)
Acc12 <- round((sum(diag(Tab12))/sum(Tab12))*100, digits = 2)
Acc12
#the Accuracy for k=3 is 85.71%

#for k=7
scaled_class3 <- knn(train = train_scaled[,-17], test = test_scaled[,-17], cl = train_scaled$`zoo$type`, k=7)
CrossTable(test_scaled$`zoo$type`, scaled_class3, prop.r = F,prop.c = F, prop.chisq = F)
Tab13 <- table(test_scaled$`zoo$type`, scaled_class3)
Acc13 <- round((sum(diag(Tab13))/sum(Tab13))*100, digits = 2)
Acc13
#The Accuracy for k=7 is 85.71

#for k=11
scaled_class4 <- knn(train = train_scaled[,-17], test = test_scaled[,-17], cl = train_scaled$`zoo$type`, k=11)
CrossTable(test_scaled$`zoo$type`, scaled_class4, prop.r = F,prop.c = F, prop.chisq = F)
Tab14 <- table(test_scaled$`zoo$type`, scaled_class4)
Acc14<- round((sum(diag(Tab14))/sum(Tab14))*100, digits = 2)
Acc14
#For k=11 the Accuracy is 81.63% Also it has not Classified Type 3 and 5 while Predicting.

#Again we can conclude that the model is best when k =1.

#Now Lets Create a loop for K-value and build Models for them

acc <- c()
for (i in 1:50) #We will take k-Values from 1 to 50
{
  print(i)
  
  #Lets Build the Model
  class1 <-knn(train = training[,-17], test = testing[,-17], cl = training$`zoo$type`, k=i) 
  CrossTable(testing$`zoo$type`, class1, prop.r = F, prop.c = F, prop.chisq = F)
  tab1 <- table(testing$`zoo$type`, class1)
  acc <- c(acc,round((sum(diag(tab1))/sum(tab1))*100, digits = 2))
}

summary(acc)
acctable <- data.frame("k" = 1:50, "Accuracy123" = acc)
attach(acctable)
ggplot(acctable, mapping = aes(k, Accuracy123)) + geom_text(aes(label = k), vjust = 3) +geom_line(linetype = "dashed", arrow = arrow())  + geom_point() +  ggtitle("Model Accuracy for 50 Different models")
#Here we can see that k = 5 has the Highest Model Accuracy