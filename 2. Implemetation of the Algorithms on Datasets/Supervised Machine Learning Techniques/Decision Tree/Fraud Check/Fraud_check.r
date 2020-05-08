#Decision Tree

#Use decision trees to prepare a model on fraud data 
#treating those who have taxable_income <= 30000 as "Risky" and others are "Good"
install.packages("gmodels")
install.packages("party")
library(party)
library(gmodels)
library(ggplot2)
library(dplyr)
library(caret)
library(C50)

#Lets Import the Data
fraud <- read.csv(file.choose())

dim(fraud) #Returns the rows and Column count of the Dataset

names(fraud) #Returns the Column Names in the dataset

str(fraud) #Gives the Entire Structure of the dataset
#Here we can see that there are 3 Numerical Columns and 3 Categorical variable. 
#Lets first convert the Categorical columns into Factors

fraud$Undergrad <- as.factor(fraud$Undergrad)
fraud$Marital.Status <- as.factor(fraud$Marital.Status)
fraud$Urban <- as.factor(fraud$Urban)

#Lets Find the Summary of the Data
summary(fraud)

#Standard Deviation
sd(fraud$Taxable.Income)
sd(fraud$City.Population)
sd(fraud$Work.Experience)

#Variance
var(fraud$Taxable.Income)
var(fraud$City.Population)
var(fraud$Work.Experience)

cor(fraud) #Returns the Correlation Matrix 

#Lets Plot the Data

ggplot(fraud) + geom_histogram(aes(Taxable.Income), binwidth = 100, fill = "darkgreen") + xlab("Taxable Income")

ggplot(fraud) + geom_histogram(aes(City.Population), binwidth = 100, fill = "darkgreen")

ggplot(fraud) + geom_histogram(aes(Work.Experience), binwidth = 0.5, fill = "darkgreen")

ggplot(fraud %>% group_by(Undergrad) %>% summarise(Count = n())) + geom_bar(aes(Undergrad, Count), stat = "identity", fill = "green")

ggplot(fraud %>% group_by(Marital.Status) %>% summarise(Count = n())) + geom_bar(aes(Marital.Status, Count), stat = "identity", fill = "green")

ggplot(fraud %>% group_by(Urban) %>% summarise(Count = n())) + geom_bar(aes(Urban, Count), stat = "identity", fill = "green")

#Here we have to Build a classification Tree
#The Condition is if Taxable.Income is <= 30000 then it is "Risky and the rest are "Good"
#So here we will have to convert the Taxable.Income column as Categorical Value with 2 Levels "Risky" and "Good"

#I will use the Ifelse condition to convert the data into Binomial
risk <- ifelse(fraud$Taxable.Income <= 30000, "Risky","Good")
risk <- as.factor(risk)
fraud1 <- cbind(fraud[,-3],risk) #Here we Exclude Taxable.Income Variable as we derived Responsive Variable risk Using it.

#Now lets divide the data into Train and Test Data with 70% partion

intraininglocal <- createDataPartition(fraud1$risk, p=.70, list = F) #Here p=.70 means 70% Partition
train <- fraud1[intraininglocal,]
test <- fraud1[-intraininglocal,]

#Now lets Buld the Decision Tree
mtree <- C5.0(risk~. ,data = train)

#Predict for test data

pred<- predict.C5.0(mtree,newdata = test[,-7])
a<- table(test$risk,pred) #Crating a table with the test$Taxable.Income values and predicted values
sum(diag(a))/sum(a) #to find the Accuracy of the Model. 

CrossTable(test$risk,pred)

#Lets Plot the Model
plot(mtree)

#Lets see the Summary of the Model
summary(mtree)

#Lets build a model for whole data
model <- ctree(risk~. ,fraud1)
summary(model)
plot(model)

pred1 <- predict(model)
CrossTable(risk,pred1)

#We can also Include Boosting in Bagging Technique

#We use For loop for bagging in order to make multiple models

acc<- c()
for (i in 1:100) #This will create 500 different models
{
  print(i)
  
  intraininglocal <- createDataPartition(fraud1$risk, p=.70, list = F) #Here p=.70 means 70% Partition
  train <- fraud1[intraininglocal,]
  test <- fraud1[-intraininglocal,]
  
  #Build  a Model
  fittree <- C5.0(train$risk~. , data = train, trials = 25) #Trials is a Boosting Parameter
  
  
  pred2<- predict.C5.0(fittree,test[,-7])
  a<- table(test$risk, pred2)
  
  #To save the Accuracy of the models
  acc<- c(acc,sum(diag(a))/sum(a)) 
}

summary(acc)
boxplot(acc)

summary(fittree)
