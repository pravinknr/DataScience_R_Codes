#Random Forest

#Use Random Forest  to prepare a model on fraud data 
#treating those who have taxable_income <= 30000 as "Risky" and others are "Good"
library(randomForest)
library(gmodels)
library(ggplot2)
library(caret)
library(dplyr)


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

cor(fraud[,c(3,4,5)]) #Returns the Correlation Matrix 

#Lets Plot the Data

ggplot(fraud) + geom_histogram(aes(Taxable.Income), binwidth = 100, fill = "darkgreen") + xlab("Taxable Income")

ggplot(fraud) + geom_histogram(aes(City.Population), binwidth = 500, fill = "darkgreen")

ggplot(fraud) + geom_histogram(aes(Work.Experience), binwidth = 0.5, fill = "darkgreen")

ggplot(fraud %>% group_by(Undergrad) %>% summarise(Count = n())) + geom_bar(aes(Undergrad, Count), stat = "identity", fill = "green")

ggplot(fraud %>% group_by(Marital.Status) %>% summarise(Count = n())) + geom_bar(aes(Marital.Status, Count), stat = "identity", fill = "green")

ggplot(fraud %>% group_by(Urban) %>% summarise(Count = n())) + geom_bar(aes(Urban, Count), stat = "identity", fill = "green")

#Here we have to Build a classification model
#The Condition is if Taxable.Income is <= 30000 then it is "Risky and the rest are "Good"
#So here we will have to convert the Taxable.Income column as Categorical Value with 2 Levels "Risky" and "Good"

#I will use the Ifelse condition to convert the data into Binomial
risk <- ifelse(fraud$Taxable.Income <= 30000, "Risky","Good")
risk <- as.factor(risk)
fraud1 <- cbind(fraud[,-3],risk) #Here we Exclude Taxable.Income Variable as we derived Responsive Variable risk Using it.

#Now lets divide the data into Train and Test Data with 80% partion

intraininglocal <- createDataPartition(fraud1$risk, p=.60, list = F) #Here p=.60 means 60% Partition
train1 <- fraud1[intraininglocal,]
test1 <- fraud1[-intraininglocal,]

#Lets Build the Random forest Model
rf1 <-  randomForest(risk~. , data = train1, ntree = 500)
rf1
print(importance(rf1))
pred1 <- predict(rf1, test1[,-6])
CrossTable(test1[,6], pred1)
tab1 <- table(test1[,6], pred1)
sum(diag(tab1))/sum(tab1)

#Lets try with more number of trees
rf2 <-  randomForest(risk~. , data = train1, ntree = 1000)
rf2
print(importance(rf2))
pred2 <- predict(rf2, test1[,-6])
CrossTable(test1[,6], pred2)
tab2 <- table(test1[,6], pred2)
sum(diag(tab2))/sum(tab2)

rf3 <-  randomForest(risk~. , data = train1, ntree = 2500)
rf3
print(importance(rf3))
pred3 <- predict(rf3, test1[,-6])
CrossTable(test1[,6], pred3)
tab3 <- table(test1[,6], pred3)
sum(diag(tab3))/sum(tab3)

rf4 <-  randomForest(risk~. , data = train1, ntree = 5000)
rf4
print(importance(rf4))
pred4 <- predict(rf4, test1[,-6])
CrossTable(test1[,6], pred4)
tab4 <- table(test1[,6], pred4)
sum(diag(tab4))/sum(tab4)

#Lets Normalise the Data and try
normalise <- function(x) {
  return((x - max(x))/(max(x) - min(x)))
}

fraud2 <- as.data.frame(lapply(fraud1[,c(3,4)], normalise))
fraud2 <- cbind(fraud2, fraud1[,c(1,2,5,6)])

intraininglocal2 <- createDataPartition(fraud2$risk, p=.60, list = F) #Here p=.60 means 60% Partition
train2 <- fraud2[intraininglocal,]
test2 <- fraud2[-intraininglocal,]

#Lets Build the Model
rf5 <- randomForest(risk~., data = train2, ntree = 1000)
rf5
print(importance(rf5))
pred5 <- predict(rf5, test2[,-6])
CrossTable(test2[,6], pred5)
tab5 <- table(test2[,6], pred5)
sum(diag(tab5))/ sum(tab5)

rf6 <- randomForest(risk~., data = train2, ntree = 2000)
rf6
print(importance(rf6))
pred6 <- predict(rf6, test2[,-6])
CrossTable(test2[,6], pred6)
tab6 <- table(test2[,6], pred6)
sum(diag(tab6))/ sum(tab6)

#Here the Variables with high MeanDecreaseGini value are City.Poluation and Work.Experience