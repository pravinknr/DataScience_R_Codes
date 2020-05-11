#Random forest

#A cloth manufacturing company is interested to know about the segment or attributes causes high sale. 

#A Random Forest can be built with target variable Sale (we will first convert it in categorical variable) & all other variable will be independent in the analysis.  

install.packages("randomForest")
library(randomForest)
library(caret)
library(gmodels)

#Lets Import the Dataset
company <- read.csv(file.choose())
attach(company)
summary(company) #Gives the Summary of the Dataset

str(company) #Gives the Structure of the Dataset
#Here we see that there are 8 columns with numeric Variable and 3 columns with character Variable
#Lets Covert the Character Variables into Categorical Variable

names(company)#Gives the Names of the Columns in the Dataset

company$ShelveLoc <- as.factor(company$ShelveLoc)
company$Urban <- as.factor(company$Urban)
company$US <- as.factor(company$US)

#Standard Deviation
sd(Sales)
sd(CompPrice)
sd(Income)
sd(Advertising)
sd(Population)
sd(Price)
sd(Age)
sd(Education)

#Variance
var(Sales)
var(CompPrice)
var(Income)
var(Advertising)
var(Population)
var(Price)
var(Age)
var(Education)

#Correlation matrix
cor(company[,-c(7,10,11)]) #Categorical Variable are not used for Correlation. cor() accepts only the numeric variable

#Lets Plot the Data

ggplot(company) + geom_histogram(aes(Sales),binwidth = 0.5, color = "cyan")

ggplot(company) + geom_histogram(aes(CompPrice),binwidth = 10, color = "cyan")

ggplot(company) + geom_histogram(aes(Income),binwidth = 1, color = "cyan")

ggplot(company) + geom_histogram(aes(Advertising),binwidth = 0.4, color = "cyan")

ggplot(company) + geom_histogram(aes(Population),binwidth = 20, color = "cyan")

ggplot(company) + geom_histogram(aes(Price),binwidth = 9, color = "cyan")

ggplot(company) + geom_histogram(aes(Age),binwidth = 0.7, color = "cyan")

ggplot(company) + geom_histogram(aes(Education),binwidth = 0.2, color = "cyan")

#When we got the Summary of the Dataset, we Saw that the Sales variable has a range of 0 to 16
#So here We will Split the Variable
#we will create a new variale "High"
#If the Sales is Greater than 8 then It is a Yes Else No

High <- ifelse(Sales > 8,"Yes","No")
High <- as.factor(High)

#Lets Combine it with the new Dataset
company_new <- cbind(company[,-1],High) #Here we have Excluded the Sales Column as we have Derived a new Variable High Using it.
str(company_new)
#Lets Create the Training and Testing sets
indatapartition <- createDataPartition(company_new$High, p=.60, list = F) #This will Hold 60% of the whole dataset
training <- company_new[indatapartition,]
testing <- company_new[-indatapartition,]

#Lets Build a model for the Entire Dataset

rf1 <- randomForest(High~. , data = company_new, ntree = 100, type = "class") #ntree is the Number of trees
rf1 #Gives the Model Details
summary(rf1)#Gives the Summary of the Model
print(importance(rf1)) #returns the MeanDecreaseGini value of the features. The feature with Highest value is the best feature of the Model
#Lets Predict the Values for the Whole Data and see the Accuracy
pred1<- predict(rf1, company_new[,-11])
tab1 <- table(company_new[,11], pred1)
tab1
Acc1 <- sum(diag(tab1))/sum(tab1)
Acc1

#Lets Build the Model for Training set and predict the Testing set
rf2 <- randomForest(High~., data = training, ntree = 500)
rf2
print(importance(rf2))
pred2 <- predict(rf2, testing[,-11])
tab2 <- table(testing[,11], pred2)
tab2
Acc2 <- sum(diag(tab2))/sum(tab2)
Acc2

#For 1000 trees
rf3 <- randomForest(High~., data = training, ntree = 1000)
rf3
print(importance(rf3))
pred3 <- predict(rf3, testing[,-11])
tab3 <- table(testing[,11], pred3)
tab3
Acc3 <- sum(diag(tab3))/sum(tab3)
Acc3

#For 1500 trees
rf4 <- randomForest(High~., data = training, ntree = 1500)
rf4
print(importance(rf4))
pred4 <- predict(rf4, testing[,-11])
tab4 <- table(testing[,11], pred4)
tab4
Acc4 <- sum(diag(tab4))/sum(tab4)
Acc4

#Lets Normalize the Data and see if the Accuracy of the Model Increases

normalise <- function(x) {
  return((x - max(x))/(max(x) - min(x)))
}

company_norm <- as.data.frame(lapply(company_new[,-c(6,9,10,11)], normalise))
company_norm_data <- cbind(company_norm, company_new[,c(6,9,10,11)])
summary(company_norm_data)

#Lets Divide the Data in train and test set
indatapartition1 <- createDataPartition(company_norm_data$High, p=.60, list = F) #This will Hold 60% of the whole dataset
training1 <- company_new[indatapartition1,]
testing1 <- company_new[-indatapartition1,]

#Lets Build the Model
rf5 <- randomForest(High~. , data = training1, ntree = 500)
rf5
print(importance(rf5))
pred5 <- predict(rf5, testing1[,-11])
tab5 <- table(testing1[,11], pred5)
tab5
Acc5 <- sum(diag(tab5))/sum(tab5)
Acc5
#So here we observe that the Accuracy has decreased

#Lets increase the tree Numbers and check
rf6 <- randomForest(High~. , data = training1, ntree = 1500)
rf6 #Here we can Observe that the Out Of Bag(OOB) estimate of error rate has decreased from 20% to 14%
print(importance(rf6))
pred6 <- predict(rf6, testing1[,-11])
tab6 <- table(testing1[,11], pred6)
tab6
Acc6 <- sum(diag(tab6))/sum(tab6)
Acc6
#Here also we can see that there is a slight Increase in the Accuracy

rf7 <- randomForest(High~. , data = training1, ntree = 5000)
rf7
print(importance(rf7))
pred7 <- predict(rf7, testing1[,-11])
tab7 <- table(testing1[,11], pred7)
tab7
Acc7 <- sum(diag(tab7))/sum(tab7)
Acc7
#So we Can Conclude that the Variables that cause High sales are Advertising, ShelveLoc and Price (Using Importance() Function)