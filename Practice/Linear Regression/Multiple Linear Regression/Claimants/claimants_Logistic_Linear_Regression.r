#Logistic Regression
#we use Logistic Regression when we have Binomial Data and we want output in terms of Binomial Datatype


claimants <- read.csv("C:/Users/pravi/Desktop/Datasets/claimants.csv")
View(claimants)

sum(is.na(claimants)) #Gives the sum of the NA values in the dataset

claimants <- na.omit(claimants) #This will remove the NA values from the Dataset
 
dim(claimants) #This will give the rows and columns count of the dataset

colnames(claimants) #This will give the column names of the dataset

claimants <- claimants[,-1] #This will remove the first column of the dataset that contains the Sr.No i.e Index

#Preparing a Linear Regression

model1 <- lm(ATTORNEY~., data = claimants) #Create a Linear Model
pred1<- predict(model1, claimants) #Predict the values for data in claimants
pred1

plot(claimants$CLMINSUR,pred1) #plots the points for pred1 and CLMINSUR column of claimants
#We cannot use the linear regression technique to classify the data
plot(pred1) #Plots the points for predicted values

#The GLM Function use Sigmoid Curve to prodce desirable results
#The output of Sigmoid Function lies between 0 and 1

model2 <- glm(ATTORNEY~.,family = "binomial", data = claimants)
summary(model2)

#Confusion Matrix table
prob <- predict(model2, claimants, type = "response")
prob

#Confusion Matrix and considering the threshold value as 0.5
confusion <- table(prob>0.5, claimants$ATTORNEY)
confusion

#Accuracy of the model
accuracy <- sum(diag(confusion)/sum(confusion))
accuracy
#Accuracy of model is 70.52