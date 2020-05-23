#Neural Network Algorithm

#Build a Neural Network model for 50_startups data to predict profit 

library(neuralnet)

#Lets Import the Data
startup <- read.csv(file.choose())
attach(startup)
summary(startup)
sum(is.na(startup))
dim(startup)

names(startup)

startup1 <- startup[,-4] #We exclude the 4th column as it holds the Name of the States

cor(startup1) #we can see that R.D.Spend and Profit are Highly Correlated.

#Standard Deviation
sd(R.D.Spend)
sd(Administration)
sd(Marketing.Spend)
sd(Profit)

#Variance
var(R.D.Spend)
var(Administration)
var(Marketing.Spend)
var(Profit)

#Plots
plot(R.D.Spend,Administration, type = "p")

plot(Marketing.Spend,Profit, main = "Marketing Expense vs Profit")
#Lets Normalize the data
normalise <- function(x) {
  return((x - min(x))/(max(x) - min(x)))
}

startup_norm <- as.data.frame(lapply(startup1, normalise))

#Lets Split the Dataset into Train and Test Set Randomly
split1 <- sort(sample(nrow(startup_norm),nrow(startup_norm)*0.7)) #Here we are Splitting it in 70:30 ratio
train1 <- startup_norm[split1,]
test1 <- startup_norm[-split1,]

#Now Lets Build the Neural Network
n1 <- neuralnet(formula = Profit~ R.D.Spend + Administration + Marketing.Spend, data = train1)
#we have Built the Neural Netwrok Model for Profit, lets plot it
plot(n1) #Here the error rate is 0.033526

#Lets predict the Profit and find the Accuracy of the Model
n1_result <- compute(n1, test1[1:3])#here in the test set we will exclude the Profit Values
profit1 <- n1_result$net.result
round(cor(test1$Profit,profit1)*100, digits = 2)
#Here we got the Accuracy of the Model as 96.2%

#Lets improve the Model Performance by Building more complex Network
#Lets include 1 hidden layer with 3 neurons
n2 <- neuralnet(formula = Profit~. , data = train1, hidden = 3)
plot(n2) #Here the error rate is 0.02744
n2_results <- compute(n2, test1[1:3])
profit2 <- n2_results$net.result
round(cor(test1[,4],profit2)*100, digits = 2)
#Here we got the Accuracy of the Model as 96.64%

#Lets include 2 hidden layers with 5 and 2 neurons
n3 <- neuralnet(formula = Profit~. , data = train1, hidden = c(5,2))
plot(n3)#Here the error rate is 0.025102
n3_results <- compute(n3,test1[1:3])
profit3 <- n3_results$net.result
round(cor(test1[,4], profit3)*100, digits = 2)
#Here the Accuracy for the model is 96.2%

#Lets Build the Model Using the Scaled Values of the Dataset
startup_scaled <- as.data.frame(scale(startup1))

#Lets Split the data randomly in Train and test set
train2 <- startup_scaled[split1,]
test2 <- startup_scaled[-split1,]

#Lets Build the Model and Check the Accuracy
n11 <- neuralnet(formula = Profit~. , data = train2)
plot(n11) #Here the Error rate is 0.61616
n11_results <- compute(n11, test2[1:3])
profit11 <- n11_results$net.result
round(cor(test2[,4], profit11)*100, digits = 2)
#Here we got the Accuracy as 96.67%

n12 <- neuralnet(formula = Profit~. , data = train2, hidden = c(3,2))
plot(n12) #Here the Error rate is 0.362998
n12_results <- compute(n12, test2[1:3])
profit12 <- n12_results$net.result
round(cor(test2[,4], profit12)*100, digits = 2)
#Here the Accuracy of the Model is 93.42%

n13 <- neuralnet(formula = Profit~. , data = train2, hidden = c(4,3,2,2))
plot(n13) #Here the Error rate is 0.17232
n13_results <- compute(n13, test2[1:3])
profit13 <- n13_results$net.result
round(cor(test2[,4], profit13)*100, digits = 2)
#Here the Accuracy of the Model is 89.5%