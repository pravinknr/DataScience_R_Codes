#Decision Tree

#Boosting in Bagging Technique
#We use For loop for bagging in order to make multiple models

datasets::iris

acc<- c()
for (i in 1:500) #This will create 500 different models
{
  print(i)
  
  #Create a Training and a Testing Set
  inTraininglocal <- createDataPartition(iris$Species, p=.85, list = F)
  training <- iris[inTraininglocal,]
  testing <- iris[-inTraininglocal,]
  
  #Build  a Model
  fittree <- C5.0(training$Species~., data = training, trials = 25) #Trials is a Boosting Parameter
  
  
  pred<- predict.C5.0(fittree,testing[,-5])
  a<- table(testing$Species, pred)
  
  #To save the Accuracy of the models
  acc<- c(acc,sum(diag(a))/sum(a)) 
}

summary(acc)
boxplot(acc)

summary(fittree)
