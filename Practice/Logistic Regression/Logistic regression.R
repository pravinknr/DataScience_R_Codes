#claimants<-read.csv(file.choose())
sum(is.na(claimants))#gives the number of NA values Available in the dataset
claimants<-na.omit(claimants)#omitting NA values from the data
#na.omit=> will omit the rows which has atleast 1NA value

dim(claimants) #Gives the total count of rows and columns in the dataset 

colnames(claimants)#column names (row,cloumn)

claimants<-claimants[,-1] #removing the first column which is an index
#preparing a linear regression

mod_lm<-lm(ATTORNEY~.,data=claimants)#create a linear model 
mod_lm
pred1<-predict(mod_lm,claimants)
pred1
plot(claimants$CLMINSUR,pred1)
#we can no way use the linear regression techinque to classify the data
plot(pred1)

#GLM function use sigmoid curve to produce desirable results
#the output of sigmoid function lies between 0-1
model<-glm(ATTORNEY~.,data=claimants,family="binomial")
summary(model)

#Confusion matrix table
prob<-predict(model,claimants,type="response")
prob

#confusion matrix and considering the threshold values as 0.5
confusion<-table(prob>0.5,claimants$ATTORNEY)
confusion 

# Model Accuracy
Accuracy<-sum(diag(confusion)/sum(confusion))#costruct a diagonal matrix
help("diag")
Accuracy #70.62

