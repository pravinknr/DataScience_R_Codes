#Multi Linear Regression

#We are using the Corolla.csv File

#We have to Prepare a model to predict the Price of the Car
#The Price are in Euro's

corolla <- read.csv(file.choose())
attach(corolla)
corolla <- data.frame(cbind(corolla$Price,corolla$Age_08_04,corolla$KM,corolla$HP,corolla$cc, corolla$Doors, corolla$Gears,corolla$Quarterly_Tax,corolla$Weight))
colnames(corolla) <- c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")

summary(corolla)

#Lets build a predicting model

corolla_model <- lm(Price~. , data = corolla)
summary(corolla_model)

#We have created a Multi Linear Regression Model to find the Price of the Vehicle and its R-Square value is 0.8638 which means that this model will give 86.38% times right answer.

#Lets Predict the Values for the Data of the Dataset

pred <- predict(corolla_model)
pred

final_data<- data.frame(Price, pred, Error = corolla_model$residuals)
view(final_data)

#As we could see that the Actual Price and Predicted Price has much difference, we will Build Another Data By Normalizing It

corolla_norm <- data.frame(scale(corolla))

#Now lets build a model Using the Scaled Values
corolla_norm_model <- lm(Price~., data = corolla_norm)
summary(corolla_norm_model)

pred_norm <- predict(corolla_norm_model)
norm_data <- data.frame(Price, pred_norm, corolla_norm_model$residuals)
norm_data

#Log  Transformation

corolla_log_model<- lm(log(Price)~. , data = corolla)
summary(corolla_log_model)
pred_log <- predict(corolla_log_model)
log_data <-data.frame(log(Price), pred_log, corolla_log_model$residuals)
