#Forecasting Model

#Build Multiple Models using Dummy Variables and compare the RMSE values
#RMSE <- Root Mean Square Error


library(xlsx)

#Lets Import the Data

coca <- read.xlsx(file.choose(),1)
View(coca)

summary(coca)

sd(coca$Sales) #Standard Deviation

var(coca$Sales) #Variance

#Lets do a line Plot of the data
plot(coca$Sales, type = "l") 

#Lets create Dummy Variables for 4 Quarters

coca["Qs"] <- rep(1:4, len=42)
coca[paste0('Q', 1:4)] <- model.matrix(~ factor(coca$Qs)-1)

#Lets change the column names 
colnames(coca$`df1$dummy_cols1`,coca$`df1$dummy_cols2`, coca$`df1$dummy_cols3`, coca$`df1$dummy_cols4`) <- c("Q1","Q2","Q3","Q4")

#Lets add a new column "t" which will have a sequence of numbers from 1 to 42
coca["t"]<- 1:42
View(coca)

#Lets Add a column which will have the Square values of "t" column
coca["t_square"] <- coca["t"] * coca["t"]
View(coca)

#Lets add One more column that has the log values of the Sales values
coca["log_sales"] <- log(coca["Sales"])
View(coca)

#So in the coca dataset, we have added multiple dummy variables such as columns with Year Quarters,
#t column that contains a number of sequence from 1 to 42 and a t_square column that contains the Square Values
#of the t column. All these Dummy variables are used to build various Models that we are going see further.


attach(coca) #We use attach function so that we can directly use the column name without using the dataset name

#Lets Divide the data into Train and Test data
#Always consider 80% of the data in Train set and the rest to the test set
train <- coca[1:28,]
test <- coca[29:42,]

#Lets Build Different Models and Compute their RMSE Values

# 1. Linear Regression Model

lin_mod <- lm(Sales~t , data = train) #Here we are building a linear model for Sales using the train data
summary(lin_mod)
lin_pred <- data.frame(predict(lin_mod, interval = "predict", newdata = test)) #Here we are pedicting the values for test set
View(lin_pred)
#Lets find the RMSE value for Linear Model
rmse_lin_model <- sqrt(mean((test$Sales - lin_pred$fit)^2, na.rm = T))
rmse_lin_model


# 2. Exponential Model

exp_mod <- lm(log_sales~t, data = train) #Here we are Building a Exponential Model
summary(exp_mod)
exp_pred <- data.frame(predict(exp_mod, interval = "predict", newdata = test))
View(exp_pred)
#Lets find the RMSE value
rmse_exp_mod <- sqrt(mean((test$Sales - exp(exp_pred$fit))^2, na.rm = T))
rmse_exp_mod


# 3. Quadratic Model

Quad_model<-lm(Sales~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad

# 4. Additive Seasonality model

sea_add_model<-lm(Sales~Q1+Q2+Q3,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add


#5. Additive Seasonality with Linear Regression model

Add_sea_Linear_model<-lm(Sales~t+Q1+Q2+Q3,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear

# 6. Additive Seasonality with Quadratic model

Add_sea_Quad_model<-lm(Sales~t+t_square+Q1+Q2+Q3,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad

# 7. Multiplicative Seasonality Model

multi_sea_model<-lm(log_sales~Q1+Q2+Q3,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea

# 8. Multiplicative Seasonality with Linear regression Model

multi_sea_lin_model<-lm(log_sales~t+Q1+Q2+Q3,data = train)
summary(multi_sea_lin_model) 
multi_sea_lin_pred<-data.frame(predict(multi_sea_lin_model,newdata=test,interval='predict'))
rmse_multi_sea_lin<-sqrt(mean((test$Sales-exp(multi_sea_lin_pred$fit))^2,na.rm = T))
rmse_multi_sea_lin


#Lets make a Table for the Models and its RMSE value
table_rmse<-data.frame('Model'=c("Linear Regression Model","Exponential Model","Quadratic Model","Additive Seasonality model","Additive Seasonality with Linear Regression model","Additive Seasonality with Quadratic model","Multiplicative Seasonality Model","Multiplicative Seasonality with Linear regression Model"),'RMSE'=c(rmse_lin_model,rmse_exp_mod,rmse_Quad,rmse_sea_add,rmse_Add_sea_Linear,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_sea_lin))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

#So after Comparing the RMSE values of all the above models, Multiplicative Seasonality with Linear Regression Model has the
#least RMSE value. So this model is the Best Model among all the others and can be used for forecasting
multi_sea_lin_model_whole <- lm(log_sales~t+Q1+Q2+Q3, data = coca)

#Lets get the Residuals
resid <- residuals(multi_sea_lin_model_whole)
resid[1:10]

windows()
hist(resid)

windows()
acf(resid,lag.max = 10)
# By principal of parcimony we will consider lag - 2  as we have so many significant lags 
# Building Autoregressive model on residuals consider lag-2 
k <- arima(resid, order=c(2,0,0))

windows()
acf(k$residuals,lag.max = 15)
pred_res<- predict(arima(resid,order=c(2,0,0)),n.ahead = 120)
str(pred_res)
pred_res$pred
acf(k$residuals)

#Lets Build a ARIMA model for whole dataset. ARIMA(Auto Regression Integrated Moving Average)

#Lets convert the data into Time Series data

library(tseries)
library(forecast)

coca_ts <- ts(coca$Sales, frequency = 4, start = c(1986)) #Create a Time Series data
View(coca_ts)
plot(coca_ts) #Plots the data into a Line chart By default as the data is a Time Series Data

#For Building ARIMA model we the AR coefficient i.e p-value then Integration Coefficient i.e d and Moving Average coefficient i.e q-value
#Lets find p-value, p-valueis Obtained by pacf
pacf(coca_ts) #Lets Consider it as 1

#Lets Find the q-value by acf
acf(coca_ts) #Lets Consider this as 2

#Also lets Consider the d-value as 1
#now lets build an ARIMA model
a <- arima(coca_ts, order = c(1,1,2), method = "ML")
a
#Lets plot the forecast using the ARIMA model
plot(forecast(a,h=5), xaxt = "n")

#Seeing the plot, we get to know that the forecast done was not accurate. This happens when we dont provide the right
# p-value and q-value to the model.

#If we dont know the p-vale and q-value for the model then we can build the model using auto.arima() function.
#This function will analyse the p and q value and build a proper model. Lets Build the Model

ab <- auto.arima(coca_ts)

windows()
plot(forecast(ab, h=5), xaxt = "n")
#So now we can see that the forecast was accurate 

prediction <- forecast(ab,h=4) #This will Predict for the next 4 Quarters
prediction
