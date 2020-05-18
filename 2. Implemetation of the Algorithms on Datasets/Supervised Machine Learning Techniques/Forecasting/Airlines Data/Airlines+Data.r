#Forecasting Model

#Build Multiple Models using Dummy Variables and compare the RMSE values
#RMSE <- Root Mean Square Error

#Lets Import the Data
library(xlsx)
airlines <- read.xlsx(file.choose(),1)
attach(airlines)
View(airlines)

summary(airlines)

sd(Passengers) #Standard Deviation

var(Passengers) #Variance

#Lets do a line Plot of the data
plot(airlines, type = "l") 

#Lets create Dummy Variables for 12 Months
x<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )# Creating dummies for 12 months
View(x)

#Lets Assigns the Column names As MonthAbbreviations
colnames(x) <- month.abb #This will give the Abbreviation of Months as Column name of x
View(x)

#Lets make a new dataframe with Original data and Dummy Variables
new_data <- cbind(airlines, x) #cbind function is used to comibine the columns of the datasets
View(new_data)

#Lets add a new column "t" which will have a sequence of numbers from 1 to 96
new_data["t"]<- 1:96
View(new_data)

#Lets Add a column which will have the Square values of "t" column
new_data["t_square"] <- new_data["t"] * new_data["t"]
View(new_data)

#Lets add One more column that has the log values of the Passenger values
new_data["log_passenger"] <- log(new_data["Passengers"])
View(new_data)

#So in the new_data dataset, we have added multiple dummy variables such as columns with month abbreviation,
#t column that contains a number of sequence from 1 to 96 and a t_square column that contains the Square Values
#of the t column. All these Dummy variables are used to build various Models that we are going see further.


attach(new_data) #We use attach function so that we can directly use the column name without using the dataset name

#Lets Divide the data into Train and Test data
#Always consider 80% of the data in Train set and the rest to the test set
train <- new_data[1:72,]
test <- new_data[73:96,]

#Lets Build Different Models and Compute their RMSE Values

# 1. Linear Regression Model

lin_mod <- lm(Passengers~t , data = train) #Here we are building a linear model for Passengers using the train data
summary(lin_mod)
lin_pred <- data.frame(predict(lin_mod, interval = "predict", newdata = test)) #Here we are pedicting the values for test set
View(lin_pred)
#Lets find the RMSE value for Linear Model
rmse_lin_model <- sqrt(mean((test$Passengers - lin_pred$fit)^2, na.rm = T))
rmse_lin_model


# 2. Exponential Model

exp_mod <- lm(log_passenger~t, data = train) #Here we are Building a Exponential Model
summary(exp_mod)
exp_pred <- data.frame(predict(exp_mod, interval = "predict", newdata = test))
View(exp_pred)
#Lets find the RMSE value
rmse_exp_mod <- sqrt(mean((test$Passengers - exp(exp_pred$fit))^2, na.rm = T))
rmse_exp_mod


# 3. Quadratic Model

Quad_model<-lm(Passengers~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Passengers-Quad_pred$fit)^2,na.rm=T))
rmse_Quad

# 4. Additive Seasonality model

sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add


#5. Additive Seasonality with Linear Regression model

Add_sea_Linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Passengers-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear

# 6. Additive Seasonality with Quadratic model

Add_sea_Quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad

# 7. Multiplicative Seasonality Model

multi_sea_model<-lm(log_passenger~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea

# 8. Multiplicative Seasonality with Linear regression Model

multi_sea_lin_model<-lm(log_passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_lin_model) 
multi_sea_lin_pred<-data.frame(predict(multi_sea_lin_model,newdata=test,interval='predict'))
rmse_multi_sea_lin<-sqrt(mean((test$Passengers-exp(multi_sea_lin_pred$fit))^2,na.rm = T))
rmse_multi_sea_lin


#Lets make a Table for the Models and its RMSE value
table_rmse<-data.frame('Model'=c("Linear Regression Model","Exponential Model","Quadratic Model","Additive Seasonality model","Additive Seasonality with Linear Regression model","Additive Seasonality with Quadratic model","Multiplicative Seasonality Model","Multiplicative Seasonality with Linear regression Model"),'RMSE'=c(rmse_lin_model,rmse_exp_mod,rmse_Quad,rmse_sea_add,rmse_Add_sea_Linear,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_sea_lin))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

#So after Comparing the RMSE values of all the above models, Multiplicative Seasonality with Linear Regression Model has the
#least RMSE value. So this model is the Best Model among all the others and can be used for forecasting
multi_sea_lin_model_whole <- lm(log_passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = new_data)

#Lets get the Residuals
resid <- residuals(multi_sea_lin_model_whole)
resid[1:10]

windows()
hist(resid)

windows()
acf(resid,lag.max = 10)
# By principal of parcimony we will consider lag - 6  as we have so many significant lags 
# Building Autoregressive model on residuals consider lag-1 
k <- arima(resid, order=c(6,0,0))

windows()
acf(k$residuals,lag.max = 15)
pred_res<- predict(arima(resid,order=c(1,0,0)),n.ahead = 120)
str(pred_res)
pred_res$pred
acf(k$residuals)

#Lets Build a ARIMA model for whole dataset. ARIMA(Auto Regression Integrated Moving Average)

#Lets convert the data into Time Series data

library(tseries)
library(forecast)

airline_ts <- ts(airlines$Passengers, frequency = 12, start = c(1995)) #Create a Time Series data
View(airline_ts)
plot(airline_ts) #Plots the data into a Line chart By default as the data is a Time Series Data

#For Building ARIMA model we the AR coefficient i.e p-value then Integration Coefficient i.e d and Moving Average coefficient i.e q-value
#Lets find p-value, p-valueis Obtained by pacf
pacf(airline_ts) #Lets Consider it as 1

#Lets Find the q-value by acf
acf(airline_ts) #Lets Consider this as 1

#Also lets Consider the d-value as 1
#now lets build an ARIMA model
a <- arima(airline_ts, order = c(1,1,1), method = "ML")
a
#Lets plot the forecast using the ARIMA model
plot(forecast(a,h=5), xaxt = "n")

#Seeing the plot, we get to know that the forecast done was not accurate. This happens when we dont provide the right
# p-value and q-value to the model.

#If we dont know the p-vale and q-value for the model then we can build the model using auto.arima() function.
#This function will analyse the p and q value and build a proper model. Lets Build the Model

ab <- auto.arima(airline_ts)

windows()
plot(forecast(ab, h=12), xaxt = "n")
#So now we can see that the forecast was accurate 

prediction <- forecast(ab, h=12) #This will predict for the next 12 months
