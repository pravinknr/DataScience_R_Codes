#EMP data

#Emp_data -> Build a prediction model for Churn_out_rate

#Do the necessary transformations for input variables for getting better R^2 value for the model prepared.

#Lets Import the Data
emp <- read.csv("E:\\Data Science\\Assignment\\Simple Linear Regression\\Employee Data\\emp_data.csv")

summary(emp) #Gives the Summary of the dataset
sd(emp$Salary_hike) #Gives Standard Deviation
var(emp$Churn_out_rate) #Gives Variance

sd(emp$Churn_out_rate)
var(emp$Salary_hike)

plot(emp)

#Lets build a Linear Model
model <- lm(emp$Churn_out_rate~., data = emp)
plot(model)
summary(model)
#Here the P-value is less than 0.05 and the R-Squared value is 0.8312 with Residual Standard Error as 4.469

#Lets do Data Transformation and build models
#Squareroot Transformation
sqrt_model<-lm(emp$Churn_out_rate~.,sqrt(emp))
summary(sqrt_model)
#Here the R-Square value is 0.84 with Residual Standard Error as 4.351

#Log Transformation
log_model<-lm(log(emp$Churn_out_rate)~.,data=emp)
summary(log_model)
#Here we get R-Square value as 0.8735 and Residual Standard Error as 0.0519

#we Conclude that Log model is the best model with Highest R-Square value and lowest Residual Standard Error