#Salary Hike

#Salary_hike -> Build a prediction model for Salary_hike

#Do the necessary transformations for input variables for getting better R^2 value for the model prepared.

#Lets Import the Data
salary <- read.csv("E:\\Data Science\\Assignment\\Simple Linear Regression\\Salary Data\\Salary_Data.csv")
attach(salary)
head(salary)
tail(salary)
summary(salary) #Gives the Summary of the dataset

sd(YearsExperience) #Gives Standard Deviation
sd(Salary)

var(YearsExperience) #Gives Variance
var(Salary)

cor(salary) #gives the Correlation Coefficient as a matrix

plot(salary)

#Lets Build a Linear Model
model <- lm(Salary~., data = salary)
plot(model)
summary(model)
#Here the R-Square value is 0.957 with Residual Standard Error as 5788

#Lets do Data Tranformation and build Different Models
#Squareroot Transformaton
sqrt_model<-lm(sqrt(Salary)~.,salary)
summary(sqrt_model)
#Here R-Square value is 0.931 with Residual Standard Error as 7399

#Log Transformaton
log_model<-lm(log(Salary)~.,data=salary)
summary(log_model)
#Here we got the R-Square value as 0.932 with Residual Standard Error as 0.09789

#So we Conclude that Log transformation model has better R-Square value with the lowest Residual Standard Error