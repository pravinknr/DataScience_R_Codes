#EMP data

emp <- read.csv("C:\\Users\\pravi\\Desktop\\Assignment\\Simple Linear Regression\\emp_data.csv")

summary(emp)
sd(emp$Salary_hike)
var(emp$Churn_out_rate)

sd(emp$Churn_out_rate)
var(emp$Salary_hike)


model <- lm(emp$Churn_out_rate~., data = emp)
plot(model)
summary(model)
#Here the P-value is less than 0.05 and the R-Squared value is 0.8312 that means that the output will be 83.12% time correct. 