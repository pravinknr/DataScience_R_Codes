#Salary Hike

salary <- read.csv("C:\\Users\\pravi\\Desktop\\Assignment\\Simple Linear Regression\\Salary_Data.csv")
summary(salary)

sd(salary$YearsExperience)
sd(salary$Salary)

var(salary$YearsExperience)
var(salary$Salary)


model <- lm(salary$Salary~., data = salary)
plot(model)

summary(model)
