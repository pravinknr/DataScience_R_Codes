#Survival Analysis

install.packages("survival")
library(survival)
survival_unemployment1<-read.csv(file.choose()) #choose the survival_unemployment1.csv file

attach(survival_unemployment1) #So that we can access the column names without addressing the dataset name and $ sign

# Define variables 
time <- spell
event <- event
X <- cbind(logwage, ui, age)
group <- ui
# Kaplan-Meier non-parametric analysis
kmsurvival <- survfit(Surv(time,event) ~ 1)
summary(kmsurvival)
plot(kmsurvival, xlab="Time", ylab="Survival Probability")
# Kaplan-Meier non-parametric analysis by group
kmsurvival1 <- survfit(Surv(time, event) ~ group)
summary(kmsurvival1)
plot(kmsurvival1, xlab="Time", ylab="Survival Probability")
