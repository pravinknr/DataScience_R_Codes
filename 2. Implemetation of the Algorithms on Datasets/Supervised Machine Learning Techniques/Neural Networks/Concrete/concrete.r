#Neural Networks

#Prepare a model for strength of concrete data using Neural Networks

library(neuralnet)

concrete <- read.csv(file.choose())
attach(concrete)
summary(concrete)

names(concrete)

cor(concrete)

#Standard deviation
sd(cement)
sd(slag)
sd(ash)
sd(water)
sd(superplastic)
sd(coarseagg)
sd(fineagg)
sd(age)
sd(strength)

#Variance
var(cement)
var(slag)
var(ash)
var(water)
var(superplastic)
var(coarseagg)
var(fineagg)
var(age)
var(strength)

#Plots
boxplot(concrete)

pairs(concrete)

histogram(strength, color = "purple", title = "Strength")

hist(age, main = "Age of Cement", col = "red")

#Lets Normalise the data
normalise <- function(x){
  return((x - min(x))/(max(x) - min(x)))
}

conc_norm <- as.data.frame(lapply(concrete, normalise))

#lets Split the Data Randomly in test and train set
split1 <- sort(sample(nrow(conc_norm), nrow(conc_norm)*0.8))
train1 <- conc_norm[split1,]
test1 <- conc_norm[-split1,]

#Lets build the Neural Network Model
n1 <- neuralnet(formula = strength~. , data = train1)
plot(n1)
n1_result <- compute(n1, test1[1:8])
str1 <- n1_result$net.result
round(cor(test1[,9],str1)*100, digits = 2)
#The Accuracy of the Model is 82.47%

#Lets Improve the Accuracy of the Model by Building complex Neural Network
n2 <- neuralnet(formula = strength~. , data = train1, hidden = c(5,3))
plot(n2)
n2_result <- compute(n2, test1[1:8])
str2 <- n2_result$net.result
round(cor(test1[,9],str2)*100, digits = 2)
#The Accuracy of the Model is 93.08%

n3 <- neuralnet(formula = strength~. , data = train1, hidden = c(5,5,3))
plot(n3)
n3_result <- compute(n3, test1[1:8])
str3 <- n3_result$net.result
round(cor(test1[,9],str3)*100, digits = 2)
#The Accuracy of the Model is 92.69%

n4 <- neuralnet(formula = strength~. , data = train1, hidden = c(5,4,3,2))
plot(n4)
n4_result <- compute(n4, test1[1:8])
str4 <- n4_result$net.result
round(cor(test1[,9],str4)*100, digits = 2)
#The Accuracy of the Model is 93.42%

n5 <- neuralnet(formula = strength~. , data = train1, hidden = c(6,5,4,3,2))
plot(n5)
n5_result <- compute(n5, test1[1:8])
str5 <- n5_result$net.result
round(cor(test1[,9],str5)*100, digits = 2)
#The Accuracy of the Model is 93.73%
