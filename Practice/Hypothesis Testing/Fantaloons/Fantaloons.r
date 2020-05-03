#Hypothesis Testing

#h0: = (Proportion of male and female is same)
#ha: != (Proportion of male and female is not same)

fantaloons<- read.csv(file.choose())
summary(fantaloons)

fanta <- as.data.frame(as.factor(fantaloons$Weekdays), as.factor(fantaloons$Weekend))

fanta1 <- fantaloons
fanta1$Weekdays <- as.factor(fanta1$Weekdays)
fanta1$Weekend<- as.factor(fanta1$Weekend)
summary(fanta1)

fanta2 <- data.frame("Weekdays"=c(287,113), "Weekend" = c(233,167))
row.names(fanta2) <- c("Female","Male")

#now lets use the chisq.test() to test the hypothesis on fanta2
chisq.test(fanta2)

#Here we can see that x-squared value is 15.434 and the p value is less than 0.05 i.e 5% significance level
#so we will reject the Null Hypothesis
#Hence the Proportion of Male and Female are not same