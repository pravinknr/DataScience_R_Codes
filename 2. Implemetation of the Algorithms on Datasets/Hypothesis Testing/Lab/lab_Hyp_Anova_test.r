#Hypothesis Testing

#H0: = 0 (Average TAT are same)
#Ha: !=0 (Average TAT are not same)

lab <- read.csv(file.choose())

lab1 <- lab$Laboratory.1
lab2 <- lab$Laboratory.2
lab3 <- lab$Laboratory.3
lab4 <- lab$Laboratory.4

summary(lab)
sd(lab1)
sd(lab2)
sd(lab3)
sd(lab4)

combined_group <- data.frame(cbind(lab1,lab2,lab3,lab4))
combined_group

stacked_group <- stack(combined_group)
stacked_group

anova_result <- aov(stacked_group$values~. , data = stacked_group)
anova_result
summary(anova_result)
#The p value is less than 0.05 i.e 5% significance level so we reject H0 and the result is 
#there is a difference in the average Turn Around Time (TAT) of reports of the laboratories on their preferred list. 
