#Hypothesis Testing

#h0: = (Male-female ratio is equal across all regions)
#ha: != (Male-female ratio is not equal accross regions)

#Lets import the data
buyer_ratio <- read.csv(file.choose())
summary(buyer_ratio)

#Lets remove the first column
buyer1 <- buyer_ratio[,2:5]

#As we can see that we have categorical data in the dataset, we will do the chisq.test() to see 
#if there is correlation between the columns

test <- chisq.test(buyer1)
test

#Here we get to see that the x-squared value is 1.5959 and the p-value is 0.6603 which is greater than the 5%
#Significance level so we do not reject null hypothesis.
#we conclude that the Male-Female buyer Ratio's are similar accross regions