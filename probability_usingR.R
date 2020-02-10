ab<-pnorm(60,60,10) #probability of the variable 60 where the mean is 60 and standard deviation is 10

1-ab #for calculating the probability of X greater than the given number

pnorm(680,711,29) #finds the probability of the given number where the mean is 711 and std deviation is 29

 pnorm(697:740,711,29) #finds the probability ofall the numbers between 697 and 740 where the mean is 711 and std deviation is 29
 
a<-1-pnorm(697,711,29) #finds the probability of X greater than 697 with mean 711 and std deviation 29
b<-pnorm(740,711,29) #finds the probability of the given number where the mean is 711 and std deviation is 29
b-a


pnorm(140,1990,2833) #find the probability of the given number with mean 1990 and standard deviation 2833

1990-1.960*2500/sqrt(140) #this is to find the lowest point of the confidence interval where mean(1990) is subtracted with normalised value of the confidence percentage(1.960) which is multiplied by the standard deviation(2500) divided by n-1 where n is the total number of objects

1990+1.645*2500/sqrt(140) #this is to find the hghest point of the confidence interval where mean(1990) is subtracted with normalised value of the confidence percentage(1.645) which is multiplied by the standard deviation(2500) divided by n-1 where n is the total number of objects


qt(0.975,139) #finds the normalised value of the confidence percent 95% where 139 is n-1


qt(0.975,15)  #finds the normalised value of the confidence percent 95% where 15 is n-1
