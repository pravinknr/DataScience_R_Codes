#Hypothesis Testing
#Hypothesis testing is done to make decisions
#h0 = NULL #This is condition 1
#ha = Alternative #this is condition 2

#This is a manual way of Hypothesis Testing

#n = no of observations of sample space
#S = Std Devition of sample space
#mean = mean of sample space
#mew = mean of population
#Sigma = Std DEviation of population

#Alpha = 0.05 #Alpha value is by default 0.05

#If we have population parameters then we should do Z test #(Std Deviation is important)
#If we dont have population Parameters we should do T test 

#The formula for Z test is  Z = mean - mew/ sigma

#The Formula for T test is t = mean - mew / (Std Deviation/sqrt(n))


#Lets take an Example

mean<- 122
mew<- 130
StdDev1<- 40
n<-80
#Here we dont have the population Parameter i.e Sigma so we will do T Test

#h0<- mew <= 120 #Condition 1
#ha<- mew > 120 #Condition 2

q1<-122-120

q2<-40/sqrt(80)

t1<- q1/q2 #This will give me the t value

#To find the probability of t Value we should use the function pt(t,n-1)

t2<-pt(t1,79) #Here we find the probability of the t value with respect to n. #here t1 is the t value and 79 is n-1

t3<- 1-t2 # we subtract the Probability value with 1 to find x > mean

#Now we compare the value of p with Alpha
# if p < Alpha, we should reject h0
#if p > Alpha , We should consider the condition ho and make decisions

#In this example p value is greater than 0.05, So we will Consider the h0 Condition and take Decision


#Example 1
day1<- -0.3 
day11 <- 3/sqrt(50)
day111<- day1/day11 #t value of the above values

pday1<- pt(day111,49)
pday11<-2*pday1 #pvalue of the t

#Example 2
day2<- 4.1-4
day22<- 3/sqrt(50)
day222<- day2/day22

pday2<- 1-pt(day222,49)
pday22 <- 2*pday2           



#To do the Hypothesis Testing in R we have the Following Function
# t.test(x,alternative = c("less","greater","both.sided"),mu="standard mean value") #Here Alternative is ha where we have to speify the condition is lesser or greater or equal

#lets take an example

x<- c(0.593,0.142,0.329,0.691,0.793,0.519,0.392,0.418)
t.test(x,alternative = "greater",mu=0.3)
