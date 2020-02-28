#Association Rule

install.packages("arules") #Install this package for Association Rules
library(arules)

Titanic <- Titanic[,-1] #Remove the first column

rules<- apriori(Titanic) #Apriori Algorithm to find the Association patterns of the data

arules::inspect(rules) #To see the Patterns formed by Apriori Algorithm

rules.sorted <- sort(rules, by = "lift") #To sort the data based on Lift Ratio

arules::inspect(rules.sorted) #To view the sorted pattern 

#Rules with rhs containing "Suvived"only'

rules <- apriori(Titanic, parameter = list(minlen=1,supp = 0.1, conf = 0.5), appearance = list(rhs=c("Survived=Yes","Survived=No")), control = list(verbose = F)) #This is to modify the Appearance of the Output Where i want the Pattern to have the Survived values yes and No on the right hand side

arules::inspect(rules) #View the Patterns 