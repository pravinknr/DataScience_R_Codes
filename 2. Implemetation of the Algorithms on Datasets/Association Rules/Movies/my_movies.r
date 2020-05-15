#Association Rule

#Prepare rules for the all the data sets 
#1) Try different values of support and confidence. Observe the change in number of rules for different support,confidence values
#2) Change the minimum length in apriori algorithm
#3) Visulize the obtained rules using different plots 

library(arules,arulesViz)

#Lets Import the Data
movies <- read.csv(file.choose()) 
summary(movies)
#So here we have the Data represented in Transaction as well as Binary Variables
#we will consider the Binary Variables to prepare the Rules

rule1 <- apriori(as.matrix(movies[,6:15]), parameter = list(supp = 0.05, conf = 0.8))
rule1 #77 rules are Prepared
inspect(head(rule1))
toprules <- head(rule1, by = "confidence", n=20)
rule1_lift <- sort(rule1, by = "lift", decreasing = TRUE)
rule1_conf <- sort(rule1, by = "confidence", decreasing = TRUE)
plot(rule1_conf, method = "grouped")

plot(toprules, method = "graph", engine = "htmlwidget")

#In this plot we can see that the movie LOTR movies was watched the most along with Green mile and Gladiator

#Lets create Other Rules with Different Parameters and Plot them
rule2 <- apriori(as.matrix(movies[,6:15],parameter=list(support=0.8, confidence = 1,minlen=6)))
rule2 #set of 77 rules was Created
inspect(head(rule2))
plot(rule2, method = "scatterplot")

rule3 <- apriori(as.matrix(movies[,6:15],parameter=list(support=0.6, confidence =0.7,minlen=5)))
rule3
inspect(head(rule3))
plot(rule3, method = "two-key plot")

plot(rule3, method = "paracoord")
