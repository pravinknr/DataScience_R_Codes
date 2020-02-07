datasets::CO2 #dataset named CO2

head(CO2) #displays the first 6 rows of the Dataset

tail(CO2)#displays the last 6 rows of the dataset

summary(CO2)#calculates the mean, median, max, min, quartiles of every column


CO2[,c(1,2,4)] #displays all the rows of the 1st 2nd and 4th column

CO2[20,c(1,5)] #displays 20 rows of the 1st and 5th column

CO2[1:20,c(1,5)] #displays 20 rows of the 1st and 5th column

summary(CO2$uptake)    #calculates the mean, median, max, min, quartiles of the specified column

summary(CO2$conc)   #calculates the mean, median, max, min, quartiles of the specified column

summary(CO2$Treatment)  #calculates the mean, median, max, min, quartiles of the specified column

plot(CO2$conc, type = "l", col="red", xlab = "Plant", ylab = "Concentration of CO2", main="CO2 Concentration in plants") #plots the grapgh represented by lines

plot(CO2$uptake, type = "s", col="green") #plots the graph represented by scales

barplot(CO2$conc, horiz = FALSE) #plots the graph represented by bar

barplot(CO2$uptake, xlab = "Plants", ylab = "Uptake of Plants", col = "blue") #plots the graph represented by bar in blue color and labels

hist(CO2$uptake, col="orange") #plots the graph epresented by histogram in orange color

boxplot(CO2) #plots the graph represented with a boxplot to find the outliers

boxplot(CO2$conc) #plots the graph represented with a boxplot of the column conc to find the outliers

boxplot(CO2[,c(4,5)]) #plots the graph represented with a boxplot of the columns 4 and 5 to find the outliers

par(mfrow=c(4,4),mar=c(2,5,2,2), bty="n", las=0) #creates a 4*4 partition represented as grids with each grid containing the following graph 
plot(CO2$conc, type = "l", col="red", xlab = "Plant", ylab = "Concentration of CO2", main="CO2 Concentration in plants")

plot(CO2$uptake, type = "s", col="green")

barplot(CO2$conc, horiz = FALSE)

barplot(CO2$uptake, xlab = "Plants", ylab = "Uptake of Plants", col = "blue")

hist(CO2$uptake, col="orange")

boxplot(CO2)

boxplot(CO2$conc)

boxplot(CO2[,c(4,5)])

plot(CO2$uptake, type = "p", col="green")

plot(CO2$conc, type = "c", col="red", xlab = "Plant", ylab = "Concentration of CO2", main="CO2 Concentration in plants")

plot(CO2$Treatment, col="pink")
