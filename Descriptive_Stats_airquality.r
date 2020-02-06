airquality<-datasets::airquality
airquality

head(airquality) #displays the first 6 rows of the dataset

tail(airquality) #displays the last 6 rows of the dataset

airquality[,c(1,2)] #displays the first two columns with all the rows of the dataset

airquality[,c(5,6)] 

airquality$Wind #displays the specific colum data of the dataset

summary(airquality$Temp) #displays the mean median max min max data of the particular column

summary(airquality$Ozone)

summary(airquality$Wind)

plot(airquality$Ozone) #plots the values of the column ozone on a graph

plot(airquality$Ozone,airquality$Temp) #plots the mentioned columns in a single graph

plot(airquality) #plots the entire dataset

summary(airquality)


#plot in the form of points and lines
plot(airquality$Ozone, type = "l",xlab = "ozone concentration", ylab = "No of Instances", main = "Ozone levels in NY city",col="blue")

plot(airquality$Temp,type="b", xlab = "Temperature", ylab = "No of instances", main = "Temperature in NY city" ,col="red")
 #Barplot
barplot(airquality$Wind,main = "Wind Levels",xlab = "wind level", col="green", horiz = FALSE)

#Histogram
hist(airquality$Solar.R)

#single boxplot
boxplot(airquality$Ozone)

#boxplot of the whole dataset
boxplot(airquality)

#boxplot of multiple columns
boxplot(airquality[,1:4], main="multiple")
boxplot(airquality[,c(1,5)])
boxplot(airquality[,c(1,3,5)])

# to plot multiple graphs on a single window
par(mfrow=c(3,3),mar=c(2,5,2,1),bty="n", las=0)
plot(airquality$Ozone,airquality$Temp)
plot(airquality$Solar.R,type = "c")
plot(airquality$Temp, type = "h")
ithuplot(airquality$Day,type = "s")
barplot(airquality$Wind,main = "Wind Levels",xlab = "wind level", col="green", horiz = FALSE)
hist(airquality$Solar.R)
boxplot(airquality[,1:4], main="multiple")