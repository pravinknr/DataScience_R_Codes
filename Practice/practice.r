#Basic calculations in R
1+1
2+3*4

3^2

#Exponentials
exp(1)
exp(2)

#squareroot
sqrt(4)

pi #value of pi

2*pi*6378

x<-10 #assigns x value as 10
y<-3 #assigns y value as 3
z<-15 #assigns z value as 15
x*y*z #multiplies x, y, z

#Assigning values
x<-1
y<-3
z<-4

#Multiplication
x*y*z
X*Y*Z

This.Year<-2004 #assigning value
This.Year

#vector construction

vect<-c(1,2,3,4,5)

x<-c(2,0,0,4)
y<-c(1,9,9,9)
x+y
x*4
sqrt(x)
y[1]

x[1] #select 1st element of the vector x
x[-1] #Exclude 1st element of the vector x
x[1]<-3 #chnage the 1st element of vector as 3
x
x[-1]<-5 #change all the elements of x as 5 except 1st element
x
x<9 #checks if x is lessthan 9
y[4] #selects 4th element of the vector y
y<9 #checks if y is less than 9
x[x<1]<-2 #this will change all the elements to 2 if they are 1ess than 1
x
x[x<4]<-1 #this will change all the elements to 1 if they are less than 4
x

df<-data.frame(x=1:3,y=c("a","b","c")) # creating a data frame
df1<-data.frame(height=c(150,160),weight=c(65,72)) #creating a data frame
df
df1
df[1,c(1,2)] #select the 1st row from column 1 and 2
df[2,2] #selects second row from second column
df[3,c(1,2)] #selects 3rd row from column 1 and 2
df[1,1] # selects 1st row from first column
df[3,1] #selects 3rd row from first column
df[3,2] #selects 3rd row from 2nd column

v<-c(1,2,3,"a","b") #Construction of a vector
v
v1<-c(1,2,3) #Construction of a vector
v2<-c("1","2","3") #construction of a vector

mean(v1) #find the mean of the specified dataset
mean(v2)#finds the mean of the specified dataset

df[3,] #selects 3rd row from all the columns

df[,2] #selects all the rows from column 2
