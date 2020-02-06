1+1
2+3*4
3^2
exp(1)
exp(2)
sqrt(4)
pi
2*pi*6378
x<-10
y<-3
z<-15
x*y*z
x<-1
y<-3
z<-4
x*y*z
X*Y*Z
This.Year<-2004
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
x<9 #checks if x<9
y[4]
y<9
x[x<1]<-2 #this will change all the elements to 2ifthey are ess than 1
x
x[x<4]<-1 #this will change all the elements to 1 if they are less than 4
x

df<-data.frame(x=1:3,y=c("a","b","c")) # creating a data frame
df1<-data.frame(height=c(150,160),weight=c(65,72))
df
df1
df[1,c(1,2)] #select the 1st row from column 1 and 2
df[2,2]
df[3,c(1,2)]
df[1,3]
df[1,c(1,2)]
df[3,c(1,2)]
df[1,1]
df[3,1]
df[3,1]
df[3,2]
c(df[1,1],df[3,1])

v<-c(1,2,3,"a","b")
v
v1<-c(1,2,3)
v2<-c("1","2","3")
mean(v1)
mean(v2)
df[3,]
df[,2]
