
#Clustering Similar Data together to understand the data 

#Heirrachical Clustering

#Load the dataset named Universities.csv

mydata <- Universities

mydata1<- scale(mydata[,2:7]) #Scale the vaues of data from column 2 to 7

d <- dist(mydata1, method = "euclidean") #Using the Euclidean method to find the distance between the Data. #Distance Matrix

#Lets do the Clustering using Average Linkage

fit <- hclust(d, method = "average") #Clustering the data using Heirarchical Clustsering using Average Linkage method

plot(fit) #Displays the Dendogram

groups <- cutree(fit,k=4) #This will cut the tree into 4 clusters

rect.hclust(fit,k=4, border = "red") #Draw red border around 4 clusters

clusters=data.frame("uni"=mydata[,1], "clusters"=groups) #Attach the cluster numbers to University

#Lets do the clustering using Centroid Linkage

fit1 <- hclust(d, method = "centroid") #Clustering the data using Heirarchical Clustsering using centroid Linkage method

plot(fit1) #Displays the Dendogram

groups <- cutree(fit1,k=4) #This will cut the tree into 4 clusters

rect.hclust(fit1,k=4, border = "red") #Draw red border around 4 clusters

clusters=data.frame("uni"=mydata[,1], "clusters"=groups) #Attach the cluster numbers to Universities
