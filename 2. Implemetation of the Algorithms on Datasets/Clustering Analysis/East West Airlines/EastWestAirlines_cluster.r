#Clustering Algorithm
#We will DO Heirarchical as well as K-Means Clustering

#Perform clustering (Both hierarchical and K means clustering) for the airlines data to obtain optimum number of clusters. 
#Draw the inferences from the clusters obtained.

#Data Description:
  
# The file EastWestAirlinescontains information on passengers who belong to an airline's frequent flier program. For each passenger the data include information on their mileage history and on different ways they accrued or spent miles in the last year. The goal is to try to identify clusters of passengers that have similar characteristics for the purpose of targeting different segments for different types of mileage offers

#Use the Below Library to import Excel Files 
library(xlsx)
airlines <- read.xlsx("E:\\Data Science\\Assignment\\Clustering\\East West Airlines\\EastWestAirlines.xlsx", 2)
summary(airlines)
airlines1 <- (scale(airlines[2:12]))

d <- dist(airlines1, method = "euclidean")
d
#Using Centroid method
heirarchical_centroid <- hclust(d, method = "centroid")

plot(heirarchical_centroid)

clusterGroup <- cutree(heirarchical_centroid,10)

rect.hclust(heirarchical_centroid,5,border = "blue")

heirarchical_centroid_Data <- data.frame(airlines[,1],clusterGroup)

table(clusterGroup) #To see the Number of Customers in Each Group

#Using Average Linkage Method
Heirarchical_Average <- hclust(d, method = "average")

plot(Heirarchical_Average)

clustergroup <- cutree(Heirarchical_Average, 10)

rect.hclust(Heirarchical_Average, 8,border = "Blue")

Heirarchical_Average_data <- data.frame(airlines[,1], clustergroup)

table(clustergroup)

#Now lets do K-Means Clustering on the data

wss <- c()
for (i in 2:15) wss[i]<- sum(kmeans(airlines1, centers = i)$withinss)
plot(1:15,wss,type = "b", xlab = "No of Clusters", ylab = "Avg Distance")

#Using the Elbow Plot we got the k value as 5

k_means_airline <- kmeans(d,5)

#If you want to See the Animated view of the Clusters then run the below Statements
install.packages("animation")
library(animation)
windows()
k_means_airline <- kmeans.ani(d,5)

k_means_airline_clusters <- data.frame(airlines[,1], k_means_airline$cluster)

############### Lets Perform Different Distance  Methods on the data########

d.manhat <- dist(airlines1, method = "manhattan")
d.manhat

library(factoextra)

d.pearson <- get_dist(airlines1, method = "pearson")
d.pearson

d.kendall <- get_dist(airlines1, method = "kendall")
d.kendall

d.spearman <- get_dist(airlines1, method = "spearman")
d.spearman

##### Lets perform various Clustering using these distances ####

sing.clust <- hclust(d.manhat, method = "single") #Single Linkage Method
fviz_dend(sing.clust)
sing.clust.cuttree <- cutree(sing.clust, k=4)
sing.clust.data <- data.frame(airlines[,1],"cluster"=sing.clust.cuttree)
sing.clust.data

comp.clust <- hclust(d.manhat, method = "complete") #Complete Linkage Method
fviz_dend(comp.clust)
comp.cuttree <- cutree(comp.clust, k=5)
comp.clust.data <- data.frame(airlines[,1],"cluster"=comp.cuttree)
comp.clust.data

#For Density Based Clustering
install.packages("fpc")
install.packages("dbscan")
library(fpc)
library(dbscan)

#To determine the eps value: dbscan::kNNdistplot(df, k =  5)
#abline(h = 0.15, lty = 2)

dens.clust <- dbscan(d.pearson, minPts = 5, eps = 0.15)
fviz_cluster(dens.clust,data = airlines1, palette ="jco", geom = "point", ggtheme = theme_classic())
dens.clust.data <- data.frame(airlines[,1], "cluster"=dens.clust$cluster)
dens.clust.data
#Cluster 0 corresponds to Outliers

#Model Based Cluster

library(mclust)
model.based <- Mclust(d.pearson)
summary(model.based)

model.based$modelName #Returns the name of the model
model.based$G #Returns the total number of Clusters

fviz_mclust(model.based, "BIC",  palette = "jco")
fviz_mclust(model.based, "classification", geom = "point",palette="jco")
fviz_mclust(model.based,"uncertainty", palette = "jco")

#Fuzzy Clustering

library(cluster)

# fanny(x, k, metric = "euclidean", stand = FALSE)
# x: A data matrix or data frame or dissimilarity matrix
# k: The desired number of clusters to be generated
# metric: Metric for calculating dissimilarities between observations
# stand: If TRUE, variables are standardized before calculating the dissimilarities

fuz <- fanny(airlines1, 3) #Fuzzy Cluster
fuz$clustering #Returns the Cluster for each value
fuz$membership #Returns the membership Coefficient for each value
fviz_cluster(fuz, ellipse.type = "norm", repel = TRUE,palette = "jco", ggtheme = theme_minimal(),legend = "right")
fuz.data <- data.frame(airlines[,1], "cluster"=fuz$clustering)
fuz.data


#Partitioning around Medoids (PAM) Also Called K-Medoids Algorithm for Clustering

#library("cluster","factoextra")
pammodel <- pam(airlines1,3, metric = "manhattan",stand = FALSE)
pammodel$medoids
pammodel$clustering
fviz_cluster(pammodel, palette="jco", repel = TRUE, ggtheme = theme_classic(), legend = "right")
pammodel.data <- data.frame(airlines[,1], "Cluster"=pammodel$clustering)
pammodel.data

