#Clustering for Crime Data

#Perform Clustering for the crime data and identify the number of clusters formed and draw inferences.


#Lets Do Heirarchical Clustering
crime <- read.csv(file.choose())
summary(crime)

crime1<- scale(crime[,2:5])

d<-dist(crime1, method = "euclidean")
d

#Lets do it Using Centroid Linkage Method
cluster1 = hclust(d, method = "centroid")
plot(cluster1)

tree1 <- cutree(cluster1,k= 4)
tree1

rect.hclust(cluster1, k=4, border = "red")

groups <- data.frame("City"=crime[,1],"Cluster Number"=tree1)

#Lets Use Average Linkage Method

cluster2 <- hclust(d, method = "average")
plot(cluster2)

tree2 <- cutree(cluster2, k=4)

rect.hclust(cluster2,k=4, border="blue")

group2 <- data.frame("City"=crime[,1], "cluster number"= tree2)
group2

#Lets Do the Clustering With KMeans
#Lets Find the K value With the help of Elbow Plot
wss <- c()
for (i in 2:15) wss[i]<- sum(kmeans(d, centers = i)$withinss)
plot(1:15,wss,type = "b", xlab = "No of Clusters", ylab = "Avg Distance")

#Lets take the Value of K as 3 as obtained by Elbow Plot

k_mean_cluster <- kmeans(d,3)
k_mean_cluster$centers
k_mean_cluster$cluster

final_Cluster_info <- data.frame("City"=crime[,1], "Cluster"=k_mean_cluster$cluster)


############### Lets Perform Different Distance  Methods on the data########

d.manhat <- dist(crime1, method = "manhattan")
d.manhat

library(factoextra)

d.pearson <- get_dist(crime1, method = "pearson")
d.pearson

d.kendall <- get_dist(crime1, method = "kendall")
d.kendall

d.spearman <- get_dist(crime1, method = "spearman")
d.spearman

##### Lets perform various Clustering using these distances ####

sing.clust <- hclust(d.manhat, method = "single") #Single Linkage Method
fviz_dend(sing.clust)
sing.clust.cuttree <- cutree(sing.clust, k=4)
sing.clust.data <- data.frame(crime[,1],"cluster"=sing.clust.cuttree)
sing.clust.data

comp.clust <- hclust(d.manhat, method = "complete") #Complete Linkage Method
fviz_dend(comp.clust)
comp.cuttree <- cutree(comp.clust, k=5)
comp.clust.data <- data.frame(crime[,1],"cluster"=comp.cuttree)
comp.clust.data

#For Density Based Clustering
install.packages("fpc")
install.packages("dbscan")
library(fpc)
library(dbscan)

#To determine the eps value: dbscan::kNNdistplot(df, k =  5)
#abline(h = 0.15, lty = 2)

dens.clust <- dbscan(d.pearson, minPts = 5, eps = 0.15)
fviz_cluster(dens.clust,data = crime1, palette ="jco", geom = "point", ggtheme = theme_classic())
dens.clust.data <- data.frame(crime[,1], "cluster"=dens.clust$cluster)
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

fuz <- fanny(crime1, 3) #Fuzzy Cluster
fuz$clustering #Returns the Cluster for each value
fuz$membership #Returns the membership Coefficient for each value
fviz_cluster(fuz, ellipse.type = "norm", repel = TRUE,palette = "jco", ggtheme = theme_minimal(),legend = "right")
fuz.data <- data.frame(crime[,1], "cluster"=fuz$clustering)
fuz.data


#Partitioning around Medoids (PAM) Also Called K-Medoids Algorithm for Clustering

#library("cluster","factoextra")
pammodel <- pam(crime1,3, metric = "manhattan",stand = FALSE)
pammodel$medoids
pammodel$clustering
fviz_cluster(pammodel, palette="jco", repel = TRUE, ggtheme = theme_classic(), legend = "right")
pammodel.data <- data.frame(crime[,1], "Cluster"=pammodel$clustering)
pammodel.data
