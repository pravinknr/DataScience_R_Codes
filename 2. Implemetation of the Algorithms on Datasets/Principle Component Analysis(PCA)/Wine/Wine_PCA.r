#Perform Principal component analysis and perform clustering using 
#first 3 principal component scores (both heirarchial and k mean clustering(scree plot or elbow curve) and obtain 
#optimum number of clusters and check whether we have obtained same number of clusters with the original data 
#(class column we have ignored at the begining who shows it has 3 clusters)df
 
#Lets import the data

wine <- read.csv(file.choose())
summary(wine)

#Now Lets perform PCA on the wine dataset. We will ignore the first column of the dataset as it contains the class of the wine

wine1 <- wine[,2:14]

wine_pca <- princomp(wine1, cor = TRUE, scores = TRUE, covmat=NULL)
summary(wine_pca)

wine_pca$loadings
wine_pca$scores
scores <- wine_pca$scores

plot(wine_pca$scores[,1:2], col = "white", pch = 19, cex = 1)
#If we want the text values of the plot then we use the text function as below
text(wine_pca$scores[,1:2], labels = c(1:178), cex = 1)

lets do some more plottings with different coluns of scores
plot(wine_pca$scores[,2:3], col = "blue", cex=1)

plot(wine_pca$scores[,5:6], col = "purple", cex = 1)

#Lets plot all the columns of Scores
pairs(wine_pca$scores)

#Now as we have got the Scores using PCA, lets perform clustering and determine whether the predicted and actual class are same or different
#We have to perform Clustering using the first 3 principle components

install.packages(c("FactoMineR", "factoextra"))
library(factoextra)
library(FactoMineR)

wine_3_pca <-  PCA(wine1,ncp = 3, graph = FALSE)#Here we gave the argument ncp = 3 because we want first 3principle component

wine_3_pca_heirarchical_clust <- HCPC(wine_3_pca,nb.clust = 3, graph = TRUE) #performing Heirarchical clustering on PCA,
#nb.clust: an integer specifying the number of clusters. Possible values are:
#0: the tree is cut at the level the user clicks on
#-1: the tree is automatically cut at the suggested level
#Any positive integer: the tree is cut with nb.clusters clusters

#Lets visualize it using another function

fviz_cluster(wine_3_pca_heirarchical_clust,repel = TRUE,show.clust.cent = TRUE, cex = 0.8, pallete = "jco", rect = TRUE, rect_fill= TRUE, rect_border = "jco", labels_track_size = 0.8)
#repel is used so that the numbers dont get overlapped

#lets check if the cluster formed matches with the original data class
wine_heirarchical_final <- data.frame(wine[,1], wine_3_pca_heirarchical_clust$data.clust$clust)

#lets make a confusion matrix
conf <- table(wine_heirarchical_final)
conf


#Lets do the K-means clustering for the first 3 principle component
#First lets find the value of k
scores <- as.data.frame(scores)

wss <- c()
for (i in 2:15) wss[i]<- sum(kmeans(scores[,1:3], centers = i)$withinss)
plot(1:15,wss,type = "b", xlab = "No of Clusters", ylab = "Avg Distance")
#Here using the Elbow plot, we got the k value as 3

wine_kmeans <- kmeans(scores[,1:3],3)
kmeans_cluster <-wine_kmeans$cluster
kmeans_cluster

wine_kmeans_final_data <- data.frame(wine[,1],kmeans_cluster)

#Lets make a confusion matrix
kmean_table <- table(wine_kmeans_final_data)
kmean_table


#The most accurate clusters that almost matched with the dataset was found with heirarchical clusters.