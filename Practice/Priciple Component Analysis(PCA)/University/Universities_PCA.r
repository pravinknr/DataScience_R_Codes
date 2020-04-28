#PCA
#Priciple Componen Analysis
#USe PCA to : 1. Identify Relation between Columns
#             2. Reduce No of Cloumns
#             3.Viu=sualize in 2D

install.packages("gdata")
install.packages("xlsx")
library(gdata)

#Import the dataset Universities.csv

pca <- princomp(Universities[,2:7], cor = TRUE, scores = TRUE, covmat = NULL) #Apply Principle Component Analysis from column 2to 7
summary(pca)
pca$scores
pca$loadings

plot(pca$scores[,1:2], col="Blue",pch = 18, cex = 0.3, lwd = 3)
text(pca$scores[,1:2], labels = c(1:25), cex = 1) #represent the Dots with text

