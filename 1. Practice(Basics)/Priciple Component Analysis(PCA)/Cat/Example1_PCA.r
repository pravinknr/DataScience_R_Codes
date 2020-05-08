#Example of PCA

install.packages("jpeg")
install.packages("imager")
install.packages("pixmap")
library(jpeg)
library(imager)
library(pixmap)

cat <- readJPEG("C:\\Users\\pravi\\Desktop\\Datasets\\cat.jpg")

ncol(cat) #Shows the Number of Columns
nrow(cat) #Shows the NUmber of Rows

r <- cat[,,1]
g <- cat[,,2]
b <- cat[,,3]

cat.r.pca <- prcomp(r , center = FALSE)
cat.g.pca <- prcomp(g, center = FALSE)
cat.b.pca <- prcomp(b, center = FALSE)

rgb.pca <- list(cat.r.pca, cat.g.pca, cat.b.pca)
pca.img <- sapply(rgb.pca, function(j) {  compressed.img <- j$x[,1:100] %*% t(j$rotation[,1:100])}, simplify = 'array')
a<-pixmap::pixmapRGB(pca.img)
windows()
plot(a)