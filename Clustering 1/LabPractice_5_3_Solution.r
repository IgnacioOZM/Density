#################################################################################
##############       LabPractice 5.3  Clustering     ############################
##############     ----------- solution ---------    ############################
#################################################################################

library(ggplot2)
library(MLTools)

## Set working directory -------------------------------------------------------------------------
x <-dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(x)
rm(list = ls())
cat("\014")

## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read.table("Aggregation.dat",header = FALSE, sep = "", stringsAsFactors = FALSE)
fdata <- fdata[,c(1,2)]
ggplot(fdata)+geom_point(aes(x=V1,y=V2))


## Hierarchical clustering -------------------------------------------------------------------------------------------------------
# Compute distances
dd <- dist(scale(fdata), method = "euclidean")
# hierarchical clustering
hc <- hclust(dd, method = "ward.D2")
plot(hc,hang = -1)
# Convert to dendrogram object
#dend <- as.dendrogram(hc)

#Cut the tree to a given number of clusters and obtain the associated cluster for each observation
cluster <-  cutree(hc, k = 4) #Use "h = " to cut by a given height 
table(cluster) #check number of observations assigned to each cluster
#ggplot(fdata)+geom_point(aes(x=V1,y=V2,color=as.factor(cluster)))
#plot the dendrogram changing the colors according to the number of clusters
library(dendextend)
hc.col=color_branches(as.dendrogram(hc),k=4)
plot(hc.col)
PlotClusters(NULL,cluster,fdata)

#silhouette plot for each point
library(cluster)
library(factoextra)
sil <- silhouette(cluster, dd)
fviz_silhouette(sil)
# average silhouette
mean(sil[,3])

#Validation indices
library(clusterCrit)
#Dunn
intCriteria(as.matrix(fdata),cluster,c("dunn"))
# S_Dbw
intCriteria(as.matrix(fdata),cluster,c("S_Dbw"))



## kmeans clustering -------------------------------------------------------------------------------------------------------
set.seed(101)
# Calculate clusters. "centers = " specifies the number of clusters to be obtained
km <- kmeans(fdata, centers = 3)
# Plot the clusters and the centers
PlotClusters(km$centers,km$cluster,fdata[,1:2])


## Selecting the number of clusters -------------------------------------------------------
tunegrid <- seq(2,10,by = 1)
ssq <- vector(length = length(tunegrid))
silv <- vector(length = length(tunegrid)) 
i <- 1
for(k in tunegrid){
  km <- kmeans(fdata, centers = k)
  #Total sum of squares and silhouette
  ssq[i] <- sum(km$tot.withinss)
  sil <- silhouette(km$cluster, dd)
  silv[i] <- mean(sil[,3])
  i <- i+1
}
# Buscamos el codo. En este caso puede ser 5 o 6.
ggplot()+geom_line(aes(x=tunegrid, y = ssq))
# Buscamos el punto más alto.
ggplot()+geom_line(aes(x=tunegrid, y = silv))


#Calculated selected number of clusters
km <- kmeans(fdata, 4)
PlotClusters(km$centers,km$cluster,fdata)

#silhouette plot for each point
sil <- silhouette(km$cluster, dd)
fviz_silhouette(sil)
# average silhouette
mean(sil[,3])

#Validation indices
intCriteria(as.matrix(fdata),km$cluster,c("Dunn"))
intCriteria(as.matrix(fdata),km$cluster,c("S_Dbw"))




################# High dimensional data #############################
## Load dataset -------------------------------------------------------------------------------------------------------
fdataTot <- read.table("Dem_2011_2015_24h.dat",header = TRUE, sep = "", stringsAsFactors = FALSE)
matplot(t(as.matrix(fdataTot[1:5,3:26])),type="l")

fdata <- fdataTot[,3:26]
##Hierarchical clustering
# Compute distances and hierarchical clustering
dd <- dist(scale(fdata), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")
plot(hc,hang = -1)

#Cut the tree to a given number of clusters
cluster <-  cutree(hc, k = 3) #Use "h = " to cut by a given height 
table(cluster) #check number of observations assigned to each cluster
#plot the dendrogram changing the colors according to the number of clusters
hc.col=color_branches(as.dendrogram(hc),k=3)
plot(hc.col)

#Color curves according to cluster
matplot(t(as.matrix(fdata)),type="l",col = cluster)
# Plot the cluster in Principal Component subspace
PlotClusters(NULL,cluster,fdata)

#Compare cluster and category
table(cluster,fdataTot$DAY)


#silhouette plot for each point
sil <- silhouette(cluster, dd)
fviz_silhouette(sil)
# average silhouette
mean(sil[,3])

#Validation indices
intCriteria(as.matrix(fdata),cluster,c("Dunn"))
intCriteria(as.matrix(fdata),cluster,c("S_Dbw"))




##Kmeans ------------------------------------------

## Selecting the number of clusters -------------------------------------------------------
tunegrid <- seq(2,10,by = 1)
ssq <- vector(length = length(tunegrid))
silv <- vector(length = length(tunegrid)) 
i <- 1
for(k in tunegrid){
  km <- kmeans(fdata, centers = k)
  #Total sum of squares and silhouette
  ssq[i] <- sum(km$tot.withinss)
  sil <- silhouette(km$cluster, dd)
  silv[i] <- mean(sil[,3])
  i <- i+1
}
ggplot()+geom_line(aes(x=tunegrid, y = ssq))
ggplot()+geom_line(aes(x=tunegrid, y = silv))


#Calculated selected number of clusters
km <- kmeans(fdata, 3)
#Color curves according to cluster
matplot(t(as.matrix(fdata)),type="l",col = km$cluster)
# Plot the cluster in Principal Component subspace and represent centers
PlotClusters(km$centers,km$cluster,fdata)

#Compare cluster and category
table(km$cluster,fdataTot$DAY)


#silhouette plot for each point
sil <- silhouette(km$cluster, dd)
fviz_silhouette(sil)
# average silhouette
mean(sil[,3])

#Validation indices
intCriteria(as.matrix(fdata),km$cluster,c("Dunn"))
intCriteria(as.matrix(fdata),km$cluster,c("S_Dbw"))

# Extracción de clusters
# jerarquico
fdata_con_cluster$cluster<-cluster

# kmeans
fdata_con_cluster$cluster<-km$cluster

# neural gas
fdata_con_cluster$cluster<-ng$cluster

# prfbn
fdata_con_cluster$cluster<-prbfn$classification

# primera PCA
fdata_con_primera_PCA$PCA<-fdata.pca$x[,1]
