#################################################################################
##############       LabPractice 5.4  Clustering     ############################
##############     ----------- solution ---------    ############################
#################################################################################

library(ggplot2)
library(cclust)
library(stats)
library(MLTools)
library(cluster)
library(factoextra)
library(clusterCrit)

## Set working directory -------------------------------------------------------------------------
x <-dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(x)
rm(list = ls())
cat("\014")

## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read.table("Aggregation.dat",header = FALSE, sep = "", stringsAsFactors = FALSE)
fdata <- fdata[,c(1,2)]
# Compute distances
dd <- dist(scale(fdata), method = "euclidean")
# Plot data
ggplot(fdata)+geom_point(aes(x=V1,y=V2))


#neural gas - selection of k
tunegrid <- seq(2,10,by = 1)
ssq <- vector(length = length(tunegrid))
silv <- vector(length = length(tunegrid)) 
i <- 1
for(k in tunegrid){
  ng <- cclust(as.matrix(fdata),centers = k,method = "neuralgas")
  #Total sum of squares and silhouette
  ssq[i] <- sum(ng$withinss)
  sil <- silhouette(ng$cluster, dd)
  silv[i] <- mean(sil[,3])
  i <- i+1
}
ggplot()+geom_line(aes(x=tunegrid, y = ssq))
ggplot()+geom_line(aes(x=tunegrid, y = silv))

#fit final model
ng <- cclust(as.matrix(fdata),centers = 4,method = "neuralgas")
PlotClusters(ng$centers,ng$cluster,fdata)


#silhouette plot for each point
sil <- silhouette(ng$cluster, dd)
fviz_silhouette(sil)
# average silhouette
mean(sil[,3])

#Validation indices
library(clusterCrit)
#Dunn
intCriteria(as.matrix(fdata),as.integer(ng$cluster),c("dunn"))
# S_Dbw
intCriteria(as.matrix(fdata),as.integer(ng$cluster),c("S_Dbw"))






#Prbfn - Gaussian Mixture ---------------
library(mclust)
tunegrid <- seq(2,10,by = 1)
ssq <- vector(length = length(tunegrid))
silv <- vector(length = length(tunegrid)) 
i <- 1
for(k in tunegrid){
  Sys.sleep(0.2)
  prbfn <- Mclust(fdata[,1:2],G=k,modelNames = "VVI")
  #Total sum of squares and silhouette
  ssq[i] <- Reconstruction.Error(t(prbfn$parameters$mean),prbfn$classification,fdata[,1:2])
  sil <- silhouette(prbfn$classification, dd)
  silv[i] <- mean(sil[,3])
  i <- i+1
}
ggplot()+geom_line(aes(x=tunegrid, y = ssq))
ggplot()+geom_line(aes(x=tunegrid, y = silv))

#fit final model
prbfn <- Mclust(fdata,G=3,modelNames = "VVI")
PlotClusters(t(prbfn$parameters$mean),as.integer(prbfn$classification),fdata)


#silhouette plot for each point
sil <- silhouette(prbfn$classification, dd)
fviz_silhouette(sil)
# average silhouette
mean(sil[,3])

#Validation indices
intCriteria(as.matrix(fdata),as.integer(prbfn$classification),c("Dunn"))
intCriteria(as.matrix(fdata),as.integer(prbfn$classification),c("S_Dbw"))



###############################################################################
#----------------- Demanda ----------------------------------------------------
###############################################################################

## Load dataset -------------------------------------------------------------------------------------------------------
fdataTot <- read.table("Dem_2011_2015_24h.dat",header = TRUE, sep = "", stringsAsFactors = FALSE)
matplot(t(as.matrix(fdataTot[1:5,3:26])),type="l")

fdata <- fdataTot[,3:26]
# Compute distances
dd <- dist(scale(fdata), method = "euclidean")

#neural gas - selection of k
tunegrid <- seq(2,10,by = 1)
ssq <- vector(length = length(tunegrid))
silv <- vector(length = length(tunegrid)) 
i <- 1
for(k in tunegrid){
  ng <- cclust(as.matrix(fdata),centers = k,method = "neuralgas")
  #Total sum of squares and silhouette
  ssq[i] <- sum(ng$withinss)
  sil <- silhouette(ng$cluster, dd)
  silv[i] <- mean(sil[,3])
  i <- i+1
}
ggplot()+geom_line(aes(x=tunegrid, y = ssq))
ggplot()+geom_line(aes(x=tunegrid, y = silv))

#fit final model
ng <- cclust(as.matrix(fdata),centers = 3,method = "neuralgas")
PlotClusters(ng$centers,ng$cluster,fdata)

#silhouette plot for each point
sil <- silhouette(ng$cluster, dd)
fviz_silhouette(sil)
# average silhouette
mean(sil[,3])

#Validation indices
intCriteria(as.matrix(fdata),as.integer(ng$cluster),c("Dunn"))
intCriteria(as.matrix(fdata),as.integer(ng$cluster),c("S_Dbw"))





#prbfn - Gaussian Mixture ---------------------------
tunegrid <- seq(2,10,by = 1)
ssq <- vector(length = length(tunegrid))
silv <- vector(length = length(tunegrid)) 
i <- 1
for(k in tunegrid){
  Sys.sleep(0.2)
  prbfn <- Mclust(fdata,G=k,modelNames = "VVI")
  #Total sum of squares and silhouette
  ssq[i] <- Reconstruction.Error(t(prbfn$parameters$mean),prbfn$classification,fdata)
  sil <- silhouette(prbfn$classification, dd)
  silv[i] <- mean(sil[,3])
  i <- i+1
}
ggplot()+geom_line(aes(x=tunegrid, y = ssq))
ggplot()+geom_line(aes(x=tunegrid, y = silv))


prbfn <- Mclust(fdata, G=3, modelNames = "VVI")
PlotClusters(t(prbfn$parameters$mean),as.integer(prbfn$classification),fdata)


#silhouette plot for each point
sil <- silhouette(prbfn$classification, dd)
fviz_silhouette(sil)
# average silhouette
mean(sil[,3])

#Validation indices
intCriteria(as.matrix(fdata),as.integer(prbfn$classification),c("Dunn"))
intCriteria(as.matrix(fdata),as.integer(prbfn$classification),c("S_Dbw"))



