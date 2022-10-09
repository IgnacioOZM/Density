#################################################################################
##############    Lab 5.2 Dimensionality reduction   ############################
#################################################################################

library(ggfortify)

## Set working directory -------------------------------------------------------------------------
x <-dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(x)
rm(list = ls())
cat("\014")

## Load dataset -------------------------------------------------------------------------------------------------------
Countries <- read.table("Countries.dat",header = TRUE, sep = "", stringsAsFactors = FALSE)

#Perform principal component analysis
Countries.pca<-prcomp(Countries[,2:9], center = TRUE, scale. = TRUE) 
summary(Countries.pca)

#Plot eigenvalues obtained
plot(Countries.pca,type="b")

#Calculate and plot variance explained
std_dev <- Countries.pca$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
barplot(prop_varex, xlab = "Principal Component",
        ylab = "Proportion of Variance Explained",
        names.arg=dimnames(Countries.pca$rotation)[[2]],
        ylim = c(0,1))
lines(cumsum(prop_varex), col="blue")
legend(x=0,y=1,legend = "Cumulative Proportion",col = "blue",lty = 1)

#Plot first 3 principal components
par(mfrow=c(3,1))
barplot(Countries.pca$rotation[,1],ylab="PC1")
barplot(Countries.pca$rotation[,2],ylab="PC2")
barplot(Countries.pca$rotation[,3],ylab="PC3")
par(mfrow=c(1,1))

#Plot data in pc axis
autoplot(Countries.pca, size= 0, #size = 0 to show only  the labels
         label = TRUE, label.label = Countries$Country, label.size = 4,
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 4)



#####################################################################################
### Electricity demand   ############################################################
#####################################################################################


fdatatot <- read.table("Dem_2011_2015_24h.dat",header = TRUE, sep = "")
fdatatot$DAY <- as.factor(fdatatot$DAY)
fdatatot$MONTH <- as.factor(fdatatot$MONTH)

fdata <- fdatatot[,4:27]
#Plot first five demand profiles in the dataset
matplot(t(as.matrix(fdata[1:5,])),type="l",
        xlab = "Hours", ylab = "Demand", main = "Five demand profiles")


fdata.pca<-prcomp(fdata, center = TRUE, scale. = FALSE) 
summary(fdata.pca)

std_dev <- fdata.pca$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
barplot(prop_varex[1:5], xlab = "Principal Component",
        ylab = "Proportion of Variance Explained",
        names.arg=dimnames(fdata.pca$rotation)[[2]][1:5],
        ylim = c(0,1))
lines(cumsum(prop_varex[1:5]), col="blue", lwd = 3)
legend(x=3,y=0.8,legend = "Cumulative Proportion",col = "blue",lty = 1)


#Plot principal component loadings
par(mfrow=c(3,1))
barplot(fdata.pca$rotation[,1],ylab="PC1")
barplot(fdata.pca$rotation[,2],ylab="PC2")
barplot(fdata.pca$rotation[,3],ylab="PC3")
par(mfrow=c(1,1))


#plot component scores
par(mfrow=c(3,1))
plot(fdata.pca$x[,1],type="l",ylab="PC1", xlab = "t(days)", main = "Time series of principal components")
plot(fdata.pca$x[,2],type="l",ylab="PC2", xlab = "t(days)")
plot(fdata.pca$x[,3],type="l",ylab="PC3", xlab = "t(days)")
par(mfrow=c(1,1))

#Reconstruction of first profile with three components
nPCA <- 3;
for(i in 1:3){
plot(1:24, as.matrix(fdata[i,]),type="l", xlab = "Hours", ylab = "Demand")
fdata.rec <- fdata.pca$center + (fdata.pca$x[i,1:nPCA] %*% t(fdata.pca$rotation[,1:nPCA]))*ifelse(fdata.pca$scale,fdata.pca$scale,1)
lines(1:24,fdata.rec[1,],col="red")
legend(x=1,y=28000,legend = c("Real", "Reconstruction"), col = c("black", "red"),lty = 1)
Sys.sleep(1)
}


#plot data in space spanned by first 2 PC
df_plot <- data.frame(PC1 = fdata.pca$x[,1], PC2 =fdata.pca$x[,2], DAY=fdatatot$DAY, MONTH=fdatatot$MONTH)
ggplot(df_plot)+geom_point(aes(x=PC1,y=PC2))
ggplot(df_plot)+geom_point(aes(x=PC1,y=PC2, color=DAY))
ggplot(df_plot)+geom_point(aes(x=PC1,y=PC2, color=MONTH))



###### ICA
library(fastICA)
fdata.ica <- fastICA(fdata, 2, alg.typ = "parallel", fun = "logcosh", alpha = 1,
             method = "R", row.norm = FALSE, maxit = 200,
             tol = 0.0001, verbose = TRUE)


#plot component scores
par(mfrow=c(2,1))
plot(fdata.ica$S[1:50,1],type="l",ylab="IC1", xlab = "t(days)", main = "Time series of independent components")
plot(fdata.ica$S[1:50,2],type="l",ylab="IC2", xlab = "t(days)")
par(mfrow=c(1,1))


#plot data in space spanned by first 2 IC
df_plot <- data.frame(IC1 = fdata.ica$S[,1], IC2 =fdata.ica$S[,2], DAY=fdatatot$DAY, MONTH=fdatatot$MONTH)
ggplot(df_plot)+geom_point(aes(x=IC1,y=IC2,color=DAY))
ggplot(df_plot)+geom_point(aes(x=IC1,y=IC2,color=MONTH))

#Boxplot of independent component by DAY
ggplot(df_plot)+geom_boxplot(aes(x=DAY,y=IC2))
