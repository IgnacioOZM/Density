#################################################################################
##############   LabPractice 5.1 Density Estimation  ############################
##############     ----------- solution ---------    ############################
#################################################################################

library(MLTools)
library(MASS)
library(stats)
library(ggplot2)
library(ggfortify)

## Set working directory -------------------------------------------------------------------------
x <-dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(x)
rm(list = ls())
cat("\014")

#Load data
fdata <- read.table("MultivariateDensityEstimation.dat",header = TRUE, sep = "")
ggplot(fdata)+geom_point(aes(x=X1, y=X2))
# 2D density plot
ggplot(fdata)+geom_point(aes(x=X1,y=X2))+geom_density2d(aes(x=X1,y=X2))

#Train Gaussian mixture
library(mclust)
#G = number of groups 
#modelNames = type of gaussians
densmod <- densityMclust(fdata[,c(1:2)], G=3, modelNames ="VVV")   #G indica el número de gaussianas y modelnames es el tipo de gaussianas
plot(fdata$X1,fdata$X2)
par(new=T)
plot(densmod, what = "density",col="red")
summary(densmod, parameters = TRUE) #Fitted parameters


#Use G=NULL and modelNames = NULL for automatic selection using BIC
densmod <- densityMclust(fdata[,c(1:2)], G=1:30, modelNames = "VVV")
plot(densmod, what = "BIC")
plot(fdata$X1,fdata$X2)
par(new=T)
plot(densmod, what = "density", col="red")
summary(densmod, parameters = TRUE) #Fitted parameters


## Generate 2D dataset with one class
#fdata <- GenMat(1,100,0.25)


## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read.table("MultivariateDensityEstimation.dat",header = TRUE, sep = "")
## Or generate dataset with three classes
#fdata <- GenMat(3,100,0.25)


# Divide dataset into the 3 classes
fdata$Y <- as.factor(fdata$Y)
class0 <- fdata[fdata$Y==0,c(1,2)]
class1 <- fdata[fdata$Y==1,c(1,2)]
class2 <- fdata[fdata$Y==2,c(1,2)]

# Original data
ggplot(fdata)+geom_point(aes(x=X1,y=X2,colour = Y))

# 2D density plot
ggplot(fdata)+geom_point(aes(x=X1,y=X2,colour = Y))+geom_density2d(aes(x=X1,y=X2,colour = Y))

# Create a grid of points
np_grid <- 100 #number of discretization points in each dimension
np.X1 <- seq(from = min(fdata$X1), to = max(fdata$X1), length.out = np_grid)
np.X2 <- seq(from = min(fdata$X2), to = max(fdata$X2), length.out = np_grid)
p.grid <- expand.grid(X1 = np.X1, X2 = np.X2) 

# PRBFN model for each class
n.kern <- 3
modNames ="VVI"
#Try changing the number of gaussians and the type to "VVV"

#Density estimation for class 0
prbfn.0 <- densityMclust(class0[,1:2], G=n.kern, modelNames = modNames)
p.grid$d0 <- predict(prbfn.0, p.grid[,1:2], what = "dens")

#Density estimation for class 1
prbfn.1 <- densityMclust(class1[,1:2], G=n.kern, modelNames = modNames)
p.grid$d1 <- predict(prbfn.1, p.grid[,1:2], what = "dens")

#Density estimation for class 2
prbfn.2 <- densityMclust(class2[,1:2], G=n.kern, modelNames = modNames)
p.grid$d2 <- predict(prbfn.2, p.grid[,1:2],what = "dens")


ggplot(p.grid)+geom_contour(aes(x=X1,y=X2,z=d0), colour= "red")+
  geom_contour(aes(x=X1,y=X2,z=d1), colour= "green")+
  geom_contour(aes(x=X1,y=X2,z=d2), colour= "blue")

# Cálculo de probabilidad de punto (2,9)
# Probabilidad de cada clase
p_class0 <- nrow(class0)/nrow(fdata)
p_class1 <- nrow(class1)/nrow(fdata)
p_class2 <- nrow(class2)/nrow(fdata)

prob_class0 <- predict(prbfn.0, cbind(2,9), what = "dens")
prob_class1 <- predict(prbfn.1, cbind(2,9), what = "dens")
prob_class2 <- predict(prbfn.2, cbind(2,9), what = "dens")

prob_x <- prob_class0*p_class0 + prob_class1*p_class1 + prob_class2*p_class2

prob_2_9_class0 <- prob_class0*p_class0/prob_x
prob_2_9_class1 <- prob_class1*p_class1/prob_x
prob_2_9_class2 <- prob_class2*p_class2/prob_x

prob_2_9_class0+prob_2_9_class1+prob_2_9_class2


# Mi intento

#Density estimation for class 0
prbfn.0 <- densityMclust(class0[,1:2], G=1, modelNames = "VVV")
p.grid$d0 <- predict(prbfn.0, p.grid[,1:2], what = "dens")

#Density estimation for class 1
prbfn.1 <- densityMclust(class1[,1:2], G=1, modelNames = modNames)
p.grid$d1 <- predict(prbfn.1, p.grid[,1:2], what = "dens")

#Density estimation for class 2
prbfn.2 <- densityMclust(class2[,1:2], G=1, modelNames = modNames)
p.grid$d2 <- predict(prbfn.2, p.grid[,1:2],what = "dens")


ggplot(p.grid)+geom_contour(aes(x=X1,y=X2,z=d0), colour= "red")+
  geom_contour(aes(x=X1,y=X2,z=d1), colour= "green")+
  geom_contour(aes(x=X1,y=X2,z=d2), colour= "blue")

