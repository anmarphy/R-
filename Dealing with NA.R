##Imputando datos

#Amelia assumes that your data is jointly distributed as multivariate normal.
iris <- read.csv("C:/Users/Andrea/Desktop/iris.csv")
iris$index <-c(1:nrow(iris))
library(naniar)
vis_miss(iris)

##Imputando como normal multivariada
library(Amelia)
iris_nvo<-amelia(iris[-5],m=5,idvars=c("index"))
iris_imp<-data.frame(iris_nvo$imputations$imp5)
write.csv(iris_imp, "imputacion1.csv")
                 
##Libreria MICE
library(VIM)
aggr_plot <- aggr(iris, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(iris), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
library(mice)
iris_mice<-mice(iris,m=5,maxit=50,meth='pmm',seed=500)
summary(iris_mice)
iris_mice$imp$Sepal.Length
completedData <- complete(iris_mice,1)
write.csv(completedData, "mice.csv") ##Estos dan mejor que Amelia
