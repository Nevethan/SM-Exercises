
#Source image read
#source('C:/Users/Bruger/Desktop/SM-Exercises/loadImage.R', echo=TRUE)
source('C:/Users/Bruger/Desktop/Statistical Mashine Learning/BaseFolder/loadImage.R')

#Get all data from folder
getAllData <- function(dataList){
  id <- data.frame()
  idList <- list()
  for(i in 1:length(dataList))
  {
    if( length(dataList[[i]]) > 0  ){
      for(j in 1:length(dataList[[i]])){
        idTemp <- loadSinglePersonsData(100,i - 1, dataList[[i]][j] ,folder)
        idList <- append(idList, list(idTemp))
      }
    }
  }
  return(idList)
}

folder<- "C:/Users/Bruger/Desktop/Statistical Mashine Learning/preProcessed/2018/group"

datalist <- list(  list( 1) ,list( 1, 2 ), list( 1, 2, 3 ),   list( 1, 2, 3 ), list( 1, 0, 4, 2, 3 ), 
                   list( 1, 5, 4, 2, 3 ), list( 0, 2, 3 ), list( 1 ), list( 1, 2, 3 ), list( 1, 2, 3 ), 
                   list( 1, 2, 3 ), list( 1, 4, 2, 3 ), list( 1, 2, 3 ), list( 1, 2 ), list( 1, 2, 3 ), 
                   list( 1, 2 ), list( 1, 4, 2, 3 ), list( 1, 4, 2, 3 ), list( 1, 2, 3 ))

idList <- getAllData(datalist)

for(i in 1:length(idList)){
  idTemp <- idList[i]
  idTemp <- data.frame(idTemp)
  id <- as.data.frame(rbind(id,idTemp))
}

dataset.all <- as.data.frame(id)

#Shuffle data
dataset.all <- datasetShuffle(dataset.all)

train <- 1:(nrow(dataset.all)/2)
###### Exercise 2.1 #####

#Split data 50/50
dataset.train <- dataset.all[train,-1]
dataset.test <- dataset.all[-train,-1]

dataset.train.labels <- factor(dataset.all[train,1])
dataset.test.labels <- factor(dataset.all[-train,1])

PCA.obj <- prcomp(dataset.train)

#plot results of PCA object
require(graphics)

PropVariancePCA <- (PCA.obj$sdev^2)/sum(PCA.obj$sdev^2)
CumuPCA <- cumsum(PCA.obj$sdev^2)/sum(PCA.obj$sdev^2)

#Sdev
plot(main = "Standard Deviation of Principle Components", PCA.obj$sdev[1:20], xlab = "Principle Components", ylab = "Standard Deviations", col = "blue", pch = 4, cex = 2, lwd=3)
lines(PCA.obj$sdev[1:20], col= "red", lwd = 3)

#Proportion of Variance & cumulative proportion of variance
plot(main = "Proportion of Variance", PropVariancePCA[1:20], xlab = "Principle Components", ylab = "Proportion of Variance", col = "blue", pch = 4, cex = 2, lwd=3)
lines(PropVariancePCA[1:20], col= "red", lwd = 3)

plot(main = "Cumulative Proportion of Variance", CumuPCA[1:20], xlab = "Principle Components", ylab = "Cumulative Proportion of Variance", col = "blue", pch = 4, cex = 2, lwd=3)
lines(CumuPCA[1:20], col= "red", lwd = 3)


conMatrix(PCA.obj, dataset.test.labels) #My own implementation in Methods.R. It gives accuracy, preciseness etc. 

model <- knn(PCA.obj, dataset.train.labels, dataset.test.labels, 5)





