
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
        idTemp <- loadSinglePersonsData(100,i - 1,j,folder)
        idList <- append(idList, list(idTemp))
      }
    }
  }
  return(idList)
}

folder<- "C:/Users/Bruger/Desktop/Statistical Mashine Learning/2018/group"

datalist <- list(list(1), list(1,2,3), list(),list(1,2,3), list(), list(), list(), list(1), list(1), list(1,2,3), 
                 list(), list(), list(), list(1,2), list(), list(1,2), list(1), list(), list(1,2,3))

idList <- getAllData(datalist)

for(i in 1:length(idList)){
  idTemp <- idList[i]
  idTemp <- data.frame(idTemp)
  id <- as.data.frame(rbind(id,idTemp))
}

dataset.all <- as.data.frame(id)

train <- 1

dataset.train <- dataset.all[train,-1]
dataset.test <- dataset.all[-train,-1]

dataset.train.labels <- factor(dataset.all[train,1])
dataset.test.labels <- factor(dataset.all[-train,1])

PCA.obj <- prcomp(dataset.train)

#plot results of PCA object
require(graphics)
plot(PCA.obj)
biplot(PCA.obj)

conMatrix(PCA.obj, dataset.test) #My own implementation in Methods.R. It gives accuracy, preciseness etc. 
#confusionMatrix(dataset.test, PCA.obj)

model <- knn(PCA.obj, dataset.train.labels, dataset.test.labels, 5)





