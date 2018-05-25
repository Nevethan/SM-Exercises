#KNN Assignment
library("gmodels")

#source('C:/Users/Bruger/Desktop/SM-Exercises/loadImage.R', echo=TRUE) #This contains Smoothing override method
#source('C:/Users/Bruger/Desktop/Statistical Mashine Learning/BaseFolder/loadImage.R')
source('loadImage.R')
source('Methods.R')

#directory <- 'C:/Users/Bruger/Desktop/Statistical Mashine Learning/2018/group';
#directory <- 'C:/Users/Anna/svn/new/trunk/preProcessed/2018/group';

getAllData <- function(dataList, folder, persons){
  index <- 0
  id <- data.frame()
  idList <- list()
  for(i in 1:length(dataList))
  {
    if( length(dataList[[i]]) > 0  ){
      for(j in 1:length(dataList[[i]])){
        idTemp <- loadSinglePersonsData(100,i - 1, dataList[[i]][j], folder)
        idList <- append(idList, list(idTemp))
        index <- index+1
        if (index == persons) {
          return(idList)
        }
      }
    }
  }
  return(idList)
}

#getDisjunctData('C:/Users/Anna/svn/new/trunk/preProcessed/2018/group', 2, 70)
getDisjunctData <- function(folder, persons, split) {

datalist <- list(  list( 1) ,list( 1, 2 ), list( 1, 2, 3 ),   list( 1, 2, 3 ), list( 1, 0, 4, 2, 3 ), 
                     list( 1, 5, 4, 2, 3 ), list( 0, 2, 3 ), list( 1 ), list( 1, 2, 3 ), list( 1, 2, 3 ), 
                     list( 1, 2, 3 ), list( 1, 4, 2, 3 ), list( 1, 2, 3 ), list( 1, 2 ), list( 1, 2, 3 ), 
                     list( 1, 2 ), list( 1, 4, 2, 3 ), list( 1, 4, 2, 3 ), list( 1, 2, 3 ))  

idList <- getAllData(datalist, folder, persons)
id <- data.frame()
for(i in 1:length(idList)){
  idTemp <- idList[i]
  idTemp <- data.frame(idTemp)
  id <- as.data.frame(rbind(id,idTemp))
}

dataresult <- as.data.frame(id)

#normalize(dataset)

dataset <- datasetShuffle(dataresult)

#Data split
total_persons <- persons*4000
split_training <- total_persons*split/100
train <- 1:split_training
test <- (split_training+1):total_persons

data.train <- dataset[train,]
data.test <- dataset[test,]

tempTrain <- datasetShuffle(data.train)
tempTest <- datasetShuffle(data.test)

dataset.train.labels <- factor(tempTrain[,1])
dataset.test.labels <- factor(tempTest[,1])
dataset.train <- tempTrain[,-1]
dataset.test <- tempTest[,-1]

return(list("train" = dataset.train, "test" = dataset.test, "train.labels" = dataset.train.labels, "test.labels" = dataset.test.labels))

}

#getAllPersonsInData('C:/Users/Anna/svn/new/trunk/preProcessed/2018/group', 2, 70)
getAllPersonsInData <- function(folder, persons, split) {
  
  datalist <- list(  list( 1) ,list( 1, 2 ), list( 1, 2, 3 ),   list( 1, 2, 3 ), list( 1, 0, 4, 2, 3 ), 
                     list( 1, 5, 4, 2, 3 ), list( 0, 2, 3 ), list( 1 ), list( 1, 2, 3 ), list( 1, 2, 3 ), 
                     list( 1, 2, 3 ), list( 1, 4, 2, 3 ), list( 1, 2, 3 ), list( 1, 2 ), list( 1, 2, 3 ), 
                     list( 1, 2 ), list( 1, 4, 2, 3 ), list( 1, 4, 2, 3 ), list( 1, 2, 3 ))  
  
  idList <- getAllData(datalist, folder, persons)

  id <- data.frame()
  for(i in 1:length(idList)){
    idTemp <- idList[i]
    idTemp <- data.frame(idTemp)
    id <- as.data.frame(rbind(id,idTemp))
  }
  
  dataresult <- as.data.frame(id)
  
  #normalize(dataset)
  
  data.shuffle <- datasetShuffle(dataresult)
  
  #Data split
  total_persons <- persons*4000
  split_training <- total_persons*split/100
  train <- 1:split_training
  test <- (split_training+1):total_persons
  
  temp.train <- data.shuffle[train,]
  temp.test <- data.shuffle[test,]
  
  
  dataset.train.labels <- factor(temp.train[,1])
  dataset.test.labels <- factor(temp.test[,1])
  dataset.train <- temp.train[,-1]
  dataset.test <- temp.test[,-1]
  
  return(list("train" = dataset.train, "test" = dataset.test, "train.labels" = dataset.train.labels, "test.labels" = dataset.test.labels))
  
}

#getIndividualData('C:/Users/Anna/svn/new/trunk/preProcessed/2018/group', 2, 70)
getIndividualData <- function(folder, persons, split) {
  
  datalist <- list(  list( 1) ,list( 1, 2 ), list( 1, 2, 3 ),   list( 1, 2, 3 ), list( 1, 0, 4, 2, 3 ), 
                     list( 1, 5, 4, 2, 3 ), list( 0, 2, 3 ), list( 1 ), list( 1, 2, 3 ), list( 1, 2, 3 ), 
                     list( 1, 2, 3 ), list( 1, 4, 2, 3 ), list( 1, 2, 3 ), list( 1, 2 ), list( 1, 2, 3 ), 
                     list( 1, 2 ), list( 1, 4, 2, 3 ), list( 1, 4, 2, 3 ), list( 1, 2, 3 ))  
  
  #Data split
  data_pr_person <- 4000
  split_training <- data_pr_person*split/100
  train <- 1:split_training
  test <- (split_training+1):data_pr_person
  
  idList <- getAllData(datalist, folder, persons)

  id.train <- data.frame()
  id.test <- data.frame()
  for(i in 1:length(idList)){
    idTemp <- idList[i]
    idTemp <- data.frame(idTemp)
    # Shuffle one person's data
    temp.shuffle <- datasetShuffle(idTemp)
    # Split one person's data
    temp.train <- temp.shuffle[train,]
    temp.test <- temp.shuffle[test,]
    
    id.train <- as.data.frame(rbind(id.train,temp.train))
    id.test <- as.data.frame(rbind(id.test,temp.test))
  }
  
  temp.train <- as.data.frame(id.train)
  temp.test <- as.data.frame(id.test)
  
  dataset.train.labels <- factor(temp.train[,1])
  dataset.test.labels <- factor(temp.test[,1])
  dataset.train <- temp.train[,-1]
  dataset.test <- temp.test[,-1]

  return(list("train" = dataset.train, "test" = dataset.test, "train.labels" = dataset.train.labels, "test.labels" = dataset.test.labels))
  
}

