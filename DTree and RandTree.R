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


###### Exercise 2.1 #####
#Split data 50/50

#Disjunct - 10 members in total
train <- 1:40000 # 10 persons
dataset.train <- dataset.all[train,]
dataset.test <- dataset.all[-train,]

dataset.train <- datasetShuffle(dataset.train)
dataset.test <- datasetShuffle(dataset.test)

dataset.train.labels <- factor(dataset.train[,1])
dataset.test.labels <- factor(dataset.test[,1])

dataset.train <- dataset.train[,-1]
dataset.test <- dataset.test[,-1]

##### Exercise 4.1.1 #####
#Running the PCA object and getting the first 5
library("FSelector")
numberOfPCA <- 5

pca.object <- prcomp(dataset.train) #PCA Object

PCA.5 <- pca.object$x[,numberOfPCA] #Getting the data for the first 5 PCAs

#Method for Entropy - Without Threshold
getEntropy <- function(dataset, labels){
  fractions <- c()
  for (i in 0:9) {
    fractions[i+1] <- length(dataset[labels == i]) / length(dataset)
  }
  
  entropy <- c()
  for(i in 1:length(fractions)){
    entropy[i] <- (-fractions[i]*log2(fractions[i]))
    
    if(is.nan(entropy[i])){
      entropy[i] <- 0
    }
  }
  entropy <- sum(entropy)
  return(entropy)
} 

entropy.before <- getEntropy(PCA.5,dataset.train.labels) 

getEntropyThreshold <- function(pc,labels,threshold){
  entro_before <- getEntropy(pc, labels)
  
  dataset1 <- c()
  dataset1.labels <- c()
  dataset2 <- c()
  dataset2.labels <- c()
  
  count1 <- 0
  count2 <- 0
  
  for (i in 1:length(pc)) {
    if (pc[i] < threshold) {
      dataset1[count1+1] <- pc[i]
      dataset1_labels[count1+1] <- dataset.train.labels[i]
      count1 <- count1 + 1
    } else {
      dataset2[count2+1] <- pc[i]
      dataset2_labels[count2+1] <- dataset.train.labels[i]
      count2 <- count2 + 1
    }
    
  }
  
  entropy1 <- (length(dataset1)*getEntropy(dataset1,datset1_labels)) / (sum(length(dataset1), length(dataset2)))
  entropy2 <- (length(dataset2)*getEntropy(dataset2,datase)) / (sum(length(dataset1), length(dataset2)))
  
  entropy.after <- sum(entropy1, entropy2)
  
  info.gain <- entropy.before - entropy.after
  
  return(info.gain)
}

PCA.1 
PCA.2

plot(PCA.1)

##### Exercise 4.1.2 #####







