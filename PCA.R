
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


train <- 1:(nrow(dataset.all)/2)
train <- 1:116000 # 29 persons
###### Exercise 2.1 #####
#Split data 50/50

# #Disjunct - 57 members in total
# dataset.train <- dataset.all[train,]
# dataset.test <- dataset.all[-train,]
# 
# dataset.train <- datasetShuffle(dataset.train)
# dataset.test <- datasetShuffle(dataset.test)
# 
# dataset.train <- dataset.train[,-1]
# dataset.test <- dataset.test[,-1]
# 
# dataset.train.labels <- factor(dataset.train[,1])
# dataset.test.labels <- factor(dataset.test[,1])

#All persons
dataset.all <- datasetShuffle(dataset.all)

dataset.train <- dataset.all[train,-1]
dataset.test <- dataset.all[-train,-1]

dataset.train.labels <- factor(dataset.all[train,1])
dataset.test.labels <- factor(dataset.all[-train,1])

PCA.obj <- prcomp(dataset.train)

##### Exercise 2.1.2 #####
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

##### Exercise 2.1.3 and Exercise 2.1.4 #####
#PC's representing 80 % of the accumulated variance 
#REMEMBER to vary k - Three values are enough
PC.80 <- PCA.obj$x[, CumuPCA < 0.8] # Results in 14 PCs

time.start <- Sys.time()
model <- knn(dataset.train[,1:14], dataset.test[,1:14], dataset.train.labels[1:14], 3)
time.end <- Sys.time()

#Run time
print(time.end-time.start)

#Performance
accuracy <- acc(model, dataset.test.labels)

#PC's representing 90 % of the accumulated variance
PC.90 <- PCA.obj$x[, CumuPCA < 0.9] #Results in 23 PCs

time.start <- Sys.time()
model <- knn(dataset.train[,1:23], dataset.test[,1:23], dataset.train.labels[1:23], 3)
time.end <- Sys.time()

#Run time
print(time.end-time.start)

#Performance
accuracy <- acc(model, dataset.test.labels)

#PC's representing 95 % of the accumulated variance
PC.95 <- PCA.obj$x[, CumuPCA < 0.95] # Results in 34 PCs

time.start <- Sys.time()
model <- knn(dataset.train[,1:34], dataset.test[,1:34], dataset.train.labels[1:34], 3)
time.end <- Sys.time()

#Run time
print(time.end-time.start)

#Performance
accuracy <- acc(model, dataset.test.labels)

#PC's representing 99 % of the accumulated variance
PC.99 <- PCA.obj$x[, CumuPCA < 0.99] # Results in 72 PCs

time.start <- Sys.time()
model <- knn(dataset.train[,1:72], dataset.test[,1:72], dataset.train.labels[1:72], 3)
time.end <- Sys.time()

#Run time
print(time.end-time.start)

#Performance
accuracy <- acc(model, dataset.test.labels)

##### Exercise 2.2.1 #####
normalize() #The best Dataset from Exercise 2.1.3 and Exercise 2.1.4

folds <- createFolds(dataset$V1, 10) #Change the dataset

a <- list()

for(i in 1:length(folds)){
  cross.test <- dataset[-folds[[i]],-1]
  cross.train
  
  cross.test.labels 
  cross.train.labels
  
  time.start <- Sys.time()
  model <- knn(cross.train, cross.test, cross.train.labels,k) #Change k values
  time.end <- Sys.time()
  
  a[i] <- acc(model, cross.test.labels) #accuracy
  
}

#Plot accuracy for each fold
plot(a)

#Apply Normalization before PCA
normalize(dataset.all)

dataset.all <- datasetShuffle(dataset.all)

dataset.train <- dataset.all[train,-1]
dataset.test <- dataset.all[-train,-1]

dataset.train.labels <- factor(dataset.all[train,1])
dataset.test.labels <- factor(dataset.all[-train,1])

PCA.obj <- prcomp(dataset.train)

folds <- createFolds(dataset$V1, 10) #Change the dataset

a <- list()

for(i in 1:length(folds)){
  cross.test <- dataset[-folds[[i]],-1]
  cross.train
  
  cross.test.labels 
  cross.train.labels
  
  time.start <- Sys.time()
  model <- knn(cross.train, cross.test, cross.train.labels,k) #Change k values
  time.end <- Sys.time()
  
  a[i] <- acc(model, cross.test.labels) #accuracy
  
}

##### Exercise 2.3.1 #####
id <- loadSinglePersonsData(DPI,group,member,folder)
imageSize <- sqrt(ncol(id) - 1)
imageM <- matrix( id[cipherNumber,2:ncol(id)],nrow =
                    imageSize,ncol = imageSize,byrow = FALSE)

imageM <- rotate(imageM) # rotate is a function to rotate the image

image( imageM )








