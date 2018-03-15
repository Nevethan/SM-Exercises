
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

#Disjunct - 57 members in total
train <- 1:116000 # 29 persons
dataset.train <- dataset.all[train,]
dataset.test <- dataset.all[-train,]

dataset.train <- datasetShuffle(dataset.train)
dataset.test <- datasetShuffle(dataset.test)

dataset.train.labels <- factor(dataset.train[,1])
dataset.test.labels <- factor(dataset.test[,1])

dataset.train <- dataset.train[,-1]
dataset.test <- dataset.test[,-1]

# dataset.train <- normalize(dataset.train)
# dataset.test <- normalize(dataset.test)


# #All persons
# dataset.all <- datasetShuffle(dataset.all)
# 
# #Split data
# train <- 1:(nrow(dataset.all)/2)
# 
# dataset.train <- dataset.all[train,-1]
# dataset.test <- dataset.all[-train,-1]
# 
# dataset.train.labels <- factor(dataset.all[train,1])
# dataset.test.labels <- factor(dataset.all[-train,1])

PCA.obj <- prcomp(dataset.train)
#PCA.obj.norm <- prcomp(dataset.train)

##### Exercise 2.1.2 #####
#plot results of PCA object
require(graphics)

PropVariancePCA <- (PCA.obj$sdev^2)/sum(PCA.obj$sdev^2) #Part of variacne as a whole. 
CumuPCA <- cumsum(PCA.obj$sdev^2)/sum(PCA.obj$sdev^2) #Total Variance

#Sdev
plot(main = "Standard Deviation of Principle Components", PCA.obj$sdev[1:20], xlab = "Principle Components", ylab = "Standard Deviations", col = "blue", pch = 4, cex = 2, lwd=3)
lines(PCA.obj$sdev[1:20], col= "red", lwd = 3)

#Proportion of Variance & cumulative proportion of variance
plot(main = "Proportion of Variance", PropVariancePCA[1:20], xlab = "Principle Components", ylab = "Proportion of Variance", col = "blue", pch = 4, cex = 2, lwd=3)
lines(PropVariancePCA[1:20], col= "red", lwd = 3)

plot(main = "Cumulative Proportion of Variance", CumuPCA[1:20], xlab = "Principle Components", ylab = "Cumulative Proportion of Variance", col = "blue", pch = 4, cex = 2, lwd=3)
lines(CumuPCA[1:20], col= "red", lwd = 3)

##### Exercise 2.1.3 and Exercise 2.1.4 #####
#Prepare for knn process - Best k values are 3, 5 and 7 from previous assignment (KNN)
#Code example https://www.kaggle.com/victorzhang/pca-knn-with-r

#PC's representing 80 % of the accumulated variance 
PC.80 <- PCA.obj$x[, CumuPCA < 0.8] # Results in 14 PCs

#PC's representing 90 % of the accumulated variance
PC.90 <- PCA.obj$x[, CumuPCA < 0.9] #Results in 23 PCs

#PC's representing 95 % of the accumulated variance
PC.95 <- PCA.obj$x[, CumuPCA < 0.95] # Results in 34 PCs

#PC's representing 99 % of the accumulated variance
PC.99 <- PCA.obj$x[, CumuPCA < 0.99] # Results in 72 PCs

#The 'train.col.used' must be opdated depending on the number of PCs for specific percentage of variance
numberOfPCs <- 1:34

#Get train and test data from PCA object
train.pca <- PCA.obj$x #x = scores vector
test.pca <- predict(PCA.obj, dataset.test) #Finding Principle Components in test

time.start <- Sys.time()
model <- knn(train.pca[,numberOfPCs], test.pca[,numberOfPCs], dataset.train.labels,5)
time.end <- Sys.time()

#Run time
print(time.end-time.start)

#Performance
acc(model, dataset.test.labels)

##### Exercise 2.2.1 #####
#Normalization After PCA 
#If Normalization has to happen before PCA is run, normalize the dataset before that.
numberOfPCs <- 1:34 #The number of PCs.
PCA.dataset <- normalize(PCA.obj$x) #The best Dataset from Exercise 2.1.3 (95 % of accumulated variance with k-value of 5)

folds <- createFolds(PCA.dataset, 10)

a <- list()
s <- list()

for(i in 1:length(folds)){
  #Use PCA.obj.norm if you want the 'normalize data before PCA first' version
  cross.train <- PCA.dataset[-folds[[i]],]
  cross.train <- cross.train[1:2000,numberOfPCs]
  #cross.test <- PCA.dataset[folds[[i]],]
  
  cross.train.labels <- dataset.train.labels[-folds[[i]]] #Get the labels for dataset. 
  cross.train.labels <- cross.train.labels[1:2000]

  #NOTE
  #The lengths of cross.train and cross.train.labels are different. The length of cross.train are all data for 34 PCs.
  
  test <- dataset.test
  #test <- dataset.test[folds[[i]],]
  #test <- norm.test[folds[[i]],] 
  cross.test <- predict(PCA.obj.norm,test)
  
  #Run KNN algorithm
  time.start <- Sys.time()
  model <- knn(cross.train, cross.test[,numberOfPCs], cross.train.labels , 5)
  time.end <- Sys.time()
  
  s[i] <- time.end - time.start #runtime 
  a[i] <- acc(model, dataset.test.labels) #accuracy
  
}

mean(a)
mean(s)

plot(a)
plot(s)

##### Exercise 2.3.1 #####
DPI <- 100
group <- 4
member <- 3

folder <- 'C:/Users/Bruger/Desktop/Statistical Mashine Learning/2018/group'

cipherNumber <- 1001

rotateSelf <- function(x) t(apply(x, 2, rev))


id <- loadSinglePersonsData(DPI,group,member,folder)

imageSize <- sqrt(ncol(x) - 1)

imageCreate <- function(x){
  imageM <- matrix( x[cipherNumber,2:ncol(x)],nrow =
                      imageSize,ncol = imageSize,byrow = FALSE)
  
  imageM <- rotateSelf(imageM) # rotate is a function to rotate the image
  
  image( imageM )
  
}

imageCreate(id)

##### Exercise 2.3.2 #####
eigenVector <- 1:10

for (e in eigenVector) {
  imageNewM <- matrix(PCA.obj$rotation[,e],nrow = imageSize,ncol = imageSize,byrow = FALSE)
  image(imageNewM)
}

##### Exercise 2.3.3 #####

#All PCs
trunc <- PCA.obj$x[cipherNumber,1:nrow(PCA.obj$rotation)] %*%
  t(PCA.obj$rotation[,1:nrow(PCA.obj$rotation)])

trunc <- scale(trunc, center = -1 * PCA.obj$center, scale=FALSE)

imageCreate(trunc)
?prcomp
##### Exercise 2.3.4 #####

#80 Variacne 
trunc <- PCA.obj$x[cipherNumber,1:14] %*%
  t(PCA.obj$rotation[,1:14])

trunc <- scale(trunc, center = -1 * PCA.obj$center, scale=FALSE)

imageCreate(trunc)

#90 Variance
trunc <- PCA.obj$x[cipherNumber,1:23] %*%
  t(PCA.obj$rotation[,1:23])

trunc <- scale(trunc, center = -1 * PCA.obj$center, scale=FALSE)

imageCreate(trunc)

#95 Variance
trunc <- PCA.obj$x[cipherNumber,1:34] %*%
  t(PCA.obj$rotation[,1:34])

trunc <- scale(trunc, center = -1 * PCA.obj$center, scale=FALSE)

imageCreate(trunc)

##### Exercise 2.3.5 #####
cipherNumber <- 3501
p <- PCA.obj$x[cipherNumber, 1:10]

cipherNumber <- 400
p <- PCA.obj$x[cipherNumber, 1:10]










