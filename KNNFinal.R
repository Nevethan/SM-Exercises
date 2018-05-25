
source("dataLoading.R")

folder <- "../../StatisticML/preProcessed/2018/group"


######################### Individual Data ######################### 
#### [I] Pre Processing ####
i.result <- getIndividualData(folder, 5, 70)

## No Pre Processing ##
i.noPreProcessingData <- i.result

## PCA ##
i.pcaData

## K-Means ##
i.kmeansData

#### [I] Cross Validation ####
## CV On noPreProcessingData ##
KNN.k = 20
i.np.cv.results <- crossValidation(noPreProcessingData, 10, KNN.k)

#plot the speed.cross and accuracy_cross
plot(1:KNN.k, i.np.cv.results$time, xlab = "Number of K", ylab = "time (seconds)")
plot(1:KNN.k, i.np.cv.results$accuracy, xlab = "Number of K", ylab = "Accuracy")

## CV On pcaData ##

## CV on K-Means ##

#### [I] Apply KNN ####
## KNN on noPreProcessingData ##

## KNN On pcaData ##

## KNN On kmeansData ##

#### [I] Parameter Tuning #####


######################### All Persons In ########################## 
#### [A] Pre Processing ####
a.result <- getAllPersonsInData(folder, 2, 70)

## No Pre Processing ##
a.noPreProcessingData <- a.result

## PCA ##
a.pcaData <- a.result

a.pca.obj <- prcomp(a.pcaData$train)

#Get train and test data from PCA object
a.train.pca <- a.pca.obj$x #x = scores vector
a.test.pca <- predict(a.pca.obj, a.pcaData$test) #Finding Principle Components in test


## K-Means ##
a.kmeansData <- a.result


#### [A] Cross Validation ####
## CV On noPreProcessingData ##
KNN.k = 20

time.start <- Sys.time() 

a.np.cv.results <- crossValidation(a.noPreProcessingData, 10, KNN.k)

time.end <- Sys.time()

print(time.end-time.start)

#plot the speed.cross and accuracy_cross
plot(1:KNN.k, a.np.cv.results$time, xlab = "Number of K", ylab = "time (seconds)")
plot(1:KNN.k, a.np.cv.results$accuracy, xlab = "Number of K", ylab = "Accuracy", ylim = c(99.3, 100))
points(1:KNN.k, a.np.cv.results$test, col= c("red"))
lines(1:KNN.k, a.np.cv.results$test, col= c("red"))
lines(1:KNN.k, a.np.cv.results$accuracy, col= c("black"))


## CV On pcaData ##

## CV On K-Means ##


#### [A] Apply KNN ####

## KNN on noPreProcessingData ##
set.seed(1234)

time.start <- Sys.time() 
a.knn.model <- knn(result$train, result$test, result$train.labels, 3)
time.end <- Sys.time()

print(time.end-time.start)

acc(a.knn.model, result$test.labels)

## KNN On pcaData ##

a.numberOfPCs <- 1:20

time.start <- Sys.time()
a.pca.model <- knn(a.train.pca[, a.numberOfPCs], a.test.pca[, a.numberOfPCs],a.pcaData$train.labels,5)
time.end <- Sys.time()

#Run time
print(time.end - time.start)

#Performance
acc(a.pca.model, a.pcaData$test.labels)

## KNN On kmeansData ##

#### [A] Parameter Tuning ####





############################ Disjunct ############################# 
#### [D] Disjunct Pre Processing ####
d.result <- getDisjunctData(folder, 5, 70)
## No Pre Processing ##
d.noPreProcessingData <- d.result

## PCA ##
d.pcaData

## K-Means ##
d.kmeansData

#### [D] Cross Validation ####
## CV On noPreProcessingData ##

## CV On pcaData ##

## CV On K-Means ##

#### [D] Apply KNN ####
## KNN on noPreProcessingData ##

# KNN On pcaData ##

## KNN On kmeansData ##

#### [D] Parameter Tuning ####


##################### Helper Methods ##############################

crossValidation <- function(data, f, k) {
  accuracyList <- list()
  accuracyList.mean <- list()
  timeList <- list()
  timeList.mean <- list()
  
  testAccuracy <- list()
  
  K <- 1:k
  folds <- createFolds(data$train.labels, f)
  
  for (i in K) {
    for (j in 1:length(folds)) {
      
      cv.validation <- data$train[folds[[j]],]
      cv.train <- data$train[-folds[[j]],]
      
      cv.train.labels <- data$train.labels[-folds[[j]]]
      cv.validation.labels <- data$train.labels[folds[[j]]]
      
      time.start <- Sys.time()
      model <- knn(cv.train, cv.validation, cv.train.labels,i)
      time.end <- Sys.time()
      
      time.taken <- time.end - time.start
      
      timeList[j] <- time.taken
      accuracyList[j] <- acc(model, cv.validation.labels)
      
    }
    
    timeList.mean[i] <- mean(timeList)
    accuracyList.mean[i] <- mean(accuracyList)
    
    t.model <- knn(data$train, data$test, data$train.labels,i)
    testAccuracy[i] <- acc(t.model, data$test.labels)
    
    accuracyList <- 0
    timeList <- 0
    
  }
  return(list("accuracy" = accuracyList.mean, "time" = timeList.mean, "test"=testAccuracy))
}
