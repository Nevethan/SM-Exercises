
source("dataLoading.R")

folder <- "../../SML_Data/preProcessed/2018/group"




###################################################################
######################### Individual Data ######################### 
###################################################################

####################### Pre Processing ############################

##### No Pre Processing #####
noPreProcessingData <- result

##### PCA #####
pcaData

##### K-Means #####
kmeansData

##################### Cross Validation ############################

##### CV On noPreProcessingData #####

##### CV On pcaData #####

##### K-Means On pcaData #####

########################## Apply KNN ##############################

##### KNN on noPreProcessingData #####

##### KNN On pcaData #####

##### KNN On kmeansData #####

########################## Parameter Tuning ######################

###################################################################
######################### All Persons In ########################## 
###################################################################


####################### Pre Processing ############################

result <- getAllPersonsInData(folder, 2, 70)

##### No Pre Processing #####

noPreProcessingData <- result

##### PCA #####
pcaData <- result

##### K-Means #####
kmeansData <- result

##################### Cross Validation ############################



##### CV On noPreProcessingData #####

crossValidation(noPreProcessingData, 10, 5)




#plot the speed.cross and accuracy_cross
plot(K, noPreProcessing.timeList.mean, xlab = "Number of K", ylab = "time (seconds)")
plot(K, noPreProcessing.accuracyList.mean, xlab = "Number of K", ylab = "Accuracy")

##### CV On pcaData #####

##### K-Means On pcaData #####

########################## Apply KNN ##############################

##### KNN on noPreProcessingData #####
set.seed(1234)

time.start <- Sys.time() 
model <- knn(result$train, result$test, result$train.labels, 1)
time.end <- Sys.time()

print(time.end-time.start)

acc(model, result$test.labels)

##### KNN On pcaData #####

##### KNN On kmeansData #####

########################## Parameter Tuning ######################

###################################################################
############################ Disjunct ############################# 
###################################################################

####################### Pre Processing ############################

##### No Pre Processing #####
noPreProcessingData <- result

##### PCA #####
pcaData

##### K-Means #####
kmeansData

##################### Cross Validation ############################

##### CV On noPreProcessingData #####

##### CV On pcaData #####

##### K-Means On pcaData #####

########################## Apply KNN ##############################

##### KNN on noPreProcessingData #####

##### KNN On pcaData #####

##### KNN On kmeansData #####

########################## Parameter Tuning ######################




##################################### Helper Methods ##########################

crossValidation <- function(data, f, k) {
  accuracyList <- list()
  accuracyList.mean <- list()
  timeList <- list()
  timeList.mean <- list()
  
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
    
    accuracyList <- 0
    timeList <- 0
    
  }
  
  return(list("accuracy" = accuracyList.mean, "time" = timeList.mean))
}
