#Source image read
#source('C:/Users/Bruger/Desktop/SM-Exercises/loadImage.R', echo=TRUE)

##### Preprocessing #####
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

#Split data 50/50

#Disjunct - 20 members in total
dataset.train <- dataset.all[1:40000,]
dataset.test <- dataset.all[40001:80000,]

dataset.train <- datasetShuffle(dataset.train)
dataset.test <- datasetShuffle(dataset.test)

dataset.train.labels <- factor(dataset.train[,1])
dataset.test.labels <- factor(dataset.test[,1])

dataset.train <- dataset.train[,-1]
dataset.test <- dataset.test[,-1]

##### Exercise 4.1.1 #####
#Running the PCA object and getting the first 5
#numberOfPCA <- 5 #Number of PCA

pca.object <- prcomp(dataset.train) #PCA Object

#Getting the data for the specific PCA
PCA.1 <- pca.object$x[,1] 
PCA.2 <- pca.object$x[,2]
PCA.3 <- pca.object$x[,3]
PCA.4 <- pca.object$x[,4]
PCA.5 <- pca.object$x[,5]

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

#With Threshold. - Dividing dataset into two seperate datasets depending on threshold.
getEntropyThreshold <- function(pc,labels,threshold){
  entropy.before <- getEntropy(pc, labels)
  
  dataset1 <- c()
  dataset1.labels <- c()
  dataset2 <- c()
  dataset2.labels <- c()
  
  count1 <- 0
  count2 <- 0
  
  for (i in 1:length(pc)) {
    if (pc[i] < threshold) {
      dataset1[count1+1] <- pc[i]
      dataset1.labels[count1+1] <- dataset.train.labels[i]
      count1 <- count1 + 1
    } else {
      dataset2[count2+1] <- pc[i]
      dataset2.labels[count2+1] <- dataset.train.labels[i]
      count2 <- count2 + 1
    }
    
  }
  
  entropy1 <- (length(dataset1)*getEntropy(dataset1,dataset1.labels)) / (sum(length(dataset1), length(dataset2)))
  entropy2 <- (length(dataset2)*getEntropy(dataset2,dataset2.labels)) / (sum(length(dataset1), length(dataset2)))
  
  entropy.after <- sum(entropy1, entropy2)
  
  info.gain <- entropy.before - entropy.after
  
  return(info.gain)
}

tholds <- list()
tholds <- seq(-1,1,0.01) #Thresholds for the formula

info.gain.1 <- list()
info.gain.2 <- list()
info.gain.3<- list()
info.gain.4 <- list()
info.gain.5 <- list()

for(i in 1:length(tholds)){
  info.gain.1[i] <- getEntropyThreshold(PCA.1, dataset.train.labels,tholds[i])
  info.gain.2[i] <- getEntropyThreshold(PCA.2, dataset.train.labels,tholds[i])
  info.gain.3[i] <- getEntropyThreshold(PCA.3, dataset.train.labels,tholds[i])
  info.gain.4[i] <- getEntropyThreshold(PCA.4, dataset.train.labels,tholds[i])
  info.gain.5[i] <- getEntropyThreshold(PCA.5, dataset.train.labels,tholds[i])
}

plot(tholds,info.gain.1, xlab = "Threshold", ylab = "Information Gain", col = colors[1], ylim = c(0.3,0.7)) #PCA 1
points(tholds,info.gain.2, col = colors[2]) #PCA 2
points(tholds,info.gain.3, col = colors[3]) #PCA 3
points(tholds,info.gain.4, col = colors[4]) #PCA 4
points(tholds,info.gain.5, col = colors[5]) #PCA 5

legend(0.65,0.65, legend = c("PC 1", "PC 2", "PC 3", "PC 4", "PC 5"), col = colors[1:5], pt.bg = colors[1:5], pch = c(1))

#Finding the highest information gain
high.info <- max(unlist(info.gain.1))
high.info

#Answers = 0.5807, 0.5852, 0.5487, 0.6202, 0.4434

#Finding the optimal point - change the info.gain 
optimal.point <- tholds[which(unlist(info.gain.1) == max(unlist(info.gain.1)))]
optimal.point

#Answers = 0.68, -0.23, -0.05, 0.09, -0.09

##### Exercise 4.1.2 #####
# Inspiration - http://trevorstephens.com/kaggle-titanic-tutorial/r-part-3-decision-trees/
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(caret)

classifier <- rpart(formula = dataset.train.labels ~ ., data = dataset.train, method = "class") #Fitting the Decision tree
 
#Different plots of the decision tree
fancyRpartPlot(classifier, cex = 0.52, type = 0, caption = NULL, leaf.round = 10)
#rpart.plot(classifier, cex = 0.5, type = 0)

plot(classifier)
text(classifier)

#Better visualization of Decision tree
prp(classifier,main = "Decision Tree",varlen=4)

#Prediction of the model
pred <- predict(classifier, newdata = dataset.test, type = "class")

#ConfusionMatrix with all 10 digits
cm <- table(dataset.test.labels, pred)

#ConfusionMatrix to calculate the Precision and Accuracy
result <- confusionMatrix(dataset.test.labels, pred)

sum(diag(result$table))/sum(result$table) # Accuracy
(diag(result$table) / rowSums(result$table)) #Precision

##### Exercise 4.1.3 #####

#Remember to run the dataset.train again with the labels
dataset.train <- datasetShuffle(dataset.train)
folds <- createFolds(dataset.train,k=10)

a <- list()

for(i in 1:length(folds)){
  cross.train <- dataset.train[-folds[[i]],-1]
  cross.test <- dataset.train[folds[[i]],-1]
  
  cross.train.labels <- factor(dataset.train[-folds[[i]],1])
  cross.test.labels <- factor(dataset.train[folds[[i]],1])
  
  classifier <- rpart(formula = cross.train.labels ~ ., data = cross.train, method = "class") #Fitting the Decision tree
  
  pred <- predict(classifier, newdata = cross.test, type = "class")
  
  #ConfusionMatrix to calculate the Precision and Accuracy
  result <- confusionMatrix(cross.test.labels, pred)
  
  a[i] <- sum(diag(result$table))/sum(result$table) # Accuracy
}

#Accuracy plot for each fold
plot(1:length(folds),a, xlab = "Folds", ylab = "Accuracy", main = "Cross Validation - Evaluation")

##### Exercise 4.2.1 #####
#Inspiration - http://trevorstephens.com/kaggle-titanic-tutorial/r-part-5-random-forests/
library(randomForest)

set.seed(1234)

model <- randomForest(dataset.train.labels ~ ., data = dataset.train, ntree = 500)

#varImpPlot(model)
plot(model, main = "Error vs Number of trees")


#Remember to re-run the dataset. The dataset should include the labels as well. Inside the cross validation, we will shuffle and divide.
dataset.train <- datasetShuffle(dataset.train)
folds <- createFolds(dataset.train,10)

#lists for different depths
a.6 <- list()
a.5 <- list()

a.6.50 <- list()
a.5.50 <- list()

for(i in 1:length(folds)){
  cross.train <- dataset.train[-folds[[i]],-1]
  cross.test <- dataset.train[folds[[i]],-1]
  
  cross.train.labels <- factor(dataset.train[-folds[[i]],1])
  cross.test.labels <- factor(dataset.train[folds[[i]],1])
  
  model <- randomForest(cross.train.labels ~ ., data = cross.train, ntree = 50, control = list(maxdepth = 6), method = "class")
  
  pred <- predict(model, newdata = cross.test, type = "class")
  
  #ConfusionMatrix to calculate the Precision and Accuracy
  result <- confusionMatrix(cross.test.labels, pred)
  
  a.6.50[i] <- sum(diag(result$table))/sum(result$table) # Accuracy
}


#Accuracy plot for each fold
plot(1:length(folds),a.6, col = colors[1], lwd = 3, cex = 3,xlab = "Folds", ylab = "Accuracy", main = "Cross Validation - Evaluation (RandomForest)")
points(1:length(folds), a.5, col = colors[3], lwd = 3, cex = 3, pch = 4)
legend(8,0.9, legend = c("depth 6", "depth 5"),lty = 0, cex = 1.5, col = c(colors[1],colors[3]), pt.bg = c(colors[1],colors[3]), pch = c(1,4))


plot(ylim = c(0.80,1.0), 1:length(folds),a.6.50, col = colors[1],lwd = 3, cex = 3,xlab = "Folds", ylab = "Accuracy", main = "Cross Validation - Evaluation (RandomForest)")
points(1:length(folds), a.5.50, col = colors[3],lwd = 3, cex = 3, pch = 4)
legend(8,0.90, legend = c("depth 6", "depth 5"),lty = 0, cex = 1.5, col = c(colors[1],colors[3]), pt.bg = c(colors[1],colors[3]), pch = c(1,4))



