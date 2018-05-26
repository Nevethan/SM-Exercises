source("dataLoading.R")
folder <- "/Users/Anna/svn/new/trunk/preProcessed/2018/group"
result <- getAllPersonsInData(folder,10,70)

library(randomForest)
library(beepr)
set.seed(1234)

#model <- randomForest(result$train.labels ~ ., data = result$train, ntree = 500)

#varImpPlot(model)
#plot(model, main = "Error vs Number of trees")

#Remember to re-run the dataset. The dataset should include the labels as well. Inside the cross validation, we will shuffle and divide.
folds <- createFolds(result$train,10)

#lists for different depths
maxdepth = c()
maxdepth.accuracy = c()
depths = c(1,2,3,4,5,6,8,9,10,12,14,16,18,20)
for (a in 1:length(depths)){
  accuracy <- c()
  for(i in 1:length(folds)){
    cross.train <- result$train[-folds[[i]],]
    cross.test <- result$train[folds[[i]],]
    
    cross.train.labels <- result$train.labels[-folds[[i]]]
    cross.test.labels <- result$train.labels[folds[[i]]]
    
    model <- randomForest(cross.train.labels ~ ., data = cross.train, ntree = 100, control = list(maxdepth = depths[a]), method = "class")
    
    pred <- predict(model, newdata = cross.test, type = "class")
    
    #ConfusionMatrix to calculate the Precision and Accuracy
    result.perf <- confusionMatrix(cross.test.labels, pred)
    
    accuracy[i] <- sum(diag(result.perf$table))/sum(result.perf$table) # Accuracy
  }
  maxdepth[a] <- depths[a]
  maxdepth.accuracy[a] <- mean(accuracy)
  beep()
}
#Accuracy plot for each fold
plot(maxdepth, maxdepth.accuracy, col = colors[1], xlab = "Max Depth", ylab = "Accuracy", main = "Cross Validation - Evaluation (RandomForest)")
lines(maxdepth, maxdepth.accuracy, col = colors[1])

#lists for different depths
ntrees = c()
ntrees.accuracy = c()
trees = c(10,20,30,40,50,75,100,150)
for (a in 1:length(trees)){
  accuracy <- c()
  for(i in 1:length(folds)){
    cross.train <- result$train[-folds[[i]],]
    cross.test <- result$train[folds[[i]],]
    
    cross.train.labels <- result$train.labels[-folds[[i]]]
    cross.test.labels <- result$train.labels[folds[[i]]]
    
    model <- randomForest(cross.train.labels ~ ., data = cross.train, ntree = trees[a], control = list(maxdepth = 5), method = "class")
    
    pred <- predict(model, newdata = cross.test, type = "class")
    
    #ConfusionMatrix to calculate the Precision and Accuracy
    result.perf <- confusionMatrix(cross.test.labels, pred)
    
    accuracy[i] <- sum(diag(result.perf$table))/sum(result.perf$table) # Accuracy
  }
  ntrees[a] <- trees[a]
  ntrees.accuracy[a] <- mean(accuracy)
  beep()
}
#Accuracy plot for each fold
plot(ntrees, ntrees.accuracy, col = colors[1],xlab = "Trees", ylab = "Accuracy", main = "Cross Validation - Evaluation (RandomForest)")
lines(ntrees, ntrees.accuracy, col = colors[1])
ntrees.test <- c()
for (b in 1:length(trees)) {
  model <- randomForest(result$train.labels ~ ., data = result$train, ntree = trees[b], control = list(maxdepth = 5), method = "class")
  
  pred <- predict(model, newdata = result$test, type = "class")
  
  #ConfusionMatrix to calculate the Precision and Accuracy
  result.perf <- confusionMatrix(result$test.labels, pred)
  
  ntrees.test[b] <- sum(diag(result.perf$table))/sum(result.perf$table) # Accuracy
}
points(ntrees, ntrees.test, col = colors[2])
lines(ntrees, ntrees.test, col = colors[2])