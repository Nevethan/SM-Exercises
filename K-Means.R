
#Source loading of images. "k-clustering" contains code to help you.
#source('C:/Users/Bruger/Desktop/SM-Exercises/k-clustering.R')

id <- loadSinglePersonsData(100,4,1,folder)
id <- data.frame(id)

id2 <- loadSinglePersonsData(100,4,2,folder)
id2 <- data.frame(id2)

id <- rbind(id, id2)

dataset <- data.frame(id)

#Disjunct dataset for two persons. 
dataset.train <- datasetShuffle(dataset[1:4000,])
dataset.test <- datasetShuffle(dataset[4001:8000,])

dataset.train.labels <- factor(dataset.train[,1])
dataset.test.labels <- factor(dataset.test[,1])

dataset.train <- normalize(dataset.train[,-1])
dataset.test <- normalize(dataset.test[,-1])

set.seed(1234)
#set.seed(2345)

##### Exercise 3.1.1 #####
cipher_cluster <- c()
label_cluster <- c()
numberOfClusters <-  25 #Changing the number (25, 50 and 100)

#Performing K-means clusering of each cipher individually for the training set. 
# -> represent the training data as a number of cluster centroids
for( i in 0:9) {
  clusterData <- kmeans(dataset.train[ dataset.train.labels == i, ],numberOfClusters)
  cipher_cluster[[i + 1]] <- clusterData$centers #Finding Centroids
  label_cluster[[i + 1]] <- c(1:numberOfClusters)*0 + i
}

train_labels <- factor(unlist(label_cluster))
train_data <- cipher_cluster[[1]]
for( i in 2:10) {
  train_data <- rbind(train_data,cipher_cluster[[i]])
}

#Performing knn on the training data (cluster centroids)
# K values 3,5 og 7
# Cluster 100, 50 og 25
time.start <- Sys.time()
model <- knn(train_data, dataset.test, train_labels,3)
time.end <- Sys.time()

print(time.end-time.start)

result <- confusionMatrix(dataset.test.labels, model)
sum(diag(result$table))/sum(result$table) # Accuracy


##### Exercise 3.1.2 #####
#Use the knn process for cluster centroid training data
#Used to compare the following (knn with raw data) in the end

#Performing knn on raw data
time.start <- Sys.time()
model <- knn(dataset.train, dataset.test, dataset.train.labels, 3)
time.end <- Sys.time()

print(time.end-time.start)

result <- confusionMatrix(dataset.test.labels, model)
sum(diag(result$table))/sum(result$table) #Accuracy

#Cross validation on KMeans
dataset.train <- datasetShuffle(dataset[1:4000,])
folds <- createFolds(dataset.train,10)

a <- c()
c <- list()
l <- list()

for(i in 1:length(folds)){
  #Training dataset for cross validation
  cross.dataset.train <- dataset.train[-folds[[i]],-1]
  cross.dataset.train.labels <- factor(dataset.train[-folds[[i]],1])
  
  cross.dataset.test <- dataset.train[folds[[i]],-1]
  cross.dataset.test.labels <- factor(dataset.train[folds[[i]],1])
  
  #Cross Validation for Kmeans
  for( j in 0:9) {
    clusterData <- kmeans(cross.dataset.train[ cross.dataset.train.labels == j, ],numberOfClusters)
    cipher_cluster[[j + 1]] <- clusterData$centers #Finding Centroids
    label_cluster[[j + 1]] <- c(1:numberOfClusters)*0 + j
  }
  
  train_labels <- factor(unlist(label_cluster))
  train_data <- cipher_cluster[[1]]
  
  for( k in 2:10) {
    train_data <- rbind(train_data,cipher_cluster[[k]])
  }
  
  c[[i]] <- train_data
  l[[i]] <- train_labels
  
  model <- knn(train_data, cross.dataset.test, train_labels,3)
  
  result <- confusionMatrix(cross.dataset.test.labels, model)
  a[i] <- sum(diag(result$table))/sum(result$table) # Accuracy
}

plot(a, ylab = "Accuracy", xlab = "Folds")

#Found the Train_data with best Centroids. Use it in knn again.
#Change the index manually. 
train_data <- c[[2]]
train_labels <- factor(unlist(l[[2]]))

time.start <- Sys.time()
model <- knn(train_data, dataset.test, train_labels,3)
time.end <- Sys.time()

print(time.end - time.start)

result <- confusionMatrix(dataset.test.labels, model)
sum(diag(result$table))/sum(result$table) # Accuracy

##### Exercise 3.1.3 #####
## Prepre the data ##
#Load personImages for 40 personer - 20/20
#Repeat the same process from exercise 3.1.1
#Disjunct dataset for 40 persons. 
dataset.train <- datasetShuffle(dataset.all[1:80000,])
dataset.test <- datasetShuffle(dataset.all[80001:160000,])

dataset.train.labels <- factor(dataset.train[,1])
dataset.test.labels <- factor(dataset.test[,1])

dataset.train <- normalize(dataset.train[,-1])
dataset.test <- normalize(dataset.test[,-1])

set.seed(1234)
#set.seed(2345)

cipher_cluster <- c()
label_cluster <- c()
numberOfClusters <-  50

#Performing K-means clusering of each cipher individually for the training set. 
# -> represent the training data as a number of cluster centroids
for( i in 0:9) {
  clusterData <- kmeans(dataset.train[ dataset.train.labels == i, ],numberOfClusters)
  cipher_cluster[[i + 1]] <- clusterData$centers
  label_cluster[[i + 1]] <- c(1:numberOfClusters)*0 + i
}

train_labels <- factor(unlist(label_cluster))
train_data <- cipher_cluster[[1]]
for( i in 2:10) {
  train_data <- rbind(train_data,cipher_cluster[[i]])
}

#Performing knn on the training data (cluster centroids)
# K values 3,5 og 7
# Cluster 100, 50 og 25
time.start <- Sys.time()
model <- knn(train_data, dataset.test,train_labels,3)
time.end <- Sys.time()

print(time.end-time.start)

result <- confusionMatrix(dataset.test.labels, model)
sum(diag(result$table))/sum(result$table) #Accuracy

##### Exercise 3.2.1 #####
library(cluster)
library(dendextend)
#Repeat this process for 10 times. Remember to change 'e' according to specific digit
inst <- data.frame()
e <- 3900
for(i in 1:5){
  data <- id2[e,-1]
  data[["id"]] <- paste("Cipher 9 (",  e , ")",sep = "")
  inst <- rbind(inst,data)
  e <- e +1
}

data.norm <- scale(inst[,-325])

#Dissimilarity function
fun <- function(x) as.dist((1-cor(t(x)))/2)

#Calculate dissimilarity matrix
data.dis <- fun(data.norm)

#Calculate hierarchical clustering (method is type of linkage)
data.hclust <- hclust(data.dis, method="ward.D2")

data.hclust$labels = inst$id #Replace labels with labels from inst.
#Plot different dendrograms
plot(data.hclust)
plot(as.dendrogram(data.hclust))
plot(as.dendrogram(agnes(inst)))
plot(as.dendrogram(diana(inst)))

##### Exercise 3.2.2 #####
cipher_cluster <- c()
label_cluster <- c()
numberOfClusters <-  5

dataset.train <- id2[,-1]
dataset.train.labels <- factor(id2[,1])

#Performing K-means clusering of each cipher individually for the training set. 
# -> represent the training data as a number of cluster centroids
for( i in 0:9) {
  clusterData <- kmeans(dataset.train[ dataset.train.labels == i, ],numberOfClusters)
  cipher_cluster[[i + 1]] <- clusterData$centers #Finding Centroids
  label_cluster[[i + 1]] <- c(1:numberOfClusters)*0 + i
  
}

train_labels <- factor(unlist(label_cluster))
train_data <- data.frame()
train_data <- cipher_cluster[[1]]

for( i in 2:10) {
  train_data <- rbind(train_data,cipher_cluster[[i]])
}

train_data <- as.data.frame(train_data)
train_data[["id"]] <- ""
for(i in 0:9){
  for(k in 1:5){
      train_data[[(i*5)+k,"id"]] <- paste("Cipher " , i , " (",  k , ")",sep = "")
  }
}
data.norm <- scale(train_data[,-325])

#Dissimilarity function
fun <- function(x) as.dist((1-cor(t(x)))/2)

#Calculate dissimilarity matrix
data.dis <- fun(data.norm)

#Calculate hierarchical clustering (method is type of linkage)
data.hclust <- hclust(data.dis, method="ward.D2")

data.hclust$labels = train_data[,325] #Replace labels with labels from inst.
#Plot different dendrograms
plot(data.hclust)
plot(as.dendrogram(data.hclust), )
plot(as.dendrogram(agnes(train_data)))
plot(as.dendrogram(diana(train_data)))


##### Exercise 3.3.1 #####

precision <- list()
recall <- list()

for(i in 1:13){
  model <- knn(dataset.train, dataset.test, dataset.train.labels, k = i, l = i)
  
  #ConfusionMatrix 
  result <- confusionMatrix(dataset.test.labels, model)
  
  precision[i] <- (diag(result$table) / rowSums(result$table)) #Precision
  recall[i] <- (diag(result$table) / colSums(result$table)) #Recall
  #sum(diag(result$table))/sum(result$table) #Accuracy
}

plot(recall, precision)
