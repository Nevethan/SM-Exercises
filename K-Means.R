
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
model <- knn(train_data, dataset.test, train_labels,7)
time.end <- Sys.time()

print(time.end-time.start)

result <- confusionMatrix(dataset.test.labels, model)
sum(diag(result$table))/sum(result$table) # Accuracy


##### Exercise 3.1.2 #####
#Use the knn process for cluster centroid training data
#Used to compare the following (knn with raw data) in the end

# #Performing knn on raw data
# time.start <- Sys.time()
# model <- knn(dataset.train, dataset.test, dataset.train.labels, 3)
# time.end <- Sys.time()
# 
# print(time.end-time.start)
# 
# result <- confusionMatrix(dataset.test.labels, model)
# sum(diag(result$table))/sum(result$table)

#Cross validation on knn

folds <- createFolds(dataset.train,10) #list levels of factor v1
#folds <- createFolds(train_data$V1,10)

#Execution time and Accuracy
s <- list()
a <- list()

for(i in length(folds)){
  #Clustered dataset - Use the following trainset, but use the same test set.
  #cross.train <- train_data[-folds[[i]],]
  #cross.train.labels <- train_labels[-folds[[i]],]
  
  #Raw dataset
  cross.train <- dataset.train[-folds[[i]],]
  cross.train.labels <- dataset.train.labels[-folds[[i]],]
  
  cross.test <- dataset.test[folds[[i]],]
  cross.test.labels <- dataset.test.labels[folds[[i]],]
  
  time.start <- Sys.time()
  model <- knn(cross.train, cross.test, cross.train.labels, 3)
  time.end <- Sys.time()
  
  s[i] <- time.end-time.start
  a[i] <- acc(model,cross.test.labels)

}

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

#Repeat this process for 10 times. Remember to change 'e' according to specific digit
inst <- data.frame()
e <- 3600
for(i in 1:5){
  inst <- rbind(inst,id2[e,-1])
  e <- e +1
}

data.norm <- scale(inst)

#Dissimilarity function
fun <- function(x) as.dist((1-cor(t(x)))/2)

#Calculate dissimilarity matrix
data.dis <- fun(data.norm)

#Calculate hierarchical clustering (method is type of linkage)
data.hclust <- hclust(data.dis, method="ward.D2")

#Plot different dendrograms
plot(data.hclust)
plot(as.dendrogram(data.hclust))
plot(as.dendrogram(agnes(inst)))
plot(as.dendrogram(diana(inst)))

##### Exercise 3.2.2 #####
cipher_cluster <- c()
label_cluster <- c()
numberOfClusters <-  5

data.train <- id2[,-1]
data.train.labels <- factor(id2[,1])

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

data.norm <- scale(train_data)

#Dissimilarity function
fun <- function(x) as.dist((1-cor(t(x)))/2)

#Calculate dissimilarity matrix
data.dis <- fun(data.norm)

#Calculate hierarchical clustering (method is type of linkage)
data.hclust <- hclust(data.dis, method="ward.D2")

#Plot different dendrograms
plot(data.hclust)
plot(as.dendrogram(data.hclust))
plot(as.dendrogram(agnes(train_data)))
plot(as.dendrogram(diana(train_data)))

##### Exercise 3.3.1 #####


