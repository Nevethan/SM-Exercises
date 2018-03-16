#source('~/workspace/svn/trunk/BaseFolder/loadImage.R', echo=TRUE)
source('C:/Users/Bruger/Desktop/Statistical Mashine Learning/BaseFolder/loadImage.R')

#folder <- "/home/frederik/workspace/svn/trunk/preProcessed/2018/group"
folder <- "C:/Users/Bruger/Desktop/Statistical Mashine Learning/preProcessed/2018/group"

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

id <- loadSinglePersonsData(100,1,2,folder)
id <- data.frame(id)

id2 <- loadSinglePersonsData(100,1,1,folder)
id2 <- data.frame(id2)

id <- rbind(id, id2)

id$X1 <- factor(id$X1)

set.seed(423)

id_shuffle <- id[sample(nrow(id)),] # We could shuffle

id_n <- as.data.frame(lapply(id[2:325], normalize))

id_train <- id_n[1:4000,]
id_test <- id_n[4001:8000,]

id_train_labels <- id[1:4000,1]
id_test_labels <- id[4001:8000,1]

set.seed(2345)

cipher_cluster <- c()
label_cluster <- c()
numberOfClusters <-  100

for( i in 0:9) {
  clusterData <- kmeans(id_train[ id_train_labels == i, ],numberOfClusters)
  cipher_cluster[[i + 1]] <- clusterData$centers
  label_cluster[[i + 1]] <- c(1:numberOfClusters)*0 + i
}

train_lab <- factor(unlist(label_cluster))
train_dat <- cipher_cluster[[1]]
for( i in 2:10) {
  train_dat <- rbind(train_dat,cipher_cluster[[i]])
}

id_test_pred <- knn(train = train_dat, test = id_test, cl = train_lab, k=1)

result <- confusionMatrix(id_test_labels, id_test_pred)
sum(diag(result$table))/sum(result$table)

id_test_pred <- knn(train = id_train, test = id_test, cl = id_train_labels, k=1)

result <- confusionMatrix(id_test_labels, id_test_pred)
sum(diag(result$table))/sum(result$table)


# Clustering all together
numberOfClusters <-  500

clusterData <- kmeans(id_train, numberOfClusters)

cipher_cluster <- c()
label_cluster <- c()

for( i in 1:numberOfClusters) {
  maxIndex <- which.max(table(id_train_labels[ clusterData$cluster[ clusterData$cluster == i ] ]))
  cipher_cluster[[i ]] <- clusterData$centers
  label_cluster[[i ]] <- c(1:numberOfClusters)*0 + i - 1
}

train_lab <- factor(unlist(label_cluster))
train_dat <- cipher_cluster[[1]]
for( i in 2:numberOfClusters) {
  train_dat <- rbind(train_dat,cipher_cluster[[i]])
}

id_test_pred <- knn(train = train_dat, test = id_test, cl = train_lab, k=1)
result <- confusionMatrix(id_test_labels, id_test_pred)
sum(diag(result$table))/sum(result$table)


