
source("dataLoading.R")

folder <- "../../SML_Data/preProcessed/2018/group"

result <- getAllPersonsInData(folder, 10, 70)

model <- knn(result$train, result$test, result$train.labels, 3)
acc(model, result$test.labels)
