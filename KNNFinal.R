
source("dataLoading.R")

folder <- "../../SML_Data/preProcessed/2018/group"

result <- getAllPersonsInData(folder, 2, 70)

set.seed(1234)

time.start <- Sys.time() 
model <- knn(result$train, result$test, result$train.labels, 3)
time.end <- Sys.time()

print(time.end-time.start)

acc(model, result$test.labels)






