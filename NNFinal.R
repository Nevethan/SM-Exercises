library(RSNNS)
library(devtools)
library(kernlab)

##### Preproccessing #####
source('loadImage.R')
source('Methods.R')
# Remember to source the two helper methods in the bottom

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

folder <- "../../StatisticML/preProcessed/2018/group"

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

#Disjunct - 20 members in total (10/10)
#dataset.train <- dataset.all[1:80000,]
#dataset.test <- dataset.all[80001:160000,]

#### All Persons in ####
dataset.shuffle <- datasetShuffle(dataset.all)

dataset.train <- dataset.shuffle[1:28000,]
dataset.validation <- dataset.shuffle[28001:34000,]
dataset.test <- dataset.shuffle[34001:40000,]

dataset.train.labels <- factor(dataset.train[,1])
dataset.validation.labels <- factor(dataset.validation[,1])
dataset.test.labels <- factor(dataset.test[,1])

dataset.train <- dataset.train[,-1]
dataset.validation <- dataset.validation[,-1]
dataset.test <- dataset.test[,-1]

##### Format classes #####
#Format the training classes so it matches a neural net with N inputs and 10
#outputs where each of the outputs matches a given class

lev <- levels(dataset.train.labels) # Number of classes

nnTrainingClass <- matrix(nrow = length(dataset.train.labels), ncol = 10, data = 0) # Create a list probabilities, for all labels
for(i in 1:length(dataset.train.labels)) { # Set probabilities to one for matching class
  matchList <- match(lev,toString(dataset.train.labels[i]))
  matchList[is.na(matchList)] <- 0
  nnTrainingClass[i,] <- matchList
}
trainingClass <- as.data.frame(nnTrainingClass)

# Variables
its <- c(10, 20, 30, 40, 50, 60, 70, 80, 90) ## Iterations
lr <- 0.08
neuron <- 20

# Train on training data
trainedModels.train <- trainModel6(its, lr, neuron, dataset.train, dataset.train.labels)

# Train on validation data
trainedModels.validation <- trainModel6(its, lr, neuron, dataset.validation, dataset.validation.labels)

# Specify hidden layers
plotResults(300)


########################### Helper Methods ##############################

## For plotting results
plotResults <- function(hiddenLayers) {
  mainText <- paste("LR =", lr, " HL =", hiddenLayers, " N =", neuron)
  
  plot(main = mainText, its,trainedModels.train$accuracyList, xlab = "Iterations",
       ylab = "Accuracy", col = colors[3], ylim=c(0.50,0.90), type = "p")
  points(its,trainedModels.train$accuracyList, col = colors[3], type = "l")
  
  axis(1, at=its, labels = its)
  points(its,trainedModels.validation$accuracyList, col = colors[1], type = "p")
  points(its,trainedModels.validation$accuracyList, col = colors[1], type = "l")
  
  legend(65, 0.7, legend=c("Training", "Validation"),
         col=c("blue", "red"), lty=1, cex=0.8)
}

## Runs a model multiple times for each specificed iteration
## To change layers or learnFunc, do it manually in the function
## Learningrate = default 0.2
trainModel <- function(iterations, learningRate, neurons, dataToPredictOn, dataToPredictOnLabel) {
  timeList <- c()
  accuracyList <- c()
  
  for (j in 1:length(iterations)) {
    k <- iterations[j]
    time.start <- Sys.time()                         # Change hidden layers here
    model <- mlp(dataset.train, trainingClass, size = c(neurons,neurons,neurons), 
                 maxit = k, learnFuncParams = c(learningRate), learnFunc = "Std_Backpropagation")
    
    time.end <- Sys.time()
    time.taken <- time.end - time.start
    timeList[j] <- time.taken
    print(time.end-time.start)
    
    predictions <- predict(model, dataToPredictOn)
    
    #Error over iterations 
    #plotIterativeError(model)
    
    #Inspiration code from exercise 
    #Remember to run 'predictions'
    responselist <- 0
    
    responselist <- matrix(nrow = length(predictions[,1]), ncol = 1, data = "Na")
    
    for(i in 1:nrow(predictions)) {
      responselist[i,] <- toString( which(predictions[i,]==max(predictions[i,])) - 1 )
    }
    
    responselist <- data.frame(responselist)
    responselist[,1] <- as.factor(responselist[,1])
    # Calculating the accuracy
    agreement_rbf <- responselist[,1] == dataToPredictOnLabel
    #agreement_rbf <- responselist[,1] == dataset.test.labels
    table(agreement_rbf)
    acc <- prop.table(table(agreement_rbf))
    
    accuracyList[j] <- acc[2]
    
  }
  return(list("accuracyList" = accuracyList, "timeList" = timeList))
}
