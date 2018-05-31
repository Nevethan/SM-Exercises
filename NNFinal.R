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

folder <- "../../SML_Data/preProcessed/2018/group"

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

dataset.train <- dataset.shuffle[1:28000,]           # 70% of 10 persons (28 000)
dataset.validation <- dataset.shuffle[28001:34000,]  # 15% of 10 persons (6 000)
dataset.test <- dataset.shuffle[34001:40000,]        # 15% of 10 persons (6 000)

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

#### [it] Tune on Iterations ####
# Variables
its <- c(10, 20) ## Iterations
lr <- 0.08
neuron <- 30

# [IT]Train on training data
it.trainedModels.train <- trainModel(its, lr, neuron, dataset.train, dataset.train.labels)
it.train.acc.1 <- it.trainedModels.train$accuracyList
it.train.time.1 <- it.trainedModels.train$timeList

# Train on validation data
it.trainedModels.validation <- trainModel(its, lr, neuron, dataset.validation, dataset.validation.labels)
it.val.acc.1 <- it.trainedModels.validation$accuracyList
it.val.time.1 <- it.trainedModels.validation$timeList

# Plot
mainText <- paste("LR =", lr, " HL =", 3, " N =", neuron)

plot(main = mainText, its,it.train.acc.1, xlab = "Iterations",
     ylab = "Accuracy", col = "blue", ylim=c(0.50,0.90), type = "p")
points(its,it.train.acc.1, col = "blue", type = "l")

axis(1, at=its, labels = its)
points(its, it.val.acc.1, col = colors[1], type = "p")
points(its, it.val.acc.1, col = colors[1], type = "l")

legend(16, 0.6, legend=c("Training", "Validation"),
       col=c("blue", "red"), lty=1, cex=0.8)

#### [n] Tune on Number of Neurons ####
# Variables
its <- c(10,20, 30, 40, 50, 60, 70, 80, 90, 100) ## Iterations
lr <- 0.08

# Training Data
# 50,80,100,130, 150
neuron <- 50
# Train on training data
n.trainedModels.train <- trainModel(its, lr, neuron, dataset.train, dataset.train.labels)
n.train.acc.1 <- n.trainedModels.train$accuracyList
n.train.time.1 <- n.trainedModels.train$timeList

# 50,80,100,130, 150
neuron <- 100
n.trainedModels.train <- trainModel(its, lr, neuron, dataset.train, dataset.train.labels)
n.train.acc.2 <- n.trainedModels.train$accuracyList
n.train.time.2 <- n.trainedModels.train$timeList

# 50,80,100,130, 150
neuron <- 150
n.trainedModels.train <- trainModel(its, lr, neuron, dataset.train, dataset.train.labels)
n.train.acc.3 <- n.trainedModels.train$accuracyList
n.train.time.3 <- n.trainedModels.train$timeList

# Validation Data
# 50,80,100,130, 150
neuron <- 50
n.trainedModels.validation <- trainModel(ns, lr, neuron, dataset.validation, dataset.validation.labels)
n.val.acc.1 <- n.trainedModels.validation$accuracyList
n.val.time.2 <- n.trainedModels.validation$timeList

# 50,80,100,130, 150
neuron <- 70
n.trainedModels.validation <- trainModel(ns, lr, neuron, dataset.validation, dataset.validation.labels)
n.val.acc.2 <- n.trainedModels.validation$accuracyList
n.val.time.2 <- n.trainedModels.validation$timeList

# 50,80,100,130, 150
neuron <- 150
n.trainedModels.validation <- trainModel(ns, lr, neuron, dataset.validation, dataset.validation.labels)
n.val.acc.3 <- n.trainedModels.validation$accuracyList
n.val.time.3 <- n.trainedModels.validation$timeList


# Plot
mainText <- paste("LR =", lr, " HL =", 3, " N =", neuron)

plot(main = mainText, its,n.train.acc.1, xlab = "Iterations",
     ylab = "Accuracy", col = "blue", ylim=c(0.50,0.90), type = "p")
points(its,n.train.acc.1, col = "blue", type = "l")

points(its, n.train.acc.2, col = "purple", type = "p")
points(its, n.train.acc.2, col = "purple", type = "l")

points(its, n.train.acc.3, col = "cyan", type = "p")
points(its, n.train.acc.3, col = "cyan", type = "l")

axis(1, at=its, labels = its)

points(its, n.val.acc.1, col = "red", type = "p")
points(its, n.val.acc.1, col = "red", type = "l")

points(its, n.val.acc.2, col = "orange", type = "p")
points(its, n.val.acc.2, col = "orange", type = "l")

points(its, n.val.acc.3, col = "darkgreen", type = "p")
points(its, n.val.acc.3, col = "darkgreen", type = "l")

# Change x, y to match the plot
legend(17.3, 0.57, legend=c("Training N=50","Training N=100","Training N=150",
                            "Validation N=50","Validation N=100","Validation N=150"),
       col=c("blue", "purple", "cyan", "red", "orange", "darkgreen"), lty=1, cex=0.5)

#### [lr] Tune Learning Rate ####
# Variables
its <- c(10,20) ## Iterations

# Change to the neuron amount that was best from previous case
neuron <- 20  
# Training Data

lr <- 0.2
# Train on training data
lr.trainedModels.train <- trainModel(its, lr, neuron, dataset.train, dataset.train.labels)
lr.train.acc.1 <- lr.trainedModels.train$accuracyList
lr.train.time.1 <- lr.trainedModels.train$timeList

lr <- 0.1
lr.trainedModels.train <- trainModel(its, lr, neuron, dataset.train, dataset.train.labels)
lr.train.acc.2 <- lr.trainedModels.train$accuracyList
lr.train.time.2 <- lr.trainedModels.train$timeList

lr <- 0.05
lr.trainedModels.train <- trainModel(its, lr, neuron, dataset.train, dataset.train.labels)
lr.train.acc.3 <- lr.trainedModels.train$accuracyList
lr.train.time.3 <- lr.trainedModels.train$timeList

# Validation Data
lr <- 0.2
lr.trainedModels.validation <- trainModel(its, lr, neuron, dataset.validation, dataset.validation.labels)
lr.val.acc.1 <- lr.trainedModels.validation$accuracyList
lr.val.time.2 <- lr.trainedModels.validation$timeList

lr <- 0.1
lr.trainedModels.validation <- trainModel(its, lr, neuron, dataset.validation, dataset.validation.labels)
lr.val.acc.2 <- lr.trainedModels.validation$accuracyList
lr.val.time.2 <- lr.trainedModels.validation$timeList

lr <- 0.05
lr.trainedModels.validation <- trainModel(its, lr, neuron, dataset.validation, dataset.validation.labels)
lr.val.acc.3 <- lr.trainedModels.validation$accuracyList
lr.val.time.3 <- lr.trainedModels.validation$timeList


# Plot
mainText <- paste("HL =", 3, " N =", neuron)

plot(main = mainText, its,lr.train.acc.1, xlab = "Iterations",
     ylab = "Accuracy", col = "blue", ylim=c(0.30,0.80), type = "p")
points(its,lr.train.acc.1, col = "blue", type = "l")

points(its, lr.train.acc.2, col = "purple", type = "p")
points(its, lr.train.acc.2, col = "purple", type = "l")

points(its, lr.train.acc.3, col = "cyan", type = "p")
points(its, lr.train.acc.3, col = "cyan", type = "l")

axis(1, at=its, labels = its)

points(its, lr.val.acc.1, col = "red", type = "p")
points(its, lr.val.acc.1, col = "red", type = "l")

points(its, lr.val.acc.2, col = "orange", type = "p")
points(its, lr.val.acc.2, col = "orange", type = "l")

points(its, lr.val.acc.3, col = "darkgreen", type = "p")
points(its, lr.val.acc.3, col = "darkgreen", type = "l")

# Change x, y to match the plot
legend(17.3, 0.57, legend=c("Training LR=0.2","Training LR=0.1","Training LR=0.05",
                         "Validation LR= 0.2","Validation LR= 0.1","Validation LR= 0.05"),
       col=c("blue", "purple", "cyan", "red", "orange", "darkgreen"), lty=1, cex=0.5)
#### [hl] Tune on Hidden Layers ####
# Variables
its <- c(10,20) ## Iterations

# Change to the best lr from previous case
lr <- 0.08

# Change to the neuron amount that was best from previous case
neuron <- 30  

# You have to manually go into "trainModel" function and change amount of layers"
# size = c(neurons,neurons,neurons)  << 3 Layers
# size = c(neurons,neurons,neurons, neurons, neurons) << 5 Layers

# Training Data

# Train on training data
hl.trainedModels.train <- trainModel(its, lr, neuron, dataset.train, dataset.train.labels)
hl.train.acc.1 <- hl.trainedModels.train$accuracyList
hl.train.time.1 <- hl.trainedModels.train$timeList


hl.trainedModels.train <- trainModel(its, lr, neuron, dataset.train, dataset.train.labels)
hl.train.acc.2 <- hl.trainedModels.train$accuracyList
hl.train.time.2 <- hl.trainedModels.train$timeList


hl.trainedModels.train <- trainModel(its, lr, neuron, dataset.train, dataset.train.labels)
hl.train.acc.3 <- hl.trainedModels.train$accuracyList
hl.train.time.3 <- hl.trainedModels.train$timeList

# Validation Data

hl.trainedModels.validation <- trainModel(its, lr, neuron, dataset.validation, dataset.validation.labels)
hl.val.acc.1 <- hl.trainedModels.validation$accuracyList
hl.val.time.2 <- hl.trainedModels.validation$timeList


hl.trainedModels.validation <- trainModel(its, lr, neuron, dataset.validation, dataset.validation.labels)
hl.val.acc.2 <- hl.trainedModels.validation$accuracyList
hl.val.time.2 <- hl.trainedModels.validation$timeList


hl.trainedModels.validation <- trainModel(its, lr, neuron, dataset.validation, dataset.validation.labels)
hl.val.acc.3 <- hl.trainedModels.validation$accuracyList
hl.val.time.3 <- hl.trainedModels.validation$timeList


# Plot
mainText <- paste("LR =", lr, " N =", neuron)

plot(main = mainText, its,hl.train.acc.1, xlab = "Iterations",
     ylab = "Accuracy", col = "blue", ylim=c(0.50,0.90), type = "p")
points(its,hl.train.acc.1, col = "blue", type = "l")

points(its, hl.train.acc.2, col = "purple", type = "p")
points(its, hl.train.acc.2, col = "purple", type = "l")

points(its, hl.train.acc.3, col = "cyan", type = "p")
points(its, hl.train.acc.3, col = "cyan", type = "l")

axis(1, at=its, labels = its)

points(its, hl.val.acc.1, col = "red", type = "p")
points(its, hl.val.acc.1, col = "red", type = "l")

points(its, hl.val.acc.2, col = "orange", type = "p")
points(its, hl.val.acc.2, col = "orange", type = "l")

points(its, hl.val.acc.3, col = "darkgreen", type = "p")
points(its, hl.val.acc.3, col = "darkgreen", type = "l")

# Change x, y to match the plot
legend(17.3, 0.57, legend=c("Training HL=3","Training HL=5","Training HL=7",
                            "Validation HL=3","Validation HL=5","Validation HL=7"),
       col=c("blue", "purple", "cyan", "red", "orange", "darkgreen"), lty=1, cex=0.5)

#### [lf] Try different Learning Functions ####
# Remove
#### [af] Try different Activation Function ####
# Remove

########################### Helper Methods ##############################

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
    print(acc[2])
    accuracyList[j] <- acc[2]
    
  }
  return(list("accuracyList" = accuracyList, "timeList" = timeList))
}
