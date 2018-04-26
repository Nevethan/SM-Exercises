library(RSNNS)
library(devtools)
library(kernlab)

##### Preproccessing #####
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

#Disjunct - 20 members in total (10/10)
dataset.train <- dataset.all[1:40000,]
dataset.test <- dataset.all[40001:80000,]

dataset.train <- datasetShuffle(dataset.train)
dataset.test <- datasetShuffle(dataset.test)

dataset.train.labels <- factor(dataset.train[,1])
dataset.test.labels <- factor(dataset.test[,1])

dataset.train <- dataset.train[,-1]
dataset.test <- dataset.test[,-1]
 
##### Exericse 5.1.1 #####
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

##### Exercise 5.1.2 #####
#Train a neural network with N inputs and 10 outputs

#Training the neural network with standard backpropagation 
#Learning Algorithms
nn.model <- mlp(dataset.train, trainingClass, size = c(40,40,40), 
                maxit = 100, learnFunc = "Std_Backpropagation")

##### Exercise 5.1.3 #####
#Evaluation of the Neural network

#Code from learning algorithms
#Predictions 
predictions <- predict(nn.model, dataset.test)

#Error over iterations 
plotIterativeError(nn.model)

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
agreement_rbf <- responselist[,1] == dataset.test.labels
table(agreement_rbf)
prop.table(table(agreement_rbf))

##### Exericse 5.1.4 #####
#Try different parameters - number of neurons, number of hidden layers and differet learning parameters
#Try with 45,50,55 and 60 neurons
nn.model <- mlp(dataset.train,trainingClass, size = c(45,45,45), 
                  maxit = 100, learnFunc = "Std_Backpropagation")
  
predictions <- predict(nn.model, dataset.test)
  
responselist <- matrix(nrow = length(predictions[,1]), ncol = 1, data = "Na")
  
for(j in 1:nrow(predictions)) {
  responselist[i,] <- toString( which(predictions[j,]==max(predictions[j,])) - 1 )
}
responselist <- data.frame(responselist)
responselist[,1] <- as.factor(responselist[,1])
  
# Calculating the accuracy
agreement_rbf <- responselist[,1] == test[,1]
table(agreement_rbf)
prop.table(table(agreement_rbf))











 


##### Exericise 5.2.1 #####

svm.model <- ksvm(dataset.train.labels~., dataset.train, kernel = "rbfdot", kpar = "automatic", C=1)

#predict svm with test data
predictions <- predict(svm.model, dataset.test)

#Confusion Matrix
table(predictions, dataset.test.labels)

#Accuracy - I think ???
mean(predictions == dataset.test.labels)

##### Exericse 5.2.2 #####
#Try making svms with different parameters
#Kernels :  linear ("vanilladot" ), polynomial ("polydot"), radial basis ("rbfdot") kernel
#Type : "C-svc" - C classification

svm.model <- ksvm(dataset.train.labels~., dataset.train, kernel = "rbfdot", kpar = "automatic", C=1)

#predict svm with test data
predictions <- predict(svm.model, dataset.test)

#Confusion Matrix
table(predictions, dataset.test.labels)

#Accuracy - I think ???
mean(predictions == dataset.test.labels)





