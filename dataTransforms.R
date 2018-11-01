# Jimmy G. Moore
# Data: 9/18/18
# Function: setTrainTest
# Description: Used in Cross Validation. This function streamlines the process of
#               generating training and test sets for 5-fold cross validation
# Input: dat --> any dataset used to build a model
#         option --> whether or not we want to return a training set or test set
#         foldToTest --> which fold will be used as test set
# Output: Either a traingset or test set used in CV for model selection
setTrainTest = function(dat,option="train",foldToTest=1){
  for(j in 1:nrow(dat)){
    foldNum = (j-1)%%5 + 1
    dat$fold[j] = foldNum
  }
  if(option == "train"){
    trainingSet = dat[-which(dat$fold==foldToTest),]
    trainingSet = subset(trainingSet, select = -c(fold))
    return(trainingSet)
  }else{
    testSet = dat[which(dat$fold==foldToTest),]
    testSet = subset(testSet,select = -c(fold))
    return(testSet)
  }
}

## Jimmy G. Moore
## Date: 9/22/18
## Function: transposeDeIdentify.
## Description: In simulation studies sometimes the data is generated in the wrong format
##              as well has having default naming conventions when read from a .csv
##              This function handles these issues by transposing the data and removing
##              any default labels attached to the data when it was read in.
## Input: xFeature --> dataset to be transposed
##        numRows & numCols --> dimensions of dataset
## Ouput: A Transposed version of our dataset with no labels on it.
transposeDeIdentify = function(xFeature, numRows, numCols){
  #initialize matrix
  inMatrix = matrix(0,nrow = numRows,ncol = numCols)
  for (i in 1:numCols) {
    transItDat = t(xFeature[i,])
    colnames(transItDat) = NULL
    inMatrix[,i] = transItDat
  }
  return(inMatrix)
}



