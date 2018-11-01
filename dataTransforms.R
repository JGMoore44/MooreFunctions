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



