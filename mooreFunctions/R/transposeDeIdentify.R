transposeDeIdentify <-
function(xFeature, numRows, numCols){
  #initialize matrix
  inMatrix = matrix(0,nrow = numRows,ncol = numCols)
  for (i in 1:numCols) {
    transItDat = t(xFeature[i,])
    colnames(transItDat) = NULL
    inMatrix[,i] = transItDat
  }
  return(inMatrix)
}
