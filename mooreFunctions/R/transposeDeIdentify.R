#' Format Data to your liking
#' 
#' This function transposes a data frame and removes all name labels
#' @param xFeature The dataset to transpose
#' @param numRows The number of rows
#' @param numCols The number of columns
#' @export
#' @examples 
#' transposeDeIdentify()
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
