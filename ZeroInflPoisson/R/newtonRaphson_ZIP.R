#' Newton Raphson Function For Zero-Inflated Poisson (ZIP) Distribution
#' 
#' This function uses Newton Raphson to Tune the Lambda Parameter of our ZIP Distribution
#' @param xVect A vector containing values belonging to a ZIP Distribution
#' @param delta Stabilizing Parameter; Default to 3
#' @export
#' @examples 
#' newtonRaphson_ZIP()
newtonRaphson_ZIP = function(xVect,delta = 3){
  sampleSize = length(xVect)
  #calculate initial lambdaEst via MME
  lambdaEst = ((sum(xVect^2)+(sampleSize^(0-delta)))/(sum(xVect)+(sampleSize^(0-delta))))-1
  #set initial value of difference equation for NR algo
  previousLambda = 0
  iteration = 1
  
  ##  Begin Newton Raphson Loop
  while(abs(lambdaEst-previousLambda)>0.0001 & iteration<=50){
    scoreFunction = fullScore(xVect,lambdaEst,p,q)
    informationFunction = fullInformation(xVect,lambdaEst,p,q)
    newEst = lambdaEst + (scoreFunction/informationFunction)
    previousLambda = lambdaEst
    lambdaEst = newEst
    iteration = iteration + 1
  }
  return(lambdaEst)
}