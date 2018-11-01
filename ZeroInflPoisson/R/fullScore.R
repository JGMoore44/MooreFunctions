#' Score Function For Zero-Inflated Poisson (ZIP) Distribution
#' 
#' This is the first derivative of Log-Liklihood function for ZIP Distribution
#' Use this for Newton Raphson Algorithm
#' @param x A vector containing values belonging to a ZIP Distribution
#' @param currentLambda The value of our Lambda Parameter
#' @param p Probability of observing a Structural 0
#' @param q Probability of not observing a structural 0
#' @export
#' @examples 
#' fullScore()
fullScore = function(x,currentLambda,p,q){
  returnVal = 0
  #initialize n0 p and q
  nZero = length(x[x==0])
  #initialize a_i
  aVect = rep(NA,length(x))
  for (i in 1:length(x)) {
    if(x[i]==0){
      aVect[i]=0
    }else{
      aVect[i]=1
    }
  }
  returnVal = (-(nZero*(q*exp(-currentLambda)))/(p+(q*exp(-currentLambda))))-
    (sum(aVect)) + (sum(aVect*x)/currentLambda)
  return(returnVal)
}