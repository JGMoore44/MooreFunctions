#' Information Function For Zero-Inflated Poisson (ZIP) Distribution
#' 
#' This is the second derivative of Log-Liklihood function for ZIP Distribution
#' Use this for Newton Raphson Algorithm
#' @param x A vector containing values belonging to a ZIP Distribution
#' @param currentLambda The value of our Lambda Parameter
#' @param p Probability of observing a Structural 0
#' @param q Probability of not observing a structural 0
#' @export
#' @examples 
#' fullInformation()
fullInformation = function(x,currentLambda,p,q){
  nSize = length(x)
  returnVal = nSize*q*
    ((((q-1)*exp(-currentLambda))/(p+(q*exp(-currentLambda))))+
       (1/currentLambda))
  return(returnVal)
}