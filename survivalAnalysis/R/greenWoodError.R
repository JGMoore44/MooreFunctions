#' Greenwood Estimate for Standard Error
#' 
#' This function calculates the Standard Error of our survival function
#' @param dataToUse The dataset used to create function
#' @param timeToMeasure Time at which we want to calculate survival probability
#' @export
#' @examples 
#' greenWoodError()
greenWoodError = function(dataToUse,timeToMeasure){
  survive = kmSurvEst(dataToUse,timeToMeasure)
  surviveSq = survive^2
  runningSum = 0
  for(i in 1:timeToMeasure){
    events = length(which(dataToUse$time == i & dataToUse$delta == 1))
    atRisk = length(which(dataToUse$time >= i))
    temp = events/(atRisk*(atRisk-events))
    if(events > 0 ){
      runningSum = runningSum+temp
    }
  }
  return(sqrt(surviveSq*runningSum))
}