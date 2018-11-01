#' Nelson Aalen Estimate for Standard Error
#' 
#' This function calculates the Standard Error of our hazard function
#' @param dataToUse The dataset used to create function
#' @param timeToMeasure Time at which we want to calculate survival probability
#' @export
#' @examples 
#' naStdErrorHazard()
naStdErrorHazard = function(dataToUse,timeToMeasure){
  variance = 0
  for (i in 1:timeToMeasure) {
    events = length(which(dataToUse$time == i & dataToUse$delta == 1))
    atRisk = length(which(dataToUse$time >= i))
    temp = events/(atRisk^2)
    if(events > 0){
      variance = variance+temp
    }
  }
  return(sqrt(variance))
}