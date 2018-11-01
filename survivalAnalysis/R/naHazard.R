#' Nelson Aalen's Estimate for Hazard Function
#' 
#' This function calculates the NA Estimate of Hazard Function
#' @param dataToUse The dataset used to create function
#' @param timeToMeasure Time at which we want to calculate survival probability
#' @export
#' @examples 
#' naHazard()
naHazard = function(dataToUse,timeToMeasure){
  cumulativeHazard = 0
  for(i in 1:timeToMeasure){
    events = length(which(dataToUse$time == i & dataToUse$delta == 1))
    atRisk = length(which(dataToUse$time >= i))
    temp = events/atRisk
    if(events > 0 ){
      cumulativeHazard = cumulativeHazard+temp
    }
  }
  return(cumulativeHazard)
}