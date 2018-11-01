#' Kaplan Meier Survival Estimate
#' 
#' This function calculates the KM-Estimate of Survival Function
#' @param dataToUse The dataset used to create function
#' @param timeToMeasure Time at which we want to calculate survival probability
#' @export
#' @examples 
#' kmSurvEst()
kmSurvEst =
function(dataToUse, timeToMeasure){
  if(timeToMeasure==0){
    return(1)
  }
  survivalFunction = 1
  for(i in 1:timeToMeasure){
    events = length(which(dataToUse$time == i & dataToUse$delta == 1))
    atRisk = length(which(dataToUse$time >= i))
    temp = 1-(events/atRisk)
    if(events > 0 ){
      survivalFunction = survivalFunction*temp
    }
  }
  return(survivalFunction)
}
#this function will calculate the survival function at at given time
## using the KM estimator