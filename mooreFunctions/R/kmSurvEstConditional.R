#' Kaplan Meier Conditional Survival Estimate FOR RIGHT CENSORED
#' 
#' This function calculates the KM-Estimate of Conditional Survival Function with Right Censoring Considered
#' @param dataToUse The dataset used to create function
#' @param timeToMeasure Time at which we want to calculate survival probability
#' @param conditionalTerm Time used as conditional term
#' @export
#' @examples 
#' kmSurvEstConditional()
kmSurvEstConditional = function(dataToUse, timeToMeasure,conditionalTerm){
  if(timeToMeasure==0){
    return(1)
  }
  survivalFunction = 1
  for(i in conditionalTerm:timeToMeasure){
    events = length(which(dataToUse$time == i & dataToUse$delta == 1))
    atRisk = length(which(dataToUse$time >= i))
    temp = 1-(events/atRisk)
    if(events > 0 ){
      survivalFunction = survivalFunction*temp
    }
  }
  return(survivalFunction)
}