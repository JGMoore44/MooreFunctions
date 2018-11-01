#' Kaplan Meier Conditional Survival Estimate FOR LEFT TRUNCATED DATA
#' 
#' This function calculates the KM-Estimate of Conditional Survival Function with Truncation Considered
#' @param dataToUse The dataset used to create function
#' @param timeToMeasure Time at which we want to calculate survival probability
#' @param conditionalTerm Time used as conditional term
#' @export
#' @examples 
#' kmTruncConditional()
kmTruncConditional = function(dataToUse, timeToMeasure,conditionalTerm){
  if(timeToMeasure==0){
    return(1)
  }
  survivalFunction = 1
  for(i in conditionalTerm:timeToMeasure){
    events = length(which(dataToUse$exit == i & dataToUse$death == 1))
    atRisk = length(which(dataToUse$entry <= i & dataToUse$exit >=i))
    temp = 1-(events/atRisk)
    if(events>0){
      survivalFunction = survivalFunction*temp
    }
  }
  return(survivalFunction)
}