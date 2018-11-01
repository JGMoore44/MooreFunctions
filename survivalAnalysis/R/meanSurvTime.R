#' Estimate for Mean Survival Time
#' 
#' This function calculates the mean survival time given a upper bound time limit.
#' @param dataToUse The dataset used to create function
#' @param maxTime The upper bound of the range of time we will estimate mean time on.
#' @export
#' @examples 
#' meanSurvTime()
meanSurvTime=function(dataToUse, maxTime){
  intervalEvents = numeric()
  intervalatRisk = numeric()
  timeEvent = numeric()
  interval = 1
  for(i in maxTime:1){
    events = length(which(dataToUse$time == i & dataToUse$delta == 1))
    atRisk = length(which(dataToUse$time >= i))
    if(events > 0 ){
      intervalEvents[interval] = events
      intervalatRisk[interval] = atRisk
      timeEvent[interval] = i
      interval = interval+1
    }
  }
  return(data.frame(events = intervalEvents,
                    atRisk = intervalatRisk,
                    timeOccur = timeEvent))
}