#' Tune the `k` parameter in flexsurvsplines()
#' 
#' This function finds the optimal number of knots for Royston-Parmar Models
#' @param  formula Formula to be fit, response must be of type Surv() and all covariates included.
#' @param  data Data set from which model will be built
#' @param  nKnot Number of Knots you would like to test over. For example, default value is 20 and so we will 20 models from 0 to 19 knots.
#' @param  criterion Information Criterion to be used. Either AIC or BIC. Default = AIC
#' @export 
#' @examples  
#' tuneKnots(Surv(time,delta)~gender+age+race,data = kidtran,nKnot = 15, criterion = "BIC")
tuneKnots = function(formula,data,nKnot = 20,criterion = "AIC"){
  library(flexsurv)
  #Define Information Vectors
  aicVect =numeric(nKnot)
  bicVect = numeric(nKnot)
  #Define CV vectors
  cvAICVect = numeric(5)
  cvBICVect = numeric(5)
  #Set our formula argument to Type Formula
  form = as.formula(formula)
  #begin knot loop
  for(i in 1:(nKnot+1)){
    #begin CV loop
    for (j in 1:5) {
      train = setTrainTest(data,"train",j,5)
      rpOut = flexsurvspline(form,data = train,k = i-1)
      cvAICVect[j] = AIC(rpOut)
      cvBICVect[j] = BIC(rpOut)
    }
    #record CV results
    aicVect[i] = mean(cvAICVect)
    bicVect[i] = mean(cvBICVect)
  }
  #return minimum criterion index based on user defined criterion
  if(criterion == "AIC"){
    return((which.min(aicVect)-1))
  }else if(criterion =="BIC"){
    return((which.min(bicVect-1)))
  }else{
    return(NA)
  }
}