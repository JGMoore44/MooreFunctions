#' Backwards Elimination Function
#' 
#' This function conducts backwards elimination for model selections
#' @param Xmatrix Predictor Variables
#' @param Yvector Response Variable
#' @export
#' @examples 
#' backElimination()
backElimination <-
function(Xmatrix, Yvector){
  ## Copy original Matrix (I am unsure if R uses pass by value or pass by reference for their functions
  ## thus I copied the X matrix to prevent tampering original data)
  reducedX = Xmatrix
  predictors = ncol(reducedX)
  
  ## This vector will return the indices of the 
  returnIndex = c(1:predictors)
  ## Repeat until none of the remaining variables meet
  # the minimum requirement, or no more variables are left in model.
  while(predictors>0){
    AICvect = numeric(predictors)
    
    ##Generate full Model
    lmOut = lm(Yvector~reducedX)
    criterion = AIC(lmOut)
    
    ## calculate conditional Contribution
    for(i in 1:predictors){
      tempMatrix = subset(reducedX, select = -c(i))
      lmTemp = lm(Yvector~tempMatrix)
      AICvect[i] = AIC(lmTemp)
    }
    
    #Eliminate according to AIC criteria.
    if(min(AICvect)<=criterion){
      elimination = which.min(AICvect)
      reducedX = subset(reducedX, select = -c(elimination))
      returnIndex = returnIndex[-elimination]
      predictors = predictors-1
    }
    else{
      return(returnIndex)#no variables meet exclusion.
    }
  }
  return(0)
}
