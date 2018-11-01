forwardSelection <-
function(Xmatrix,Yvector){
  predictors = ncol(Xmatrix)
  originalIndex = c(1:predictors)
  returnIndex = c(0)
  variableToAdd = 1
  while(predictors>0){
    AICvect = c(0)
    
    ##Start with Intercept only
    if(variableToAdd == 1){
      lmOut = lm(Yvector~1)
    }
    else{
      lmOut = lm(Yvector~reducedMatrix)
    }
    criterion = AIC(lmOut)
    
    ##Calculate the contribution of each variable
    for(i in 1:predictors){
      if(variableToAdd == 1){
        tempMatrix = Xmatrix[,i]
      }
      else{
        tempMatrix = cbind(reducedMatrix,Xmatrix[,i])
      }
      lmTemp = lm(Yvector~tempMatrix)
      AICvect[i] = AIC(lmTemp)
    }
    
    ## Include variable according to AIC criteria
    if(min(AICvect)<criterion){
      addition = which.min(AICvect)
      if(variableToAdd==1){
        reducedMatrix = Xmatrix[,addition]
      }
      else{
        reducedMatrix = cbind(reducedMatrix,Xmatrix[,addition])
      }
      returnIndex[variableToAdd] = originalIndex[addition]
      originalIndex = originalIndex[-addition]
      Xmatrix = subset(Xmatrix, select = c(-addition))
      variableToAdd = variableToAdd+1
      predictors = predictors-1
    }
    else{
      return(returnIndex)
    }
  }
  return(returnIndex)
}
