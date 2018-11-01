## Jimmy Moore
## 11/10/17
## Function: forwardSelection.R
## Input: A matrix of independent variables (Xmatrix) and a vector of response variables (Yvector)
## Output: Vector of indices indicating chosen x variables
## Description: The purpose of this function is to execute forwardSelection of variables.
## This function uses minimum AIC as the criteria of selection

forwardSelection = function(Xmatrix,Yvector){
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

## Jimmy Moore
## 11/10/17
## Name: backElimination.R
## Input: A matrix of independent variables (Xmatrix) and a vector of response variables (Yvector)
## Output: Vector of indices indicating chosen x variables
## Description: The purpose of this function is to execute backward selection of variables.
## This function uses minimum AIC as the criteria of selection

backElimination = function(Xmatrix, Yvector){
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