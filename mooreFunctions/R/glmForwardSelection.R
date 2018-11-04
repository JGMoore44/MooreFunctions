#' Forward glm Selection Method
#'
#'This Function conducts forward variable selection
#' @param Xmatrix Predictor Variables
#' @param Yvector Response Variables
#' @param fam Family of glm. Default = gaussian
#' @export
#' @examples 
#' forwardSelection()
glmForwardSelection <-
  function(Xmatrix,Yvector,fam = "gaussian"){
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
        lmOut = glm(Yvector~reducedMatrix,family = fam)
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
        lmTemp = glm(Yvector~tempMatrix,family = fam)
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