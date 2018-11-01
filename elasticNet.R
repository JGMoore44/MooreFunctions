###
# Function: optimumElastic()
# Description: This function determines the optimal Elastic Net model for prediction,
#               given a sequence of alpha values from 0.5 to .95 to test
# Input: dat --> The dataSet which you would like to conduct analysis on
#         resp --> The indicated response variable as a string
# Ouput: listToReturn --> A list containing the index for the optimal alpha value
#                           A vector of the alpha values tested,
#                           A vector of Error Rate at each alpha level
###
optimumElastic = function(dat,resp){
  library(glmnet)
  #Format Response
  response = c(resp)
  dat$responseFeature = dat[,resp]
  dat = dat[,!(names(dat)%in% response)]
  
  #initialize alpha and Error vectors
  alphaVect = seq(from=0.05, to=0.95, by = 0.01)
  MSEP = numeric(length(alphaVect))
  
  #Set up folds for CV
  for(j in 1:nrow(dat)){
    foldNum = (j-1)%%5 + 1
    dat$fold[j] = foldNum
  }
  #Begin Alpha Loop
  for (i in 1:length(alphaVect)) {
    #Begin Cross Validation Loop
    for (j in 1:5) {
      #Training and Test Sets
      trainingSet = dat[-which(dat$fold==j),]
      trainingSet = subset(trainingSet, select = -c(fold))
      testSet = dat[which(dat$fold==j),]
      testSet = subset(testSet,select = -c(fold))
      trainingMat = model.matrix(responseFeature ~ ., data = trainingSet)
      testMat = model.matrix(responseFeature ~ ., data = testSet)
      
      #Beginin Regression
      cvElastic = cv.glmnet(trainingMat,trainingSet$responseFeature,alpha = alphaVect[i])
      minLambda = cvElastic$lambda.min
      tempMod = glmnet(trainingMat,trainingSet$responseFeature,alpha = alphaVect[i],lambda = minLambda)
      #predict values
      pred.elastic = predict(tempMod,testMat)
      #Calculate MSE for this fold
      MSEPTemp = mean((pred.elastic-testSet$responseFeature)^2)
      MSEP[i] = MSEP[i] + MSEPTemp
    }
    #Record Final MSE for Alpha level
    MSEP[i] = MSEP[i]/5
  }
  #record index of best Alpha
  bestAlpha = which.min(MSEP)
  listToReturn = list(bestAlphaIndex = bestAlpha,
                      alphaVect=alphaVect,
                      errorRate = MSEP)
  return(listToReturn)
}
