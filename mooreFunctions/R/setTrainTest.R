#' Set the Training and Test Set
#' 
#' This function determines Training and Test sets for 5 fold CV
#' @param  dat the data to be analyzed
#' @param  option Build Training or Test Set
#' @param  foldToTest Indicates which fold will be test set
#' @export
#' @examples 
#' setTrainTest()
setTrainTest <-
function(dat,option="train",foldToTest=1){
  for(j in 1:nrow(dat)){
    foldNum = (j-1)%%5 + 1
    dat$fold[j] = foldNum
  }
  if(option == "train"){
    trainingSet = dat[-which(dat$fold==foldToTest),]
    trainingSet = subset(trainingSet, select = -c(fold))
    return(trainingSet)
  }else{
    testSet = dat[which(dat$fold==foldToTest),]
    testSet = subset(testSet,select = -c(fold))
    return(testSet)
  }
}
