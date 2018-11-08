#' Set the Training and Test Set
#' 
#' This function determines Training and Test sets for 5 fold CV
#' @param  dat The Data Pbject to be Parsed into training and test
#' @param  option Build Training or Test Set
#' @param  foldToTest Indicates which fold will be test set
#' @param nfolds Indicates the number of k-folds for CV; Default = 5
#' @export set a training or test set for the data based on arguments `nfolds` and `option`
#' @examples 
#' setTrainTest()
setTrainTest <-
function(dat,option="train",foldToTest=1,nfolds=5){
  #random indexing to determine which fold to enter
  x = seq(1:nrow(dat))
  set.seed(1234)
  xperm = sample(x,length(x))
  
  #set fold number
  for(j in 1:nrow(dat)){
    foldNum = xperm[j]%%nfolds + 1
    dat$fold[j] = foldNum
  }
  #create training or test depending on argument
  if(option == "train"){
    set = dat[-which(dat$fold==foldToTest),]
    set = subset(set, select = -c(fold))
    return(set)
  }else{
    set = dat[which(dat$fold==foldToTest),]
    set = subset(set,select = -c(fold))
    return(set)
  }
}
