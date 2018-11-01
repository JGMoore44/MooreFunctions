##Function Script
install.packages("devtools")
install.packages("roxygen2")
library(devtools)
library(roxygen2)


create("mooreFunctions")
setwd("./mooreFunctions")
document()



#package.skeleton(list = c("forwardSelection","backElimination","optimumElastic",
#                          "setTrainTest","transposeDeIdentify"),
#                 name = "mooreFunctions")
#devtools::document("C:/Users/James Moore/Documents/MooreFunctions/mooreFunctions")
