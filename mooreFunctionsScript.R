##Function Script
install.packages("devtools")
install.packages("roxygen2")
library(devtools)
library(roxygen2)

package.skeleton(list = c("forwardSelection","backElimination","optimumElastic",
                          "setTrainTest","transposeDeIdentify"),
                 name = "mooreFunctions")
devtools::document("C:/Users/James Moore/Documents/MooreFunctions/mooreFunctions")
