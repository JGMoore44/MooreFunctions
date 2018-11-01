##Function Script
install.packages("devtools")
library(devtools)

package.skeleton(list = c("forwardSelection","backElimination","optimumElastic",
                          ""),
                 name = "mooreFunctions")
