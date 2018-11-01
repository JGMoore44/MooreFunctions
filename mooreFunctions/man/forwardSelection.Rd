\name{forwardSelection}
\alias{forwardSelection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
forwardSelection(Xmatrix, Yvector)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Xmatrix}{
%%     ~~Describe \code{Xmatrix} here~~
}
  \item{Yvector}{
%%     ~~Describe \code{Yvector} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (Xmatrix, Yvector) 
{
    predictors = ncol(Xmatrix)
    originalIndex = c(1:predictors)
    returnIndex = c(0)
    variableToAdd = 1
    while (predictors > 0) {
        AICvect = c(0)
        if (variableToAdd == 1) {
            lmOut = lm(Yvector ~ 1)
        }
        else {
            lmOut = lm(Yvector ~ reducedMatrix)
        }
        criterion = AIC(lmOut)
        for (i in 1:predictors) {
            if (variableToAdd == 1) {
                tempMatrix = Xmatrix[, i]
            }
            else {
                tempMatrix = cbind(reducedMatrix, Xmatrix[, i])
            }
            lmTemp = lm(Yvector ~ tempMatrix)
            AICvect[i] = AIC(lmTemp)
        }
        if (min(AICvect) < criterion) {
            addition = which.min(AICvect)
            if (variableToAdd == 1) {
                reducedMatrix = Xmatrix[, addition]
            }
            else {
                reducedMatrix = cbind(reducedMatrix, Xmatrix[, 
                  addition])
            }
            returnIndex[variableToAdd] = originalIndex[addition]
            originalIndex = originalIndex[-addition]
            Xmatrix = subset(Xmatrix, select = c(-addition))
            variableToAdd = variableToAdd + 1
            predictors = predictors - 1
        }
        else {
            return(returnIndex)
        }
    }
    return(returnIndex)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
