\name{backElimination}
\alias{backElimination}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
backElimination(Xmatrix, Yvector)
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
    reducedX = Xmatrix
    predictors = ncol(reducedX)
    returnIndex = c(1:predictors)
    while (predictors > 0) {
        AICvect = numeric(predictors)
        lmOut = lm(Yvector ~ reducedX)
        criterion = AIC(lmOut)
        for (i in 1:predictors) {
            tempMatrix = subset(reducedX, select = -c(i))
            lmTemp = lm(Yvector ~ tempMatrix)
            AICvect[i] = AIC(lmTemp)
        }
        if (min(AICvect) <= criterion) {
            elimination = which.min(AICvect)
            reducedX = subset(reducedX, select = -c(elimination))
            returnIndex = returnIndex[-elimination]
            predictors = predictors - 1
        }
        else {
            return(returnIndex)
        }
    }
    return(0)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
