\name{transposeDeIdentify}
\alias{transposeDeIdentify}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
transposeDeIdentify(xFeature, numRows, numCols)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xFeature}{
%%     ~~Describe \code{xFeature} here~~
}
  \item{numRows}{
%%     ~~Describe \code{numRows} here~~
}
  \item{numCols}{
%%     ~~Describe \code{numCols} here~~
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
function (xFeature, numRows, numCols) 
{
    inMatrix = matrix(0, nrow = numRows, ncol = numCols)
    for (i in 1:numCols) {
        transItDat = t(xFeature[i, ])
        colnames(transItDat) = NULL
        inMatrix[, i] = transItDat
    }
    return(inMatrix)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
