\name{setTrainTest}
\alias{setTrainTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
setTrainTest(dat, option = "train", foldToTest = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
%%     ~~Describe \code{dat} here~~
}
  \item{option}{
%%     ~~Describe \code{option} here~~
}
  \item{foldToTest}{
%%     ~~Describe \code{foldToTest} here~~
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
function (dat, option = "train", foldToTest = 1) 
{
    for (j in 1:nrow(dat)) {
        foldNum = (j - 1)\%\%5 + 1
        dat$fold[j] = foldNum
    }
    if (option == "train") {
        trainingSet = dat[-which(dat$fold == foldToTest), ]
        trainingSet = subset(trainingSet, select = -c(fold))
        return(trainingSet)
    }
    else {
        testSet = dat[which(dat$fold == foldToTest), ]
        testSet = subset(testSet, select = -c(fold))
        return(testSet)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
