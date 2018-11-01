\name{optimumElastic}
\alias{optimumElastic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
optimumElastic(dat, resp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
%%     ~~Describe \code{dat} here~~
}
  \item{resp}{
%%     ~~Describe \code{resp} here~~
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
function (dat, resp) 
{
    library(glmnet)
    response = c(resp)
    dat$responseFeature = dat[, resp]
    dat = dat[, !(names(dat) \%in\% response)]
    alphaVect = seq(from = 0.05, to = 0.95, by = 0.01)
    MSEP = numeric(length(alphaVect))
    for (j in 1:nrow(dat)) {
        foldNum = (j - 1)\%\%5 + 1
        dat$fold[j] = foldNum
    }
    for (i in 1:length(alphaVect)) {
        for (j in 1:5) {
            trainingSet = dat[-which(dat$fold == j), ]
            trainingSet = subset(trainingSet, select = -c(fold))
            testSet = dat[which(dat$fold == j), ]
            testSet = subset(testSet, select = -c(fold))
            trainingMat = model.matrix(responseFeature ~ ., data = trainingSet)
            testMat = model.matrix(responseFeature ~ ., data = testSet)
            cvElastic = cv.glmnet(trainingMat, trainingSet$responseFeature, 
                alpha = alphaVect[i])
            minLambda = cvElastic$lambda.min
            tempMod = glmnet(trainingMat, trainingSet$responseFeature, 
                alpha = alphaVect[i], lambda = minLambda)
            pred.elastic = predict(tempMod, testMat)
            MSEPTemp = mean((pred.elastic - testSet$responseFeature)^2)
            MSEP[i] = MSEP[i] + MSEPTemp
        }
        MSEP[i] = MSEP[i]/5
    }
    bestAlpha = which.min(MSEP)
    listToReturn = list(bestAlphaIndex = bestAlpha, alphaVect = alphaVect, 
        errorRate = MSEP)
    return(listToReturn)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
