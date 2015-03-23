\name{read.yearly.output}
\alias{read.yearly.output}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function to do ... ~~
}
\description{
A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.yearly.output(path)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
%%     ~~Describe \code{path} here~~
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
function (path) 
{
    ncell <- get.output.info(path)[1]
    nyear <- get.output.info(path)[2]
    temp <- array(NA, dim = nyear)
    yearly.data.frame <- data.frame(firec = temp, firef = "", 
        flux_estab = "", flux_harvest = "", litc = "", soilc = "", 
        vegc = "")
    for (i in 1:length(yearly.files.list)) {
        yearly.fn <- file(paste(path, yearly.files.list[i], ".bin", 
            sep = ""), "rb")
        cat("Reading", yearly.files.list[i], "...")
        temp <- readBin(yearly.fn, double(), nyear, size = sizeof.data)
        yearly.data.frame[, yearly.files.list[i]] <- temp
        cat("done\n")
        closeAllConnections()
    }
    return(yearly.data.frame)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
