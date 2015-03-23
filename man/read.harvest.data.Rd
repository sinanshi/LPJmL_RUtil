\name{read.harvest.data}
\alias{read.harvest.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
read harvest data
}
\description{
A concise (1-5 lines) description of what the function does. 
}
\usage{
read.harvest.data(path, harvest.name.seclect)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
%%     ~~Describe \code{path} here~~
}
  \item{harvest.name.seclect}{
%%     ~~Describe \code{harvest.name.seclect} here~~
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
function (path, harvest.name.seclect) 
{
    cat("reading harvest output...")
    npixel.out <- get.output.info(path)[1]
    nyear.out <- get.output.info(path)[2]
    harvest.fn.out <- file(paste(path, "pft_harvest.pft.bin", 
        sep = ""), "rb")
    harvest.temp <- array(NA, c(npixel.out, nft, nyear.out))
    harvest.temp[, , ] <- readBin(harvest.fn.out, double(), npixel.out * 
        nyear.out * 32, size = 4)
    harvest.data <- array(NA, c(npixel.out, nyear.out, 32))
    for (i in 1:length(harvest.name)) harvest.data[, , i] <- harvest.temp[, 
        i, ]
    close(harvest.fn.out)
    cat("done\n")
    return(harvest.data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
