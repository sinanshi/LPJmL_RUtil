\name{read.output.carbon}
\alias{read.output.carbon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function to do ... d
}
\description{
A concise (1-5 lines) description of what the function does.
}
\usage{
read.output.carbon(path, npixel.out, nyear.out)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
%%     ~~Describe \code{path} here~~
}
  \item{npixel.out}{
%%     ~~Describe \code{npixel.out} here~~
}
  \item{nyear.out}{
%%     ~~Describe \code{nyear.out} here~~
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
function (path, npixel.out, nyear.out) 
{
    vegc.data.out <<- array(NA, c(npixel.out, nyear.out))
    soilc.data.out <<- array(NA, c(npixel.out, nyear.out))
    litc.data.out <<- array(NA, c(npixel.out, nyear.out))
    firec.data.out <<- array(NA, c(npixel.out, nyear.out))
    fluxestab.data.out <<- array(NA, c(npixel.out, nyear.out))
    vegc.fn.out <- file(paste(path, "vegc.bin", sep = ""), "rb")
    vegc.data.out[, ] <<- readBin(vegc.fn.out, double(), npixel.out * 
        nyear.out, size = 4)
    soilc.fn.out <- file(paste(path, "soilc.bin", sep = ""), 
        "rb")
    soilc.data.out[, ] <<- readBin(soilc.fn.out, double(), npixel.out * 
        nyear.out, size = 4)
    litc.fn.out <- file(paste(path, "litc.bin", sep = ""), "rb")
    litc.data.out[, ] <<- readBin(litc.fn.out, double(), npixel.out * 
        nyear.out, size = 4)
    firec.fn.out <- file(paste(path, "firec.bin", sep = ""), 
        "rb")
    firec.data.out[, ] <<- readBin(firec.fn.out, double(), npixel.out * 
        nyear.out, size = 4)
    fluxestab.fn.out <- file(paste(path, "flux_estab.bin", sep = ""), 
        "rb")
    fluxestab.data.out[, ] <<- readBin(fluxestab.fn.out, double(), 
        npixel.out * nyear.out, size = 4)
    closeAllConnections()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
