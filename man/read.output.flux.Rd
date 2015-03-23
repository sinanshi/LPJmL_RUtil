\name{read.output.flux}
\alias{read.output.flux}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function to do ... ~~
}
\description{
A concise (1-5 lines) description of what the function does. 
}
\usage{
read.output.flux(path, npixel.out, nyear.out)
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
    mnpp.data.out <<- array(NA, c(npixel.out, 12, nyear.out))
    mrh.data.out <<- array(NA, c(npixel.out, 12, nyear.out))
    mevap.data.out <<- array(NA, c(npixel.out, 12, nyear.out))
    mtransp.data.out <<- array(NA, c(npixel.out, 12, nyear.out))
    mrunoff.data.out <<- array(NA, c(npixel.out, 12, nyear.out))
    mnpp.fn.out <- file(paste(path, "mnpp.bin", sep = ""), "rb")
    mnpp.data.out[, , ] <<- readBin(mnpp.fn.out, double(), npixel.out * 
        nyear.out * 12, size = 4)
    mrh.fn.out <- file(paste(path, "mrh.bin", sep = ""), "rb")
    mrh.data.out[, , ] <<- readBin(mrh.fn.out, double(), npixel.out * 
        nyear.out * 12, size = 4)
    mevap.fn.out <- file(paste(path, "mevap.bin", sep = ""), 
        "rb")
    mevap.data.out[, , ] <<- readBin(mevap.fn.out, double(), 
        npixel.out * nyear.out * 12, size = 4)
    mtransp.fn.out <- file(paste(path, "mtransp.bin", sep = ""), 
        "rb")
    mtransp.data.out[, , ] <<- readBin(mtransp.fn.out, double(), 
        npixel.out * nyear.out * 12, size = 4)
    mrunoff.fn.out <- file(paste(path, "mrunoff.bin", sep = ""), 
        "rb")
    mrunoff.data.out[, , ] <<- readBin(mrunoff.fn.out, double(), 
        npixel.out * nyear.out * 12, size = 4)
    closeAllConnections()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
