\name{read.input.yearband}
\alias{read.input.yearband}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
read input year band
}
\description{
read input year band
}
\usage{
read.input.yearband(filename, data.size, year, band)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
%%     ~~Describe \code{filename} here~~
}
  \item{data.size}{
%%     ~~Describe \code{data.size} here~~
}
  \item{year}{
%%     ~~Describe \code{year} here~~
}
  \item{band}{
%%     ~~Describe \code{band} here~~
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
function (filename, data.size, year, band) 
{
    fileHeader <- read.input.header(filename)
    data.year <- year - fileHeader$firstyear + 1
    file.in <- file(sprintf(filename), "rb")
    data.in <- array(NA, dim = c(fileHeader$ncells))
    seek(file.in, where = HEADER_SIZE + data.size * ((data.year - 
        1) * fileHeader$nband * fileHeader$ncells + (band - 1)), 
        origin = "start")
    for (i in 1:fileHeader$ncells) {
        data.in[i] <- readBin(file.in, integer(), n = 1, size = data.size) * 
            fileHeader$scalar
        seek(file.in, where = (fileHeader$nbands - 1) * 2, origin = "current")
    }
    close(file.in)
    return(data.in)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
