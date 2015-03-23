\name{read.input.header}
\alias{read.input.header}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
read input header
}
\description{
read input header
}
\usage{
read.input.header(filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
%%     ~~Describe \code{filename} here~~
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
function (filename) 
{
    file.in <- file(filename, "rb")
    name <- readChar(file.in, nchar = 7)
    version <- readBin(file.in, integer(), n = 1, size = 4)
    order <- readBin(file.in, integer(), n = 1, size = 4)
    firstyear <- readBin(file.in, integer(), n = 1, size = 4)
    nyears <- readBin(file.in, integer(), n = 1, size = 4)
    firstcell <- readBin(file.in, integer(), n = 1, size = 4)
    ncells <- readBin(file.in, integer(), n = 1, size = 4)
    nbands <- readBin(file.in, integer(), n = 1, size = 4)
    cellsize <- readBin(file.in, numeric(), n = 1, size = 4)
    scalar <- readBin(file.in, numeric(), n = 1, size = 4)
    header <- data.frame(name, version, order, firstyear, nyears, 
        firstcell, ncells, nbands, cellsize, scalar)
    close(file.in)
    return(header)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
