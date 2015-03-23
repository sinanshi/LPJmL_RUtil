\name{read.output.harvest}
\alias{read.output.harvest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function to do ... ~~
}
\description{
 A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.output.harvest(filename, ncells, nbands, startyear, year, data.size = 4, band = "ALL", par = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
%%     ~~Describe \code{filename} here~~
}
  \item{ncells}{
%%     ~~Describe \code{ncells} here~~
}
  \item{nbands}{
%%     ~~Describe \code{nbands} here~~
}
  \item{startyear}{
%%     ~~Describe \code{startyear} here~~
}
  \item{year}{
%%     ~~Describe \code{year} here~~
}
  \item{data.size}{
%%     ~~Describe \code{data.size} here~~
}
  \item{band}{
%%     ~~Describe \code{band} here~~
}
  \item{par}{
%%     ~~Describe \code{par} here~~
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
function (filename, ncells, nbands, startyear, year, data.size = 4, 
    band = "ALL", par = 1) 
{
    nyears <- file.info(filename)$size/ncells/nbands/data.size
    if (nyears/as.integer(nyears) != 1) 
        stop("nyears:", nyears, " error\n")
    if (band[1] == "ALL") 
        band <- c(1:nbands)
    harvest <- list()
    data <- array(NA, c(ncells, length(band), length(year)))
    fn <- file(filename, "rb")
    for (y in 1:length(year)) {
        for (b in 1:length(band)) {
            seek(fn, where = (year[y] - startyear) * nbands * 
                ncells * data.size + (band[b] - 1) * ncells * 
                data.size, origin = "start")
            data[, b, y] <- readBin(fn, double(), size = data.size, 
                n = ncells)
        }
    }
    harvest$data <- data
    if (length(par) != 1) {
        for (i in 1:length(band)) {
            harvest$data[, i, ] <- data[, i, ] * par[i]
        }
    }
    else harvest$data <- harvest$data * par
    harvest$band <- band
    harvest$year <- year
    close(fn)
    return(harvest)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
