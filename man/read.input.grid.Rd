\name{read.input.grid}
\alias{read.input.grid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
read input grid
}
\description{
read input grid
}
\usage{
read.input.grid(path.in)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path.in}{
%%     ~~Describe \code{path.in} here~~
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
function (path.in) 
{
    input.list <- dir(path.in)
    grid.name <- paste(path.in, input.list[grep("grid", input.list)], 
        sep = "")
    grid.header <- read.input.header(grid.name)
    prec <- abs(log(grid.header$scalar)/log(10))
    gridfile <- file(grid.name, "rb")
    seek(gridfile, HEADER_SIZE, origin = "start")
    grid.temp <<- readBin(gridfile, integer(), n = 2 * grid.header$ncells, 
        size = 2)
    grid.data <<- round(trunc(grid.temp, digits = 0) * grid.header$scalar, 
        digits = 2)
    lon <<- grid.data[c(1:grid.header$ncells) * 2 - 1]
    lat <<- grid.data[c(1:grid.header$ncells) * 2]
    EAST <<- round(max(lon), prec)
    SOUTH <<- round(min(lat), prec)
    WEST <<- round(min(lon), prec)
    NORTH <<- round(max(lat), prec)
    RES <<- grid.header$cellsize
    NC <<- (NORTH - SOUTH)/RES + 1
    NR <<- (EAST - WEST)/RES + 1
    ind_lon <<- ceiling(lon/RES - min(lon)/RES + 1)
    ind_lat <<- ceiling(lat/RES - min(lat)/RES + 1)
    close(gridfile)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
