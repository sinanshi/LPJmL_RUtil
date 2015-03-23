\name{map.create}
\alias{map.create}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
map create
}
\description{
A concise (1-5 lines) description of what the function does. 
}
\usage{
map.create(data.raw, startyear, endyear)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.raw}{
%%     ~~Describe \code{data.raw} here~~
}
  \item{startyear}{
%%     ~~Describe \code{startyear} here~~
}
  \item{endyear}{
%%     ~~Describe \code{endyear} here~~
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
function (data.raw, startyear, endyear) 
{
    npixel.out <- get.output.info(path.out)[1]
    simyears <- get.output.info(path.out)[2]
    startyear <- startyear - simstartyear + 1
    endyear <- endyear - simstartyear + 1
    if (startyear < 0) {
        print("map.create: the chosen start year should be later than simulation start year.")
        stop()
    }
    if (length(dim(data.raw)) == 2) {
        map.data <- array(NA, dim = c(NCOLS, NROWS, simyears))
        for (y in startyear:endyear) {
            for (i in 1:npixel.out) map.data[ind_lon[i], ind_lat[i], 
                y] <- data.raw[i, y]
        }
    }
    if (length(dim(data.raw)) == 3) {
        map.data <- array(NA, dim = c(NCOLS, NROWS, 12, simyears))
        for (y in startyear:endyear) {
            for (m in 1:12) {
                for (i in 1:npixel.out) map.data[ind_lon[i], 
                  ind_lat[i], m, y] <- data.raw[i, m, y]
            }
        }
    }
    map.data
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
