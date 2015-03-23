\name{map.build}
\alias{map.build}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
build map
}
\description{
convert vectors to matrix for display. read.input.grid() has to ben run before. 
}
\usage{
map.build(raw_)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{raw_}{
%%     ~~Describe \code{raw_} here~~
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
function (raw_) 
{
    map <- array(NA, dim = c(NR, NC))
    for (i in 1:length(raw_)) map[ind_lon[i], ind_lat[i]] <- raw_[i]
    return(map)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
