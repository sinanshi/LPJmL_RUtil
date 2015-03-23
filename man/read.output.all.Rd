\name{read.output.all}
\alias{read.output.all}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
read all outputs
}
\description{
A concise (1-5 lines) description of what the function does. 
}
\usage{
read.output.all(path)
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
    pixel_year <- get.output.info(path)
    cat("=======================================================\n")
    cat("Path=", path, "\n")
    cat("pixel number=", pixel_year[1], "simulation years=", 
        pixel_year[2], "\n")
    cat("Reading the output data...\n")
    read.grid(path)
    read.cow(path)
    read.output.carbon(path, pixel_year[1], pixel_year[2])
    read.output.flux(path, pixel_year[1], pixel_year[2])
    cat("read: <sucessful>\n")
    cat("=======================================================\n")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
