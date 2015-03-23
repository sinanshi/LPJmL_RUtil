\name{read.monthly.output}
\alias{read.monthly.output}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
read monthly output
}
\description{
A concise (1-5 lines) description of what the function does. 
}
\usage{
read.monthly.output(path)
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
    ncell <- get.output.info(path)[1]
    nyear <- get.output.info(path)[2]
    temp <- array(NA, dim = nyear * 12)
    monthly.data.frame <- data.frame(mevap = temp, mfpar = "", 
        mgpp = "", minterc = "", mnpp = "", mrh = "", mrunoff = "", 
        mswc1 = "", mswc2 = "", mtransp = "")
    for (i in 1:length(monthly.files.list)) {
        monthly.fn <- file(paste(path, monthly.files.list[i], 
            ".bin", sep = ""), "rb")
        cat("Reading", monthly.files.list[i], "...")
        temp <- readBin(monthly.fn, double(), nyear * 12, size = sizeof.data)
        monthly.data.frame[, monthly.files.list[i]] <- temp
        cat("done\n")
        closeAllConnections()
    }
    return(monthly.data.frame)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
