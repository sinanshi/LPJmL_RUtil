\name{read.input.files}
\alias{read.input.files}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
read input files
}
\description{
A concise (1-5 lines) description of what the function does.
}
\usage{
read.input.files(filename, data.size)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
%%     ~~Describe \code{filename} here~~
}
  \item{data.size}{
%%     ~~Describe \code{data.size} here~~
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
function (filename, data.size) 
{
    fileHeader <- read.input.header(filename)
    file.in <- file(sprintf(filename), "rb")
    data.in <- array(NA, dim = c(fileHeader$nbands, fileHeader$nyears, 
        fileHeader$ncells))
    seek(file.in, where = HEADER_SIZE, origin = "start")
    for (i in 1:fileHeader$nyears) {
        for (j in 1:fileHeader$ncells) {
            data.in[, i, j] <- readBin(file.in, integer(), n = fileHeader$nbands, 
                size = data.size) * fileHeader$scalar
        }
    }
    close(file.in)
    return(data.in)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
