\name{map.switch}
\alias{map.switch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function to do 
}
\description{
A concise (1-5 lines) description of what the function does. 
}
\usage{
map.switch(map.data, startyear, endyear, xlim = NULL, ylim = NULL, xaxs = "r", yaxs = "r", data.info)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{map.data}{
%%     ~~Describe \code{map.data} here~~
}
  \item{startyear}{
%%     ~~Describe \code{startyear} here~~
}
  \item{endyear}{
%%     ~~Describe \code{endyear} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{xaxs}{
%%     ~~Describe \code{xaxs} here~~
}
  \item{yaxs}{
%%     ~~Describe \code{yaxs} here~~
}
  \item{data.info}{
%%     ~~Describe \code{data.info} here~~
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
function (map.data, startyear, endyear, xlim = NULL, ylim = NULL, 
    xaxs = "r", yaxs = "r", data.info) 
{
    map.show(map.data, switch.year, switch.month, data.info = data.info)
    devset <- function() if (dev.cur() != eventEnv$which) 
        dev.set(eventEnv$which)
    keydown <- function(key) {
        if (key == "q") 
            return(invisible(1))
        eventEnv$onMouseMove <- NULL
        if (key == "6") {
            if (length(dim(map.data)) == 4) {
                if (switch.year == endyear && switch.month == 
                  12) {
                  switch.year <<- startyear
                  switch.month <<- 1
                }
                else {
                  if (switch.month == 12) {
                    switch.year <<- switch.year + 1
                    switch.month <<- 1
                  }
                  else switch.month <<- switch.month + 1
                }
            }
            if (length(dim(map.data)) == 3) {
                if (switch.year == endyear) 
                  switch.year <<- startyear
                else switch.year <<- switch.year + 1
            }
            map.show(map.data, switch.year, switch.month, data.info)
            Sys.sleep(0.1)
        }
        if (key == "4") {
            if (length(dim(map.data)) == 4) {
                if (switch.year == startyear && switch.month == 
                  1) {
                  switch.year <<- endyear
                  switch.month <<- 12
                }
                else {
                  if (switch.month == 1) {
                    switch.year <<- switch.year - 1
                    switch.month <<- 12
                  }
                  else switch.month <<- switch.month - 1
                }
            }
            if (length(dim(map.data)) == 3) {
                if (switch.year == startyear) 
                  switch.year <<- endyear
                else switch.year <<- switch.year - 1
            }
            map.show(map.data, switch.year, switch.month, data.info)
            Sys.sleep(0.1)
            NULL
        }
    }
    setGraphicsEventHandlers(prompt = "hit q to quit", onKeybd = keydown)
    eventEnv <- getGraphicsEventEnv()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
