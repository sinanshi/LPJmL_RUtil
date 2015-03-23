\name{map.show}
\alias{map.show}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
unction to do 
}
\description{
A concise (1-5 lines) description of what the function does. 
}
\usage{
map.show(map.data, y, m, data.info)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{map.data}{
%%     ~~Describe \code{map.data} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{m}{
%%     ~~Describe \code{m} here~~
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
function (map.data, y, m, data.info) 
{
    yg.palette <- colorRampPalette(c("yellow3", "yellow", "greenyellow", 
        "green", "green3", "darkgreen"))
    colo = yg.palette(101)
    colo.cm = cm.colors(32)
    tit <- paste(data.info$des, "(", data.info$name, ")")
    if (length(dim(map.data)) == 4) {
        ylab <- paste(paste(as.character(y + simstartyear - 1), 
            "-", as.character(m)))
        image(x = seq(west, east, len = ((east - west + 0.5) * 
            2)), y = seq(south, north, len = ((north - south + 
            0.5) * 2)), map.data[, , m, y], col = colo, xlab = "", 
            ylab = ylab, axes = T)
        map(add = T, boundary = T)
        title(main = tit)
        image.plot(x = seq(west, east, len = ((east - west + 
            0.5) * 2)), y = seq(south, north, len = ((north - 
            south + 0.5) * 2)), map.data[, , m, y], col = colo, 
            legend.only = T, horizontal = T, legend.args = list(text = data.info$unit, 
                col = "black", cex = 1, side = 1, line = 0), 
            legend.width = 0.3)
    }
    if (length(dim(map.data)) == 3) {
        xlab <- as.character(y + simstartyear - 1)
        image(x = seq(west, east, len = ((east - west + 0.5) * 
            2)), y = seq(south, north, len = ((north - south + 
            0.5) * 2)), map.data[, , y], col = colo, xlab = xlab, 
            ylab = "", axes = T)
        map(add = T, boundary = T)
        title(main = tit)
        image.plot(x = seq(west, east, len = ((east - west + 
            0.5) * 2)), y = seq(south, north, len = ((north - 
            south + 0.5) * 2)), map.data[, , y], col = colo, 
            legend.only = T, horizontal = T, legend.args = list(text = data.info$unit, 
                col = "black", cex = 1, side = 1, line = 0), 
            legend.width = 0.3)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
