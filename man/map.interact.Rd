\name{map.interact}
\alias{map.interact}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
map interact
}
\description{
A concise (1-5 lines) description of what the function does.
}
\usage{
map.interact(map.data, startyear, endyear, colour, data.info)
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
  \item{colour}{
%%     ~~Describe \code{colour} here~~
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
function (map.data, startyear, endyear, colour, data.info) 
{
    map.itc.data <<- array(NA, dim = dim(map.data))
    map.itc.data <<- map.data
    startyear <- startyear - simstartyear + 1
    endyear <- endyear - simstartyear + 1
    switch.year <<- startyear
    switch.month <<- 1
    yhscale <- 5.5
    Myvscale <- 5.5
    CopyToClip <- function() {
        png(file = paste(as.character(switch.year + simstartyear - 
            1), "-", as.character(switch.month)), bg = "white")
        map.show(map.itc.data, switch.year, switch.month, data.info = data.info)
        dev.off()
    }
    NewWin <- function() {
        X11(type = "Xlib")
        map.show(map.itc.data, switch.year, switch.month, data.info = data.info)
    }
    NewSwitchWin <- function() {
        X11(type = "Xlib")
        map.switch(map.itc.data, startyear, endyear, data.info = data.info)
        getGraphicsEvent()
    }
    tt <- tktoplevel()
    tkwm.title(tt, "LPJmL Data")
    copy.but <- tkbutton(tt, text = "Copy to Clipboard", command = CopyToClip)
    newwins.but <- tkbutton(tt, text = "New Window(Static)", 
        command = NewWin)
    newwind.but <- tkbutton(tt, text = "New Window(Dynamic)", 
        command = NewSwitchWin)
    monorun.but <- tkbutton(tt, text = "Mono Pixel run", command = monop.graph)
    l.general <- tklabel(tt, text = "You may choose the functions here")
    l.ym <- tklabel(tt, text = "Year/Month")
    e.year <- tkentry(tt, width = 5)
    e.month <- tkentry(tt, width = 3)
    tkgrid(l.general)
    tkgrid(copy.but, sticky = "w")
    tkgrid(newwins.but, sticky = "w")
    tkgrid(newwind.but, sticky = "w")
    tkgrid(monorun.but, sticky = "w")
    tkgrid(l.ym, e.year, e.month)
    cat("for dynamic windows, press 'q' to quit")
    getGraphicsEvent()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
