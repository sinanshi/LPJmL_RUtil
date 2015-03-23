\name{read.daily.output}
\alias{read.daily.output}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
read daily output
}
\description{
A concise (1-5 lines) description of what the function does.
}
\usage{
read.daily.output(path)
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
    nyear <- get.output.daily.info(paste(path, daily.files.list[1], 
        ".bin", sep = ""))
    temp <- array(NA, dim = nyear * 365)
    daily.data.frame <<- data.frame(d_cleaf = temp, d_cpool = "", 
        d_croot = "", d_cso = "", d_daylength = "", d_evap = "", 
        d_fhiopt = "", d_fphu = "", d_froot = "", d_gpp = "", 
        d_gresp = "", d_growingday = "", d_hi = "", d_himind = "", 
        d_husum = "", d_irrig = "", d_lai = "", d_laimax_adjusted = "", 
        d_laimaxnppdeficit = "", d_npp = "", d_par = "", d_perc = "", 
        d_pet = "", d_phen = "", d_phu = "", d_prec = "", d_pvd = "", 
        d_rd = "", d_rpool = "", d_rroot = "", d_rso = "", d_sun = "", 
        d_temp = "", d_trans = "", d_vdsum = "", d_w0 = "", d_w1 = "", 
        d_wdf = "", d_wevap = "", d_wscal = "")
    for (i in 1:length(daily.files.list)) {
        daily.fn <- file(paste(path, daily.files.list[i], ".bin", 
            sep = ""), "rb")
        nyear <- get.output.daily.info(paste(path, daily.files.list[i], 
            ".bin", sep = ""))
        cat("Reading", daily.files.list[i], "...")
        temp <- readBin(daily.fn, double(), 365 * nyear, size = sizeof.data)
        daily.data.frame[, daily.files.list[i]] <- temp
        cat("done\n")
        closeAllConnections()
    }
    return(daily.data.frame)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
