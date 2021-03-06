% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/read.input.r
\name{read.input.header}
\alias{read.input.header}
\title{Read header of LPJ inputs in clm. The current header layout is 43 bytes, with name, version,
order, firstyear, nyears, firstcell, ncells, scalar. Return data in data.frame.}
\usage{
read.input.header(filename)
}
\arguments{
\item{filename}{this input file name, with full path}
}
\value{
data.frame header
}
\description{
Read header of LPJ inputs in clm. The current header layout is 43 bytes, with name, version,
order, firstyear, nyears, firstcell, ncells, scalar. Return data in data.frame.
}
\examples{
header <- read.input.header("cru_temp.clm")
}

