#' read data of a selected band and year of LPJ binary output. 
#' (need to be checked, maybe it doesn't work)
#' @param filename output file name
#' @param year select year
#' @param band select band
#' @param start_year start year of the LPJ output
#' @param ncells ncells of the LPJ output 
#' @param nyears nyears of the LPJ output 
#' @param nbands nbands of the LPJ output 
#' @param data.size data size, which in general equal to 4.
#' @return data in vector with ncells elements
#' @examples 
#' data<-read.output.yearband("mnpp.bin", year=1982, band=2, 
#'                             nyears=1900, ncells=67420, nyears=120, nbands=12)
read.output.yearband <- function(filename, year, band, start_year, 
                                 ncells, nyears, nbands, data.size = 4)
{
    ind_year <- year - start_year
    file_out <- file(sprintf(filename),"rb")
    pos_start <- data.size * (ind_year * nbands * ncells + (band - 1) * ncells)
    seek(file_out, where = pos_start, origin = "start")
    data<-readBin(file_out, numeric(), n = ncells, size = data.size)
    close(file_out)
    return(data)
}

# yearly data bands = 1
read.output.file <- function(outlist){
    nbands <- outlist[["nbands"]]
    ncells <- outlist[["ncells"]]
    nyears <- outlist[["nyears"]]
    data <- array(NA, c(outlist[["nyears"]], outlist[["nbands"]], outlist[["ncells"]]))
    for(year in 1:nyears){
        for(band in 1:nbands){
            data[year, band, ]<-read.output.yearband(outlist[["path"]],
                                                     year = year,
                                                     band = band,
                                                     start_year = outlist[["start_year"]],
                                                     ncells = outlist[["ncells"]],
                                                     nyears = outlist[["nyears"]],
                                                     nbands = outlist[["nbands"]],
                                                     data.size = outlist[["data.size"]])
        }
    }
    return(data)
}

#' read output by given a year block
#' @param syear start year of the year block
#' @param lyear end year of the year block
read.output.yearblock<- function(filename, syear, lyear, start_year,
                                 ncells, nyears, nbands, data.size = 4)
{
    ind_year <- syear - start_year
    file_out <- file(sprintf(filename),"rb")
    pos_start <- data.size * (ind_year * nbands * ncells)
    seek(file_out, where = pos_start, origin = "start")
    data<-array(0,dim=c((lyear-syear+1),nbands,ncells))
    for(i in 1:(lyear-syear+1)){
        for(j in 1:nbands){
            data[i,j,]<-readBin(file_out, numeric(), n = ncells, size = data.size)
        }
    }
    close(file_out)
    return(data)
}


#' read output grid and return global variable lon, lat, and index. 
#' @param path output path.
#' @param ncells ncells.
read.output.grid<-function (path, file, ncells)
{
    grid.name <- paste(path, file, sep="")
    gridfile <- file(grid.name, "rb")
    data <- array(NA, dim=2*ncells)
    data <- readBin(gridfile, integer(), n=2*ncells, size=2) / 100
    lon <<- data[c(1:ncells)*2-1]
    lat <<- data[c(1:ncells)*2]
    ind_lon <<- ceiling(lon*2-min(lon)*2+1)
    ind_lat <<- ceiling(lat*2-min(lat)*2+1)
    close(gridfile)
}
