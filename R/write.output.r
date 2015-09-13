#' Write data of a selected band and year of LPJ binary output.
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
write.output.yearband <- function(filename, data, year, band, start_year, 
                                 ncells, nyears, nbands, data.size = 4)
{
    ind_year <- year - start_year
    file_out <- file(sprintf(filename),"wb")
    pos_start <- data.size * (ind_year * nbands * ncells + (band - 1) * ncells)
    seek(file_out, where = pos_start, origin = "start")
    writeBin(data, file_out, size = data.size)
    close(file_out)
}

