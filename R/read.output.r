#read output 
read.output.yearband <- function(filename, year, band, start_year, 
                                 ncells, nyears, nbands, data.size = 4)
{
    ind_year <- year - start_year
    file_out <- file(sprintf(filename),"rb")
    pos_start <- data.size * (ind_year * nbands * ncells + (band - 1) * ncells)
    #print(pos_start/data.size/ncells)
    seek(file_out, where = pos_start, origin = "start")
    data<-readBin(file_out, numeric(), n = ncells, size = data.size)

    close(file_out)
    return(data)
}

