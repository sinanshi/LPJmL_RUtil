HEADER_SIZE<-43

#' Read header of LPJ inputs in clm. The current header layout is 43 bytes, with name, version, 
#' order, firstyear, nyears, firstcell, ncells, scalar. Return data in data.frame. 
#' @param filename this input file name, with full path
#' @return data.frame header
#' @examples 
#' header <- read.input.header("cru_temp.clm")
read.input.header<-function(filename){
    file.in<-file(filename,"rb")

    name<-readChar(file.in,nchar=7)
    version<-readBin(file.in,integer(),n=1,size=4)
    order<-readBin(file.in,integer(),n=1,size=4)
    firstyear<-readBin(file.in,integer(),n=1,size=4)
    nyears<-readBin(file.in,integer(),n=1,size=4)
    firstcell<-readBin(file.in,integer(),n=1,size=4)   
    ncells<-readBin(file.in,integer(),n=1,size=4)
    nbands<-readBin(file.in,integer(),n=1,size=4)
    cellsize<-readBin(file.in,numeric(),n=1,size=4)
    scalar<-readBin(file.in,numeric(),n=1,size=4)
    header<-data.frame(name,version,order,firstyear,nyears,firstcell,ncells,nbands,cellsize,scalar)
    close(file.in)
    return(header)

}

#' Read input grid (clm), return global values lon, lat, EAST, SOUTH, WEST, NORTH, RES, NC, int_lon, ind_lat ...
#' @param path.in file location of grid.bin
#' @return lon vector longitiute
#' @return lat vector latitide
read.input.grid<-function(path.in){
    input.list<-dir(path.in)
    grid.name<-paste(path.in,input.list[grep("grid",input.list)],sep="")
    grid.header<-read.input.header(grid.name)

    prec<-abs(log(grid.header$scalar)/log(10))
    gridfile<- file(grid.name,"rb")
    seek(gridfile,HEADER_SIZE, origin = "start")
    grid.temp<<-readBin(gridfile,integer(),n=2*grid.header$ncells,size=2)
    grid.data<<-round(trunc(grid.temp,digits=0)*grid.header$scalar,digits=2)
    lon<<-grid.data[c(1:grid.header$ncells)*2-1]
    lat<<-grid.data[c(1:grid.header$ncells)*2]
    EAST<<-round(max(lon),prec)
    SOUTH<<-round(min(lat),prec)
    WEST<<-round(min(lon),prec)
    NORTH<<-round(max(lat),prec)
    RES<<-grid.header$cellsize
    NC<<-(NORTH-SOUTH)/RES+1
    NR<<-(EAST-WEST)/RES+1


    ind_lon<<-ceiling(lon/RES-min(lon)/RES+1)
    ind_lat<<-ceiling(lat/RES-min(lat)/RES+1)

    close(gridfile)
}



# a different layout comparing with read.input.files() 
# this layout enables as.vector(inputs)
read.input.files<-function(filename, data.size, with.scalar=TRUE){
    cat("reading LPJ input:",filename,"\n")

    fileHeader<-read.input.header(filename)
    pb <- txtProgressBar(min = 0, max = fileHeader$nyears,style = 3)
    file.in <- file(sprintf(filename),"rb")
    data.in<-array(NA,dim=c(fileHeader$nbands,fileHeader$ncells,fileHeader$nyears))
    seek(file.in,where=HEADER_SIZE,origin="start")
    for(i in 1:fileHeader$nyears){
        for(j in 1:fileHeader$ncells){
            data.in[,j,i]<-readBin(file.in, integer(), n=fileHeader$nbands, size=data.size)
            if(with.scalar) data.in[, j, i] <- data.in[, j, i] * fileHeader$scalar
        }
        setTxtProgressBar(pb, i)
    }
    close(file.in)
    return(data.in)
}



#' Read one year and one band of LPJ clm data, and return a vector of the select year and band.
#' @param filename input file path
#' @param data.size data size of input data, generally equal to 2.
#' @param year absolute value of select year, e.g. 1900
#' @param band band
#' @return vector of npix
#' @examples 
#'  read.input.yearband("temp.clm", 1983, 1, 2)
read.input.yearband<-function(filename,year,band, data.size, with.scalar=TRUE){#year,band, start from 1 
    fileHeader<-read.input.header(filename)
    data.year<-year-fileHeader$firstyear+1
    file.in <- file(sprintf(filename),"rb")
    data.in<-array(NA,dim=c(fileHeader$ncells))
    seek(file.in,where=HEADER_SIZE+data.size*((data.year-1)*fileHeader$nband*fileHeader$ncells+(band-1)),origin="start")
    for(i in 1:fileHeader$ncells){
        data.in[i]<-readBin(file.in, integer(), n=1, size=data.size)
        if(with.scalar) data.in[i] <- data.in[i]*fileHeader$scalar
        seek(file.in,where=(fileHeader$nbands-1)*2,origin="current")
    }
    close(file.in)
    return(data.in)
}

#' Read one year and all bands of LPJ clm data, and return an ncells by nbands
#' array. 
#' @param filename input file path
#' @param data.size data size of input data, generally equal to 2.
#' @param year absolute value of select year, e.g. 1900
#' @param bands a vector of selected bands, if not specified read all bands. 
#' @return vector of $ncells * bands$
#' @examples 
#'  read.input.yearband("temp.clm", 1983, 1, 2)
read.input.yearbands<-function(filename, year, bands=NULL, data.size){
    fileHeader<-read.input.header(filename)
    if(is.null(bands))
        data<-array(NA, c(fileHeader$ncells, fileHeader$nbands))
    else
        data<-array(NA, c(fileHeader$ncells, length(bands)))

    for(i in 1:length(bands))
        data[, i]<-read.input.yearband(filename, year, bands[i],
                                            data.size=data.size)

    return(data)
}



