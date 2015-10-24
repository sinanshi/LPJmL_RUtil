#' Aggregate the global average of selected years and bands
#' @param year absolute values of years, e.g. c(1901:2000)
#' @param bands e.g. c(1:12)
input.global.mean <- function(filename, years=NULL, bands=NULL){
    header <- read.input.header(filename)

    if(is.null(years))  YEARS <- seq(header$firstyear,length.out=header$nyears)
    else  YEARS <- years

    if(is.null(bands)) BANDS <-seq(1,header$nbands)
    else{
        if(any(bands > header$nbands)) stop("bands > total bands of the file")
        BANDS <- bands
    }

    mean_data <- array(NA, length(YEARS))

    cat("aggregating data from", YEARS[1],"-", YEARS[length(YEARS)],"\n")
    pb <- txtProgressBar(min = 0, max = length(YEARS),style = 3)
    for(i in 1:length(YEARS)){
        data.this.year<- read.input.yearbands(filename, 
                                year=YEARS[i], band=BANDS, data.size=2)
        if(length(BANDS) > 1){# aggregate through bands
            data.this.year <- rowMeans(data.this.year)
        }
        mean_data[i] <- mean(data.this.year)
        setTxtProgressBar(pb,i)
    }
    return(mean_data)
}
