#' @title output.operation
#' Do user defined operations on LPJ outputs. 
#' 
#' @description Convert one LPJ output file by a user defined operation. 
#'              Write out new data in LPJ output (clm)
#'              format.
#' @param outfile the information list of LPJ output, see example.
#' @param operation a user defined function/operation. Note: this 
#'        function should have only one scalar/vector as an argument
#'        and return a scalar/vector value too. 
#' @examples
#' lpjoutfile<-list()
#' lpjoutfile[["path"]]="../tests/test_data/soilc.bin"
#' lpjoutfile[["nyears"]]=40
#' lpjoutfile[["nbands"]]=1
#' lpjoutfile[["ncells"]]=67420
#' lpjoutfile[["start_year"]]=1901
#' 
#' fun<-function(old){ # here identify the user operation
#'     new <- old
#'     new[old>0] <- old/1000
#'     return((old/10000))
#' }
#' output.operation(lpjoutfile, fun, newfile="soilc_new.bin")
output.operation<-function(outfile, operation, 
                           newfile=paste(outfile[["path"]],"_new",sep=""),
                           VERBOSE=FALSE){
    if(VERBOSE)   cat("Doing operations", outfile[["path"]], "\t...")
    file_out <- file(sprintf(newfile),"wb")

    for(i in 1:outfile[["nyears"]]){
        for(j in 1:outfile[["nbands"]]){
            data <- read.output.yearband(outfile[["path"]], 
                                         year=outfile[["start_year"]] + i - 1,
                                         band=j,
                                         start_year=outfile[["start_year"]],
                                         ncells=outfile[["ncells"]],
                                         nyears=outfile[["nyears"]],
                                         nbands=outfile[["nbands"]])
            writeBin(data,file_out,outfile[["data.size"]])
        }
    }
    close(file_out)
    if(VERBOSE)  cat("[done]\n")
}
