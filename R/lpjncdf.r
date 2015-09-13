#' create an empty ncdf file with single variable. 
#' @param lpjgrid: the path of LPJ grid
#' @param var_name: variable name
#' @param time_start: start year for yearly output, start month for monthly output. 
#'            e.g. "1900" and "1900-01-01"
#' @param time_interval: "years" or "months" or "days"
#' @param time_dim: length of time dimention, e.g. months * years 
#' @param lonname: the description of the variable
#' @return list(cout, vardef): cout is the new ncdf file, and vardef \
#' is the definition of the all variables defined
# TODO:
#    change the nasty read.input.grid to a list()
new.var.ncdf<-function(ncfile, lpjgrid_path, var_name, units, 
                       time_start, time_interval, time_dim,
                       longname = var_name, missval = 1e32){
    nvar <- length(var_name)
    stopifnot(length(units) == nvar)
    stopifnot(length(longname) == nvar)

    read.input.grid(lpjgrid_path)
    nclat <- seq(SOUTH,NORTH,RES)
    nclon <- seq(WEST,EAST,RES)
    # strings to for time
    tchar<-paste(time_interval, "since", time_start)

    londim <- dim.def.ncdf('Longitude',"deg_E",as.double(nclon)) 
    latdim <- dim.def.ncdf('Latitude',"deg_N",as.double(nclat))
    timedim <- dim.def.ncdf('Time', units=tchar, 1:time_dim, unlim=TRUE)
    # define variable
    vardef<-list()
    for(i in 1:length(var_name)){
        vardef[[i]] <- var.def.ncdf(var_name[i], units[i], list(londim, latdim, timedim),
                                    missval, longname[i], prec = "single")
    }

    cout<-create.ncdf(ncfile, vardef)
    att.put.ncdf(cout,"Longitude","axis","X")
    att.put.ncdf(cout,"Latitude","axis","Y")
    att.put.ncdf(cout,"Time","axis","T")

    return(list(nc=cout,vardef=vardef))
}


#put lpjvars
put.lpjvar.ncdf <- function(nc, varid, lpj_raster, start, count){
    xdim <- nc[["nc"]]$dim[["Longitude"]]$lene
    ydim <- nc[["nc"]]$dim[["Latitude"]]$lene
    put.var.ncdf(nc[["nc"]], varid,
                 vals = as.vector(lpj_raster),
                 start = c(1, 1, start),
                 count = c(xdim, ydim, count))
    cat(start,"|")

}
#' convert any lpjoutput to ncdf
#' @param lpjoutput: list
lpjoutput2ncdf <- function(lpjoutput)
{
    stopifnot(length(lpjoutput[["var_name"]]) == lpjoutput[["nbands"]]||lpjoutput[["nbands"]]==12)
    if(length(lpjoutput[["var_name"]])>1)
        IS_MULTI_VAR <- TRUE
    else
        IS_MULTI_VAR <- FALSE
    nc_single_var <- 
        new.var.ncdf(lpjoutput[["ncfile"]],
                     lpjoutput[["lpjgrid"]],
                     lpjoutput[["var_name"]],
                     lpjoutput[["units"]],
                     lpjoutput[["time_start"]],
                     lpjoutput[["time_interval"]],
                     lpjoutput[["time_dim"]],
                     lpjoutput[["longname"]],
                     lpjoutput[["missval"]])
    read.input.grid(lpjoutput[["lpjgrid"]])
    end_year <- lpjoutput[["start_year"]] + lpjoutput[["nyears"]] - 1
    count_ncdf_band <- 1
    pb <- txtProgressBar(min = 0, max = lpjoutput[["nyears"]],style = 3)
    for(this_year in lpjoutput[["start_year"]]:end_year){
        for(this_band in 1:lpjoutput[["nbands"]]){
            this_data <- read.output.yearband(lpjoutput[["filename"]], 
                                              this_year, this_band, 
                                              lpjoutput[["start_year"]], lpjoutput[["ncells"]], 
                                              lpjoutput[["nyears"]], lpjoutput[["nbands"]])
            this_map <- map.build(this_data)
            if(IS_MULTI_VAR)
                put.lpjvar.ncdf(nc_single_var, lpjoutput[["var_name"]][this_band], this_map, start = count_ncdf_band, count = 1)
            else{
                put.lpjvar.ncdf(nc_single_var, lpjoutput[["var_name"]], this_map, start = count_ncdf_band, count = 1)
                count_ncdf_band <- count_ncdf_band + 1
            }
        }
        if(IS_MULTI_VAR)
            count_ncdf_band <- count_ncdf_band + 1
        setTxtProgressBar(pb,(this_year + 1 -lpjoutput[["start_year"]]))
    }
    close(pb)
    close.ncdf(nc_single_var[["nc"]])
}
