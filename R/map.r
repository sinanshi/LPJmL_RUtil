#' Convert vector data to raster
#' @param raw_ vector 
#' @return 2-D array [NR, NC]
map.build<-function(raw_){
map<-array(NA, dim=c(NR,NC))
for(i in 1:length(raw_))
    map[ind_lon[i],ind_lat[i]]<-raw_[i]
    return(map)
}
   
