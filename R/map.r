#############
#convert 1 dimentional
#raw data to a map. raw[NPIX]
#############
map.build<-function(raw_){
map<-array(NA, dim=c(NR,NC))
for(i in 1:length(raw_))
    map[ind_lon[i],ind_lat[i]]<-raw_[i]
    return(map)
}
   
