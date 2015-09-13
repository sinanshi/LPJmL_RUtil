library(lpjutil)
cat("test read and write output\t...\t")
year<-1888
band<-5
data<-read.output.yearband("test_data/soilc.bin", year=year, band=band, 
                           start_year=1861, ncells=67420, nyears=40, nbands=1)
write.output.yearband("test_data/output.bin", data, year=year, band=band, 
                           start_year=1861, ncells=67420, nyears=40, nbands=1)
data2<-read.output.yearband("test_data/output.bin", year=year, band=band, 
                           start_year=1861, ncells=67420, nyears=40, nbands=1)

if(all((data2-data)==0)){
    cat("pass\n")
}else{
    cat("pass\n")
}
