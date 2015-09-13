library(lpjutil) 
fun<-function(old){
    return((old/10000))
}


lpjoutfile<-list()
lpjoutfile[["path"]]="test_data/soilc.bin"
lpjoutfile[["nyears"]]=40
lpjoutfile[["nbands"]]=1
lpjoutfile[["ncells"]]=67420
lpjoutfile[["start_year"]]=1901
lpjoutfile[["data.size"]]=4

output.operation(lpjoutfile,fun,"soilc_new.bin")
data<-read.output.file(lpjoutfile)
lpjoutfile[["path"]]<-"soilc_new.bin"
data2<-read.output.file(lpjoutfile)
data2<-data2*10000

if((max(abs((data-data2)/data), na.rm=T) < 1e-7)){
   cat("pass\n")
}else{
    cat("failed!\n")

}



