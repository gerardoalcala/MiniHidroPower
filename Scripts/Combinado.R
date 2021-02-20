# MinihidroPower
# Combinacion Potencial
# Coded by: Gerardo Alcal'a
# Universidad Veracruzana
# First Version 27 Noviembre 2020

# Updated 27 Noviembre 2020

  graphics.off() 
  remove(list=ls())
  
  ##setwd("C:/Users/Gerardo Alcala/Desktop/MiniHidro")
#  setwd("/home/proy_ext_gerardo.alcala")

 setwd(Sys.getenv("PWD"))
  
 Rt <-2000
 Ri <-2500

 library(raster) 

  ## Leer Rasters y Vectoriales
  RasDEM <- raster("git/MiniHidroPower/B-RasterDEM.tif")

 # datos_Rt2000_Ri2000
 # DatosMHP_0.csv

 # datos<-c(19262,19264,19265,19266,19267,19268,19269,19270,19271,19272,19273,19274,19275,19276,19277,19278,19279,19280,19281,19282)
 file0 <- paste0("datos_Rt",Rt,"_Ri",Ri,"/DatosMHP_1.csv")

 DatosC<-read.csv(file0)

 for(i in 2:20){
  file0 <- paste0("datos_Rt",Rt,"_Ri",Ri,"/DatosMHP_",i,".csv")
  DatosT<-read.csv(file0)
  DatosC <-rbind(DatosC,DatosT)
 }


###III. Malla de

 RasMHP <- raster(ncol=ncol(RasDEM),nrow=nrow(RasDEM),ext=extent(RasDEM),res=res(RasDEM),crs=crs(RasDEM))

 RasMHP$PotenciakW     <- DatosC$PotenciakW                       # 1
 RasMHP$Altura         <- DatosC$Altura                           # 2
 RasMHP$GastoMax       <- DatosC$GastoMax                         # 3
 RasMHP$HMax           <- DatosC$HMax                             # 4

 RasMHP$xTurbina       <- DatosC$xTurbina                         # 5
 RasMHP$yTurbina       <- DatosC$yTurbina                         # 6
 RasMHP$zTurbina       <- DatosC$zTurbina                         # 7
 RasMHP$BaseTurbina    <- DatosC$BaseTurbina                      # 8

 RasMHP$xIntake        <- DatosC$xIntake                          # 9
 RasMHP$yIntake        <- DatosC$yIntake                          # 10
 RasMHP$zIntake        <- DatosC$zIntake                          # 11
 RasMHP$BaseIntake     <- DatosC$BaseIntake                       # 12

 RasMHP <- brick(RasMHP)
  
 # Guardar los archivos en las respectivas carpetas

 file1 <- paste0("datos_Rt",Rt,"_Ri",Ri,"/RasMHP_Rt",Rt,"_Ri",Ri,".tif")
 file2 <- paste0("datos_Rt",Rt,"_Ri",Ri,"/DatosMHP_Rt",Rt,"_Ri",Ri,".csv")

 writeRaster(RasMHP, filename= file1, overwrite=TRUE) # Con todas las columnas de Atributos
 write.csv(DatosC,file=file2,row.names=FALSE)


# Guardar los archivos en carpeta General
 file1 <- paste0("DatosFinales/RasMHP_Ri",Ri,"_Rt",Rt,".tif")
 file2 <- paste0("DatosFinales/DatosMHP_Ri",Ri,"_Rt",Rt,".csv")

 writeRaster(RasMHP, filename= file1, overwrite=TRUE) # Con todas las columnas de Atributos
 write.csv(DatosC,file=file2,row.names=FALSE)
