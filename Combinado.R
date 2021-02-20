# MinihidroPower
# Combinacion Potencial
# Coded by: Gerardo Alcal'a
# Universidad Veracruzana
# First Version 27 Noviembre 2020

# Updated 27 Noviembre 2020

  graphics.off() 
  remove(list=ls())
  
  ##setwd("C:/Users/Gerardo Alcala/Desktop/MiniHidro")
  setwd(Sys.getenv("PWD"))
  
  library(raster) 

 ## Leer Rasters y Vectoriales
 RasDEM <- raster("B-RasterDEM.tif")
    
 datos1 <- read.csv("scratch/mini.hydro.1962/Datos3MHP.csv")
# datos2 <- read.csv("scratch/mini.hydro.1964/Datos3MHP.csv") 
# datos3 <- read.csv("scratch/mini.hydro.1965/Datos3MHP.csv")
# datos4 <- read.csv("scratch/mini.hydro.1966/Datos3MHP.csv")
# datos5 <- read.csv("scratch/mini.hydro.1967/Datos3MHP.csv")
# datos6 <- read.csv("scratch/mini.hydro.1968/Datos3MHP.csv")
# datos7 <- read.csv("scratch/mini.hydro.1969/Datos3MHP.csv")
# datos8 <- read.csv("scratch/mini.hydro.1970/Datos3MHP.csv")
# datos9 <- read.csv("scratch/mini.hydro.1971/Datos3MHP.csv")
# datos10 <- read.csv("scratch/mini.hydro.1972/Datos3MHP.csv")
# datos11 <- read.csv("scratch/mini.hydro.1973/Datos3MHP.csv")
# datos12 <- read.csv("scratch/mini.hydro.1974/Datos3MHP.csv")
# datos13 <- read.csv("scratch/mini.hydro.1975/Datos3MHP.csv")
# datos14 <- read.csv("scratch/mini.hydro.1976/Datos3MHP.csv")
# datos15 <- read.csv("scratch/mini.hydro.1977/Datos3MHP.csv")
# datos16 <- read.csv("scratch/mini.hydro.1978/Datos3MHP.csv")
# datos17 <- read.csv("scratch/mini.hydro.1979/Datos3MHP.csv")
# datos18 <- read.csv("scratch/mini.hydro.1980/Datos3MHP.csv")
# datos19 <- read.csv("scratch/mini.hydro.1981/Datos3MHP.csv")
# datos20 <- read.csv("scratch/mini.hydro.1982/Datos3MHP.csv")


# DatosC <- rbind(datos1,datos2,datos3,datos4,datos5,datos6,datos7,datos8,datos9,datos10,datos11,datos12,datos13,datos14,datos15,datos16,datos17,datos18,datos19,datos20)

###III. Malla de

#RasMHP <- raster(ncol=ncol(RasDEM),nrow=nrow(RasDEM),ext=extent(RasDEM),res=res(RasDEM),crs=crs(RasDEM))

# RasMHP$PotenciakW     <- DatosC$PotenciakW                       # 1
# RasMHP$Altura         <- DatosC$Altura                           # 2
# RasMHP$GastoMax       <- DatosC$GastoMax                         # 3
# RasMHP$HMax           <- DatosC$HMax                             # 4

# RasMHP$xTurbina       <- DatosC$xTurbina                         # 5
# RasMHP$yTurbina       <- DatosC$yTurbina                         # 6
# RasMHP$zTurbina       <- DatosC$zTurbina                         # 7
# RasMHP$BaseTurbina    <- DatosC$BaseTurbina                      # 8

# RasMHP$xIntake        <- DatosC$xIntake                          # 9
# RasMHP$yIntake        <- DatosC$yIntake                          # 10
# RasMHP$zIntake        <- DatosC$zIntake                          # 11
# RasMHP$BaseIntake     <- DatosC$BaseIntake                       # 12

# RasMHP <- brick(RasMHP)
  
 # Nombres Archivos  
# file1 <- 'RasMHP.tif',
 file2 <- 'DatosCMHP.csv'

 # Guardar Raster
# writeRaster(RasFinal, filename= file1, overwrite=TRUE) # Con todas las columnas de Atributos

 # Guardar dataframe Combinado
 write.csv(datos1,file=file2,row.names=FALSE)
