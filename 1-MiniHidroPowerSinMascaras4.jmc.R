# MinihidroPower
# Calculation of MHP potential
# Coded by: Gerardo Alcal'a
# Universidad Veracruzana
# First Version December 19 2018

# Updated December 09 2019

####I. Procesos Iniciales
{
  graphics.off() 
  remove(list=ls())
  
  ##setwd("C:/Users/Gerardo Alcalá/Desktop/MiniHidro")
  setwd(Sys.getenv("PWD"))
  
  library(sp)
  library(raster)
  library(rgeos)
  library(rgdal)
  library(tools)
  library(gdistance)
  
  radIntake <- 2000
  radTurbina <- 2000
  print(paste0("radIntake: ",radIntake," radTurbina: ",radTurbina))
}
###II. Raster y Vectoriales
{
  ## Leer Rasters y Vectoriales
  RasRio <- brick("A-RasterRio.tif")
  RasDEM <- raster("B-RasterDEM.tif")
  RasMask <- raster("D-Edificaciones.tif")
  ## Homologar Proyecci?n
  RasRio <- projectRaster(RasRio,RasDEM)
  RasMask <- projectRaster(RasMask,RasDEM)
  ## Homologar Extension
  RasRio  <- crop(RasRio,RasDEM)
  RasMask <- crop(RasMask,RasDEM)
  ## Nombres Atributo
  names(RasRio) <- c("ID","Gasto","Base","Entrada","Salida")
  names(RasDEM) <- "Altura"
  names(RasMask) <- "Mascara"
  
  ShpMask <- shapefile("VectorFiles/Edificaciones/Edificaciones.shp")
  ShpMask <- spTransform(ShpMask,crs(RasDEM))
  ShpMask <- crop(ShpMask,RasDEM)
  
  # Negativo de Mascara
  RasMask0 <- RasMask
  j <- which(0.9<= RasMask[] | RasMask[] <= 1.1)
  RasMask[] <- 1
  RasMask[j] <- NA
  
  RasDEMMask <-RasDEM 
  RasDEMMask[] <- RasDEMMask[]*RasMask[]
}

###III. Malla de puntos
{
  ## a. Puntos Rio (Points from Raster)
  SPPointsRio <- as(RasRio,"SpatialPointsDataFrame")
  SPPointsRio <- extract(RasDEM,SPPointsRio,sp=TRUE)
  ## b. Raster MHP (Variables Calculadas)
  RasMHP <- RasDEM
  RasMHP$PotenciakW  <- NA
  RasMHP$GastoMax    <- 0
  RasMHP$HMax        <- 0
  RasMHP$xTurbina    <- NA
  RasMHP$yTurbina    <- NA
  RasMHP$zTurbina    <- NA
  RasMHP$BaseTurbina <- NA
  RasMHP$xIntake     <- NA
  RasMHP$yIntake     <- NA
  RasMHP$zIntake     <- NA
  RasMHP$BaseIntake  <- NA
  RasMHP$Mascara <- 1
  
  RasMHP$Mascara[j] <- 0
  
  ## c. Raster MHP a Malla de Puntos (OJO! Celdas con puro NA's no generan punto)
  SPPointsGrid <- as(RasDEM,"SpatialPointsDataFrame")
  SPPointsGrid$Mascara <- RasMHP$Mascara[]

  if(length(SPPointsGrid)!=ncell(RasMHP)) {
    cat("Error, hay celdas del Raster con puro NA's")  
    stop("FIN")
  } else {
    cat("No hay NA's en el Raster: El programa puede seguir\n") 
  }
}
###IV. Funcion de NumCruces. Intersecta Linea Recta con Rio (Sitios Radio)
fNumCruces <- function(NumCoordsRioRadiox,NumCoordsRioRadioy,NumCoordPiletaix,NumCoordPiletaiy)
{
  Numxc <- c(NumCoordsRioRadiox, NumCoordPiletaix)
  Numyc <- c(NumCoordsRioRadioy, NumCoordPiletaiy)
  Matxyc <- cbind(Numxc,Numyc)
  SLLineaRecta <- spLines(Matxyc,crs=crs(RasMHP))
  SPolLineaRecta <- buffer(SLLineaRecta, width=res(RasMHP)[1]*0.75)
  InterCruces <- intersect(SPPointsRioRadio,SPolLineaRecta)
  NumCruces <- dim(InterCruces)[1]
}

#V. Funcion de CruceMascara. Intersecta Linea Recta con Mascara (Poligono)
fCruceMascara <- function(NumCoordsRioRadiox,NumCoordsRioRadioy,NumCoordPiletaix,NumCoordPiletaiy) {
  Numxc  <- c(NumCoordsRioRadiox, NumCoordPiletaix) 
  Numyc  <- c(NumCoordsRioRadioy, NumCoordPiletaiy)
  Matxyc <- cbind(Numxc,Numyc)
  SLLineaRecta   <- spLines(Matxyc,crs=crs(RasMHP))
  SPolLineaRecta <- buffer(SLLineaRecta, width=res(RasMHP)[1]*0.75)
  SPolInterMask  <- intersect(ShpMask,SPolLineaRecta)
  CruceMascara   <- length(SPolInterMask)
}

##x11()
##plot(RasDEMMask)             # Tiene valores de altura y NA
##plot(ShpMask,col='orange',add=TRUE)
##plot(SPPointsRio,col='blue',add=TRUE,pch=19,cex=0.1)
##mult <- 5000

##############################################
###VII  INICIA CICLO POR EL GRID
library(doParallel)
#cores=Sys.getenv("SLURM_NTASKS_PER_NODE")
cores=38
print(paste0("Running program with : ",cores[1]," cores."))
print(paste0("Grid size            : ",length(SPPointsGrid)))
cl <- makeCluster(cores[1],outfile="")
registerDoParallel(cl)

ptime <- system.time({
  
  foreach(i=1:length(SPPointsGrid)) %dopar% {
  
    PotenciakW    <- 0
    HMax          <- 0
    GastoMax      <- 0
    NumCruces     <- 0
    xTurbina      <- 0
    yTurbina      <- 0
    zTurbina      <- 0
    BaseTurbina   <- 0
    xIntake       <- 0
    yIntake       <- 0
    zIntake       <- 0
    BaseIntake    <- 0
    
    library(sp)
    library(raster)
    sink("log.txt", append=TRUE)
    cat(paste("Starting iteration",i,"\n"))
    sink()
    ### PILETA ###
    
    ###1. Inicio: Ubicar la pileta
    SPPointPiletai <- SPPointsGrid[i,]
    hi <- SPPointPiletai$Altura
    ## Coordenadas Pileta (punto actual sobre el DEM)
    NumCoordPiletaix <- SPPointPiletai@coords[1]
    NumCoordPiletaiy <- SPPointPiletai@coords[2]
    
    #1.2 Si la Pileta toca la Mascara (NEXT)
    if(SPPointPiletai$Mascara==0){
      PotenciakW <- 0; 
      GastoMax <- 0
      HMax <- 0
    }else{
    ###2. Si rio esta fuera del radio de la Pileta (NEXT)
    ## a. Crear buffer para Intake y Turbina
    SPolRadio <- buffer(SPPointPiletai, width=radTurbina)
    SPolRadio2 <- buffer(SPPointPiletai, width=radIntake)
    SPPointsRioRadio <- intersect(SPPointsRio,SPolRadio)
    SPPointsRioRadio2 <- intersect(SPPointsRio,SPolRadio2)
    
    ## b. Verificar si se intersecta el rio, sino terminar Iteracion
    if(length(SPPointsRioRadio)==0 | length(SPPointsRioRadio2)==0) {
      PotenciakW <- 0
      if(length(SPPointsRioRadio)==0){
        HMax <- 0
      }
      if(length(SPPointsRioRadio2)==0){
        GastoMax <- 0
      }
    }  else {
      ###3 TURBINA ###
      ## a. Sitios con menor y mayor  altura a la Pileta (else NEXT)
      k1 <- which(SPPointsRioRadio$Altura+4 < SPPointPiletai$Altura)
      j1 <- which(hi+3<SPPointsRioRadio2$Altura)
      
      ## b. En caso que no haya sitios mas bajos o altos que la pileta
      if(length(k1)==0 | length(j1)==0) {
        PotenciakW <- 0
        if(length(k1) ==0){
          HMax <- 0
        }
        if(length(j1) ==0){
          GastoMax <- 0
        }
      } else {
        ###4. Localizacion de la Turbina
        ## a. Cruces Turbina
        SPPointsTurbina <- SPPointsRioRadio[k1,]
        
        NumCoordsTurbinax <- SPPointsTurbina@coords[,1]
        NumCoordsTurbinay <- SPPointsTurbina@coords[,2]
        NumCruces <- mapply(fNumCruces,NumCoordsTurbinax,NumCoordsTurbinay,NumCoordPiletaix,NumCoordPiletaiy)
        k2 <- which(NumCruces ==1)
        
        if(length(k2) == 0) {
          PotenciakW <- 0
          HMax <- 0
        } else {
          SPPointsTurbinaUnCruce <- SPPointsTurbina[k2,]
          # Interseccion con Mascara
          #5. Ruta Recta del Penstock (Turbina) que no TOQUE la mascara (else NEXT)
            #a. Determinar las rectas que intersectan la mascara
              NumCoordsTurbinax <- SPPointsTurbinaUnCruce@coords[,1]; NumCoordsTurbinay <- SPPointsTurbinaUnCruce@coords[,2]
              CrucesMascara <- mapply(fCruceMascara,NumCoordsTurbinax,NumCoordsTurbinay,NumCoordPiletaix,NumCoordPiletaiy)
              #SPPointsTurbina$CrucesMascara <- mapply(fCruceMascara,NumCoordsTurbinax,NumCoordsTurbinay,NumCoordPiletaix,NumCoordPiletaiy)
              k3 <- which(CrucesMascara == 0) # 0 los que no tocan la máscara
            #b. Si todas las rectas tocan la mascara (NEXT)
            if(length(k3) < 1){
              PotenciakW <- 0
              HMax[i] <- 0
            }else{
            #6. Turbina, punto con altura minima
            #.  Rectas que no tocan la mascara
            SPPointsTurbinaMask<- SPPointsTurbinaUnCruce[k3,]
          # Obtener Altura Minima
          k4 <- which.min(SPPointsTurbinaMask$Altura)
          ## Posicion con menor altura sin obstaculos
          SPPointTurb <- SPPointsTurbinaMask[k4[1],] 
          ## c. Gradiente de altura maximo
          HTurb <- SPPointTurb$Altura
          ## d. Guardamos Gradiente Altura maxima
          HMax <- hi-HTurb 
          ## e. Guardamos la coordenada de la turbina
          xTurbina <- SPPointTurb@coords[1]
          yTurbina <- SPPointTurb@coords[2]
          zTurbina <- HTurb
          BaseTurbina <- SPPointTurb$Base
          ### DESVIACION ###
          ###7. El Intake pertenece a la misma rama del rio (else NEXT)
          
          j2 <- (SPPointsRioRadio2$Base==SPPointTurb$Base | SPPointsRioRadio2$Salida==SPPointTurb$Entrada)
          SPPointsMismaRamaIntake <- SPPointsRioRadio2[j2,]
          
          ###8. Sitios con mayor altura que la pileta (else NEXT) 
          j3 <- which(hi+3<SPPointsMismaRamaIntake$Altura)
          if(length(j3)==0) {
            ## Guardamos Gasto Maximo
            GastoMax <- 0
            PotenciakW <- 0
          } else {
            ###9. Localizacion de Intake
            
            ## a. Cruces Intake
            SPPointsIntake <- SPPointsMismaRamaIntake[j3,]
            
            NumCoordsIntakex <- SPPointsIntake@coords[,1]
            NumCoordsIntakey <- SPPointsIntake@coords[,2]
            
            NumCruces <- mapply(fNumCruces,NumCoordsIntakex,NumCoordsIntakey,NumCoordPiletaix,NumCoordPiletaiy)
            j4 <- which(NumCruces ==1)
            
            if(length(j4) ==0) {
              PotenciakW <- 0
              GastoMax <- 0
            } else{
              SPPointsIntakeUnCruce <- SPPointsIntake[j4,]
              
              ###10. Cruces con Mascara
              #. Ver qué líneas nada más tienen un cruce
              NumCoordsIntakex <- SPPointsIntakeUnCruce@coords[,1]
              NumCoordsIntakey <- SPPointsIntakeUnCruce@coords[,2]
              CrucesMascara <- mapply(fCruceMascara,NumCoordsIntakex,NumCoordsIntakey,NumCoordPiletaix,NumCoordPiletaiy)
              j5 <- which(CrucesMascara == 0) # 0 los que no tocan la máscara
              
              #c. Si todas las rectas tocan la mascara (NEXT)
              if(length(j5) < 1){
                PotenciakW <- 0 
                GastoMax <- 0
              }else{
              #d. Rectas que no tocan la mascara
              SPPointsIntakeMask<- SPPointsIntakeUnCruce[j5,]
              #e. Localizacion Intake
              j6 <- which.max(SPPointsIntakeMask$Gasto)
              SPPointIntakei <- SPPointsIntakeMask[j6[1],] # Posicion con mayor gasto sin obstaculos
              ##f. Guardamos Gasto Maximo
              GastoMax <- SPPointIntakei$Gasto
              ## d. Guardamos puntos el Intake
              xIntake <- SPPointIntakei@coords[1]
              yIntake <- SPPointIntakei@coords[2]
              zIntake <- SPPointIntakei$Altura
              BaseIntake <- SPPointIntakei$Base
              ### POTENCIA ###
              ###12. Asignacion PotenciakW
              PotenciakW <- 1000*9.81*GastoMax*HMax/1000
             }
            }
           }
          }
         }
       }
      }
    }
    #return(data.frame("index"=i,"PotenciakW"=PotenciakW,"HMax"=HMax,"GastoMax"=GastoMax,"xTurbina"=xTurbina,"yTurbina"=yTurbina,"zTurbina"=zTurbina,"BaseTurbina"=BaseTurbina,"xIntake"=xIntake,"yIntake"=yIntake,"zIntake"=zIntake,"BaseIntake"=BaseIntake))        
  }
})[3]
stopCluster(cl)
ptime

#### Termina Ciclo for del grid
##################################
### FIN CICLO POR TODO EL GRID ###
##16. Guardar Raster
{
  SPPointsGrid$index <- 1:length(SPPointsGrid)
  SPPointsGrid <- merge(SPPointsGrid,test)
  
  ##View(SPPointsGrid@data)
  RasFinal <- raster(SPPointsGrid,ncol=ncol(RasMHP),nrow=nrow(RasMHP),ext=extent(RasMHP),res=res(RasMHP),crs=crs(RasMHP))
  
  RasFinal$PotenciakW  <- SPPointsGrid$PotenciakW   # 1
  RasFinal$Altura      <- SPPointsGrid$Altura       # 2
  RasFinal$GastoMax    <- SPPointsGrid$GastoMax     # 3
  RasFinal$HMax        <- SPPointsGrid$HMax         # 4
  RasFinal$xTurbina    <- SPPointsGrid$xTurbina     # 5
  RasFinal$yTurbina    <- SPPointsGrid$yTurbina     # 6
  RasFinal$zTurbina    <- SPPointsGrid$zTurbina     # 7
  RasFinal$BaseTurbina <- SPPointsGrid$BaseTurbina  # 8
  
  RasFinal$xIntake <- SPPointsGrid$xIntake          # 9
  RasFinal$yIntake <- SPPointsGrid$yIntake          # 10
  RasFinal$zIntake <- SPPointsGrid$zIntake          # 11
  RasFinal$BaseIntake <- SPPointsGrid$BaseIntake    # 12
  
  RasFinal$RadioTurbina <- radTurbina               # 13
  RasFinal$RadioIntake  <- radIntake                # 14
  
  RasFinal <- brick(RasFinal)
  ## Con todas las columnas de Atributos
  x <- writeRaster(RasFinal, '1-RasterMHP.tif', overwrite=TRUE)
  
  dffinal <- data.frame(SPPointsGrid)
  archivof1 <- paste("DatosMHP.csv",sep="")
  write.csv(dffinal, file = archivof1,row.names=FALSE)
}

