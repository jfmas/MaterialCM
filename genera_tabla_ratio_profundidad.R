## Complemento artículo Ciencias Marinas
## Manuscrito Mapeo batimétrico en aguas costeras con imágenes de satélite: 
## Un estudio de caso en el Caribe Mexicano

# Calcula el logratio de las imágenes y hace el extract
# para generar una tabla 
# hace iteraciones sobre sensores, opción correccion glint, opción filtrado
# para cada iteración (combinación) salva una tabla con valores del ratio para cada
# punto batimétrico in situ


# librerias
library(terra)
library(sf)
library(dplyr)
library(stats)

### Lee las imágenes Landsat
setwd("/home/jf/pCloudDrive/proyectos/Batimetria")

## Functions
# Correr función para correccion del brillo (Correct_Glint_Effect.R)
source("Correct_Glint_Effect.R")

sitio <- "mah"


############# Ratio blue/green Transform Algorithm Stumpf et al. (2003) 

# Lee archivo de pts batimétricos y polígono aguas profundas
pts_bati <- vect(paste0("mapas/pts_bati",sitio,".gpkg"))
maskdeep <- vect(paste0("mapas/deep16_",sitio,".gpkg"))

## Tipos sobre los 3 sensores, opción glint y opción filtrado (3 x 2 x 2 combinaciones)
for (tipo in c("L","S","P")){ 
  for (GlintCorrection in c(TRUE,FALSE)){
    for (Filter in c(TRUE,FALSE)){
      
if (GlintCorrection == T){glintopcion <- "Glint"}else{glintopcion <- "NoGlint"}
if (Filter == T){filteropcion <- "Filter"}else{filteropcion <- "NoFilter"}
    
## Landsat coastal blue, blue, green, nir #################################################
if (tipo == "L"){ 
imagen <- rast(paste0("cortes_L/LC09_L2SP_019047_20230115_20230313_02_T1_",sitio,".tif"))
cblue <- imagen[[1]]
blue <- imagen[[2]] ##2
green <- imagen[[3]]
nir <- imagen[[5]]
if (GlintCorrection == T){ 
  # correct_glint(bi,nir,nsample,deepmask)  
  blue <- correct_glint(blue,nir,500,maskdeep)  
  green <- correct_glint(green,nir,500,maskdeep)  
}
if (Filter == T){ 
  # correct_glint(bi,nir,nsample)  
  blue <- focal(blue, w=3, fun="mean", na.rm=TRUE) 
  green <- focal(green, w=3, fun="mean", na.rm=TRUE) 
}
} # Fin if Landsat
      
## Sentinel blue, blue, green, nir ##############################################
if (tipo == "S"){
imagen <- rast("cortes_S/T16QDF_20220720T160829_mah.tif")
names(imagen)
blue <- imagen[[2]] 
green <- imagen[[3]]
nir <- imagen[[5]]
plot(blue)
if (GlintCorrection == T){ 
  # correct_glint(bi,nir,nsample)  
  blue <- correct_glint(blue,nir,500,maskdeep)  
  green <- correct_glint(green,nir,500,maskdeep)  
}
if (Filter == T){ 
  # correct_glint(bi,nir,nsample)  
  blue <- focal(blue, w=3, fun="mean", na.rm=TRUE) 
  green <- focal(green, w=3, fun="mean", na.rm=TRUE) 
}
} # Fin if Sentinel

## Planet "coastal_blue" "blue" "green_i" "green" "yellow" "red"  "rededge" "nir" ###########3
if (tipo == "P"){
imagen <- rast("cortes_P/Mosaico20230117_3B_AnalyticMS_SR_8b.tif")
names(imagen)
blue <- imagen[[2]] ##2
green <- imagen[[4]]
nir <- imagen[[8]]
if (GlintCorrection == T){ 
  # correct_glint(bi,nir,nsample)  
  blue <- correct_glint(blue,nir,500,maskdeep)  
  green <- correct_glint(green,nir,500,maskdeep)  
}
if (Filter == T){ 
  # correct_glint(bi,nir,nsample)  
  blue <- focal(blue, w=3, fun="mean", na.rm=TRUE) 
  green <- focal(green, w=3, fun="mean", na.rm=TRUE) 
}
} # Fin if Planet
 
      ###############################################################################
      ######## CALCULO BATI
      ## Calcula ratios, elimina outliers
      n<-10000
      ratio <- log(n*blue)/log(n*green)
      cuantiles <- quantile(values(ratio),c(0.01,0.99),na.rm=T)
      plot(ratio)
      ratio[ratio < cuantiles[1]] <- NA
      ratio[ratio > cuantiles[2]] <- NA
      ratio_name <- paste0("ratio/ratio_",sitio,"-",tipo,"-",glintopcion,"-", filteropcion,".tif")
      writeRaster(ratio, file=ratio_name, overwrite=TRUE, wopt= list(gdal=c("COMPRESS=NONE"), datatype='FLT4S'))
      plot(pts_bati, add=T)
      names(ratio) <- "ratio"
      ## extrae valores ratio para pts batimétricos
      tab <- extract(ratio,pts_bati)
      tab$depth <- pts_bati$DEPTH_M
      tab$CALVE <- pts_bati$CALVE
      head(tab)
      ## salva la tabla
      write.csv(tab,paste0("Tablas/",sitio,"-",tipo,"-",glintopcion,"-", filteropcion,".csv"))   
      
} # loop filter
} # loop glint
} # loop tipo

