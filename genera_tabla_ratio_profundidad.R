## BATIMETRIA para cortes de Landsat, Sentinel y Planet en Mahahual
# Deriving Bathymetry from Multispectral Remote Sensing Data, William J. Hernandez * and Roy A. Armstrong # 3.2. Bathymetry Retrieval

# Calcula el logratio de las imágenes y hace el extract
# para generar una tabla 
# hace iteración sobre sensores, correccion glint, filtrado

library(terra)
library(sf)
library(dplyr)
library(stats)

### Lee las imágenes Landsat
setwd("/home/jf/pCloudDrive/proyectos/Batimetria")




## Functions


sitio <- "mah"
sitio <- "xcan"
sitios <- c("mah","xcan","xcas")

GlintCorrection <- F
Filter <- T
tipo <- "S"

################## Batimetría #####################################
print("------------------- Ratio Transform Algorithm")
############# Ratio Transform Algorithm Stumpf et al. (2003) 
# blue/green
tipo <- "L"; sitio <- "mah"; GlintCorrection <- T; Filter <- T

for (sitio in sitios){
  print(paste0("-------------", sitio))
  pts_bati <- vect(paste0("mapas/pts_bati",sitio,".gpkg"))
  maskdeep <- vect(paste0("mapas/deep16_",sitio,".gpkg"))
#for (tipo in c("L","S")){ 
for (tipo in c("L","S","P")){ 
  for (GlintCorrection in c(TRUE,FALSE)){
    for (Filter in c(TRUE,FALSE)){
      
if (GlintCorrection == T){glintopcion <- "Glint"}else{glintopcion <- "NoGlint"}
if (Filter == T){filteropcion <- "Filter"}else{filteropcion <- "NoFilter"}
    
## Landsat coastal blue, blue, green, nir
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
      
## Sentinel blue, blue, green, nir
  if (tipo == "S"){
    #imagen <- rast(paste0("cortes_S/T16QDF_20230116T161619_",sitio,".tif")) # R2 0.54 rmse 9.5
#imagen <- rast("cortes_S/T16QDF_20230116T161619_mah1C.tif") # R2 0.54 rmse 1.03

#imagen <- rast("cortes_S/T16QDF_20230401T160831_mah.tif") # R2 0.54 rmse 10.2
#imagen <- rast("cortes_S/T16QDF_20221023T161331_mah.tif") # R2 0.59 rms 12.9
imagen <- rast("cortes_S/T16QDF_20220720T160829_mah.tif") # R2 0.63 rms 11.6
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

## Planet "coastal_blue" "blue" "green_i" "green" "yellow" "red"  "rededge" "nir"
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
      #tab <- NULL
      tab <- extract(ratio,pts_bati)
      tab$depth <- pts_bati$DEPTH_M
      tab$CALVE <- pts_bati$CALVE
      head(tab)
      
      write.csv(tab,paste0("Tablas/",sitio,"-",tipo,"-",glintopcion,"-", filteropcion,".csv"))   
      
} # loop filter
} # loop glint
} # loop tipo
} # loop sitio

