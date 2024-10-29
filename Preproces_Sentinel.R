## Complemento artículo Ciencias Marinas
##  PREPROCESSING: SENTINEL 2 files, apply mask, cut, read metadata, convert to SR and save.



library(terra)
library(sf)
library(dplyr)
library(XML)
library(xml2)
### Lee las imágenes Sentinel
setwd("/home/jf/pCloudDrive/proyectos/Batimetria")


## Functions
# Function to reclassify CLS layer

# De 0 a 11
# “No Data”, “Saturated or Defective”, “Dark area pixels”, “Cloud Shadows”, “Vegetation”, 
# “non-Vegetated”, “Water”, “Unclassified”, “Cloud medium”, “Cloud high”, “Thin cirrus” and “Snow”.

## medium y high cloud, cirrus, cloud shadow, saturated
cloud_ext <- function(SCL){
  SCL == 1 | SCL == 3 | SCL == 8 | SCL == 9 | SCL == 10
}

water <- function(SCL){
  SCL == 6
}


## Lee los mapas de pts batimétricos
sitios <- c("rbsk","mah","xcan","xcas")
sitios <- c("mah","xcan","xcas")

pts_batirbsk <- vect("mapas/pts_batirbsk.gpkg") # 28980 puntos
nrow(pts_batirbsk)
pts_batimah <- vect("mapas/pts_batimah.gpkg") # 2844
nrow(pts_batimah)
pts_batixcan <- vect("mapas/pts_batixcan.gpkg") # 19975
nrow(pts_batixcan)
pts_batixcas <- vect("mapas/pts_batixcas.gpkg") # 29700
nrow(pts_batixcas)
pts_batichin <- vect("mapas/pts_batichin.gpkg") # 426
nrow(pts_batichin)
pts_batipmor <- vect("mapas/pts_batipmor.gpkg") # 19479
nrow(pts_batipmor)

##############################################################################
################# IMAGES SENTINEL ya es reflectancia
##############################################################################

rutaS <- "/home/jf/pCloudDrive/proyectos/Batimetria/Sentinel/S2B_MSIL2A_20230116T161619_N0509_R140_T16QDF_20230116T191039.SAFE/GRANULE/L2A_T16QDF_A030625_20230116T161811/"
prefix <- "T16QDF_20230116T161619_" 

# rutaS <- "/home/jf/pCloudDrive/proyectos/Batimetria/Sentinel/S2A_MSIL2A_20230401T160831_N0509_R140_T16QDF_20230401T235100.SAFE/GRANULE/L2A_T16QDF_A040606_20230401T161949/"
# prefix <- "T16QDF_20230401T160831_"
# 
# rutaS <- "/home/jf/pCloudDrive/proyectos/Batimetria/Sentinel/S2A_MSIL2A_20220221T163311_N0400_R083_T15QZD_20220221T214833.SAFE/GRANULE/L2A_T15QZD_A034829_20220221T163642/"
# prefix <- "T15QZD_20220221T163311_" 
# 
# rutaS <- "/home/jf/pCloudDrive/proyectos/Batimetria/Sentinel/S2A_MSIL2A_20221023T161331_N0400_R140_T16QDF_20221023T215559.SAFE/GRANULE/L2A_T16QDF_A038318_20221023T162200/"
# prefix <- "T16QDF_20221023T161331_"
# 
# rutaS <- "/home/jf/pCloudDrive/proyectos/Batimetria/Sentinel/S2B_MSIL2A_20220720T160829_N0400_R140_T16QDF_20220720T203355.SAFE/GRANULE/L2A_T16QDF_A028051_20220720T162133/"
# prefix <- "T16QDF_20220720T160829_"

#rutaS <- "/home/jf/pCloudDrive/proyectos/Batimetria/Sentinel/S2B_MSIL1C_20230116T161619_N0509_R140_T16QDF_20230116T180834.SAFE/GRANULE/L1C_T16QDF_A030625_20230116T161811/"
#prefix <- "T16QDF_20230116T161619_"
#prefix <- substr(tarfile,56,95)

## Nivel 2A
input_b1 <- paste0(rutaS,"IMG_DATA/R20m/",prefix,"B01_20m.jp2") # blue
input_b2 <- paste0(rutaS,"IMG_DATA/R10m/",prefix,"B02_10m.jp2") # blue
input_b3 <- paste0(rutaS,"IMG_DATA/R10m/",prefix,"B03_10m.jp2") # green 
input_b4 <- paste0(rutaS,"IMG_DATA/R10m/",prefix,"B04_10m.jp2") # red
input_b8 <- paste0(rutaS,"IMG_DATA/R10m/",prefix,"B08_10m.jp2") # nir

input_cloudP <- paste0(rutaS, "QI_DATA/MSK_CLDPRB_20m.jp2")

# De 0 a 11
# “No Data”, “Saturated or Defective”, “Dark area pixels”, “Cloud Shadows”, “Vegetation”, 
# “non-Vegetated”, “Water”, “Unclassified”, “Cloud medium”, “Cloud high”, “Thin cirrus” and “Snow”.
input_clasif <- paste0(rutaS,"IMG_DATA/R20m/",prefix,"SCL_20m.jp2") # clasif

## Nivel 1C
# input_b1 <- paste0(rutaS,"IMG_DATA/",prefix,"B01.jp2") # blue
# input_b2 <- paste0(rutaS,"IMG_DATA/",prefix,"B02.jp2") # blue
# input_b3 <- paste0(rutaS,"IMG_DATA/",prefix,"B03.jp2") # green 
# input_b4 <- paste0(rutaS,"IMG_DATA/",prefix,"B04.jp2") # red
# input_b8 <- paste0(rutaS,"IMG_DATA/",prefix,"B08.jp2") # nir


b2 <- rast(input_b2)/10000
b1 <- project(rast(input_b1)/10000,b2)
b3 <- rast(input_b3)/10000
b4 <- rast(input_b4)/10000
b8 <- rast(input_b8)/10000
SCL <- project(rast(input_clasif),b2)

plot(b2)
plot(SCL)

print("------------------- QA")
mask_water <- water(SCL)
#plot(mask_water) 

## Aplica las mascara de agua, nube y sombra de nube

b1[mask_water ==0] <- NA
b2[mask_water ==0] <- NA
b3[mask_water ==0] <- NA
b4[mask_water ==0] <- NA
b8[mask_water ==0] <- NA

# plot(b1)
# plot(b2)
# plot(b3)
# plot(b4)

sitio <- "mah"
for(sitio in sitios){ 
  pts_bati <- vect(paste0("mapas/pts_bati",sitio,".gpkg"))
  ext <- ext(pts_bati) +c(0,200,0,200)
  subb1 <- crop(b1,ext)
  subb2 <- crop(b2,ext)
  subb3 <- crop(b3,ext)
  subb4 <- crop(b4,ext)
  subb8 <- crop(b8,ext)
  plot(subb2)
  name <- paste0("cortes_S/",prefix,sitio,".tif")
  writeRaster(c(subb1,subb2,subb3,subb4,subb8), file=name, overwrite=TRUE, wopt= list(gdal=c("COMPRESS=NONE"), datatype='FLT4S'))
} # loop sobre sitios con datos batimetria
}# condicion sigue (todos los tifs presentes)
## borra los TIF
print("borra los tif")
listatifdel <- list.files(path="/tmp",include.dirs = T, recursive=T,pattern=".TIF",full.names = T)
file.remove(listatifdel)
} else {print("tar ya procesado")} # si este tar ya fue procesado
} # for sobre los tar



##############################################################################
################# IMAGES SENTINEL ya es reflectancia
##############################################################################

