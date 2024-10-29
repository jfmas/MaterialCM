## Complemento artículo Ciencias Marinas
## Manuscrito Mapeo batimétrico en aguas costeras con imágenes de satélite: 
## Un estudio de caso en el Caribe Mexicano
##  PREPROCESSING: Untar Landsat files, apply mask, cut, read metadata, convert to SR and save.

## QA https://pages.cms.hu-berlin.de/EOL/gcg_eo/02_data_quality.html

library(terra)
library(sf)
library(dplyr)
library(XML)
library(xml2)
### Lee las imágenes Landsat
setwd("/home/jf/pCloudDrive/proyectos/Batimetria")

## Functions
# Function to find fill values from Landsat QA_PIXEL
## Ojo hay un bit 0, la position 8 es para bit 7
fill_pixels <- function(x){
  return(intToBits(x)[1] == T)
}

## Cloud with high confidence
cloud <- function(x){
  return(intToBits(x)[4] == T)
}

## Cloud shadow with high confidence
cloud_shadow <- function(x){
  return(intToBits(x)[5] == T)
}

## Dilated cloud, cirrus, cloud or shadow
cloud_ext <- function(x){
  return(intToBits(x)[2] == 1 | intToBits(x)[3] == 1 | intToBits(x)[4] == 1 | intToBits(x)[5] == 1)
}

water <- function(x){
  return(intToBits(x)[8] == T)
}


## Lee los mapas de pts batimétricos
pts_batimah <- vect("mapas/pts_batimah.gpkg") # 2844
nrow(pts_batimah)


##############################################################################
################# IMAGES LANDSAT 8 y 9 L2SP ya es reflectancia
##############################################################################

tarfile <- "/home/jf/pCloudDrive/proyectos/BlueHole/images_Landsat/LC08_L2SP_019047_20141114_20200910_02_T1.tar"

prefix <- substr(tarfile,56,95)
untar(tarfile, exdir = "/tmp")
print("-----------------------------------------------------------------")
print("-----------------------------------------------------------------")
print(prefix)
print("-----------------------------------------------------------------")
print("-----------------------------------------------------------------")

print("---------------- lee los tifs")
input_b1 <- paste0("/tmp/",prefix,"_SR_B1.TIF") # coastal blue
input_b2 <- paste0("/tmp/",prefix,"_SR_B2.TIF") # blue
input_b3 <- paste0("/tmp/",prefix,"_SR_B3.TIF") # green 
input_b4 <- paste0("/tmp/",prefix,"_SR_B4.TIF") # red
input_b5 <- paste0("/tmp/",prefix,"_SR_B5.TIF") # nir

input_qa <- paste0("/tmp/",prefix,"_QA_PIXEL.TIF") # QA
input_meta <- paste0("/tmp/",prefix,"_MTL.xml")

# Saca gain y offset del metadata
  result <- read_xml(input_meta)
  gainb1 <- as.numeric(xml_text(xml_find_all(result, xpath = "//LEVEL2_SURFACE_REFLECTANCE_PARAMETERS/REFLECTANCE_MULT_BAND_1")))
  gainb2 <- as.numeric(xml_text(xml_find_all(result, xpath = "//LEVEL2_SURFACE_REFLECTANCE_PARAMETERS/REFLECTANCE_MULT_BAND_2")))
  gainb3 <- as.numeric(xml_text(xml_find_all(result, xpath = "//LEVEL2_SURFACE_REFLECTANCE_PARAMETERS/REFLECTANCE_MULT_BAND_3")))
  gainb4 <- as.numeric(xml_text(xml_find_all(result, xpath = "//LEVEL2_SURFACE_REFLECTANCE_PARAMETERS/REFLECTANCE_MULT_BAND_4")))
  gainb5 <- as.numeric(xml_text(xml_find_all(result, xpath = "//LEVEL2_SURFACE_REFLECTANCE_PARAMETERS/REFLECTANCE_MULT_BAND_5")))
  
  offsetb1 <- as.numeric(xml_text(xml_find_all(result, xpath = "//LEVEL2_SURFACE_REFLECTANCE_PARAMETERS/REFLECTANCE_ADD_BAND_1")))
  offsetb2 <- as.numeric(xml_text(xml_find_all(result, xpath = "//LEVEL2_SURFACE_REFLECTANCE_PARAMETERS/REFLECTANCE_ADD_BAND_2")))
  offsetb3 <- as.numeric(xml_text(xml_find_all(result, xpath = "//LEVEL2_SURFACE_REFLECTANCE_PARAMETERS/REFLECTANCE_ADD_BAND_3")))
  offsetb4 <- as.numeric(xml_text(xml_find_all(result, xpath = "//LEVEL2_SURFACE_REFLECTANCE_PARAMETERS/REFLECTANCE_ADD_BAND_4")))
  offsetb5 <- as.numeric(xml_text(xml_find_all(result, xpath = "//LEVEL2_SURFACE_REFLECTANCE_PARAMETERS/REFLECTANCE_ADD_BAND_5")))
  
print("---------------  Aplica gain y offset -------------------")
b1 <- gainb1*rast(input_b1) + offsetb1
b2 <- gainb2*rast(input_b2) + offsetb2
b3 <- gainb3*rast(input_b3) + offsetb3
b4 <- gainb4*rast(input_b4) + offsetb4
b5 <- gainb5*rast(input_b5) + offsetb5

print("------------------- QA")
qa <- rast(input_qa)
levels(qa) <- NULL 
plot(qa)

# extract values from rast-object into array
val_qa <- unique(values(qa))[,1]

val_mask <- sapply(val_qa, water)  # apply function to each element and return vector 
mask_water <- subst(qa, val_qa,val_mask)
plot(mask_water) 

## máscara nube y sombra
val_mask <- sapply(val_qa, cloud_ext)
mask_cloud <- subst(qa, val_qa,val_mask)
plot(mask_cloud)

## Aplica las mascara de agua, nube y sombra de nube

b1[mask_cloud == 1 | mask_water ==0] <- NA
b2[mask_cloud == 1 | mask_water ==0] <- NA
b3[mask_cloud == 1 | mask_water ==0] <- NA
b4[mask_cloud == 1 | mask_water ==0] <- NA
b5[mask_cloud == 1 | mask_water ==0] <- NA

plot(b1)
plot(b2)
plot(b3)
plot(b4)

## Sitio Mahahual
sitio <- "mah"
pts_bati <- vect(paste0("mapas/pts_bati",sitio,".gpkg"))
  ext <- ext(pts_bati)
  subb1 <- crop(b1,ext)
  subb2 <- crop(b2,ext)
  subb3 <- crop(b3,ext)
  subb4 <- crop(b4,ext)
  subb5 <- crop(b5,ext)
  plot(subb1)
  name <- paste0("cortes_L/",prefix,"_",sitio,".tif")
  writeRaster(c(subb1,subb2,subb3,subb4,subb5), file=name, overwrite=TRUE, wopt= list(gdal=c("COMPRESS=NONE"), datatype='FLT4S'))

##############################################################################
################# FIN
##############################################################################

