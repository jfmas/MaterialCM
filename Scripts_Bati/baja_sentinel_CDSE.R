########### Proyecto Batimetria YUC #####################
# https://zivankaraman.github.io/CDSE/articles/BeforeYouStart.html
# https://zivankaraman.github.io/CDSE/articles/CDSE.html#introduction
# Probando CDSE
# Client ID: usar variable de entorno CDSE_ID
# secret: usar variable de entorno CDSE_SECRET
# remotes::install_github("zivankaraman/CDSE")
library(CDSE)
library(sf)
library(terra)

setwd("/home/jf/pCloudDrive/proyectos/Batimetria/")
## No parece aceptar bbox
caja <- st_bbox(c(xmin = -89.8400371379694, xmax = -89.6561684859538, ymin = 21.2628691249548, ymax = 21.3953271341371), crs = st_crs(4326))
aoi <- st_sf(st_as_sfc(caja))
#aoi <- st_read("Yuc/cuadro_Chelem.gpkg")
ext(aoi)
# https://dataspace.copernicus.eu/
# https://shapps.dataspace.copernicus.eu/dashboard/#/account/settings
id <- Sys.getenv("CDSE_ID")        # export CDSE_ID="..." en el entorno
secret <- Sys.getenv("CDSE_SECRET")  # export CDSE_SECRET="..." en el entorno

token <- GetOAuthToken(id = id, secret = secret)
OAuthClient <- GetOAuthClient(id = id, secret = secret)


colecs <- GetCollections()
head(colecs)

#images <- SearchCatalog(aoi = aoi, from = "2023-01-01", to = "2024-12-28",
#                        collection = "sentinel-2-l2a", with_geometry = TRUE, client = OAuthClient)
images <- SearchCatalog(aoi = aoi, from = "2024-09-01", to = "2024-12-31",
                        collection = "sentinel-2-l2a", with_geometry = TRUE, client = OAuthClient)



images
nrow(images)
buenas <- images$acquisitionDate[images$tileCloudCover < 70]
imagesbuenas <- images[images$tileCloudCover < 70,]
length(buenas)
as.Date(buenas)
script_file_bands <- "/home/jf/pCloudDrive/trabajos/PresaSolis/Bands-SR_1-8A.js"
script_file_clouds <- "/home/jf/pCloudDrive/trabajos/PresaSolis/scriptCLD.js"
#fechas con poca nube


### Primero la máscara de nubes
fecha <- buenas[1]
for (fecha in buenas){ 
### Script filesalgunos en /home/jf/R/x86_64-pc-linux-gnu-library/4.3/CDSE/scripts
# script_file <- system.file("scripts", "RawBands.js", package = "CDSE")

fecha <-  as.Date(fecha) 
print(fecha)
filename <- paste0("Yuc/serieSYuc/cl",fecha,".tif")
if(!file.exists(filename)){ 
GetImage(aoi = aoi, time_range = fecha, script = script_file_clouds,
                       collection = "sentinel-2-l2a",format = "image/tiff",
                       mosaicking_order = "leastCC", resolution = 20, 
                       client = OAuthClient, file= filename)
}#if

filename <- paste0("Yuc/serieSYuc/im",fecha,".tif")
if(!file.exists(filename)){ 
  GetImage(aoi = aoi, time_range = fecha, script = script_file_bands,
           collection = "sentinel-2-l2a",format = "image/tiff",
           mosaicking_order = "leastCC", resolution = 20, 
           client = OAuthClient, file= filename)
}#if
}#loop fechas

### Mide la cantidad de nube en cima del area
Presa <- st_read("mapas/ContornoSolis_lcc.gpkg")
x <- rast("imagenes/sample.tif")
Presa <- st_transform(Presa,crs(x))
plot(Presa)
zcloud <- c()
for (fecha in buenas){ 
  fecha <- as.Date(fecha)
cloud <- rast(paste0("imagenes/cl",fecha,".tif"))
plot(cloud)
plot(Presa, add=T)
zcloud <- c(zcloud, as.numeric(zonal(cloud,vect(Presa), fun = mean)))
}
imagesbuenas$zcloud <- zcloud
head(imagesbuenas)
nrow(imagesbuenas)

hist(zcloud) #(es el promedio sobre la presa del % de presencia de nube)

imagessuperbuenas <- imagesbuenas[imagesbuenas$zcloud < 40,]
nrow(imagessuperbuenas)
## selecciona las superbuenas imagen menos nublosa de 40
head(imagessuperbuenas)

fecha <- imagessuperbuenas$acquisitionDate[1]
for (fecha in imagessuperbuenas$acquisitionDate){ 
  ### Script filesalgunos en /home/jf/R/x86_64-pc-linux-gnu-library/4.3/CDSE/scripts
  # script_file <- system.file("scripts", "RawBands.js", package = "CDSE")
  
  fecha <-  as.Date(fecha)
  filename <- paste0("imagenes/im",fecha,".tif")
  if(!file.exists(filename)){
  GetImage(aoi = aoi, time_range = fecha, script = script_file_bands,
                  collection = "sentinel-2-l2a",format = "image/tiff",
                  mosaicking_order = "leastCC", resolution = 20, 
                  client = OAuthClient, file= paste0("imagenes/im",fecha,".tif"))
  
}}

### script  del API
https://documentation.dataspace.copernicus.eu/APIs/SentinelHub/Process/Examples/S2L2A.html

//Other S2L2A specific data (Aerosol Optical Thickness, Scene Classification, S>

function setup() {
  return {
    input: [{bands:["AOT", "SCL", "SNW", "CLD", "sunAzimuthAngles", "sunZenithA>
    output: {
      id: "default",
      bands: 8,
      sampleType: SampleType.FLOAT32
    }
    }
  }

function evaluatePixel(sample) {
  return [ sample.AOT, sample.SCL, sample.SNW, sample.CLD, sample.sunAzimuthA>
}

