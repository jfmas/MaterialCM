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

caja <- st_bbox(c(xmin = -89.84004, xmax = -89.65617, ymin = 21.26287, ymax = 21.672), crs = st_crs(4326))
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
images <- SearchCatalog(aoi = aoi, from = "2025-03-01", to = "2025-05-31",
                        collection = "sentinel-2-l2a", with_geometry = TRUE, client = OAuthClient)


## Seleccion menos de 20% de nubes y zona 16
images
nrow(images)
plot(images)
buenas <- images$acquisitionDate[images$tileCloudCover < 20 & substr(images$sourceId, 40,41) == "16" & images$areaCoverage > 90 ]
imagesbuenas <- images[images$tileCloudCover < 20 & substr(images$sourceId, 40,41) == "16"& images$areaCoverage > 90,]

plot(imagesbuenas)
images$areaCoverage
length(buenas)
as.Date(buenas)
# bands: ["B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A"]
script_file_bands <- "/home/jf/pCloudDrive/trabajos/PresaSolis/Bands-SR_1-8A.js"
script_file_clouds <- "/home/jf/pCloudDrive/trabajos/PresaSolis/scriptCLD.js"
script_file_SCL <- "Scripts_Bati/evalscript_SCL.js"
#fechas con poca nube


### Primero la máscara de nubes
fecha <- buenas[1]
for (fecha in buenas[1:20]){ 
  print(paste("****************", fecha))
### Script filesalgunos en /home/jf/R/x86_64-pc-linux-gnu-library/4.3/CDSE/scripts
# script_file <- system.file("scripts", "RawBands.js", package = "CDSE")

fecha <-  as.Date(fecha) 
print(fecha)
filename <- paste0("Sentinel03a05/cl",fecha,".tif")
if(!file.exists(filename)){
  
  resp <- GetImage(aoi = aoi, time_range = fecha, script = script_file_clouds,
                collection = "sentinel-2-l2a",format = "image/tiff",
                mosaicking_order = "leastCC", resolution = 20, 
                client = OAuthClient, file= filename)
  
  print(httr2::resp_status(resp)) # Check the HTTP status code
  print(httr2::resp_headers(resp)) # Check all response headers, especially Content-Type
  print(httr2::resp_body_string(resp)) # See the raw response body
  
  
GetImage(aoi = aoi, time_range = fecha, script = script_file_clouds,
                       collection = "sentinel-2-l2a",format = "image/tiff",
                       mosaicking_order = "leastCC", resolution = 20, 
                       client = OAuthClient, file= filename)
}#if

filename <- paste0("Sentinel03a05/im",fecha,".tif")
if(!file.exists(filename)){ 
  GetImage(aoi = aoi, time_range = fecha, script = script_file_bands,
           collection = "sentinel-2-l2a",format = "image/tiff",
           mosaicking_order = "leastCC", resolution = 20, 
           client = OAuthClient, file= filename)
}#if

filename <- paste0("Sentinel03a05/scl",fecha,".tif")
if(!file.exists(filename)){ 
  GetImage(aoi = aoi, time_range = fecha, script = script_file_SCL,
           collection = "sentinel-2-l2a",format = "image/tiff",
           mosaicking_order = "leastCC", resolution = 20, 
           client = OAuthClient, file= filename)
}#if





}#loop fechas








for (fecha in buenas){ 
  fecha <- as.Date(fecha)
cloud <- rast(paste0("Sentinel03a05/cl",fecha,".tif"))
scl <- rast(paste0("Sentinel03a05/scl",fecha,".tif"))
plot(cloud)
im <- rast(paste0("Sentinel03a05/im",fecha,".tif"))
im[cloud > 50] <- NA
im[!scl == 6] <- NA
filename <- paste0("Sentinel03a05/S",fecha,".tif")
writeRaster(im, filename, overwrite=TRUE, wopt= list(gdal=c("COMPRESS=NONE"), datatype='FLT4S'))
}


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

