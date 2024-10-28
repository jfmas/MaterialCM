## BATIMETRIA
## Apply Glint Correction
##  J. D. Hedley Corresponding author, A. R. Harborne & P. J. Mumby (2005) 
## Technical note: Simple and robust removal of sun glint for mapping 
## shallow‐water benthos, International Journal of Remote Sensing, 26:10, 
## 2107-2112, DOI: 10.1080/01431160500034086

## QA https://pages.cms.hu-berlin.de/EOL/gcg_eo/02_data_quality.html


# Deriving Bathymetry from Multispectral Remote Sensing Data, William J. Hernandez * and Roy A. Armstrong # 3.2. Bathymetry Retrieval

library(terra)
library(sf)
library(dplyr)

### Lee las imágenes Landsat
setwd("/home/jf/pCloudDrive/proyectos/Batimetria")


## Glint ###############################################################################################
print("------------------- Glint correction")

### Function correct glint, input banda a procesar, bande NIR (enmascarada para agua)
correct_glint <- function(bi,nir,nsample,maskdeep){
## Identifica Rmin NIR valor mas bajos de la bande NIR en el agua
minNIR <- quantile(values(mask(nir,maskdeep)),na.rm=T,0.01)
NIRgc <- nir - minNIR
# busqueda relación entre cada banda y NIR
sample_pts <- spatSample(mask(nir,maskdeep), nsample, method="random", replace=FALSE, na.rm=T, as.points = T)
y <- extract(bi, sample_pts)[,2]
x <- extract(nir, sample_pts)[,2]
# plot(x,y)
lm <- lm(y ~ x)
# slope of regression model
ki <- lm$coefficients["x"]
bigc = bi - ki*NIRgc
return(bigc)} # fin function

b1gc <- correct_glint(b1,b4,1000)
#plot(b1gc)
b2gc <- correct_glint(b2,b4,1000)
#plot(b2gc)
b3gc <- correct_glint(b3,b4,1000)
#plot(b3gc)
#writeRaster(c(b1gc,b2gc,b3gc), file=paste0("b123gc",prefix,".tif"), overwrite=TRUE, wopt= list(gdal=c("COMPRESS=NONE"), datatype='FLT4S'))




