## Complemento artículo Ciencias Marinas
## Manuscrito Mapeo batimétrico en aguas costeras con imágenes de satélite: 
## Un estudio de caso en el Caribe Mexicano

## Apply Glint Correction
##  J. D. Hedley Corresponding author, A. R. Harborne & P. J. Mumby (2005) 
## Technical note: Simple and robust removal of sun glint for mapping 
## shallow‐water benthos, International Journal of Remote Sensing, 26:10, 
## 2107-2112, DOI: 10.1080/01431160500034086

## QA https://pages.cms.hu-berlin.de/EOL/gcg_eo/02_data_quality.html
# Deriving Bathymetry from Multispectral Remote Sensing Data, William J. Hernandez * and Roy A. Armstrong # 3.2. Bathymetry Retrieval

## ------------------- Function Glint correction --------------

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
return(bigc)} 
# fin function