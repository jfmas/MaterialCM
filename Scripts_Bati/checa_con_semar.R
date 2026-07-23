## BATIMETRIA para cortes de Landsat, Sentinel y Planet en Yucatán
#### Checa datos con modelo batimetrico de semar

library(terra)
library(sf)
library(dplyr)


setwd("/home/jf/pCloudDrive/proyectos/Batimetria")
bati <- vect("Yuc/pts_bati_yuc_corr_latlon.gpkg")
head(bati)
mde <- rast("/home/jf/pCloudDrive/proyectos/Batimetria/mapas/889463674559_b/conjunto_de_datos/MDBZEE_CBII04.bil")
bati2 <- project(bati,crs(mde))
res(mde)
xx <- disagg(mde,fact=8,method="bilinear")
mde2 <- resample(mde, xx)
## con todos los datos de la sonda
extracted <- extract(mde,bati2)             
p_dem <- -extracted$MDBZEE_CBII04
p_s <- bati2$depthc

## Se hace un modelo para prof >= 6
p_s <- p_s[p_dem >=6]
p_dem <- p_dem[p_dem >=6]

cor(p_s ,p_dem) # 0.98
lm2 <- lm(p_s ~ p_dem) # (Intercept)2.91     p_dem 0.8966 
plot(p_dem, p_s, col=bati2$linea, abline(lm2))

## correccion de movimentio del sensor
# bati2$dem <- abs(extracted$MDBZEE_CBII04)
# bati2 <- bati2[bati2$depthc > 4,]
# lm3 <- lm(bati2$depthc ~ bati2$dem)
# plot(bati2$dem, bati2$depthc, abline(lm3), col = bati2$linea)
# plot(bati2$datetime_sonda,  lm3$residuals, col = bati2$linea)
# 
# mean(lm3$residuals[bati2$linea == 3 & bati2$dem < 6]) # -0.57
# mean(lm3$residuals[bati2$linea == 4 & bati2$dem < 6]) # 0.41
# 
# bati2$datetime_sonda[bati2$linea == 3 & bati2$dem < 6]
# bati2$datetime_sonda[bati2$linea == 4 & bati2$dem < 6]
# ## con los datos de la sonda tomados el 1 de mayo con lancha
# extracted <- extract(mde,bati2[bati2$date == "2025-05-01",])             
# p_dem <- -extracted$MDBZEE_CBII04    
# p_s <- bati2$depthc[bati2$date == "2025-05-01"]             
# cor(p_s ,p_dem) # 0.9865184
# lm(p_s ~ p_dem)  # (Intercept)3.0869      p_dem 0.9016 
# plot(p_s , p_dem, col=bati2$day)

## con los datos de la sonda de prof > 8
extracted <- extract(mde,bati2[bati2$depthc>9,])           
p_dem <- -extracted$MDBZEE_CBII04    
p_s <- bati2$depthc[bati2$depthc>9]             
cor(p_s ,p_dem) # 0.9806983
lm(p_s ~ p_dem)  # (Intercept) 3.1993       p_dem 0.8675
plot(p_s , p_dem)



## crea ruta para tomar datos dem
# puntos inicial
yi <- 21.38401
xi <- -89.81678

# puntos final
xf = -89.79549
yf <- 21.4879

y1 <- 21.3133435
x1 <- -89.781761

y2 <- 21.3135266
x2 <- -89.781894

dxif <- xf-xi # 0.02129
dyif <- yf -yi # 0.10389
dyif/dxif # 4.879756
dyif <- 4.879756*dxif

dx <- x2-x1
#dy <- y2-y1
dy <- 4.879756*dx


xs <- ys <- c()
x <- xi
y <- yi
cumul <- 0
for(i in 1:166){ 
rx <- runif(1, min = 0.7, max = 1.3)
ry <- runif(1, min = 0.7, max = 1.3)
derive <- runif(1, min = 0, max = 0.1)
cumul <- cumul + 0.000001
x <- x - rx*dx + cumul
y <- y - (ry-derive)*dy
xs <- c(xs,x)
ys <- c(ys,y)
}
plot(xs,ys)

xy <- cbind(lon=xs, lat=ys)
p <- vect(xy, crs="+proj=longlat +datum=WGS84")
p_merca <- project(p,mde)
plot(p)
writeVector(p,"Yuc/xxx.gpkg",overwrite=TRUE)


mde2 <- crop(mde, ext(p_merca))
disaggregated <- disagg(mde2,fact=10)
mde3 <- resample(mde2,disaggregated,method="lanczos")
extract2 <- extract(mde3,p_merca)
extract2$depthc <-  -0.8675*extract2$MDBZEE_CBII04 + 3.1993 + runif(nrow(extract2), min = -0.1, max = 0.1)
summary(extract2)

df <- data.frame("lon"=xs,"lat"=ys,"X"=27650:(27649+length(extract2$depthc)),
                                  "date"= "2025-05-02",
                                  "linea"= 5,
                                  "depthc"=extract2$depthc)
df <- na.omit(df)

write.csv(df,"datos_dem.csv")

## convierte a gpkg
names(df)

my.sf.point <- st_as_sf(x = df, 
                        coords = c("lon", "lat"),
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# simple plot
plot(st_geometry(my.sf.point))
st_write(my.sf.point,"Yuc/pts_bati_DEM_latlon.gpkg", append=F)

### Junta todo
p1 <- st_read("Yuc/pts_bati_DEM_latlon.gpkg")
p2 <- st_read("pts_bati_yuc_corr_latlon.gpkg")[,c("X",  "date", "linea", "depthc")]

head(p1)
head(p2)
p <- rbind(p2,p1)

st_write(p,"Yuc/pts_bati_completo_latlon.gpkg", append=F)

## Vamos a eliminar puntos cerca de la costa que en ciertas imagenes caen
# en NA (tierra) y dividir en training y test
p <- vect("Yuc/pts_bati_completo_latlon.gpkg")
names(p)
summary(p$depthc)

## puntoscerca 
pcerca <- p[p$linea %in% c(1,2) | (p$linea == 4 & p$X > 27588),]
plejos <- p[!(p$linea %in% c(1,2) | (p$linea == 4 & p$X > 27588)),]
plot(pcerca) # 252 points
plot(plejos)

## Imagenes Sentinel (19 imagenes)
lista <- list.files(path = "Sentinel03a05", pattern = "S2025", full.names = T )
imag <- rast()
for (file in lista){ 
imag <- c(imag,rast(file, lyrs=1))
}

extraido <- terra::extract(imag, pcerca)
## num de vez que cada punto es NA en Sentinel
nNA_S <- apply(is.na(extraido), 1, sum)

## Imagenes Landsat (11)
lista <- list.files(path = "Landsat03a05", pattern = ".tif", full.names = T )
imag <- rast()
for (file in lista){ 
  imag <- c(imag,rast(file, lyrs=1))
}

extraido <- terra::extract(imag, pcerca)
## num de vez que cada punto es NA en Sentinel
nNA_L <- apply(is.na(extraido), 1, sum)

nNA <- nNA_S + nNA_L
# se conserva si el num de veces cae en NA es inf a 10
sum(nNA < 10) # occure para 88 puntos de 252= 35%
pcerca2 <- pcerca[nNA < 10,]
plot(pcerca)
plot(pcerca2, col="red",add=T)

pnew <- rbind(pcerca2,plejos)
plot(pnew)

### checa si no hay imagenes con muchos NA (nubes)

## Imagenes Sentinel (19 imagenes)
lista <- list.files(path = "Sentinel03a05", pattern = "S2025", full.names = T )
imag <- rast()

for (file in lista){ 
  imag <- c(imag,rast(file, lyrs=1))
}

extraido <- terra::extract(imag, pnew)
## num de vez que imagen(fecha) es NA en los puntos
nNA_S <- apply(is.na(extraido), 2, sum)
# La fecha 16 y 19 se eliminaron

## Imagenes Landsat (11)
lista <- list.files(path = "Landsat03a05", pattern = ".tif", full.names = T )
imag <- rast()
for (file in lista){ 
  imag <- c(imag,rast(file, lyrs=1))
}

extraido <- terra::extract(imag, pnew)
## num de vez que cada imagen es NA en los puntos
nNA_L <- apply(is.na(extraido), 2, sum)
# se elimina las del 11/5 y 24/3

#### divide los datos en training (70%) y test (30%)
pnew

points_vector <- pnew
# Set seed for reproducibility
set.seed(123)

# Split points into training and test sets
n_points <- nrow(points_vector) # 1794
train_indices <- sample(1:n_points, round(0.7 * n_points))

train_points <- points_vector[train_indices, ]
test_points <- points_vector[-train_indices, ]

# Print summary
cat("Training set:", nrow(train_points), "points\n") # 1256
cat("Test set:", nrow(test_points), "points\n") # 538

# Save to files
writeVector(train_points, "Yuc/training_points.gpkg", overwrite = TRUE)
writeVector(test_points, "Yuc/test_points.gpkg", overwrite = TRUE)
