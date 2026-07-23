## BATIMETRIA 

library(terra)
library(waterquality)
#install.packages("TTR", dep=T)
library(TTR)

### Lee las imágenes Landsat
setwd("/home/jf/pCloudDrive/proyectos/Batimetria/")
ruta_im <- "/home/jf/pCloudDrive/proyectos/Batimetria/Yuc/serieSYuc"
lista_c <- list.files(path=ruta_im, pattern="cl20",full.names = T)
lista_c  # 118 imágenes

## ve sitios sin demasiadas nubes en la 
clouds <- rast(lista_c)
s <- (sum(clouds))
plot(s<1200)
writeRaster(s<1200, "Yuc/sintantanube.tif")
## se ponen puntos visualmente en qgis
pts <- vect("Yuc/puntos_turbidity.gpkg")
casitodo <- vect("Yuc/casitodo.gpkg")

plot(pts)


### Lee todos las imagnes, pone NA si hay nubes
lista_i <- list.files(path=ruta_im, pattern="im20",full.names = T)
lista_i
fechas <- substr(lista_i,59,68)
julianos <- julian(as.Date(fechas), origin = as.Date("2024-09-01"))

## ve sitios sin demasiadas nubes en la 
lista_out <- list()
i <- 0
for(file in lista_i){ 
  i <- i + 1
image <- rast(file)
image[clouds[[i]] >0] <- NA
# plot(image)
#indices <- wq_calc(image, alg =  "turbidity" ,sat = "sentinel2")
indices <- wq_calc(image, alg =  "TurbFrohn09GreenPlusRedBothOverBlue" ,sat = "sentinel2")
lista_out[[i]] <- indices
}
indices <- rast(lista_out)

tab <- extract(indices,pts)
tab <- extract(indices,casitodo)
unique(names(tab))


names_indices <- c("TurbBe16GreenPlusRedBothOverViolet" ,
 "TurbBe16RedOverViolet" ,"TurbBow06RedOverGreen",              
 "TurbChip09NIROverGreen","TurbDox02NIRoverRed",                
 "TurbFrohn09GreenPlusRedBothOverBlue", "TurbHarr92NIR",                      
 "TurbLath91RedOverBlue" ,  "TurbMoore80Red") 
indice <- "TurbBow06RedOverGreen"
indice <- "TurbFrohn09GreenPlusRedBothOverBlue"
for (indice in names_indices){ 
index <- names(tab) %in% indice
tab1 <-  tab[,index]

#for(p in 1:nrow(tab1)){ 
#plot(1:ncol(tab1),as.numeric(tab1[p,]),type="l" ,title=p)
#Sys.sleep(1)
#  }

#y <- colMeans(tab1, na.rm=T)
y <- apply(tab1,2,median, na.rm=T)
x <- as.Date(fechas)[!is.na(y)]
y <- y[!is.na(y)]
  plot(x,as.numeric(y),type="l" , main=indice)
  n <- 5
  ysma <- SMA(y, n = n)
  lines(x,ysma,col="red")
  
  ywma <- WMA(y, n = n, wts = 1:n)
  lines(x,ywma,col="blue")
  # Fecha de la turbidez mínima
  print(x[which.min(y)])
  Sys.sleep(5)
  }

# En water quality (w558 + w658)/w458)
#  Para TurbFrohn09GreenPlusRedBothOverBlue el "2025-03-09" es la fecha de menos turbidez
# TurbFrohn09GreenPlusRedBothOverBlue	turbidity	(559 – 658) / 459
# S y 4: (b3 - b4)/b2
# P 

?EMA

SMA(y, n = 10) # media móvil simple
ysma <- SMA(y, n = 3)
EMA(x, n = 10, wilder = FALSE, ratio = NULL, …) # exponencial

DEMA(x, n = 10, v = 1, wilder = FALSE, ratio = NULL) # exponencial doble

WMA(x, n = 10, wts = 1:n, …) #(linear weighting) Ponderada

#########  FIGURA
#tab1 <-  tab[,"TurbFrohn09GreenPlusRedBothOverBlue"]
tab1 <- tab
Sys.setlocale("LC_TIME", "es_ES.utf8")
y <- apply(tab1,2,median, na.rm=T)
x <- as.Date(fechas)[!is.na(y)]
y <- y[!is.na(y)]
png(paste0("Figuras/Fig_turbidez2024-25.png"), width = 14, height = 12, units='cm', res = 300)
plot(x,as.numeric(y),type="l",xlab="",ylab="Índice de turbidez")
n <- 5
ysma <- SMA(y, n = n)
lines(x,ysma,col="red",lwd=2)
dev.off()
#ywma <- WMA(y, n = n, wts = 1:n)
#lines(x,ywma,col="blue")

# Fecha de la turbidez mínima
print(x[which.min(y)])


######### CALCULA TURBIDEZ para los 6 imágenes usafos en el trabajo
######## Cálculo de los indices con paquete waterquality
indices <- wq_calc(image, alg =  "turbidity" ,sat = "sentinel2")
plot(indices)
i <- 0
file <- "Yuc/imagenes/cortes/LC08_L2SP_020045_20250308_20250324_02_T1.tif"

for (file in c("Yuc/imagenes/cortes/LC08_L2SP_020045_20250308_20250324_02_T1.tif","Yuc/imagenes/cortes/LC09_L2SP_020045_20250503_20250504_02_T1.tif",
"Yuc/imagenes/cortes/T16QBJ_A041822_20250309T162600.tif", "Yuc/imagenes/cortes/T16QBJ_A003442_20250503T163420.tif",
"Yuc/imagenes/cortes/PlanetScope_20250308_165229_70_24f6_L2R.tif","Yuc/imagenes/cortes/PlanetScope_2522_2025_05_04_17_01_18_L2R.tif")) {
  i <- i+1
    imagen <- rast(file)
if (i <= 4){turb <- (imagen[[3]]+imagen[[4]])/imagen[[2]]}
  
if (i > 4) {turb <- (imagen[[2]]+imagen[[3]])/imagen[[1]]}  ## para dove b g r nir
  plot(turb)
  tab <-extract(turb,casitodo)
  #tab <-extract(turb,pts)
  print(file)
  print(paste("mean", round(mean(tab[,2], na.rm=T),2)))
  print(paste("median", round(median(tab[,2], na.rm=T),2)))
}
# 
# ## con casi todo
# [1] "Yuc/imagenes/cortes/LC08_L2SP_020045_20250308_20250324_02_T1.tif"
# [1] "mean 0.75"
# [1] "median 0.59"
# [1] "Yuc/imagenes/cortes/LC09_L2SP_020045_20250503_20250504_02_T1.tif"
# [1] "mean 1.38"
# [1] "median 1.21"
# [1] "Yuc/imagenes/cortes/T16QBJ_A041822_20250309T162600.tif"
# [1] "mean 1.5"
# [1] "median 1.47"
# [1] "Yuc/imagenes/cortes/T16QBJ_A003442_20250503T163420.tif"
# [1] "mean 1.69"
# [1] "median 1.68"
# [1] "Yuc/imagenes/cortes/PlanetScope_20250308_165229_70_24f6_L2R.tif"
# [1] "mean " 1.09
# [1] "median 1.02"
# [1] "Yuc/imagenes/cortes/PlanetScope_2522_2025_05_04_17_01_18_L2R.tif"
# [1] "mean 1.85"
# [1] "median 1.85"
# 
# # Con pts
# [1] "Yuc/imagenes/cortes/LC08_L2SP_020045_20250308_20250324_02_T1.tif"
# [1] "mean 0.78"
# [1] "median 0.62"
# [1] "Yuc/imagenes/cortes/LC09_L2SP_020045_20250503_20250504_02_T1.tif"
# [1] "mean 1.46"
# [1] "median 1.17"
# [1] "Yuc/imagenes/cortes/T16QBJ_A041822_20250309T162600.tif"
# [1] "mean 1.52"
# [1] "median 1.49"
# [1] "Yuc/imagenes/cortes/T16QBJ_A003442_20250503T163420.tif"
# [1] "mean 1.71"
# [1] "median 1.72"
# [1] "Yuc/imagenes/cortes/PlanetScope_20250308_165229_70_24f6_L2R.tif"
# [1] "mean 1.07"
# [1] "median 0.98"
# [1] "Yuc/imagenes/cortes/PlanetScope_2522_2025_05_04_17_01_18_L2R.tif"
# [1] "mean 1.84"
# [1] "median 1.84"