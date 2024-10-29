## Complemento artículo Ciencias Marinas
## Manuscrito Mapeo batimétrico en aguas costeras con imágenes de satélite: 
## Un estudio de caso en el Caribe Mexicano

# Determina la profundidad max a la cual "alcanza" la señal
# reduce la profundidad max de manera iterativa
# y  calcula la correlación R2 del modelo lineal 
# Crea una tabla con el R2 para diferentes profundidades max para
# todas las combinaciones de sensor, opcion glint y opcion filtrado

library(dplyr)
library(stats)

### 
setwd("/home/jf/pCloudDrive/proyectos/Batimetria")

## Functions

sitio <- "mah"

## Crea matriz para recibir los resultados
results <-matrix(nrow= 3*3*2*2,ncol=30)
colnames(results)<- c("sitio","tipo","GlintCorrection","Filter",paste0("p",seq(10,35,1)))
opcion_outlier <- "dejaoutliers" #"eliminaoutliers" # 

i <- 0
  for (tipo in c("L","S","P")){ 
    print(paste0("-------------", tipo))
    for (GlintCorrection in c(TRUE,FALSE)){
      for (Filter in c(TRUE,FALSE)){

        if (GlintCorrection == T){glintopcion <- "Glint"}else{glintopcion <- "NoGlint"}
        if (Filter == T){filteropcion <- "Filter"}else{filteropcion <- "NoFilter"}
        print(paste0("-------------", glintopcion))
        print(paste0("-------------", filteropcion))
file <-paste0("Tablas/",sitio,"-",tipo,"-",glintopcion,"-", filteropcion,".csv")       
tab <- read.csv(file)
tab <- na.omit(tab)

if(opcion_outlier == "eliminaoutliers" & nrow(tab) > 9){
## eliminate outliers tab (no se aplicó)
quartiles <- quantile(tab$ratio, probs=c(.25, .75), na.rm = T)
IQR <- IQR(tab$ratio, na.rm = T)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
tab <- tab[tab$ratio > Lower & tab$ratio < Upper, ]
}

### Aplica diferentes umbrales de prof max y calcula R2
profmax <- round(max(min(tab$depth), -35))
umbrales <- rev(seq(profmax,-10, 1))
summary(tab)
i <- i+1
for (umbral in umbrales){
  tab2 <- tab[tab$depth != 0 & tab$depth > umbral & !is.na(tab$ratio),]
  if (nrow(tab2) > 9){
  cor(tab2$depth, tab2$ratio)
  ## Regresión lineal
  lm <- lm(depth ~ ratio, data=tab2)
  plot(tab2$ratio,tab2$depth)
   resumen <- summary(lm)
   r2adj <- round(resumen$adj.r.squared,digits=2)
   print(paste0("prof=",umbral,"; R2adj=",r2adj))
  } else{r2adj<- NA}
  
  results[i,"sitio"] <- sitio
  results[i,"GlintCorrection"] <- GlintCorrection
  results[i,"tipo"] <- tipo
  results[i,"Filter"] <- Filter
  results[i,paste0("p",abs(umbral))] <- r2adj 
}


} # loop filter
 } # loop glint
  } # loop tipo


print(results)
write.csv(results, paste0("Tablas/correlaciones_profmax",opcion_outlier,".csv"))

