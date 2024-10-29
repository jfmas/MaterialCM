## BATIMETRIA para cortes de Landsat, Sentinel y Planet en Mahahual
# Deriving Bathymetry from Multispectral Remote Sensing Data, William J. Hernandez * and Roy A. Armstrong # 3.2. Bathymetry Retrieval
# Determina la profundidad max 
# reduce la profundidad max de manera iterativa
# y  calcula la correlación R2 del modelo lineal 

library(terra)
library(sf)
library(dplyr)
library(stats)
library(deming)

### 
setwd("/home/jf/pCloudDrive/proyectos/Batimetria")

## Functions

sitio <- "mah"
sitio <- "xcan"
sitios <- c("mah","xcan","xcas")


tipo <- "L"; sitio <- "mah"; GlintCorrection <- T; Filter <- T

results <-matrix(nrow= 3*3*2*2,ncol=30)
colnames(results)<- c("sitio","tipo","GlintCorrection","Filter",paste0("p",seq(10,35,1)))
opcion_outlier <- "dejaoutliers" #"eliminaoutliers" # 

pts_bati <- vect(paste0("mapas/pts_bati",sitio,".gpkg"))
maskdeep <- vect(paste0("mapas/deep16_",sitio,".gpkg"))


i <- 0
# for (sitio in sitios){
  print(paste0("-------------", sitio))

  #for (tipo in c("L","S")){ 
  for (tipo in c("L","S","P")){ 
    print(paste0("-------------", tipo))
    for (GlintCorrection in c(TRUE,FALSE)){
      for (Filter in c(TRUE,FALSE)){

        if (GlintCorrection == T){glintopcion <- "Glint"}else{glintopcion <- "NoGlint"}
        if (Filter == T){filteropcion <- "Filter"}else{filteropcion <- "NoFilter"}
        print(paste0("-------------", glintopcion))
        print(paste0("-------------", filteropcion))
file <-paste0("Tablas/",sitio,"-",tipo,"-",glintopcion,"-", filteropcion,".csv")       
file.exists(file)
tab <- read.csv(file)
tab <- na.omit(tab)

if(opcion_outlier == "eliminaoutliers" & nrow(tab) > 9){
## eliminate outliers tab
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
  #afit1 <- theilsen(depth ~ ratio, symmetric=TRUE, data= tab2)
  #summary(afit1)
  plot(tab2$ratio,tab2$depth)
  # Step 2: Calculate residuals
  # tab2$residuals <- resid(lm)
  
  # Step 3: Define a threshold for outliers (e.g., 3 standard deviations)
  # threshold <- 3 * sd(data$residuals)
  
  # Identify outliers
  # data <- data %>%
  #   mutate(outlier = abs(residuals) > threshold)
  
  # Step 4: Remove outliers
  # data_clean <- filter(data, outlier == FALSE)
  
  # Plot the cleaned data
  # ggplot(data_clean, aes(x = x, y = y)) +
  #   geom_point() +
  #   ggtitle("Data After Removing Outliers")
  
  # Optionally, refit the model on the cleaned data
  # model_clean <- lm(y ~ x, data = data_clean)
  # summary(model_clean)
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
#} # loop sitio

print(results)
write.csv(results, paste0("Tablas/correlaciones_profmax",opcion_outlier,".csv"))

## Regresión lineal
lm <- lm(depth ~ ratio, data=tab)
resumen <- summary(lm)
#print(resumen)


#tab <- tab[tab$linea == 10,] # una sola linea
## eliminate outliers tab
quartiles <- quantile(tab$ratio, probs=c(.25, .75), na.rm = T)
IQR <- IQR(tab$ratio, na.rm = T)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
tab <- tab[tab$ratio > Lower & tab$ratio < Upper, ]
tab <- na.omit(tab)




# gam
gam = gam(depth ~ s(ratio), data=tab)
resumengam <- summary(gam)
aicgam <- resumengam$aic
prof <- predict(gam, data.frame(ratio=newx)) #,type="response")
lines(newx, prof,col="green")

## glm
glm <- glm(depth ~ ratio, data=tab)
resumenglm <- summary(glm)
aicglm <- resumenglm$aic
# RF
rf.fit <- randomForest(depth ~ ratio, data=tab,type="regression")
# newx <- seq(min(tab$ratio), max(tab$ratio), length.out=200)
# pred_rf <- predict(rf.fit,data=data.frame(ratio=newx),type="response",proximity=F)

RFspR2 <- mean(rf.fit$rsq)


#Sys.sleep(3)
plot(tab$ratio,tab$depth,xlab="ratio",ylab="depth")

png(paste0("Figuras/",tipo,sitio,".png"), width = 14, height = 12, units='cm', res = 300)
plot(tab$ratio,tab$depth,xlab="ratio",ylab="depth", main=paste(sitio, tipo, "R2=",round(resumen$adj.r.squared,digits=2)))
# una línea de regresión 
abline (lm, col="red")
# linea gam
lines(newx, prof,col="green")
#definir rango de valores x
# predicts + interval
# newx <- seq(min(tab$ratio), max(tab$ratio), length.out=300)
# preds <- predict(lm, newdata = data.frame(ratio=newx), interval = 'confidence')
# # intervals
#lines(newx, as.numeric(pred_rf), lty = 'dashed', col = 'red')
# lines(newx, preds[ ,2], lty = 'dashed', col = 'red')
dev.off()
#Sys.sleep(3)

######## rellena resultados sitio","tipo","Glint R2","aicgam","aicglm" RFspR2
i <- i+1
resultados[i,1] <- sitio
resultados[i,2] <- tipo
resultados[i,3] <- GlintCorrection
resultados[i,4] <- Filter
resultados[i,5] <-  round(resumen$adj.r.squared,digits=2)
resultados[i,6] <- round(aicgam)
resultados[i,7] <- round(aicglm)
resultados[i,8] <- round(RFspR2,digits=2)

#### Evalua el modelo con los datos test
## lm
head(tabtest)
tabtest$profest <- resumen$coefficients[1,1]+resumen$coefficients[2,1]*tabtest$ratio
tabtest$profestGAM <- predict(gam, data.frame(ratio=tabtest$ratio)) 
tabtest$dif <- tabtest$depth - tabtest$profest
tabtest$dif2 <- (tabtest$depth - tabtest$profest)*(tabtest$depth - tabtest$profest)
tabtest$difGAM <- tabtest$depth - tabtest$profestGAM
tabtest$dif2GAM <- (tabtest$depth - tabtest$profestGAM)*(tabtest$depth - tabtest$profestGAM)
meanerror <- mean(tabtest$dif, na.rm=T)
RMSerror <- mean(tabtest$dif2, na.rm=T)
meanerror0a10 <- mean(tabtest$dif[tabtest$depth < -10], na.rm=T)
RMSerror0a10 <- mean(tabtest$dif2[tabtest$depth < -10], na.rm=T)
# GAM
meanerrorGAM <- mean(tabtest$difGAM, na.rm=T)
RMSerrorGAM <- mean(tabtest$dif2GAM, na.rm=T)
meanerror0a10GAM <- mean(tabtest$difGAM[tabtest$depth < -10], na.rm=T)
RMSerror0a10GAM <- mean(tabtest$dif2GAM[tabtest$depth < -10], na.rm=T)

resultados[i,9] <- meanerror
resultados[i,10] <- RMSerror
resultados[i,11] <- meanerror0a10
resultados[i,12] <- RMSerror0a10
# GAM
resultados[i,13] <- meanerrorGAM
resultados[i,14] <- RMSerrorGAM
resultados[i,15] <- meanerror0a10GAM
resultados[i,16] <- RMSerror0a10GAM

} else {print("Hay menos de 10 observaciones ---------------")} # if
head(resultados)
write.csv(resultados,"resultados_fit.csv")

# sitio tipo Glint   Filter R2     aicgam aicglm RFspR2 meanerror            RMSE                meanerror0a10 RMSE0a10
# [1,] "mah" "L"  "FALSE" "TRUE" "0.58" "861"  "885"  "0.6"  "-0.388128779373095" "0.878566669780551" "NaN"         "NaN"   
# [2,] "mah" "S"  "FALSE" "TRUE" "0.56" "856"  "909"  "0.58" "-0.441380710933574" "0.949444291439624" "NaN"         "NaN"   
# [3,] "mah" "P"  "FALSE" "TRUE" "0.6"  "853"  "886"  "0.59" "-0.184612581204404" "0.889021696017441" "NaN"         "NaN"   

