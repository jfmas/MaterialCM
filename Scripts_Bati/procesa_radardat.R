## BATIMETRIA
##  PREPROCESSING: datos del mareagrafo de radar del Instituto de Geofísica de la UNAM


library(sf)
library(dplyr)

library(fuzzyjoin)
library(lubridate)


# UTC es + 6 en comparación con hora mx, la sonda registra hora UTC
### Working directory and input files
setwd("/home/jf/pCloudDrive/proyectos/Batimetria/Yuc")
## Tablas de la sonda y del mareografo
# Diferencia entre el cero de regla y el nivel medio del mar NMM de la estación
# (se resta a los datos del mareografo para obtener el nivel NMM)
# La relación entre el nivel medio del mar de progreso de marzo a mayo con respecto al cero 
# de regla es: NM de progreso es 5.4932 m
#   
  
difnivel = 5.4932 # 5.4932   #4.1 # datos de la estación progreso 5.78 y 5.95 es el nivel mas bajo y mas alto durante los dias de toma
datos_mareografo0105 <- "Progreso_Radar.dat"
datos_mareografoMayo <- "resolicituddatosmareografo/Progreso_RadarMayo.dat"
datos_mareografoMarzo <- "resolicituddatosmareografo/Progreso_RadarMarzo.dat"

datos_sonda <- "pts_bati_yuc.csv"

## Lectura datos de la sonde echomap
tab_sonda <- read.csv(datos_sonda)
tab_sonda <- na.omit(tab_sonda)
tab_sonda <- tab_sonda %>%
  mutate(datetime = ymd_hms(paste(date, time)))
## Elimina unos puntos en tierra tomadas el 2025:04:28 antes de las 13:00:00
tab_sonda <- tab_sonda[!(tab_sonda$date == "2025-04-28" & tab_sonda$X < 12430),]
tab_sonda <- tab_sonda[!(tab_sonda$date == "2025-04-28" & tab_sonda$X >= 12485 & tab_sonda$X <= 12575),]
## lee las tablas, pone campo datetime


## mareógrafo
tab_mareografo <- read.table(datos_mareografo0105, sep = "" , header = F ,
                   na.strings ="", stringsAsFactors= F)
tab_mareografomayo <- read.table(datos_mareografoMayo, sep = "" , header = F ,
                             na.strings ="", stringsAsFactors= F)
tab_mareografomarzo <- read.table(datos_mareografoMarzo, sep = "" , header = F ,
                             na.strings ="", stringsAsFactors= F)

tab_mareografo <- rbind(tab_mareografo, tab_mareografomayo, tab_mareografomarzo)

names(tab_mareografo) <- c("year","month","day","hour","min","sec","nivel")

tab_mareografo$datetime <- make_datetime(tab_mareografo$year, tab_mareografo$month, tab_mareografo$day,
              tab_mareografo$hour, tab_mareografo$min, tab_mareografo$sec)

head(tab_mareografo)

## Cual es la altura min, max y media durante la toma de los datos de 28 abril
head(tab_sonda)
bat <- tab_sonda[tab_sonda$date=="2025-04-28",]
nrow(bat)
inicio <- min(bat$datetime) # "2025-04-28 13:01:00 UTC"
final <- max(bat$datetime) # "2025-04-28 14:07:59 UTC"
duracion <- final - inicio # Time difference of 1.11 hours
mare <- tab_mareografo[tab_mareografo$datetime > inicio & tab_mareografo$datetime < final,]
mare <- na.omit(mare)
plot(mare$datetime,mare$nivel)
## niveles mínimo, amx, promedio
min(mare$nivel) # 5.57
max(mare$nivel) # 5.60
mean(mare$nivel) # 5.58

## Cual es la altura min, max y media durante la toma de los datos de 30 abril
head(tab_sonda)
bat <- tab_sonda[tab_sonda$date=="2025-04-30",]
nrow(bat)
inicio <- min(bat$datetime) # "2025-04-30 13:44:20 UTC"
final <- max(bat$datetime) # "2025-04-30 14:25:20 UTC"
duracion <- final - inicio # Time difference  41 mins
mare <- tab_mareografo[tab_mareografo$datetime > inicio & tab_mareografo$datetime < final,]
mare <- na.omit(mare)
plot(mare$datetime,mare$nivel)
## niveles mínimo, amx, promedio
min(mare$nivel) # 5.803
max(mare$nivel) # 5.853
mean(mare$nivel) # 5.83

## Cual es la altura min, max y media durante la toma de los datos de 1o de mayo
head(tab_sonda)
bat <- tab_sonda[tab_sonda$date=="2025-05-01",]
nrow(bat)
inicio <- min(bat$datetime) # "2025-05-01 13:46:30 UTC"
final <- max(bat$datetime) # "2025-05-01 17:23:33 UTC"
duracion <- final - inicio # Time difference of 3.6175 hours
mare <- tab_mareografo[tab_mareografo$datetime > inicio & tab_mareografo$datetime < final,]
mare <- na.omit(mare)
plot(mare$datetime,mare$nivel)
## niveles mínimo, amx, promedio
min(mare$nivel) # 5.83
max(mare$nivel) # 5.95
mean(mare$nivel) # 5.89



# Para esta estación hay que restar difnil (aqui 5.49 m) para obtener el nivel minimo (restando oscillaciones durante tomas) 
tab_mareografo$NMM <- tab_mareografo$nivel - difnivel
summary(tab_mareografo)
head(tab_mareografo)

## corrección de los datos tomados con la sonda
# corrección de la profundidad medida tomando en cuenta la posición
# de la sonda debajo de la embarcacion 1o mayo 30 cm (lancha), otros dias 15 cm (tabla SUP) 
tab_sonda$depthc <- tab_sonda$depth + 0.15
tab_sonda$depthc[tab_sonda$date == "2025-05-01"] <- tab_sonda$depth[tab_sonda$date == "2025-05-01"] + 0.30
nrow(tab_sonda)
head(tab_sonda)

# Create datetime columns
#tab_mareografo <- tab_mareografo %>%
#  mutate(datetime = as.POSIXct(paste(year, month, day, hour, min,sec)))


summary(tab_mareografo)
tab_sonda <- tab_sonda %>%
  mutate(datetime = as.POSIXct(paste(date, time)))
summary(tab_sonda)


# Perform fuzzy join and keep only the closest match for each left table row
tab_corr <- difference_left_join(
  tab_sonda,
  tab_mareografo,
  by = "datetime",
  max_dist = Inf,  # Consider setting a reasonable limit like 3600 (1 hour)
  distance_col = "time_diff"
) %>%
  # Group by the left table's datetime to ensure one match per left row
  group_by(datetime.x) %>%
  # Keep only the row with the smallest time difference for each group
  filter(time_diff == min(time_diff)) %>%
  # If there are ties (multiple rows with same min time_diff), take the first one
  slice(1) %>%
  ungroup() %>%
  # Optional: rename columns for clarity
  rename(datetime_sonda = datetime.x,
         datetime_mareografo = datetime.y)

# The result should now have the same number of rows as tab_sonda
nrow(tab_sonda) == nrow(tab_corr)

head(tab_corr)
summary(tab_corr) # verificar que time_diff no sea excesivo 

### Durante la toma de datos el nivel del mar fue siempre por encima del NMM, entre 1.420 y 1.722
### Para normalizar vamos a restar a depthc NMM y sumar el min de MNN como si todos
# los datos fueran tomadas al momento donde el nivel del mar eran 1.42 por encima de NMM
# para el mapa final de batimetria tendriamos que restar 1.42 m para tener algo al NMM
#minNMM <- min(tab_corr$NMM)
tab_corr$depthc <- tab_corr$depthc - tab_corr$NMM   #+ minNMM
summary(tab_corr)

## Pone el número de linea
tab_corr$linea <- 1
tab_corr$linea[tab_corr$date=="2025-04-30"] <- 2
tab_corr$linea[tab_corr$date=="2025-05-01" & tab_corr$X < 26949  ] <- 3
tab_corr$linea[tab_corr$date=="2025-05-01" & tab_corr$X >= 26949  ] <- 4
## salva tabla
write.csv(tab_corr,"pts_bati_yuc_corr.csv")

## convierte a gpkg
names(tab_corr)
my.sf.point <- st_as_sf(x = tab_corr, 
                        coords = c("lon", "lat"),
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# simple plot
plot(my.sf.point, col=my.sf.point$linea)
st_write(my.sf.point,"pts_bati_yuc_corr_latlon.gpkg", append=F)


##### Nivel en fechas y  hora de las diferentes imágenes usadas
## hora local es UTC -6, o asea las imagenes se tomaron alrededor de las 10:20 a 10:50 am
# L8 8/03 <SCENE_CENTER_TIME>16:21:59.1495590Z</SCENE_CENTER_TIME>
# L9 3/05  <SCENE_CENTER_TIME>16:21:50.4997309Z</SCENE_CENTER_TIME>
# S 9/03 SENSING_TIME metadataLevel="Standard">2025-03-09T16:37:46.069259Z</SENSING_TIME>
# S 3/5 <SENSING_TIME metadataLevel="Standard">2025-05-03T16:38:04.632414Z</SENSING_TIME>
# P 4/05  <eop:acquisitionDate>2025-05-04T17:01:21+00:00</eop:acquisitionDate>
# P 8/03 <eop:acquisitionDate>2025-03-08T16:52:29+00:00</eop:acquisitionDate>


#### Data la más cercana
tabsat <-  data.frame("Imagen" = c("L8","L9","S","S","P","P"),
"fechas" = c("2025-03-08","2025-05-03","2025-03-09","2025-05-03","2025-03-08","2025-05-04"),
"hora" = c("16:21:59","16:21:51","16:37:46","16:38:05","16:52:29","17:01:21"))
tabsat <- tabsat %>%
  mutate(datetime = ymd_hms(paste(fechas, hora)))
dim(tabsat)
names(tabsat)
head(tabsat)

## mareógrafo

tab_mareografo <- read.table("Progreso_Radar.dat", sep = "" , header = F ,
                                  na.strings ="", stringsAsFactors= F)

tab_mareografoMarzo <- read.table("resolicituddatosmareografo/Progreso_RadarMarzo.dat", sep = "" , header = F ,
                             na.strings ="", stringsAsFactors= F)
tab_mareografoMayo <- read.table("resolicituddatosmareografo/Progreso_RadarMayo.dat", sep = "" , header = F ,
                             na.strings ="", stringsAsFactors= F)

tab_mareografoMM <- rbind(tab_mareografo,tab_mareografoMarzo,tab_mareografoMayo)

names(tab_mareografoMM) <- c("year","month","day","hour","min","sec","nivel")

tab_mareografoMM$datetime <- make_datetime(tab_mareografoMM$year, tab_mareografoMM$month, tab_mareografoMM$day,
                                         tab_mareografoMM$hour, tab_mareografoMM$min, tab_mareografoMM$sec)

write.csv(tab_mareografoMM, "xxtab_mareografoMM.csv")

# Perform fuzzy join and keep only the closest match for each left table row
tabsat2 <- difference_left_join(
  tabsat,
  tab_mareografoMM,
  by = "datetime",
  max_dist = Inf,  # Consider setting a reasonable limit like 3600 (1 hour)
  distance_col = "time_diff"
) %>%
  # Group by the left table's datetime to ensure one match per left row
  group_by(datetime.x) %>%
  # Keep only the row with the smallest time difference for each group
  filter(time_diff == min(time_diff)) %>%
  # If there are ties (multiple rows with same min time_diff), take the first one
  slice(1) %>%
  ungroup() %>%
  # Optional: rename columns for clarity
  rename(datetime_sat = datetime.x,
         datetime_mareografo = datetime.y)

# verificar que time_diff no sea excesivo
tabsat2$time_diff/60 # hasta 49 min
head(tabsat2[,c(1:4,11:13)])

head(tabsat2)
names(tabsat2)
##### Como hay diferencias de tiempo importante entre hora de toma del 
# mareografo y de las imágenes (en particular Landsat)

# Hace un plot para cada fecha n horas antes y despues
i <-0
for (time_sat in tabsat2$datetime_sat){
  i <- i+1
t_image <- as.POSIXct(time_sat, format = "%Y-%m-%d %H:%M:%S")
t1 <- t_image - hours(10)
t2 <- t_image + hours(10)
subtab <- tab_mareografoMM[tab_mareografoMM$datetime > t1 & tab_mareografoMM$datetime < t2,]
plot(subtab$datetime, subtab$nivel, main=paste0(tabsat2$Imagen[i], as.Date(t_image)))
abline(v=t_image, col="red")
}

## para las fechas sin datos cercanos la interpolacion deberia funcionar

# Function to perform linear interpolation
interpolate_nivel <- function(target_time, df) {
  # Ensure data is sorted by datetime
  df <- df %>% arrange(datetime)
  
  # Convert all times to numeric for easier calculation
  target_numeric <- as.numeric(target_time)
  df_numeric <- as.numeric(df$datetime)
  
  # Find the index of the last time that is <= target time
  before_indices <- which(df_numeric <= target_numeric)
  
  # Check if target time is before all data points
  if (length(before_indices) == 0) {
    warning("Target time is before all data points. Cannot interpolate.")
    return(NA)
  }
  
  before_idx <- max(before_indices)
  
  # Find the index of the first time that is >= target time
  after_indices <- which(df_numeric >= target_numeric)
  
  # Check if target time is after all data points
  if (length(after_indices) == 0) {
    warning("Target time is after all data points. Cannot interpolate.")
    return(NA)
  }
  
  after_idx <- min(after_indices)
  
  # If target time is exactly at a data point, return that value
  if (before_idx == after_idx) {
    return(df$nivel[before_idx])
  }
  
  # Get the two data points for interpolation
  time_before <- df_numeric[before_idx]
  time_after <- df_numeric[after_idx]
  nivel_before <- df$nivel[before_idx]
  nivel_after <- df$nivel[after_idx]
  
  # Perform linear interpolation
  fraction <- (target_numeric - time_before) / (time_after - time_before)
  interpolated_value <- nivel_before + fraction * (nivel_after - nivel_before)
  
  return(interpolated_value)
}


times <- tab_mareografoMM$datetime
nivel <- tab_mareografoMM$nivel
df <- data.frame(datetime = times, nivel = nivel)
head(df)

target_time <- "2025-03-08 16:21:59 UTC"
# Target time
interpol <- c()
for (target_time in tabsat2$datetime_sat){ 
target_time <- as.POSIXct(target_time, tz = "UTC")
print(paste("------------- target_time =", target_time))

# Apply interpolation
result <- interpolate_nivel(target_time, df)

# Print detailed results
print("INTERPOLATION DETAILS:")
print(paste("Target time:", target_time))

df_numeric <- as.numeric(df$datetime)
before_idx <- max(which(df_numeric <= as.numeric(target_time)))
after_idx <- min(which(df_numeric >= as.numeric(target_time)))

print(paste("Closest time BEFORE:", df$datetime[before_idx], "- Nivel:", df$nivel[before_idx]))
print(paste("Closest time AFTER:", df$datetime[after_idx], "- Nivel:", df$nivel[after_idx]))
print(paste("Time difference:", round(difftime(df$datetime[after_idx], df$datetime[before_idx], units = "mins"), 1), "minutes"))
print(paste("Target position in gap:", round(difftime(target_time, df$datetime[before_idx], units = "mins"), 1), "minutes after start of gap"))
print(paste("Interpolated nivel value:", round(result, 4)))
interpol <- c(interpol,round(result, 4))
}

tabsat2$nivelinterpol <- interpol
head(tabsat2)
tabsat2 <- tabsat2[,c("Imagen","fechas","hora","nivelinterpol")]



write.csv(as.data.frame(tabsat2), "../Tablas/NivelesImagenesYuc.csv")
