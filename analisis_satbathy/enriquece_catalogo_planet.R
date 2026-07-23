## Añade calidad y cobertura a planet_procesadas.csv, para poder filtrar por
## esos criterios sin reprocesar las imágenes.
##
## Fuentes:
##   planet_por_fecha.csv    (Scripts_Bati/busca_planet.R)  hora, % utilizable,
##                           % nube, cobertura conjunta de la zona de sondeos
##   planet_disponibles.csv  calidad por escena, agregada por fecha
setwd("/home/jf/pcloud_sync/Batimetria")

pp <- read.csv("analisis_satbathy/output/planet_procesadas.csv", stringsAsFactors = FALSE)
pf <- read.csv("analisis_satbathy/output/planet_por_fecha.csv", stringsAsFactors = FALSE)
pd <- read.csv("analisis_satbathy/output/planet_disponibles.csv", stringsAsFactors = FALSE)

## idempotente: si ya se enriqueció antes (p. ej. al añadir una fila nueva a
## mano, como el 8/3 reprocesado aparte), se quitan las columnas antes de
## recalcularlas para no duplicarlas con sufijos .x/.y
pp <- pp[, setdiff(names(pp),
                   c("hora_utc", "pct_utilizable", "pct_nube",
                     "cobertura_union", "calidad"))]

cal <- aggregate(calidad ~ fecha, pd, function(x) paste(sort(unique(x)), collapse = "+"))

pp <- merge(pp, pf[, c("fecha", "hora_utc", "pct_utilizable", "pct_nube",
                       "cobertura_union")], by = "fecha", all.x = TRUE)
pp <- merge(pp, cal, by = "fecha", all.x = TRUE)
pp <- pp[order(pp$fecha), ]

write.csv(pp, "analisis_satbathy/output/planet_procesadas.csv", row.names = FALSE)
cat("catálogo enriquecido:", nrow(pp), "fechas\n")
print(pp[, c("fecha", "n_franjas", "hora_utc", "pct_utilizable", "pct_nube",
             "cobertura_union", "calidad")], row.names = FALSE)
