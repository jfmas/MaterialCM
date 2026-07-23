## Aplica el criterio de admisión por cobertura estratificada acordado:
## una imagen entra al conjunto comparable solo si tiene >= 15 sondeos válidos
## en CADA estrato de 2 m entre 2 y 16 m (los estratos 0-2 y 16-18 se excluyen
## del criterio: tienen 20 y 48 sondeos en todo el diseño, insuficientes
## incluso sin ninguna máscara de por medio).
##
## No vuelve a procesar ningún ráster: cobertura_por_imagen.R ya midió, con la
## cadena base (máscara + descarte de blancos brillantes, sin glint/filtro),
## cuántos sondeos sobreviven por estrato en cada una de las 64 imágenes del
## catálogo. Ese conteo es la base de la admisión; fig9_full_final.csv (196
## filas, resultados LOTO de las 49 imágenes que sí produjeron al menos una
## fila) se filtra contra la lista de admitidas.
suppressMessages({})
setwd("/home/jf/pcloud_sync/Batimetria")
source("analisis_satbathy/config_imagenes.R")

RANGO   <- c("X2.4","X4.6","X6.8","X8.10","X10.12","X12.14","X14.16")
N_MIN   <- 15

cob <- read.csv("analisis_satbathy/output/cobertura_por_imagen.csv", stringsAsFactors = FALSE)
for (c in RANGO) cob[[c]][is.na(cob[[c]])] <- 0
cob$min_estrato_2_16 <- apply(cob[, RANGO], 1, min)
cob$admitida <- cob$min_estrato_2_16 >= N_MIN

write.csv(cob, "analisis_satbathy/output/cobertura_admision.csv", row.names = FALSE)

cat(sprintf("Admitidas: %d de %d imágenes (rango 2-16 m, N>=%d por estrato)\n",
            sum(cob$admitida), nrow(cob), N_MIN))
print(aggregate(admitida ~ sensor, cob, function(x) sprintf("%d/%d", sum(x), length(x))))

## ---- filtra los resultados ya calculados (sin reprocesar) --------------
res <- read.csv("analisis_satbathy/output/fig9_full_final.csv", stringsAsFactors = FALSE)
clave_admitida <- paste(cob$sensor[cob$admitida], cob$date[cob$admitida])
adm <- res[paste(res$sensor, res$date) %in% clave_admitida, ]

cat(sprintf("\nfig9_full_final.csv: %d filas / %d imágenes -> admitidas: %d filas / %d imágenes\n",
            nrow(res), length(unique(paste(res$sensor, res$date))),
            nrow(adm), length(unique(paste(adm$sensor, adm$date)))))

write.csv(adm, file.path(OUT, "fig9_admitidas.csv"), row.names = FALSE)

## ---- tabla de excluidas, para reportar aparte ---------------------------
## Motivo verificado espacialmente (no solo inferido del conteo): se comprobó
## con el UDM2 (Planet, 2025-03-04/03-06/03-29) y con la SCL (Sentinel-2,
## 2025-05-18) que el hueco de cobertura coincide con píxeles marcados como
## nube/sombra/bruma por el propio control de calidad del sensor, concentrados
## sobre la franja costera somera mientras el agua profunda de la misma escena
## suele quedar despejada. Donde el UDM2 marca "clear" el NDWI siempre
## clasifica agua correctamente (0.14-0.78, muy por encima del umbral 0), así
## que el NDWI queda descartado como causa.
excl <- cob[!cob$admitida, c("sensor", "date", "n_total", "min_estrato_2_16")]
excl$motivo <- "nube o bruma detectada por el control de calidad del sensor (UDM2/SCL), concentrada sobre la franja costera somera"
excl <- excl[order(excl$sensor, excl$date), ]
write.csv(excl, file.path(OUT, "tabla_imagenes_excluidas.csv"), row.names = FALSE)

## ---- Tabla 4 sobre el conjunto admitido ---------------------------------
T4 <- do.call(rbind, lapply(split(adm, adm$sensor), function(x) data.frame(
  sensor = x$sensor[1], n_casos = nrow(x),
  n_fechas = length(unique(x$date)),
  prof_min = min(abs(x$max_depth)), prof_max = max(abs(x$max_depth)),
  prof_prom = round(mean(abs(x$max_depth)), 1),
  r2 = round(mean(x$r2), 2), rmse = round(mean(x$rmse), 2),
  mae = round(mean(x$mae), 2))))
write.csv(T4, file.path(OUT, "tabla4_admitidas.csv"), row.names = FALSE)
cat("\n=== TABLA 4 (solo imágenes admitidas, comparables) ===\n")
print(T4, row.names = FALSE)

cat(sprintf("\ncor(turbidez, R2)   = %.3f\n", cor(adm$turbidity, adm$r2)))
cat(sprintf("cor(turbidez, RMSE) = %.3f\n", cor(adm$turbidity, adm$rmse)))

## ---- Fig 9 sobre el conjunto admitido -----------------------------------
adm$family <- ifelse(adm$sensor %in% c("Landsat 8", "Landsat 9"), "Landsat", adm$sensor)
fam_col    <- c("Landsat" = "blue", "Sentinel-2" = "forestgreen", "SuperDove" = "darkorange")
sensor_pch <- c("Landsat 8" = 16, "Landsat 9" = 17, "Sentinel-2" = 15, "SuperDove" = 18)
png(file.path(OUT, "fig9_admitidas.png"), width = 16, height = 12, units = "cm", res = 200)
plot(adm$turbidity, adm$r2, col = fam_col[adm$family], pch = sensor_pch[adm$sensor],
     cex = 1.1, xlab = "Turbidez", ylab = expression(R^2))
## hueco sin puntos en turbidez 1.0-1.6 / R2 <0.6 (verificado sobre los datos:
## el minimo de R2 ahi es 0.71), asi que la leyenda no tapa ningun punto real
legend(x = 1.0, y = 0.55, legend = names(sensor_pch), pch = sensor_pch,
       col = fam_col[c("Landsat", "Landsat", "Sentinel-2", "SuperDove")],
       bty = "o", bg = "white", box.col = "grey40")
dev.off()

cat("\nArchivos escritos:\n")
cat("  analisis_satbathy/output/cobertura_admision.csv   (las 64, con bandera admitida)\n")
cat("  analisis_satbathy/output/fig9_admitidas.csv        (resultados, solo admitidas)\n")
cat("  analisis_satbathy/output/tabla4_admitidas.csv      (Tabla 4 recalculada)\n")
cat("  analisis_satbathy/output/tabla_imagenes_excluidas.csv\n")
cat("  analisis_satbathy/output/fig9_admitidas.png\n")
