## Define el conjunto de sondeos usado para TODAS las imágenes:
## puntos con profundidad (corregida por marea) >= PROF_MIN que además
## sobreviven al enmascarado de agua en las seis imágenes.
##
## Motivo: las máscaras no tratan igual la franja somera. La QA de Landsat
## conserva casi todo el agua somera mientras que la SCL de Sentinel-2 y el
## NDWI de Planet la eliminan en buena parte. Como el error en 0-5 m es de
## 2-3 m, evaluar cada sensor sobre los puntos que su propia máscara deja
## pasar penaliza al que conserva más agua somera y produce una comparación
## sesgada. Fijando un único conjunto la comparación queda limpia.
suppressMessages({ library(satbathy); library(terra) })
source("analisis_satbathy/config_imagenes.R")
source("analisis_satbathy/mask_helpers.R")

pts <- puntos_sondeo()
val <- list()
for (nm in names(IMAGENES)) {
  cfg <- IMAGENES[[nm]]
  r <- log_ratio(prepara(cfg), "blue", "green", n = N_RATIO)
  v <- terra::extract(r, pts, ID = FALSE)[, 1]
  prof <- as.numeric(pts$depthc) + (NIVEL[[nm]] - REF_NM)
  val[[nm]] <- pts$idp[!is.na(v) & prof >= PROF_MIN]
  message(sprintf("%-4s válidos con prof >= %g m: %d", nm, PROF_MIN, length(val[[nm]])))
}

com <- Reduce(intersect, val)
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
saveRDS(com, file.path(OUT, "puntos_comunes.rds"))

p <- as.numeric(pts$depthc)[pts$idp %in% com]
message("\nconjunto común: ", length(com), " puntos")
message("transectos: ", paste(names(table(pts$linea[pts$idp %in% com])), collapse = ", "))
message(sprintf("profundidad: %.2f a %.2f m", min(p), max(p)))
write.csv(data.frame(idp = com), file.path(OUT, "puntos_comunes.csv"), row.names = FALSE)
