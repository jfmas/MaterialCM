## Selección de sondeos utilizables, rehecha con la cadena de proceso corregida.
##
## MOTIVO
## El conjunto training_points + test_points (1794 sondeos) se obtuvo en
## Scripts_Bati/checa_con_semar.R:150-185 descartando 164 de los 252 sondeos
## más cercanos a la costa (líneas 1 y 2, más el tramo somero de la 4) por caer
## en NA en 10 o más de las 25 imágenes evaluadas. Quedaron 1794 de 1940.
##
## Ese criterio se aplicó con la cadena antigua, que incluía el recorte por
## percentil del ratio (log_ratio(trim = c(0.01, 0.99))). Después comprobamos
## que ese recorte era la causa principal de las pérdidas en agua somera: los
## sondeos someros ocupan el extremo del gradiente de ratio y el percentil los
## cortaba. Con la cadena corregida, Sentinel-2 pasó de conservar el 0% del
## transecto 2 a conservar el 100%.
##
## Los 146 sondeos ausentes están todos entre 0.59 y 3.01 m, y 124 son del
## transecto 2. Como el análisis ya no aplica umbral mínimo de profundidad,
## arrastrar ese descarte sesgaría justo la franja que ahora sí se evalúa.
## Además es circular: se descartan sondeos porque las imágenes no los ven, y
## luego se usan para juzgar cuán bien las imágenes estiman la profundidad.
##
## QUÉ HACE
## Repite el conteo de NA punto a punto sobre las mismas imágenes Landsat y
## Sentinel-2 del análisis, pero con la cadena actual (máscara del sensor ->
## descarte de blancos brillantes por NIR -> filtro que respeta la máscara ->
## ratio sin recorte por percentil), y aplica el mismo criterio relativo.
suppressMessages({ library(satbathy); library(terra) })
source("analisis_satbathy/config_imagenes.R")
source("analisis_satbathy/mask_helpers.R")

MAX_FALLOS <- 0.40    # mismo criterio relativo del original (10 de 25 = 40%)

p <- vect("Yuc/pts_bati_completo_latlon.gpkg")
p$idp <- seq_len(nrow(p))
message("sondeos completos: ", nrow(p))

## misma partición costa/mar adentro que el criterio original
cerca <- p$linea %in% c(1, 2) | (p$linea == 4 & p$X > 27588)
message("cercanos a la costa: ", sum(cerca), " | resto: ", sum(!cerca))

## catálogo: las mismas imágenes Landsat y Sentinel-2 que usa el análisis
cat_rows <- list()
for (f in list.files("Landsat03a05", pattern = "\\.tif$", full.names = TRUE))
  cat_rows[[length(cat_rows) + 1]] <- list(path = f, blue = 1, green = 3,
    red = 2, nir = 4, mask = "qa", qa = 5)
for (f in list.files("Sentinel03a05", pattern = "^im.*\\.tif$", full.names = TRUE)) {
  d <- sub("^im", "", sub("\\.tif$", "", basename(f)))
  sclf <- file.path("Sentinel03a05", paste0("scl", d, ".tif"))
  if (file.exists(sclf))
    cat_rows[[length(cat_rows) + 1]] <- list(path = f, blue = 2, green = 3,
      red = 4, nir = 8, mask = "scl", scl = sclf)
}
message("imágenes para el conteo: ", length(cat_rows))

fallos <- integer(nrow(p)); usadas <- 0L
for (cfg in cat_rows) {
  r <- tryCatch(log_ratio(prepara(cfg), "blue", "green", n = N_RATIO),
                error = function(e) NULL)
  if (is.null(r)) { message("  omitida: ", basename(cfg$path)); next }
  v <- terra::extract(r, p, ID = FALSE)[, 1]
  fallos <- fallos + is.na(v)
  usadas <- usadas + 1L
  message("  ", basename(cfg$path), " -> NA en ", sum(is.na(v)), " sondeos")
}

p$fallos <- fallos
p$pct_fallos <- round(100 * fallos / usadas, 1)
conserva <- !cerca | (p$pct_fallos < 100 * MAX_FALLOS)

message("\n=== RESULTADO (", usadas, " imágenes) ===")
message("conservados: ", sum(conserva), " de ", nrow(p))
message("descartados: ", sum(!conserva),
        " (todos de la franja costera, por caer en NA en >= ",
        100 * MAX_FALLOS, "% de las imágenes)")

ant <- c(as.data.frame(vect("Yuc/training_points.gpkg"))$X,
         as.data.frame(vect("Yuc/test_points.gpkg"))$X)
rec <- p$X[conserva & !(p$X %in% ant)]
message("recuperados respecto al conjunto anterior: ", length(rec))
if (length(rec)) {
  d <- as.data.frame(p)[p$X %in% rec, ]
  message(sprintf("  profundidad: %.2f a %.2f m", min(d$depthc), max(d$depthc)))
  message("  por transecto: ",
          paste(names(table(d$linea)), table(d$linea), sep = "=", collapse = " "))
}

sel <- p[conserva, ]
writeVector(sel, "Yuc/puntos_seleccionados.gpkg", overwrite = TRUE)
write.csv(as.data.frame(p)[, c("X", "linea", "depthc", "fallos", "pct_fallos")],
          file.path(OUT, "seleccion_sondeos.csv"), row.names = FALSE)

message("\nGuardado: Yuc/puntos_seleccionados.gpkg (", nrow(sel), " sondeos)")
message("Detalle del conteo: ", file.path(OUT, "seleccion_sondeos.csv"))
