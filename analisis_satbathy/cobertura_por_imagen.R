## Cobertura de sondeos por estrato de profundidad, para las 49 imágenes del
## catálogo (Landsat + Sentinel + SuperDove), como insumo para decidir un
## criterio de admisión a la comparación entre sensores/fechas.
##
## Motivo: fig9_full_final.csv evalúa cada imagen contra los sondeos que su
## propia máscara deja pasar, sin garantizar que las imágenes se comparen
## sobre la misma nube de puntos. 2025-05-07 y 2025-05-13 (SuperDove) quedaron
## con solo 130-313 sondeos, todos del transecto profundo (nube real sobre la
## costa), y aun así entraron en los promedios de la Tabla 4 igual que
## imágenes con >1000 puntos repartidos en los cinco transectos.
##
## No aplica ninguna combinación de preprocesamiento (glint/filtro): mide la
## cobertura tras máscara + descarte de blancos brillantes únicamente, que es
## lo que determina qué sondeos existen para cualquier combinación.
suppressMessages({ library(satbathy); library(terra) })
source("analisis_satbathy/config_imagenes.R")
source("analisis_satbathy/mask_helpers.R")
terraOptions(todisk = TRUE, memfrac = 0.4, progress = 0)

CKPT <- file.path(OUT, "cobertura_por_imagen.csv")
p <- puntos_sondeo()  # 1789 sondeos, profundidad sin corregir por marea

## catálogo: mismas imágenes que recalculo_fig9_4m.R
cat_rows <- list()
for (f in list.files("Landsat03a05", pattern = "\\.tif$", full.names = TRUE)) {
  b <- basename(f)
  cat_rows[[length(cat_rows) + 1]] <- list(path = f,
    sensor = if (grepl("^LC08", b)) "Landsat 8" else "Landsat 9",
    date = as.Date(sub(".*_0*(\\d{8})_.*", "\\1", b), "%Y%m%d"),
    blue = 1, green = 3, red = 2, nir = 4, mask = "qa", qa = 5)
}
for (f in list.files("Sentinel03a05", pattern = "^im.*\\.tif$", full.names = TRUE)) {
  d <- sub("^im", "", sub("\\.tif$", "", basename(f)))
  sclf <- file.path("Sentinel03a05", paste0("scl", d, ".tif"))
  if (!file.exists(sclf)) next
  cat_rows[[length(cat_rows) + 1]] <- list(path = f, sensor = "Sentinel-2",
    date = as.Date(d), blue = 2, green = 3, red = 4, nir = 8, mask = "scl", scl = sclf)
}
pl <- read.csv("analisis_satbathy/output/planet_procesadas.csv", stringsAsFactors = FALSE)
for (i in seq_len(nrow(pl))) {
  cat_rows[[length(cat_rows) + 1]] <- list(path = pl$path[i], sensor = "SuperDove",
    date = as.Date(pl$fecha[i]), blue = 1, green = 2, red = 3, nir = 4,
    mask = "udm2", udm = pl$udm[i])
}
message("imágenes en catálogo: ", length(cat_rows))

CORTES <- c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18)
etiquetas <- sprintf("%d-%d", CORTES[-length(CORTES)], CORTES[-1])
estrato <- cut(p$depthc, CORTES, labels = etiquetas, include.lowest = TRUE)

## checkpoint: si ya existe, se retoma y se saltan las imagenes ya hechas
res <- if (file.exists(CKPT)) read.csv(CKPT, stringsAsFactors = FALSE) else data.frame()
hecho <- if (nrow(res)) unique(paste(res$sensor, res$date)) else character(0)
if (length(hecho)) message(length(hecho), " imagen(es) ya en el checkpoint, se omiten")

for (cfg in cat_rows) {
  if (paste(cfg$sensor, cfg$date) %in% hecho) next
  ok <- tryCatch({
    img <- read_masked(cfg)
    img <- satbathy::mask_bright_targets(img, "nir")
    r <- log_ratio(img, "blue", "green", n = N_RATIO)
    v <- terra::extract(r, p, ID = FALSE)[, 1]
    !is.na(v)
  }, error = function(e) { message("  ERR ", cfg$sensor, " ", cfg$date, ": ",
                                   conditionMessage(e)); rep(FALSE, nrow(p)) })
  tab <- table(estrato[ok])
  fila <- as.data.frame(as.list(tab))
  names(fila) <- etiquetas
  fila$sensor <- cfg$sensor; fila$date <- as.character(cfg$date)
  fila$n_total <- sum(ok)
  res <- rbind(res, fila[, c("sensor", "date", "n_total", etiquetas)])
  message(cfg$sensor, " ", cfg$date, " -> ", sum(ok), " sondeos validos")

  write.csv(res, CKPT, row.names = FALSE)
  tmpFiles(current = TRUE, remove = TRUE)
  gc(full = TRUE)
}

cat("\n=== cobertura por estrato de profundidad (n de sondeos validos) ===\n")
print(res, row.names = FALSE)
