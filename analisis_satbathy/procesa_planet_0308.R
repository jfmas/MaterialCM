## Reprocesa la escena SuperDove del 8/3/2025 desde su producto SR de 8 bandas,
## para que sea homogenea con las demas fechas Planet (analisis_satbathy/
## procesa_planet_nuevas.R). Sustituye al producto L2R/Acolite usado antes.
## Salida con la misma convencion: planet_procesadas/20250308_sr.tif + _udm.tif
suppressMessages({ library(terra) })
setwd("/home/jf/pcloud_sync/Batimetria")
terraOptions(todisk = TRUE, memfrac = 0.25, progress = 0)

SALIDA <- "Yuc/imagenes/planet_procesadas"
dir.create(SALIDA, showWarnings = FALSE, recursive = TRUE)
BANDAS <- c(blue = 2, green = 4, red = 6, nir = 8); ESCALA <- 10000
aoi_ll <- vect(ext(-89.84004, -89.65617, 21.26287, 21.672), crs = "EPSG:4326")

dir <- "Yuc/imagenes/chelem_marzo_jfmas_psscene_analytic_8b_sr_udm2/PSScene"
fs  <- list.files(dir, pattern = "20250308.*_AnalyticMS_SR_8b_clip\\.tif$",
                  full.names = TRUE)
message("franjas SR del 8/3: ", length(fs))

ims <- list(); uds <- list()
for (f in fs) {
  img <- rast(f)[[BANDAS]] / ESCALA; names(img) <- names(BANDAS)
  ims[[length(ims) + 1]] <- img
  uf <- sub("_AnalyticMS_SR_8b_clip", "_udm2_clip", f)
  if (file.exists(uf)) {
    u <- rast(uf)[[1]]
    if (!isTRUE(try(compareGeom(u, img, stopOnError = FALSE), silent = TRUE)))
      u <- resample(u, img, method = "near")
    uds[[length(uds) + 1]] <- u
  }
}
im  <- if (length(ims) == 1) ims[[1]] else do.call(terra::merge, ims)
im  <- crop(im, project(aoi_ll, crs(im)))
udm <- if (length(uds) == 1) uds[[1]] else do.call(terra::merge, uds)
udm <- crop(udm, project(aoi_ll, crs(udm)))

writeRaster(im, file.path(SALIDA, "20250308_sr.tif"), overwrite = TRUE,
            gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "TILED=YES"))
writeRaster(udm, file.path(SALIDA, "20250308_udm.tif"), overwrite = TRUE,
            gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
message(sprintf("20250308: %d franja(s) -> %d x %d px  blue %.4f-%.4f",
                length(ims), nrow(im), ncol(im),
                min(values(im[["blue"]]), na.rm = TRUE),
                max(values(im[["blue"]]), na.rm = TRUE)))

## contraste rapido L2R (viejo) vs SR (nuevo) en los sondeos, sobre el ratio
suppressMessages(library(satbathy))
p <- vect("Yuc/puntos_seleccionados.gpkg")
r_sr <- log_ratio(im, "blue", "green", n = 10000)
old <- rast("Yuc/imagenes/cortes/PlanetScope_20250308_165229_70_24f6_L2R.tif")
names(old) <- c("blue", "green", "red", "nir")
r_l2r <- log_ratio(old, "blue", "green", n = 10000)
a <- terra::extract(r_sr, p, ID = FALSE)[, 1]
b <- terra::extract(r_l2r, p, ID = FALSE)[, 1]
k <- !is.na(a) & !is.na(b)
message(sprintf("ratio SR vs L2R en %d sondeos: correlacion %.4f, dif media %.4f",
                sum(k), cor(a[k], b[k]), mean(a[k] - b[k])))
