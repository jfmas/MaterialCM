## Recálculo del conjunto completo (todas las imágenes Landsat, Sentinel-2 y
## SuperDove de marzo a mayo de 2025) x 4 combinaciones de preprocesamiento,
## con validación leave-one-transect-out.
##
## Diferencias respecto a make_fig9_full.R:
##   - el error sale de validación LOTO, no de una partición aleatoria
##   - incluye las 34 fechas SuperDove procesadas por procesa_planet_nuevas.R
##
## Marea: se interpola el nivel del mar a la hora de paso de cada imagen a
## partir del mareógrafo de radar de Progreso
## (Yuc/resolicituddatosmareografo/Progreso_Radar_01a07_2025.dat, resolución de
## un minuto, enero-julio 2025). Para Landsat/Sentinel se usa una hora típica
## por sensor; para SuperDove, la hora real de cada fecha (varía 16:14-17:04
## UTC), tomada de planet_procesadas.csv.
##
## CHECKPOINT Y MEMORIA: la primera corrida (2026-07-22 madrugada) murió por
## OOM tras ~21 fechas SuperDove (el proceso R llegó a 26 GB de RSS) y, como
## los resultados solo se escribían al final del bucle, se perdió TODO lo ya
## calculado -incluidas Landsat y Sentinel, que llevaban horas hechas-. Ahora:
##   - se escribe el progreso a disco tras CADA imagen, así un corte solo
##     pierde la imagen en curso
##   - al reiniciar, se saltan las imágenes (sensor, fecha) ya presentes en el
##     checkpoint, así que un restart no rehace lo ya calculado
##   - terra escribe a disco en vez de acumular en RAM, y se fuerza el
##     recolector de basura tras cada imagen para acotar el crecimiento
suppressMessages({ library(satbathy); library(terra) })
source("analisis_satbathy/config_imagenes.R")
source("analisis_satbathy/mask_helpers.R")
set.seed(42)
terraOptions(todisk = TRUE, memfrac = 0.4, progress = 0)

CKPT <- file.path(OUT, "fig9_full_final.csv")

pts  <- puntos_sondeo()                     # prof = depthc (sin marea)
pts  <- pts[pts$prof >= PROF_MIN, ]
deep <- vect("Yuc/deep_yuc2.gpkg")
message("sondeos con prof >= ", PROF_MIN, " m: ", nrow(pts))

## ---- nivel del mar por imagen ------------------------------------------
marea <- read.table("Yuc/resolicituddatosmareografo/Progreso_Radar_01a07_2025.dat",
                    col.names = c("y","m","d","H","M","S","nivel"))
marea$dt <- as.POSIXct(sprintf("%04d-%02d-%02d %02d:%02d:%02d",
                               marea$y, marea$m, marea$d, marea$H, marea$M, marea$S),
                       tz = "UTC")
marea <- marea[order(marea$dt), ]
hora_paso <- function(s) if (grepl("Landsat", s)) "16:21:55" else "16:37:56"
nivel_en <- function(sensor, fecha, hora = NULL) {
  h <- if (!is.null(hora) && !is.na(hora)) hora else hora_paso(sensor)
  t <- as.POSIXct(paste(fecha, h), tz = "UTC")
  stats::approx(as.numeric(marea$dt), marea$nivel, xout = as.numeric(t))$y
}

## ---- catálogo de imágenes ----------------------------------------------
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
    date = as.Date(d), blue = 2, green = 3, red = 4, nir = 8,
    mask = "scl", scl = sclf)
}
message("imágenes Landsat+Sentinel en catálogo: ", length(cat_rows))

pl <- read.csv("analisis_satbathy/output/planet_procesadas.csv", stringsAsFactors = FALSE)
for (i in seq_len(nrow(pl))) {
  cat_rows[[length(cat_rows) + 1]] <- list(path = pl$path[i], sensor = "SuperDove",
    date = as.Date(pl$fecha[i]), blue = 1, green = 2, red = 3, nir = 4,
    mask = "udm2", udm = pl$udm[i], hora = pl$hora_utc[i])
}
message("imágenes SuperDove añadidas: ", nrow(pl))
message("imágenes en catálogo (total): ", length(cat_rows))

## ---- checkpoint: cargar lo ya calculado y saltarlo ---------------------
res <- if (file.exists(CKPT)) read.csv(CKPT, stringsAsFactors = FALSE) else data.frame()
hecho <- if (nrow(res)) unique(paste(res$sensor, res$date)) else character(0)
if (length(hecho))
  message(length(hecho), " imagen(es) ya en el checkpoint, se omiten: ",
          paste(utils::head(hecho, 3), collapse = "; "), if (length(hecho) > 3) " ..." else "")

combos <- expand.grid(glint = c(FALSE, TRUE), filter = c(FALSE, TRUE))

for (cfg in cat_rows) {
  clave <- paste(cfg$sensor, cfg$date)
  if (clave %in% hecho) next

  turb_i <- NA_real_
  niv   <- nivel_en(cfg$sensor, as.character(cfg$date), cfg$hora)
  depth_img <- -(pts$prof + (niv - REF_NM))
  for (k in seq_len(nrow(combos))) {
    tryCatch({
      img <- prepara(cfg, w = if (combos$filter[k]) 3 else 1,
                     glint = combos$glint[k], deep = deep)
      r <- log_ratio(img, "blue", "green", n = N_RATIO)

      v <- terra::extract(r, pts, ID = FALSE)[, 1]
      d <- stats::na.omit(data.frame(ratio = v, depth = depth_img, linea = pts$linea))
      if (nrow(d) < 60) stop("pocos puntos")

      md <- optimal_max_depth(d); best <- md$max_depth[which.max(md$r2)]
      fit <- fit_bathymetry(d, max_depth = best)
      L <- loto(d)
      if (!length(L$obs)) stop("sin pliegues validos")

      if (is.na(turb_i)) {
        timg <- read_masked(cfg)
        turb_i <- stats::median(terra::extract(
          turbidity_index(timg, "blue", "green", "red"), pts, ID = FALSE)[, 1],
          na.rm = TRUE)
        rm(timg)
      }
      res <- rbind(res, data.frame(sensor = cfg$sensor, date = as.character(cfg$date),
        glint = combos$glint[k], filter = combos$filter[k],
        n = length(L$obs), nivel = round(niv, 4), max_depth = best, r2 = fit$r.squared,
        rmse = rmse_(L$obs, L$prd), mae = mae_(L$obs, L$prd), turbidity = turb_i))
      rm(img, r, v, d, fit, L)
    }, error = function(e) message("   ERR ", cfg$sensor, " ", cfg$date, ": ",
                                   conditionMessage(e)))
  }
  message(cfg$sensor, " ", cfg$date, " ok")

  ## checkpoint tras cada imagen: si el proceso muere despues, esto se conserva
  write.csv(res, CKPT, row.names = FALSE)
  ## libera la memoria de los rasters temporales de esta imagen antes de
  ## seguir con la siguiente (la fuga que provoco el OOM anterior)
  tmpFiles(current = TRUE, remove = TRUE)
  gc(full = TRUE)
}

cat("\ncasos:", nrow(res), " imágenes:", length(unique(paste(res$sensor, res$date))), "\n")
cat("cor(turbidez, R2)   =", round(cor(res$turbidity, res$r2), 3), "\n")
cat("cor(turbidez, RMSE) =", round(cor(res$turbidity, res$rmse), 3), "\n")

## ---- Tabla 4: resumen por sensor ---------------------------------------
T4 <- do.call(rbind, lapply(split(res, res$sensor), function(x) data.frame(
  sensor = x$sensor[1], n_casos = nrow(x),
  prof_min = min(abs(x$max_depth)), prof_max = max(abs(x$max_depth)),
  prof_prom = round(mean(abs(x$max_depth)), 1),
  r2 = round(mean(x$r2), 2), rmse = round(mean(x$rmse), 2),
  mae = round(mean(x$mae), 2))))
write.csv(T4, file.path(OUT, "tabla4_final.csv"), row.names = FALSE)
cat("\n=== TABLA 4 (conjunto completo, LOTO) ===\n"); print(T4, row.names = FALSE)

## ---- Fig 9 --------------------------------------------------------------
res$family <- ifelse(res$sensor %in% c("Landsat 8", "Landsat 9"), "Landsat", res$sensor)
fam_col    <- c("Landsat" = "blue", "Sentinel-2" = "forestgreen", "SuperDove" = "darkorange")
sensor_pch <- c("Landsat 8" = 16, "Landsat 9" = 17, "Sentinel-2" = 15, "SuperDove" = 18)
png(file.path(OUT, "fig9_r2_vs_turbidez.png"), width = 16, height = 12,
    units = "cm", res = 200)
plot(res$turbidity, res$r2, col = fam_col[res$family], pch = sensor_pch[res$sensor],
     cex = 1.1, xlab = "Turbidez", ylab = expression(R^2))
legend("bottomleft", legend = names(sensor_pch), pch = sensor_pch,
       col = fam_col[c("Landsat", "Landsat", "Sentinel-2", "SuperDove")], bty = "n")
dev.off()
message("Fig 9 regenerada.")
