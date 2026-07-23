## Recálculo completo sobre el conjunto común (mismos sondeos para todas las
## imágenes, sin umbral de profundidad mínima) y con la profundidad referida al nivel del mar de cada toma.
##
## Genera:
##   tabla3_final.csv            error por imagen (LOTO, IC 95% por bootstrap)
##   tabla_estratos_final.csv    RMSE/MAE por estrato de profundidad
##   tabla_filtro_final.csv      sensibilidad al tamaño de ventana (3,5,7,9)
##   tabla_glint_final.csv       magnitud del glint y efecto de la corrección
##   tabla_sensores_final.csv    resumen por sensor
suppressMessages({ library(satbathy); library(terra) })
source("analisis_satbathy/config_imagenes.R")
source("analisis_satbathy/mask_helpers.R")
set.seed(42)

com <- puntos_comunes()
message("conjunto común: ", length(com), " puntos")

## tabla base por imagen: ratio (con y sin cada preprocesamiento) + profundidad
datos <- function(nm, glint = FALSE, w = 3) {
  cfg <- IMAGENES[[nm]]
  img <- prepara(cfg, w = w, glint = glint,
                 deep = if (glint) vect("Yuc/deep_yuc2.gpkg") else NULL)
  r <- log_ratio(img, "blue", "green", n = N_RATIO)
  p <- puntos_sondeo(nm)
  v <- terra::extract(r, p, ID = FALSE)[, 1]
  d <- data.frame(ratio = v, depth = p$depth, prof = p$prof,
                  linea = p$linea, idp = p$idp)
  stats::na.omit(d[d$idp %in% com, ])
}

## ---------------------------------------------- 1. error por imagen (LOTO)
T3 <- data.frame(); EST <- data.frame()
for (nm in names(IMAGENES)) {
  cfg <- IMAGENES[[nm]]; message("== ", nm, " ==")
  d <- datos(nm)
  L <- loto(d)
  ci <- ic_rmse(L$obs, L$prd)
  ## R2 del ajuste sobre todos los puntos (comparable con el del manuscrito)
  md <- optimal_max_depth(d); best <- md$max_depth[which.max(md$r2)]
  fit <- fit_bathymetry(d, max_depth = best)
  T3 <- rbind(T3, data.frame(id = nm, sensor = cfg$sensor, date = cfg$date,
    n = length(L$obs), prof_max = best, r2 = round(fit$r.squared, 3),
    rmse = round(rmse_(L$obs, L$prd), 3),
    ic95 = sprintf("[%.2f, %.2f]", ci[1], ci[2]),
    mae = round(mae_(L$obs, L$prd), 3)))

  dd <- -L$obs
  for (lo in c(0, 2, 4, 6, 8, 10, 12, 14)) {
    hi <- lo + 2; k <- dd > lo & dd <= hi
    if (sum(k) >= 15) EST <- rbind(EST, data.frame(id = nm, sensor = cfg$sensor,
      estrato = sprintf("%d-%d m", lo, hi), n = sum(k),
      rmse = round(rmse_(L$obs[k], L$prd[k]), 3),
      mae = round(mae_(L$obs[k], L$prd[k]), 3)))
  }
}
write.csv(T3,  file.path(OUT, "tabla3_final.csv"), row.names = FALSE)
write.csv(EST, file.path(OUT, "tabla_estratos_final.csv"), row.names = FALSE)

## ------------------------------------------------- 2. sensibilidad al filtro
FIL <- data.frame()
for (nm in names(IMAGENES)) {
  for (w in c(1, 3, 5, 7, 9)) {
    d <- datos(nm, w = w); L <- loto(d)
    if (!length(L$obs)) next
    FIL <- rbind(FIL, data.frame(id = nm, sensor = IMAGENES[[nm]]$sensor,
      ventana = if (w == 1) "sin filtro" else sprintf("%dx%d", w, w),
      n = length(L$obs), rmse = round(rmse_(L$obs, L$prd), 3),
      mae = round(mae_(L$obs, L$prd), 3)))
  }
  message("filtro ", nm, " ok")
}
write.csv(FIL, file.path(OUT, "tabla_filtro_final.csv"), row.names = FALSE)

## --------------------------- 3. glint: magnitud y efecto de la corrección
GL <- data.frame()
deep0 <- vect("Yuc/deep_yuc2.gpkg")
for (nm in names(IMAGENES)) {
  cfg <- IMAGENES[[nm]]
  img <- read_masked(cfg)
  dm <- if (crs(deep0) != crs(img)) project(deep0, crs(img)) else deep0
  v <- values(mask(img[["nir"]], dm)); v <- v[!is.na(v)]
  q <- stats::quantile(v, c(0.01, 0.99))

  sin_g <- loto(datos(nm, glint = FALSE))
  con_g <- loto(datos(nm, glint = TRUE))
  r0 <- rmse_(sin_g$obs, sin_g$prd); r1 <- rmse_(con_g$obs, con_g$prd)

  GL <- rbind(GL, data.frame(id = nm, sensor = cfg$sensor, date = cfg$date,
    nir_sd = round(stats::sd(v), 5),
    nir_p1p99 = round(as.numeric(q[2] - q[1]), 5),
    rmse_sin_glint = round(r0, 3), rmse_con_glint = round(r1, 3),
    efecto = round(r1 - r0, 3)))
  message("glint ", nm, " ok")
}
write.csv(GL, file.path(OUT, "tabla_glint_final.csv"), row.names = FALSE)

## ------------------------------------------------------ 4. resumen por sensor
SEN <- aggregate(cbind(rmse, mae, r2) ~ sensor, T3, mean)
SEN[, -1] <- round(SEN[, -1], 3)
write.csv(SEN, file.path(OUT, "tabla_sensores_final.csv"), row.names = FALSE)

cat("\n=== ERROR POR IMAGEN (LOTO, conjunto comun) ===\n"); print(T3, row.names = FALSE)
cat("\n=== ERROR POR ESTRATO ===\n"); print(EST, row.names = FALSE)
cat("\n=== SENSIBILIDAD AL FILTRO ===\n"); print(FIL, row.names = FALSE)
cat("\n=== GLINT: magnitud y efecto de la correccion ===\n"); print(GL, row.names = FALSE)
if (nrow(GL) >= 4) {
  ct <- stats::cor.test(GL$nir_sd, GL$efecto, method = "spearman", exact = FALSE)
  cat(sprintf("\ncorrelacion Spearman (magnitud NIR vs efecto): rho = %.3f, p = %.3f, n = %d\n",
              ct$estimate, ct$p.value, nrow(GL)))
}
cat("\n=== RESUMEN POR SENSOR ===\n"); print(SEN, row.names = FALSE)
