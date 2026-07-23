## Configuración única de las imágenes y constantes del análisis.
## Fuente de verdad: antes estaba duplicada en seis scripts, con el riesgo de
## que se desincronizaran los índices de banda o los umbrales.
##
##   source("analisis_satbathy/config_imagenes.R")

N_RATIO   <- 10000          # n de la transformación de Stumpf, ln(n*R)
## TRIM retirado: sustituido por satbathy::mask_bright_targets() (criterio NIR)
REF_NM    <- 5.49           # nivel medio (m) al que están alineados los sondeos
PROF_MIN  <- 0              # sin umbral: se evalúa todo el rango sondeado
## Se fijó en 4 m mientras se creía que las máscaras de agua descartaban el
## somero de forma desigual entre sensores. Comprobado despues: ese descarte lo
## causaba el recorte por percentil del ratio, ya eliminado. Con la cadena
## corregida las cinco imágenes conservan el somero (Sentinel-2 pasó del 0% al
## 100% en el transecto 2), y el conjunto común sin umbral es MAYOR que con 4 m
## (1468 frente a 1390 puntos) y cubre de 1.1 a 17.2 m.
OUT       <- "analisis_satbathy/output"

## Nivel del mar interpolado a la hora de cada toma (Tablas/NivelesImagenesYuc.csv).
## La corrección aplicada es: profundidad = depthc + NIVEL - REF_NM
NIVEL <- c(L8 = 5.5952, L9 = 5.8749, S2a = 5.5499,
           S2b = 5.8749, P1 = 5.6685, P2 = 5.7450)   # P2 se conserva por si se reactiva

## Índices de banda dentro de cada archivo y máscara de agua correspondiente:
##   qa   -> QA_PIXEL de Landsat (bit 7 agua, bits 1-4 nube)
##   scl  -> Scene Classification Layer de Sentinel-2 (6 agua; 1,3,8,9,10 nube)
##   ndwi -> NDWI de McFeeters, para Planet, que no trae capa de calidad
IMAGENES <- list(
  L8  = list(sensor="Landsat 8",  date="8/3/2025",
             path="Landsat03a05/LC08_L2SP_020045_20250308_02_T1.tif",
             blue=1, green=3, red=2, nir=4, mask="qa", qa=5),
  L9  = list(sensor="Landsat 9",  date="3/5/2025",
             path="Landsat03a05/LC09_L2SP_020045_20250503_02_T1.tif",
             blue=1, green=3, red=2, nir=4, mask="qa", qa=5),
  S2a = list(sensor="Sentinel-2", date="9/3/2025",
             path="Sentinel03a05/im2025-03-09.tif",
             blue=2, green=3, red=4, nir=8, mask="scl",
             scl="Sentinel03a05/scl2025-03-09.tif"),
  S2b = list(sensor="Sentinel-2", date="3/5/2025",
             path="Sentinel03a05/im2025-05-03.tif",
             blue=2, green=3, red=4, nir=8, mask="scl",
             scl="Sentinel03a05/scl2025-05-03.tif"),
  P1  = list(sensor="SuperDove",  date="8/3/2025",
             path="Yuc/imagenes/cortes/PlanetScope_20250308_165229_70_24f6_L2R.tif",
             blue=1, green=2, red=3, nir=4, mask="udm2",
             udm=c("Yuc/imagenes/chelem_marzo_jfmas_psscene_analytic_8b_sr_udm2/PSScene/20250308_165229_70_24f6_3B_udm2_clip.tif",
                   "Yuc/imagenes/chelem_marzo_jfmas_psscene_analytic_8b_sr_udm2/PSScene/20250308_165227_50_24f6_3B_udm2_clip.tif")))

  ## P2 (SuperDove 4/5/2025) EXCLUIDA por calidad insuficiente.
  ## El UDM2 de Planet marca 655 de 1689 sondeos (39%) como no utilizables por
  ## bruma sobre los transectos. Mantenerla reducia el conjunto comun de las
  ## seis imagenes de ~1400 a 582 puntos, es decir, la bruma de una sola escena
  ## dictaba sobre que sondeos se evaluaban todas las demas. Se documenta como
  ## descarte por control de calidad del proveedor, no por su resultado.
  ## Para reactivarla, descomentar el bloque siguiente.
  # P2  = list(sensor="SuperDove",  date="4/5/2025",
  #            path="Yuc/imagenes/cortes/PlanetScope_2522_2025_05_04_17_01_18_L2R.tif",
  #            blue=1, green=2, red=3, nir=4, mask="udm2",
  #            udm=c(".../20250504_170118_99_2522_3B_udm2_clip.tif",
  #                  ".../20250504_170121_13_2522_3B_udm2_clip.tif"))


## ---- utilidades comunes -------------------------------------------------

## sondeos con la profundidad referida al nivel del mar de la toma
## Fuente: Yuc/puntos_seleccionados.gpkg (1789 sondeos), generado por
## analisis_satbathy/selecciona_sondeos.R a partir de los 1940 sondeos completos
## (Yuc/pts_bati_completo_latlon.gpkg), descartando los de la franja costera que
## caen en NA en >= 40% de las imagenes, con la cadena de proceso corregida.
##
## Sustituye a training_points + test_points (1794), que aplicaban el mismo
## criterio pero con la cadena antigua y sobre una serie de imagenes distinta.
## Los dos conjuntos difieren en 39 sondeos (2%): el descarte original era
## correcto. Ya no se usa la particion 70/30 con set.seed(123), porque la
## validacion es leave-one-transect-out.
puntos_sondeo <- function(id = NULL) {
  p <- terra::vect("Yuc/puntos_seleccionados.gpkg")
  p$idp <- seq_len(nrow(p))
  if (!is.null(id)) {
    p$prof  <- as.numeric(p$depthc) + (NIVEL[[id]] - REF_NM)   # positiva hacia abajo
  } else {
    p$prof  <- as.numeric(p$depthc)
  }
  p$depth <- -p$prof                                            # negativa, como el modelo
  p
}

## conjunto de puntos válido en las seis imágenes con prof >= PROF_MIN
puntos_comunes <- function() {
  f <- file.path(OUT, "puntos_comunes.rds")
  if (!file.exists(f)) stop("Falta ", f, ": ejecuta analisis_satbathy/define_comunes.R")
  readRDS(f)
}

rmse_ <- function(o, p) sqrt(mean((o - p)^2))
mae_  <- function(o, p) mean(abs(o - p))

## IC 95% del RMSE por bootstrap sobre los pares observado/predicho
ic_rmse <- function(o, p, B = 2000) {
  n <- length(o); if (n < 20) return(c(NA, NA))
  as.numeric(stats::quantile(
    replicate(B, { i <- sample.int(n, n, replace = TRUE); rmse_(o[i], p[i]) }),
    c(0.025, 0.975)))
}

## validación leave-one-transect-out: devuelve observados y predichos
## El umbral de profundidad máxima se fija SOLO con el entrenamiento de cada
## pliegue, para no filtrar información del conjunto de prueba.
loto <- function(d, min_test = 15, min_train = 30) {
  obs <- c(); prd <- c(); lin <- c(); best_v <- c()
  for (L in sort(unique(d$linea))) {
    tr <- d[d$linea != L, ]; te <- d[d$linea == L, ]
    md <- satbathy::optimal_max_depth(tr)
    best <- md$max_depth[which.max(md$r2)]
    trf <- tr[tr$depth > best, ]
    tef <- te[te$depth > best & te$depth < 0, ]
    if (nrow(tef) < min_test || nrow(trf) < min_train) next
    m <- stats::lm(depth ~ ratio, data = trf)
    obs <- c(obs, tef$depth)
    prd <- c(prd, as.numeric(stats::predict(m, newdata = data.frame(ratio = tef$ratio))))
    lin <- c(lin, tef$linea); best_v <- c(best_v, rep(best, nrow(tef)))
  }
  list(obs = obs, prd = prd, linea = lin, max_depth = best_v)
}
