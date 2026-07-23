## Fig 6 (regenerada con la cadena corregida) - Coeficiente de determinacion
## vs profundidad maxima, para las 4 combinaciones glint/filtro, un panel por
## imagen de estudio. Usa config_imagenes.R (puntos_seleccionados.gpkg,
## enmascarado de blancos brillantes en vez de recorte por percentil).
## Las 5 imagenes cuantificadas (L8,L9,S2a,S2b,P1) usan el conjunto comun de
## sondeos (consistente con la Tabla 3); P2 (SuperDove 4/5, excluida del
## analisis cuantitativo por bruma) se muestra con su propia cobertura.
suppressMessages({ library(satbathy); library(terra) })
terraOptions(todisk = TRUE, memfrac = 0.4)
source("analisis_satbathy/config_imagenes.R")
source("analisis_satbathy/mask_helpers.R")

com  <- puntos_comunes()
deep <- vect("Yuc/deep_yuc2.gpkg")

## P2 (SuperDove 4/5/2025), definida aparte por estar excluida en config
P2CFG <- list(sensor = "SuperDove", date = "4/5/2025",
  path = "Yuc/imagenes/cortes/PlanetScope_2522_2025_05_04_17_01_18_L2R.tif",
  blue = 1, green = 2, red = 3, nir = 4, mask = "udm2",
  udm = c("Yuc/imagenes/chelem_mayo_jfmas_psscene_analytic_8b_sr_udm2/PSScene/20250504_170118_99_2522_3B_udm2_clip.tif",
          "Yuc/imagenes/chelem_mayo_jfmas_psscene_analytic_8b_sr_udm2/PSScene/20250504_170121_13_2522_3B_udm2_clip.tif"))

cfg_de <- function(nm) if (nm == "P2") P2CFG else IMAGENES[[nm]]

## curva R2 vs profundidad maxima para una imagen y una combinacion
curva <- function(nm, glint, filt) {
  cfg <- cfg_de(nm)
  img <- prepara(cfg, w = if (filt) 3 else 1, glint = glint,
                 deep = if (glint) deep else NULL)
  r <- log_ratio(img, "blue", "green", n = N_RATIO)
  p <- puntos_sondeo(nm)
  v <- terra::extract(r, p, ID = FALSE)[, 1]
  d <- data.frame(ratio = v, depth = p$depth, idp = p$idp)
  d <- if (nm == "P2") stats::na.omit(d) else stats::na.omit(d[d$idp %in% com, ])
  ## barrido desde -5 m (no solo desde -10) para que la curva sea continua y
  ## cubra tambien el rango somero
  md <- optimal_max_depth(d, depths = seq(-5, floor(min(d$depth)), by = -1))
  terra::tmpFiles(remove = TRUE); gc(full = TRUE)
  md
}

combos <- list(
  list(g = FALSE, f = FALSE, col = "black",       lty = 1, lab = "Ninguna de las dos"),
  list(g = TRUE,  f = FALSE, col = "red",         lty = 2, lab = "Correccion brillo"),
  list(g = FALSE, f = TRUE,  col = "blue",        lty = 3, lab = "Filtro"),
  list(g = TRUE,  f = TRUE,  col = "forestgreen", lty = 1, lab = "Correccion brillo y filtro"))

RUN <- c("L8", "L9", "S2a", "S2b", "P1", "P2")

png(file.path(OUT, "fig6_r2_vs_profmax.png"),
    width = 18, height = 22, units = "cm", res = 200)
op <- par(mfrow = c(3, 2), mar = c(4, 4, 2.5, 1))
for (nm in RUN) {
  cfg <- cfg_de(nm)
  plot(NA, xlim = c(-17.5, -4.5), ylim = c(0, 1),
       xlab = "Profundidad maxima (m)", ylab = "Coef. de determinacion",
       main = paste0(cfg$sensor, " (", cfg$date, ")"))
  curvas_nm <- list()
  for (cb in combos) {
    md <- curva(nm, cb$g, cb$f)
    curvas_nm[[cb$lab]] <- md
    lines(md$max_depth, md$r2, col = cb$col, lty = cb$lty, lwd = 2)
  }
  saveRDS(curvas_nm, file.path(OUT, paste0("fig6_curvas_", nm, ".rds")))
  if (nm == "L8")
    legend("bottomleft", legend = sapply(combos, `[[`, "lab"),
           col = sapply(combos, `[[`, "col"), lty = sapply(combos, `[[`, "lty"),
           lwd = 2, bty = "n", cex = 0.85)
  message("panel ", nm, " ok")
}
par(op); dev.off()
cat("Fig 6 regenerada:", file.path(OUT, "fig6_r2_vs_profmax.png"), "\n")
