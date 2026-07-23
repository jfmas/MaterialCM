## Fig 7 (regenerada con la cadena corregida) - Dispersion ratio vs profundidad
## para las cinco imagenes de estudio, con la recta de regresion ajustada sobre
## los puntos dentro del umbral de profundidad maxima y el umbral marcado.
## Misma cadena que make_fig6_v2.R (config_imagenes.R, puntos_seleccionados,
## enmascarado de blancos brillantes, SIN P2). El preprocesamiento por imagen es
## el de mejor ajuste usado en la Tabla 3 (sin glint; filtro 3x3).
suppressMessages({ library(satbathy); library(terra) })
terraOptions(todisk = TRUE, memfrac = 0.4)
source("analisis_satbathy/config_imagenes.R")
source("analisis_satbathy/mask_helpers.R")

com <- puntos_comunes()

## P2 (SuperDove 4/5/2025), definida aparte por estar excluida en config
P2CFG <- list(sensor = "SuperDove", date = "4/5/2025",
  path = "Yuc/imagenes/cortes/PlanetScope_2522_2025_05_04_17_01_18_L2R.tif",
  blue = 1, green = 2, red = 3, nir = 4, mask = "udm2",
  udm = c("Yuc/imagenes/chelem_mayo_jfmas_psscene_analytic_8b_sr_udm2/PSScene/20250504_170118_99_2522_3B_udm2_clip.tif",
          "Yuc/imagenes/chelem_mayo_jfmas_psscene_analytic_8b_sr_udm2/PSScene/20250504_170121_13_2522_3B_udm2_clip.tif"))
cfg_de <- function(nm) if (nm == "P2") P2CFG else IMAGENES[[nm]]

## tabla ratio/profundidad por imagen (cadena base: sin glint, filtro 3x3).
## Las 5 cuantificadas usan el conjunto comun; P2 usa su propia cobertura.
tabla <- function(nm) {
  cfg <- cfg_de(nm)
  img <- prepara(cfg, w = 3, glint = FALSE)
  r <- log_ratio(img, "blue", "green", n = N_RATIO)
  p <- puntos_sondeo(nm)
  v <- terra::extract(r, p, ID = FALSE)[, 1]
  d <- stats::na.omit(data.frame(ratio = v, depth = p$depth, idp = p$idp))
  terra::tmpFiles(remove = TRUE); gc(full = TRUE)
  if (nm == "P2") d else d[d$idp %in% com, ]
}

RUN <- c("L8", "L9", "S2a", "S2b", "P1", "P2")

## dimensiones que coinciden con la imagen embebida original (2228x2868 px)
png(file.path(OUT, "fig7_scatter_array.png"),
    width = 2228, height = 2868, units = "px", res = 220)
op <- par(mfrow = c(3, 2), mar = c(4.2, 4.2, 3, 1))
for (nm in RUN) {
  cfg <- cfg_de(nm)
  d <- tabla(nm)
  md <- optimal_max_depth(d)
  best <- md$max_depth[which.max(md$r2)]           # umbral (negativo)
  dentro <- d[d$depth > best & d$depth < 0, ]      # dentro del umbral (ajuste)
  fuera  <- d[d$depth <= best, ]                   # mas profundos que el umbral
  m <- stats::lm(depth ~ ratio, data = dentro)
  plot(dentro$ratio, dentro$depth, pch = 16, col = "steelblue", cex = 0.6,
       xlim = range(d$ratio, na.rm = TRUE), ylim = range(d$depth, na.rm = TRUE),
       xlab = "Ratio", ylab = "Profundidad (m)",
       main = paste0(cfg$sensor, " (", cfg$date, ")"))
  points(fuera$ratio, fuera$depth, pch = 2, col = "black", cex = 0.6)
  abline(m, col = "red", lwd = 2)
  abline(h = best, lty = 2, col = "black")
  leyenda <- c(sprintf("umbral prof. max. = %d m", abs(best)),
               sprintf("R2 = %.2f", summary(m)$r.squared))
  if (nm == "L8")
    leyenda <- c(leyenda, "", "circulo azul: dentro del umbral",
                 "triangulo negro: fuera del umbral")
  legend("bottomleft", bty = "n", cex = 0.9, legend = leyenda)
  message("panel ", nm, " ok (umbral ", best, ")")
}
par(op); dev.off()
cat("Fig 7 regenerada:", file.path(OUT, "fig7_scatter_array.png"), "\n")
