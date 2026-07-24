## BATIMETRIA para cortes de Landsat, Sentinel y Planet en Yuc
# Deriving Bathymetry from Multispectral Remote Sensing Data, William J. Hernandez * and Roy A. Armstrong # 3.2. Bathymetry Retrieval
# Figure ratio (material suplementario, Figura S1)
# paper CM
#
# 2026: en lugar de una sola imagen (Landsat 8), arma un arreglo 3x1 con las
# TRES imágenes de marzo 2025 (Landsat 8, Sentinel-2 y SuperDove).
library(tmap)
library(terra)
library(sf)
library(magick)

setwd("/home/jf/pCloudDrive/proyectos/Batimetria")

mes <- "marzo"
## sensor -> combinación de preprocesamiento del ratio y título con fecha
info <- list(
  L = list(tit = "Landsat 8 (8/3/2025)",  glint = "NoGlint", filt = "NoFilter"),
  S = list(tit = "Sentinel-2 (9/3/2025)", glint = "NoGlint", filt = "NoFilter"),
  P = list(tit = "SuperDove (8/3/2025)",  glint = "NoGlint", filt = "NoFilter"))

mapas <- list()
for (tipo in c("L", "S", "P")) {
  ratio_name <- sprintf("ratio/ratio_Yuc-%s-%s-%s-%s.tif",
                        tipo, info[[tipo]]$glint, info[[tipo]]$filt, mes)
  ratio <- rast(ratio_name)
  ## la escena SuperDove es de 3 m (~59 Mpx); se agrega para dibujar más rápido
  if (tipo == "P") ratio <- aggregate(ratio, fact = 4, fun = "mean", na.rm = TRUE)

  mapas[[tipo]] <- tm_shape(ratio) +
    tm_raster(style = "cont", n = 8, palette = "Blues", title = "Ratio") +
    tm_layout(main.title = info[[tipo]]$tit, main.title.size = 0.9,
              main.title.fontface = "bold",
              legend.position = c("right", "top"), legend.text.size = 0.55,
              legend.title.size = 0.7, legend.bg.color = "white",
              legend.format = list(scientific = FALSE, digits = NA,
                                   text.separator = "a"),
              inner.margins = 0)
}

## arreglo 2x2: fila 1 = Landsat 8 | Sentinel-2 ; fila 2 = SuperDove centrado
## se guarda cada panel y se componen con magick para poder centrar el 3o
tmp <- tempfile(c("L", "S", "P"), fileext = ".png")
names(tmp) <- c("L", "S", "P")
for (t in c("L", "S", "P"))
  tmap_save(mapas[[t]], tmp[[t]], width = 9, height = 9, units = "cm", dpi = 300)

L <- image_read(tmp[["L"]]); S <- image_read(tmp[["S"]]); P <- image_read(tmp[["P"]])
fila1 <- image_append(c(L, S))                       # fila 1: los dos primeros
W     <- image_info(fila1)$width
lienzo <- image_blank(W, image_info(P)$height, color = "white")
offx  <- round((W - image_info(P)$width) / 2)
fila2 <- image_composite(lienzo, P, offset = sprintf("+%d+0", offx))  # 3o centrado
combi <- image_append(c(fila1, fila2), stack = TRUE)
image_write(combi, "Figuras/Fig_ratio_S1.png")
cat("Figura S1 (3 imágenes de ratio de marzo, 2x2): Figuras/Fig_ratio_S1.png\n")
