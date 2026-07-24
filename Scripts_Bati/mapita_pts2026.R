## BATIMETRIA para cortes de Landsat, Sentinel y Planet en Yucatán
# Deriving Bathymetry from Multispectral Remote Sensing Data, William J. Hernandez * and Roy A. Armstrong # 3.2. Bathymetry Retrieval
# Figure pts batimetria, fondo open street
# paper CM
#
# 2026: versión de mapita_pts2.R que colorea los sondeos por TRANSECTO
# (campo `linea`, 1-5), con una leyenda de 5 líneas etiquetadas por la fecha
# de toma (la fecha se repite en los transectos tomados el mismo día). Al
# final combina el mapa principal con el detalle costero (inset) en la esquina
# superior derecha, como en la Figura 3 publicada. Se conserva la extensión.
library(sf)
library(tmap)
library(tmaptools)
#install.packages("OpenStreetMap",dep=T)
library(OpenStreetMap)
library(magick)
# https://stackoverflow.com/questions/56903210/how-to-add-a-basemap-to-a-tmap-plot-in-r

#  Ruta del espacio de trabajo

setwd("/home/jf/pCloudDrive/proyectos/Batimetria")
## Mapa
pts1 <- st_read("Yuc/pts_bati_yuc_corr_latlon.gpkg")   # transectos 1-4
pts1 <- pts1[, c("date", "linea")]
pts2 <- st_read("Yuc/pts_bati_DEM_latlon.gpkg")        # transecto 5 (2025-05-02)
pts2 <- pts2[, c("date", "linea")]
pts <- rbind(pts1, pts2)
pts$Transecto <- factor(pts$linea, levels = 1:5)

## 5 colores (uno por transecto) y la leyenda "Transecto N (fecha)"
cols   <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")  # 1..5
etiq_leyenda <- c("Transecto 1 (2025-04-28)", "Transecto 2 (2025-04-30)",
                  "Transecto 3 (2025-05-01)", "Transecto 4 (2025-05-01)",
                  "Transecto 5 (2025-05-02)")

## la extensión se fija con los transectos 1-4 (recorta ~21.4 N como el
## original); el transecto 5 se muestra solo hasta ese límite
bbox_fig <- tmaptools::bb(pts1, ext = 1.1)
bb_num   <- as.numeric(sf::st_bbox(pts1))      # xmin, ymin, xmax, ymax
lon_span <- (bb_num[3] - bb_num[1]) * 1.1      # ancho en grados (con ext=1.1)
lat_span <- (bb_num[4] - bb_num[2]) * 1.1      # alto en grados

## --- extensión del inset costero (transectos 1 y 2) --------------------
## se usa tanto para el recuadro en el mapa principal como para el zoom,
## de modo que coincidan exactamente
sub12      <- pts[pts$linea %in% c(1, 2), ]
bbc        <- as.numeric(sf::st_bbox(sub12))    # xmin, ymin, xmax, ymax
## los transectos costeros son muy apaisados; se expande más en latitud para
## que la caja quede ~2:1 (acorde al marco del inset) y no deje franjas blancas
ex_lon     <- 0.30; ex_lat <- 0.80
dx         <- (bbc[3] - bbc[1]) * ex_lon; dy <- (bbc[4] - bbc[2]) * ex_lat
bbox_inset <- c(xmin = bbc[1] - dx, ymin = bbc[2] - dy,
                xmax = bbc[3] + dx, ymax = bbc[4] + dy)
rect_inset <- sf::st_as_sfc(sf::st_bbox(bbox_inset, crs = 4326))

mapita2 <- tm_basemap("OpenStreetMap") +
  tm_shape(pts, bbox = bbox_fig) +
  tm_symbols(col = "Transecto",
             size = 0.02,
             col.scale = tm_scale_categorical(values = cols, labels = etiq_leyenda),
             col.legend = tm_legend_hide()) +            # leyenda automática oculta
  ## recuadro que marca en el mapa principal la extensión del inset
  tm_shape(rect_inset) +
  tm_borders(col = "black", lwd = 2) +
  ## leyenda propia: puntos más grandes y desplazada al este
  tm_add_legend(type = "symbols", labels = etiq_leyenda, fill = cols,
                col = cols, size = 0.6, shape = 16, title = "",
                position = tm_pos_in("right", "center"),
                bg.color = "white", frame = TRUE, text.size = 0.75) +
  tm_scalebar(position = c("left", "bottom"), breaks = c(0, 1, 5)) +
  tm_graticules(n.x = 2, n.y = 5, alpha = 0.5, lwd = 0.8, labels.size = 0.5) +
  tm_layout(main.title.size = 1.5)

## lienzo casi cuadrado, acorde a la extensión recortada (evita márgenes blancos)
tmap_save(mapita2, "Figuras/Fig_ptsYuc.png", width = 13, height = 13, units = "cm", dpi = 300)

#### Zoom Costera (transectos 1 y 2, junto a la costa)
pts_cerca <- pts[pts$date != "2025-05-01" & pts$linea %in% c(1, 2), ]

mapita_c <-
  tm_basemap("OpenStreetMap") +
  tm_shape(pts_cerca, bbox = bbox_inset) +
  tm_symbols(col = "Transecto",
             size = 0.10,                       # puntos más grandes (mapa ampliado)
             col.scale = tm_scale_categorical(values = cols[1:2], labels = etiq_leyenda[1:2]),
             col.legend = tm_legend_hide()) +
  tm_scalebar(position = c("left", "bottom"), breaks = c(0, 0.5, 1)) +
  ## sin coordenadas (tm_graticules), para aprovechar todo el marco
  tm_layout(legend.show = FALSE, inner.margins = 0)

tmap_save(mapita_c, "Figuras/Fig_ptsYucZoom.png", width = 6, height = 3, units = "cm", dpi = 300)

#### Combinar: el inset costero en la esquina superior derecha del mapa principal
principal <- image_read("Figuras/Fig_ptsYuc.png")
inset     <- image_read("Figuras/Fig_ptsYucZoom.png")
w_prin    <- image_info(principal)$width
h_prin    <- image_info(principal)$height
inset     <- image_trim(inset, fuzz = 5)                             # quita márgenes blancos
inset     <- image_scale(inset, as.character(round(w_prin * 0.5)))   # ~50% del ancho
inset     <- image_border(inset, "grey30", "3x3")                    # recuadro
margen    <- round(w_prin * 0.02)
## desplazar el inset 0.01 grados al sur (abajo) y al oeste (izquierda)
d_oeste   <- round(0.01 / lon_span * w_prin)
d_sur     <- round(0.01 / lat_span * h_prin)
off_x     <- w_prin - image_info(inset)$width - margen - d_oeste
off_y     <- margen + d_sur
combinada <- image_composite(principal, inset, offset = sprintf("+%d+%d", off_x, off_y))
image_write(combinada, "Figuras/Fig_ptsYuc_combinada.png")
cat("Figura combinada: Figuras/Fig_ptsYuc_combinada.png\n")
