## Preprocesado de las escenas PlanetScope descargadas (Orders API) para el
## análisis batimétrico.
##
## Las escenas vienen como producto analytic_8b_sr: 8 bandas
## (coastal_blue, blue, green_i, green, yellow, red, rededge, nir), reflectancia
## de superficie escalada x10000, en franjas sueltas por pasada. El análisis
## espera 4 bandas (blue, green, red, nir) en reflectancia 0-1, una imagen por
## fecha. Este script, por cada fecha:
##   1. selecciona las bandas 2,4,6,8 (blue, green, red, nir del 8b) y divide /10000
##   2. une (mosaico) todas las franjas de esa fecha
##   3. compone tambien la banda "clear" de los UDM2 en un unico raster
##   4. guarda imagen + udm y una fila en el catalogo
##
## NOTA DE CONSISTENCIA: la imagen SuperDove del 8/3/2025 ya en el estudio (P1)
## es el producto L2R (correccion atmosferica Acolite), distinto del SR estandar
## de Planet que se usa aqui. El modelo empirico se recalibra por imagen, asi que
## un sesgo atmosferico constante lo absorbe la regresion, pero conviene tenerlo
## presente al comparar. Para maxima homogeneidad, mas adelante se puede
## reprocesar tambien el 8/3 desde su producto SR.
suppressMessages({ library(terra) })
setwd("/home/jf/pcloud_sync/Batimetria")
terraOptions(todisk = TRUE, memfrac = 0.5, progress = 0)

ENTRADA <- "Yuc/imagenes/planet_nuevas"
SALIDA  <- "Yuc/imagenes/planet_procesadas"
dir.create(SALIDA, showWarnings = FALSE, recursive = TRUE)

## bandas del producto 8b que corresponden a blue/green/red/nir
BANDAS <- c(blue = 2, green = 4, red = 6, nir = 8)
ESCALA <- 10000

## recorte al area de estudio. Las escenas ya vienen recortadas del pedido
## (clip server-side), asi que esto solo garantiza una extension homogenea.
## El recuadro esta en lon/lat y las escenas en UTM: se proyecta al vuelo.
aoi_ll <- vect(ext(-89.84004, -89.65617, 21.26287, 21.672), crs = "EPSG:4326")
recorta <- function(r) {
  a <- project(aoi_ll, crs(r))
  if (is.null(intersect(ext(r), ext(a)))) return(r)   # sin solape: no recortar
  crop(r, a)
}

sr_files  <- list.files(ENTRADA, pattern = "_3B_AnalyticMS_SR_8b_clip\\.tif$",
                        recursive = TRUE, full.names = TRUE)
message("escenas SR encontradas: ", length(sr_files))

## agrupar por fecha (YYYYMMDD, primeros 8 digitos del nombre)
fecha_de <- function(f) sub("^([0-9]{8}).*", "\\1", basename(f))
fechas <- split(sr_files, vapply(sr_files, fecha_de, character(1)))
message("fechas a componer: ", length(fechas))

catalogo <- data.frame()
for (fch in names(fechas)) {
  fs <- fechas[[fch]]
  destino_im  <- file.path(SALIDA, paste0(fch, "_sr.tif"))
  destino_udm <- file.path(SALIDA, paste0(fch, "_udm.tif"))

  if (file.exists(destino_im) && file.exists(destino_udm)) {
    message(fch, ": ya existe, se omite")
  } else {
    ims <- list(); uds <- list()
    for (f in fs) {
      r <- try(rast(f), silent = TRUE)
      if (inherits(r, "try-error") || nlyr(r) < 8) {
        message("  ", basename(f), ": no es 8b, se omite"); next
      }
      img <- r[[BANDAS]] / ESCALA
      names(img) <- names(BANDAS)
      ims[[length(ims) + 1]] <- img

      uf <- sub("_AnalyticMS_SR_8b_clip", "_udm2_clip", f)
      if (file.exists(uf)) {
        u <- rast(uf)[[1]]                    # banda "clear"
        if (!isTRUE(try(compareGeom(u, img, stopOnError = FALSE), silent = TRUE)))
          u <- resample(u, img, method = "near")
        uds[[length(uds) + 1]] <- u
      }
    }
    if (!length(ims)) { message(fch, ": sin escenas validas"); next }

    im  <- if (length(ims) == 1) ims[[1]] else do.call(terra::merge, ims)
    im  <- recorta(im)
    udm <- if (!length(uds)) NULL else
           if (length(uds) == 1) uds[[1]] else do.call(terra::merge, uds)
    if (!is.null(udm)) udm <- recorta(udm)

    writeRaster(im, destino_im, overwrite = TRUE,
                gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "TILED=YES"))
    if (!is.null(udm))
      writeRaster(udm, destino_udm, overwrite = TRUE,
                  gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
    message(sprintf("%s: %d franja(s) -> %d x %d px", fch, length(ims),
                    nrow(im), ncol(im)))
  }

  catalogo <- rbind(catalogo, data.frame(
    fecha = as.character(as.Date(fch, "%Y%m%d")),
    yyyymmdd = fch, n_franjas = length(fs),
    path = destino_im,
    udm  = if (file.exists(destino_udm)) destino_udm else NA_character_,
    stringsAsFactors = FALSE))
}

write.csv(catalogo, "analisis_satbathy/output/planet_procesadas.csv",
          row.names = FALSE)
message("\ncatalogo: analisis_satbathy/output/planet_procesadas.csv (",
        nrow(catalogo), " fechas)")
message("imagenes en: ", normalizePath(SALIDA))
