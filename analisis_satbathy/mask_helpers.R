## Enmascarado de agua/nube ESPECÍFICO POR SENSOR (método original del manuscrito).
## Landsat -> QA_PIXEL ; Sentinel-2 -> SCL ; Planet -> NDWI (satbathy::water_mask).
suppressMessages(library(terra))

## Landsat Collection-2 QA_PIXEL: conservar agua (bit 7), quitar nube/sombra/
## cirrus/dilatada (bits 1-4). Índices 1-based de intToBits: agua=8; nube=2,3,4,5.
mask_landsat_qa <- function(img, qa) {
  qa <- round(qa)
  vals <- unique(values(qa))[, 1]; vals <- vals[!is.na(vals)]
  is_water <- function(x) as.integer(intToBits(x)[8]) == 1L
  is_cloud <- function(x) any(as.integer(intToBits(x)[c(2, 3, 4, 5)]) == 1L)
  mw <- subst(qa, vals, vapply(vals, is_water, logical(1)))
  mc <- subst(qa, vals, vapply(vals, is_cloud, logical(1)))
  img[mc == 1 | mw == 0] <- NA
  img
}

## Sentinel-2 SCL: conservar agua (6), quitar nube/sombra/cirrus (1,3,8,9,10).
mask_sentinel_scl <- function(img, scl) {
  scl <- round(scl)
  if (!isTRUE(try(compareGeom(scl, img, stopOnError = FALSE), silent = TRUE)))
    scl <- resample(scl, img, method = "near")
  mw <- scl == 6
  mc <- scl == 1 | scl == 3 | scl == 8 | scl == 9 | scl == 10
  img[mc == 1 | mw == 0] <- NA
  img
}

## Lee una imagen y devuelve reflectancia (blue,green,red,nir) enmascarada a agua
## según el sensor. cfg necesita: path, blue, green, red, nir, mask ("qa"/"scl"/"ndwi"),
## y qa (índice de banda) o scl (ruta) según el caso.
## Planet UDM2: banda 1 = "clear" (1 donde el píxel es utilizable; excluye nube,
## bruma ligera y densa, sombra y nieve). Planet SÍ entrega control de calidad;
## antes no se usaba y las nubes de las escenas SuperDove no se filtraban.
## El UDM2 solo distingue utilizable/no utilizable, no agua de tierra, así que
## se combina con NDWI para quedarse con agua utilizable.
## `udm_path` puede ser un vector: las escenas SuperDove son mosaicos de varias
## pasadas y cada una trae su propio UDM2 recortado, ninguno de los cuales cubre
## por si solo toda el area. Se unen en el orden dado (el primero tiene prioridad
## donde solapan).
mask_planet_udm2 <- function(img, udm_path, ndwi_thr = 0) {
  us <- lapply(udm_path, function(f) rast(f)[[1]])   # banda "clear"
  u  <- if (length(us) == 1) us[[1]] else do.call(terra::merge, us)
  if (!isTRUE(try(compareGeom(u, img, stopOnError = FALSE), silent = TRUE)))
    u <- resample(u, img, method = "near")
  agua <- satbathy::water_mask(img, threshold = ndwi_thr)
  img[u != 1 | agua != 1] <- NA
  img
}

read_masked <- function(cfg, ndwi_thr = 0) {
  r <- rast(cfg$path)
  img <- r[[c(cfg$blue, cfg$green, cfg$red, cfg$nir)]]
  names(img) <- c("blue", "green", "red", "nir")
  if (identical(cfg$mask, "qa"))        img <- mask_landsat_qa(img, r[[cfg$qa]])
  else if (identical(cfg$mask, "scl"))  img <- mask_sentinel_scl(img, rast(cfg$scl))
  else if (!is.null(cfg$udm))           img <- mask_planet_udm2(img, cfg$udm, ndwi_thr)
  else img <- mask(img, satbathy::water_mask(img, threshold = ndwi_thr),
                   maskvalues = c(FALSE, NA))
  img
}

## NOTA: `ratio_limpio()` y `filtra_enmascarado()` se han retirado. Sus dos
## arreglos están ahora en el paquete y son el comportamiento por defecto:
##   - satbathy::mask_bright_targets()  sustituye al recorte por percentil
##   - satbathy::lowpass_filter(keep_na = TRUE)  ya no resucita enmascarados
## El orden correcto es:
##   read_masked() -> mask_bright_targets() -> lowpass_filter() -> log_ratio()
## Quitar las anomalías ANTES de filtrar evita que contaminen a sus vecinos.
prepara <- function(cfg, w = 3, glint = FALSE, deep = NULL, ndwi_thr = 0) {
  img <- read_masked(cfg, ndwi_thr = ndwi_thr)
  if (glint) {
    dm <- if (crs(deep) != crs(img)) project(deep, crs(img)) else deep
    img <- satbathy::correct_glint(img, c("blue", "green"), "nir", dm)
  }
  img <- satbathy::mask_bright_targets(img, "nir")
  if (w > 1) img <- satbathy::lowpass_filter(img, w = w)
  img
}
