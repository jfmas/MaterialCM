########################################################################
## Batimetría Yucatán — búsqueda de escenas PlanetScope sobre Chelem
##
## Consulta la Data API de Planet y lista las escenas disponibles sobre el
## área de estudio en el periodo indicado, con su nubosidad y su porcentaje
## de píxeles utilizables. NO descarga nada: buscar no consume cuota, solo
## los pedidos (orders) la consumen.
##
## Credenciales: NUNCA en el código. La clave se obtiene en
##   https://www.planet.com/account/#/  ->  My Settings  ->  API Key
## y se exporta antes de ejecutar:
##   export PL_API_KEY="..."
##
## Uso:
##   Rscript Scripts_Bati/busca_planet.R
##   Rscript Scripts_Bati/busca_planet.R 2025-03-01 2025-05-31 0.2
##     (desde, hasta, nubosidad máxima 0-1)
##
## Salida: analisis_satbathy/output/planet_disponibles.csv
########################################################################

suppressMessages({ library(httr2); library(sf); library(jsonlite) })
setwd("/home/jf/pcloud_sync/Batimetria")

key <- Sys.getenv("PL_API_KEY")
if (key == "")
  stop("Falta PL_API_KEY en el entorno.\n",
       "  Obtenla en https://www.planet.com/account/#/ (My Settings -> API Key)\n",
       '  y expórtala:  export PL_API_KEY="..."')
if (grepl("^\\.*$", key))
  stop("PL_API_KEY sigue con el texto de ejemplo. Pon la clave real.")

args  <- commandArgs(trailingOnly = TRUE)
DESDE <- if (length(args) >= 1) args[1] else "2025-03-01"
HASTA <- if (length(args) >= 2) args[2] else "2025-05-31"
NUBE  <- if (length(args) >= 3) as.numeric(args[3]) else 0.2

## ---- AOI: mismo recuadro del estudio -----------------------------------
caja <- st_bbox(c(xmin = -89.84004, xmax = -89.65617,
                  ymin = 21.26287,  ymax = 21.672), crs = st_crs(4326))
geom <- list(type = "Polygon", coordinates = list(matrix(c(
  caja["xmin"], caja["ymin"],  caja["xmax"], caja["ymin"],
  caja["xmax"], caja["ymax"],  caja["xmin"], caja["ymax"],
  caja["xmin"], caja["ymin"]), ncol = 2, byrow = TRUE)))

filtro <- list(type = "AndFilter", config = list(
  list(type = "GeometryFilter", field_name = "geometry", config = geom),
  list(type = "DateRangeFilter", field_name = "acquired",
       config = list(gte = paste0(DESDE, "T00:00:00Z"),
                     lte = paste0(HASTA, "T23:59:59Z"))),
  list(type = "RangeFilter", field_name = "cloud_cover",
       config = list(lte = NUBE)),
  ## solo lo que la cuenta puede descargar realmente
  list(type = "PermissionFilter", config = list("assets:download"))
))

cuerpo <- list(item_types = list("PSScene"), filter = filtro)

## zona de sondeos: envolvente convexa de los transectos con 300 m de margen.
## La busqueda devuelve escenas que INTERSECAN el AOI, no que lo cubran; muchas
## solo rozan una esquina y no sirven. Se mide que fraccion de la zona de
## trabajo cubre cada huella.
pts  <- rbind(sf::st_read("Yuc/training_points.gpkg", quiet = TRUE),
              sf::st_read("Yuc/test_points.gpkg", quiet = TRUE))
zona <- sf::st_convex_hull(sf::st_union(sf::st_transform(pts, 32616)))
zona <- sf::st_transform(sf::st_buffer(zona, 300), 4326)

## ---- consulta con paginación -------------------------------------------
message(sprintf("Buscando PSScene  |  %s a %s  |  nubosidad <= %.0f%%",
                DESDE, HASTA, 100 * NUBE))

resp <- request("https://api.planet.com/data/v1/quick-search") |>
  req_auth_basic(key, "") |>
  req_body_json(cuerpo) |>
  req_error(is_error = function(r) FALSE) |>
  req_perform()

if (resp_status(resp) == 401)
  stop("Planet rechazó la clave (401). Comprueba PL_API_KEY.")
if (resp_status(resp) >= 400)
  stop("Error ", resp_status(resp), ": ", resp_body_string(resp))

filas <- list(); pagina <- 0
repeat {
  d <- resp_body_json(resp)
  pagina <- pagina + 1
  for (f in d$features) {
    p <- f$properties
    ## cobertura real de la zona de sondeos por la huella de la escena
    cob <- NA_real_; wkt <- NA_character_
    try({
      g <- sf::st_sfc(sf::st_polygon(lapply(f$geometry$coordinates, function(anillo)
             do.call(rbind, lapply(anillo, function(v) c(v[[1]], v[[2]]))))), crs = 4326)
      inter <- suppressWarnings(sf::st_intersection(g, zona))
      cob <- if (length(inter)) 100 * as.numeric(sf::st_area(inter)) /
                                as.numeric(sf::st_area(zona)) else 0
      wkt <- sf::st_as_text(g)
    }, silent = TRUE)
    filas[[length(filas) + 1]] <- data.frame(
      cobertura_sondeos = round(cob, 1),
      huella_wkt    = wkt,
      id            = f$id,
      fecha         = substr(p$acquired, 1, 10),
      hora_utc      = substr(p$acquired, 12, 19),
      instrumento   = p$instrument %||% NA_character_,
      nubosidad     = p$cloud_cover %||% NA_real_,
      pct_nube      = p$cloud_percent %||% NA_real_,
      pct_utilizable= p$clear_percent %||% NA_real_,
      pct_bruma     = (p$light_haze_percent %||% 0) + (p$heavy_haze_percent %||% 0),
      calidad       = p$quality_category %||% NA_character_,
      angulo_off    = p$view_angle %||% NA_real_,
      sol_elev      = p$sun_elevation %||% NA_real_,
      stringsAsFactors = FALSE)
  }
  nxt <- d$`_links`$`_next`
  if (is.null(nxt) || !length(d$features)) break
  resp <- request(nxt) |> req_auth_basic(key, "") |> req_perform()
}

if (!length(filas)) {
  message("Sin resultados. Prueba a ampliar el periodo o subir el umbral de nubosidad.")
  quit(save = "no")
}

res <- do.call(rbind, filas)
res <- res[order(res$fecha, res$hora_utc), ]

## agrupa por fecha: cada adquisición suele venir en varias pasadas
resumen <- do.call(rbind, lapply(split(res, res$fecha), function(x) data.frame(
  fecha          = x$fecha[1],
  n_escenas      = nrow(x),
  pct_utilizable = round(mean(x$pct_utilizable, na.rm = TRUE), 1),
  pct_nube       = round(mean(x$pct_nube, na.rm = TRUE), 1),
  pct_bruma      = round(mean(x$pct_bruma, na.rm = TRUE), 1),
  cobertura_max  = round(max(x$cobertura_sondeos, na.rm = TRUE), 1),
  cobertura_union = {
    w <- x$huella_wkt[!is.na(x$huella_wkt)]
    if (!length(w)) 0 else {
      u <- sf::st_union(sf::st_as_sfc(w, crs = 4326))
      i <- suppressWarnings(sf::st_intersection(u, zona))
      round(if (length(i)) 100 * as.numeric(sf::st_area(i)) /
                                 as.numeric(sf::st_area(zona)) else 0, 1)
    }
  },
  hora_utc       = x$hora_utc[1],
  stringsAsFactors = FALSE)))
resumen <- resumen[order(-resumen$pct_utilizable), ]

dir.create("analisis_satbathy/output", showWarnings = FALSE, recursive = TRUE)
write.csv(res[, setdiff(names(res), "huella_wkt")],
          "analisis_satbathy/output/planet_disponibles.csv", row.names = FALSE)
write.csv(resumen, "analisis_satbathy/output/planet_por_fecha.csv", row.names = FALSE)

cat(sprintf("\nEscenas encontradas: %d en %d fechas (%d paginas)\n",
            nrow(res), nrow(resumen), pagina))
cat("\n=== POR FECHA, ordenado por % utilizable ===\n")
print(resumen, row.names = FALSE)

ya <- c("2025-03-08", "2025-05-04")
cat("\nYa descargadas:", paste(ya, collapse = ", "), "\n")
nuevas <- resumen[!(resumen$fecha %in% ya) & resumen$pct_utilizable >= 90 &
                  resumen$cobertura_union >= 75, ]
cat(sprintf("Fechas NUEVAS con >=90%% utilizable y >=75%% de la zona cubierta: %d\n",
            nrow(nuevas)))
if (nrow(nuevas))
  print(nuevas[order(-nuevas$cobertura_union),
               c("fecha", "n_escenas", "pct_utilizable", "cobertura_union")],
        row.names = FALSE)

cat("\nDetalle completo en analisis_satbathy/output/planet_disponibles.csv\n")
cat("Buscar no consume cuota; solo la consume pedir la descarga.\n")
