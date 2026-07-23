########################################################################
## Batimetría Yucatán — pedido de escenas PlanetScope (Orders API)
##
## Crea un pedido con las escenas de planet_solicitud.csv, recortadas al área
## de estudio, espera a que Planet lo prepare y descarga el resultado.
##
## ESTE SCRIPT SÍ CONSUME CUOTA. Revisa la lista antes de lanzarlo:
##   Rscript Scripts_Bati/lista_planet_solicitud.R 90 5 test
##
## Credenciales: NUNCA en el código.
##   export PL_API_KEY="..."
##
## Uso:
##   Rscript Scripts_Bati/pide_planet.R --simular      # no pide nada, solo informa
##   Rscript Scripts_Bati/pide_planet.R                # pide TODAS las de la lista
##   Rscript Scripts_Bati/pide_planet.R --coincidentes # solo las que coinciden
##                                                     # con Landsat/Sentinel
##   Rscript Scripts_Bati/pide_planet.R --descargar    # NO pide nada: solo baja
##                                                     # los pedidos ya registrados
##
## El pedido se parte en lotes porque la Orders API limita el número de
## elementos por pedido. Se guarda el identificador de cada lote para poder
## reanudar sin volver a pedir.
########################################################################

suppressMessages({ library(httr2); library(sf); library(jsonlite); library(curl) })
setwd("/home/jf/pcloud_sync/Batimetria")

key <- Sys.getenv("PL_API_KEY")
if (key == "" || grepl("^\\.*$", key))
  stop("Falta PL_API_KEY (o sigue con el texto de ejemplo).\n",
       "  Obtenla en https://www.planet.com/account/#/ -> My Settings -> API Key")

args     <- commandArgs(trailingOnly = TRUE)
SIMULAR  <- "--simular" %in% args
SOLO_BAJA<- "--descargar" %in% args
SOLO_CO  <- "--coincidentes" %in% args
LOTE     <- 100                       # elementos por pedido
DESTINO  <- "Yuc/imagenes/planet_nuevas"
REGISTRO <- "analisis_satbathy/output/planet_pedidos.csv"

f <- "analisis_satbathy/output/planet_solicitud.csv"
if (!file.exists(f)) stop("Falta ", f, ": ejecuta antes lista_planet_solicitud.R")
sel <- read.csv(f, stringsAsFactors = FALSE)
if (SOLO_CO) sel <- sel[sel$coincide != "-", ]
sel <- sel[sel$ya_descargada != "si", ]
if (!nrow(sel)) stop("No queda ninguna escena por pedir con esos criterios.")

## ---- recorte al área de estudio ----------------------------------------
caja <- st_bbox(c(xmin = -89.84004, xmax = -89.65617,
                  ymin = 21.26287,  ymax = 21.672), crs = st_crs(4326))
recorte <- list(type = "Polygon", coordinates = list(lapply(
  list(c(caja["xmin"], caja["ymin"]), c(caja["xmax"], caja["ymin"]),
       c(caja["xmax"], caja["ymax"]), c(caja["xmin"], caja["ymax"]),
       c(caja["xmin"], caja["ymin"])), unname)))

cat(sprintf("\nEscenas a pedir: %d en %d fechas\n",
            nrow(sel), length(unique(sel$fecha))))
cat(sprintf("  standard: %d | test: %d\n",
            sum(sel$calidad == "standard", na.rm = TRUE),
            sum(sel$calidad == "test", na.rm = TRUE)))
cat(sprintf("  lotes de %d -> %d pedido(s)\n", LOTE, ceiling(nrow(sel) / LOTE)))
cat(sprintf("  producto: analytic_8b_sr_udm2, recortado al AOI\n"))
cat(sprintf("  destino: %s\n", DESTINO))
cat(sprintf("  tamaño estimado: ~%.0f GB (a ~140 MB por escena)\n",
            nrow(sel) * 0.14))

if (SIMULAR) {
  cat("\n--simular: no se ha pedido nada.\n")
  print(utils::head(sel[, c("fecha", "id", "calidad", "pct_utilizable")], 15),
        row.names = FALSE)
  quit(save = "no")
}

dir.create(DESTINO, recursive = TRUE, showWarnings = FALSE)

if (SOLO_BAJA) {
  if (!file.exists(REGISTRO)) stop("No hay ", REGISTRO, ": nada que descargar.")
  reg <- read.csv(REGISTRO, stringsAsFactors = FALSE)
  message("Modo --descargar: ", nrow(reg), " pedido(s) registrado(s); no se pide nada nuevo.")
} else {

grupos <- split(sel$id, ceiling(seq_len(nrow(sel)) / LOTE))
reg <- data.frame()

for (k in seq_along(grupos)) {
  ids <- grupos[[k]]
  nombre <- sprintf("batimetria_yuc_%s_lote%02d", format(Sys.Date(), "%Y%m%d"), k)
  cuerpo <- list(
    name = nombre,
    products = list(list(item_ids = as.list(ids),
                         item_type = "PSScene",
                         product_bundle = "analytic_8b_sr_udm2")),
    tools = list(list(clip = list(aoi = recorte))),
    delivery = list(archive_type = "zip", single_archive = TRUE))

  r <- request("https://api.planet.com/compute/ops/orders/v2") |>
    req_auth_basic(key, "") |> req_body_json(cuerpo) |>
    req_error(is_error = function(x) FALSE) |> req_perform()

  if (resp_status(r) >= 400) {
    message("Lote ", k, " RECHAZADO (", resp_status(r), "): ", resp_body_string(r))
    next
  }
  oid <- resp_body_json(r)$id
  message(sprintf("Lote %d pedido: %d escenas | id %s", k, length(ids), oid))
  reg <- rbind(reg, data.frame(lote = k, order_id = oid, n = length(ids),
                               nombre = nombre, fecha_pedido = as.character(Sys.time())))
}

if (!nrow(reg)) stop("Ningún lote se aceptó.")
## se ANADEN al registro en vez de sobrescribirlo: si no, un pedido anterior
## todavia sin descargar queda huerfano
if (file.exists(REGISTRO)) {
  prev <- read.csv(REGISTRO, stringsAsFactors = FALSE)
  reg <- rbind(prev[!(prev$order_id %in% reg$order_id), ], reg)
}
write.csv(reg, REGISTRO, row.names = FALSE)
cat("\nPedidos registrados en ", REGISTRO, "\n", sep = "")
}

## ---- espera y descarga -------------------------------------------------
for (i in seq_len(nrow(reg))) {
  oid <- reg$order_id[i]
  message("\nEsperando lote ", reg$lote[i], " (", oid, ")...")
  repeat {
    e <- request(paste0("https://api.planet.com/compute/ops/orders/v2/", oid)) |>
      req_auth_basic(key, "") |> req_perform() |> resp_body_json()
    if (e$state %in% c("success", "partial", "failed")) break
    message("  estado: ", e$state, " — reintento en 60 s")
    Sys.sleep(60)
  }
  if (e$state == "failed") { message("  lote FALLIDO, se omite"); next }

  for (lnk in e$`_links`$results) {
    if (!grepl("\\.zip$", lnk$name)) next
    ## Planet nombra output.zip el archivo de TODOS los pedidos, asi que hay que
    ## renombrarlo por lote; si no, el segundo se salta por "ya existe" y se
    ## pierde silenciosamente.
    destino <- file.path(DESTINO, sprintf("%s_%s.zip", reg$nombre[i],
                                          substr(oid, 1, 8)))
    if (file.exists(destino)) { message("  ya existe: ", basename(destino)); next }

    ## Descarga reanudable a un .part que solo se renombra al terminar bien.
    ## Son archivos de varios GB y la conexion se corta: escribir directamente
    ## sobre el nombre final dejaba un truncado que la siguiente ejecucion
    ## tomaba por completo y saltaba.
    parte <- paste0(destino, ".part")
    for (intento in 1:5) {
      ya <- if (file.exists(parte)) file.size(parte) else 0
      message(sprintf("  descargando %s (intento %d/5%s)...", basename(destino),
                      intento, if (ya > 0) sprintf(", reanudando desde %.1f GB",
                                                   ya / 2^30) else ""))
      ## sin timeout total (son archivos de decenas de GB): se aborta solo si
      ## la conexion se queda parada de verdad -menos de 100 B/s durante 5 min-
      ## y entonces el bucle reanuda desde donde iba
      r <- curl::multi_download(lnk$location, parte, resume = TRUE,
                                progress = TRUE,
                                low_speed_limit = 100, low_speed_time = 300)
      if (isTRUE(r$success)) break
      message("    fallo: ", r$error, " — reintento en 30 s")
      Sys.sleep(30)
    }
    if (!isTRUE(r$success)) {
      message("  NO se pudo completar; el .part se conserva para reanudar.")
      next
    }
    ## comprobacion de integridad antes de dar por buena la descarga
    ok <- suppressWarnings(
      system2("unzip", c("-t", "-qq", shQuote(parte)), stdout = FALSE,
              stderr = FALSE) == 0)
    if (!ok) {
      message("  zip corrupto, se conserva el .part para reanudar")
      next
    }
    file.rename(parte, destino)
    message(sprintf("  ok: %s (%.1f GB)", basename(destino),
                    file.size(destino) / 2^30))
  }
  message("  lote ", reg$lote[i], " completado (", e$state, ")")
}

cat("\nDescarga terminada en ", DESTINO, "\n", sep = "")
cat("Descomprime y actualiza analisis_satbathy/config_imagenes.R con las nuevas escenas.\n")
