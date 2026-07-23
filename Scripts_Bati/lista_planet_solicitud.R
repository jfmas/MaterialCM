########################################################################
## Lista de escenas PlanetScope a solicitar, filtradas por calidad.
##
## Parte de analisis_satbathy/output/planet_disponibles.csv (generado por
## Scripts_Bati/busca_planet.R) y aplica los criterios de calidad acordados.
## No descarga nada ni consume cuota: produce la lista de IDs para el pedido.
##
## Criterios:
##   1. calidad: por defecto solo "standard". Las marcadas "test" no pasaron el
##      control de calidad completo de Planet (suele ser calibración radiométrica
##      o geométrica no verificada). Con incluir_test=TRUE se añaden, marcadas
##      como tales, para decidir tras inspeccionarlas. Puede interesar cuando una
##      fecha "test" coincide con otros sensores y esa coincidencia vale más que
##      la certificación, documentando la salvedad.
##   2. % utilizable >= UMBRAL_UTIL   (banda "clear" del UDM2)
##   3. nubosidad <= UMBRAL_NUBE
##   4. cobertura de la zona de sondeos >= UMBRAL_COB, evaluada POR FECHA:
##      las escenas PlanetScope son franjas y varias de una misma pasada cubren
##      lo que una sola no. Sin este filtro la lista es casi inutil: de las 332
##      escenas encontradas, 153 no tocan los transectos y la mediana de
##      cobertura por escena es del 2%.
##
## Uso:
##   Rscript Scripts_Bati/lista_planet_solicitud.R              # solo standard
##   Rscript Scripts_Bati/lista_planet_solicitud.R 90 5 test    # + las "test"
##   Rscript Scripts_Bati/lista_planet_solicitud.R 95           # >=95% utilizable
##
## Salida:
##   analisis_satbathy/output/planet_solicitud.csv   detalle por escena
##   analisis_satbathy/output/planet_solicitud_ids.txt  IDs, uno por linea
########################################################################

setwd("/home/jf/pcloud_sync/Batimetria")
args <- commandArgs(trailingOnly = TRUE)
UMBRAL_UTIL <- if (length(args) >= 1) as.numeric(args[1]) else 90
UMBRAL_NUBE <- if (length(args) >= 2) as.numeric(args[2]) else 5
UMBRAL_COB  <- 75
INCLUIR_TEST <- length(args) >= 3 && tolower(args[3]) %in% c("test", "si", "true")

f <- "analisis_satbathy/output/planet_disponibles.csv"
if (!file.exists(f)) stop("Falta ", f, ": ejecuta antes Scripts_Bati/busca_planet.R")
d <- read.csv(f, stringsAsFactors = FALSE)

## fechas de las imágenes Landsat/Sentinel ya procesadas, para marcar coincidencias
f9 <- "analisis_satbathy/output/fig9_full_resultados.csv"
coinc <- if (file.exists(f9)) {
  x <- read.csv(f9, stringsAsFactors = FALSE)
  stats::aggregate(sensor ~ date, unique(x[, c("sensor", "date")]),
                   function(s) paste(sort(unique(s)), collapse = " + "))
} else NULL

YA <- c("2025-03-08", "2025-05-04")   # fechas ya descargadas

## cobertura por fecha: se usa la union si busca_planet.R la calculo, y si no
## la suma de las franjas, que la sobreestima cuando se solapan
por_fecha <- "analisis_satbathy/output/planet_por_fecha.csv"
cob <- NULL
if (file.exists(por_fecha)) {
  pf <- read.csv(por_fecha, stringsAsFactors = FALSE)
  if ("cobertura_union" %in% names(pf)) {
    cob <- stats::setNames(pf$cobertura_union, pf$fecha); tipo_cob <- "union"
  }
}
if (is.null(cob)) {
  agg <- stats::aggregate(cobertura_sondeos ~ fecha, d,
                          function(x) min(100, sum(x, na.rm = TRUE)))
  cob <- stats::setNames(agg$cobertura_sondeos, agg$fecha); tipo_cob <- "suma (aprox.)"
  message("Aviso: sin cobertura_union; se usa la suma de franjas, que sobreestima.\n",
          "  Vuelve a ejecutar busca_planet.R para el calculo exacto.")
}
d$cobertura_fecha <- round(as.numeric(cob[d$fecha]), 1)

cal_ok <- if (INCLUIR_TEST) d$calidad %in% c("standard", "test") else
                            d$calidad == "standard"
## which() evita que un NA en los porcentajes (dos escenas no traen
## clear_percent) genere filas vacias al subconjuntar con un indice logico
sel <- d[which(cal_ok &
               d$pct_utilizable >= UMBRAL_UTIL &
               d$pct_nube <= UMBRAL_NUBE &
               d$cobertura_fecha >= UMBRAL_COB), ]
descartadas_na <- sum(is.na(d$pct_utilizable) | is.na(d$pct_nube))
sel <- sel[order(sel$fecha, sel$hora_utc), ]

sel$coincide <- if (!is.null(coinc))
  coinc$sensor[match(sel$fecha, coinc$date)] else NA_character_
sel$coincide[is.na(sel$coincide)] <- "-"
sel$ya_descargada <- ifelse(sel$fecha %in% YA, "si", "")

cols <- c("fecha", "hora_utc", "id", "calidad", "pct_utilizable", "pct_nube",
          "pct_bruma", "cobertura_fecha", "angulo_off", "coincide", "ya_descargada")
write.csv(sel[, cols], "analisis_satbathy/output/planet_solicitud.csv",
          row.names = FALSE)
writeLines(sel$id, "analisis_satbathy/output/planet_solicitud_ids.txt")

cat(sprintf("\nCriterios: calidad %s | utilizable >= %g%% | nube <= %g%%\n",
            if (INCLUIR_TEST) "standard + test" else "solo standard",
            UMBRAL_UTIL, UMBRAL_NUBE))
cat(sprintf("  cobertura de la zona de sondeos >= %g%% por fecha (%s)\n",
            UMBRAL_COB, tipo_cob))
if (INCLUIR_TEST)
  cat(sprintf("  de las seleccionadas, %d son de calidad 'test' (revisar antes de usar)\n",
              sum(sel$calidad == "test", na.rm = TRUE)))
if (descartadas_na)
  cat(sprintf("  %d escena(s) sin metadatos de calidad, excluidas\n", descartadas_na))
cat(sprintf("Escenas seleccionadas: %d en %d fechas\n",
            nrow(sel), length(unique(sel$fecha))))
cat(sprintf("De ellas, coinciden con Landsat/Sentinel: %d escenas en %d fechas\n",
            sum(sel$coincide != "-"),
            length(unique(sel$fecha[sel$coincide != "-"]))))

cat("\n=== ESCENAS A SOLICITAR ===\n")
print(sel[, cols], row.names = FALSE)

cat("\nIDs para el pedido: analisis_satbathy/output/planet_solicitud_ids.txt\n")
cat("Detalle: analisis_satbathy/output/planet_solicitud.csv\n")

if (INCLUIR_TEST) {
  ## las "test" que coinciden con otro sensor son las que compensan el riesgo
  t <- sel[sel$calidad == "test" & sel$coincide != "-", ]
  if (nrow(t)) {
    cat("\n=== escenas 'test' que COINCIDEN con Landsat/Sentinel ===\n")
    print(t[, c("fecha", "id", "pct_utilizable", "angulo_off", "coincide")],
          row.names = FALSE)
  }
}
