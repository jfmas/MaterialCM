## =====================================================================
## Turbidity time series for image-date selection
## (bitacora step 2; manuscript Fig 4) using satbathy::turbidity_index.
##
## For every Sentinel-2 date in Yuc/serieSYuc (im<date>.tif) the cloud mask
## (cl<date>.tif) is applied, the Frohn & Autrey (2009) turbidity index is
## computed with satbathy, and its median over the study polygon is tracked
## through time to find the least-turbid period.
##
## Series band order (from Bands-SR_1-8A.js): B1,B2,B3,B4,B5,B6,B7,B8,B8A
##   -> blue = 2, green = 3, red = 4
## =====================================================================

suppressMessages({
  library(satbathy)
  library(terra)
})
setwd("/home/jf/pcloud_sync/Batimetria")
data_dir <- "."
serie_dir <- file.path(data_dir, "Yuc/serieSYuc")
out_dir   <- file.path("analisis_satbathy", "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

BLUE <- 2; GREEN <- 3; RED <- 4     # band indices in the 9-band stack

## study-area polygon over which turbidity is summarised
aoi <- vect(file.path(data_dir, "Yuc/casitodo.gpkg"))

## optional subset for a quick test, e.g. SATBATHY_NDATES=20
n_max <- as.integer(Sys.getenv("SATBATHY_NDATES", "0"))

im_files <- sort(list.files(serie_dir, pattern = "^im.*\\.tif$", full.names = TRUE))
if (n_max > 0) im_files <- im_files[seq_len(min(n_max, length(im_files)))]
dates <- as.Date(sub("^im", "", sub("\\.tif$", "", basename(im_files))))

turb_median <- rep(NA_real_, length(im_files))
for (i in seq_along(im_files)) {
  img <- rast(im_files[i])
  ## apply matching cloud mask if present (cl<date>.tif, >0 == cloud)
  clf <- file.path(serie_dir, sub("^im", "cl", basename(im_files[i])))
  if (file.exists(clf)) {
    cl <- rast(clf)
    img[cl > 0] <- NA
  }
  ti <- turbidity_index(img, blue = BLUE, green = GREEN, red = RED)
  ti <- mask(crop(ti, aoi), project(aoi, crs(ti)))
  turb_median[i] <- stats::median(values(ti), na.rm = TRUE)
}

ts <- data.frame(date = dates, turbidity = turb_median)
ts <- ts[!is.na(ts$turbidity), ]
ts <- ts[order(ts$date), ]
write.csv(ts, file.path(out_dir, "fig4_turbidez_serie.csv"), row.names = FALSE)

## simple trailing moving average (n dates)
sma <- function(x, n = 5) as.numeric(stats::filter(x, rep(1 / n, n), sides = 1))

png(file.path(out_dir, "fig4_turbidez_serie.png"),
    width = 16, height = 10, units = "cm", res = 200)
plot(ts$date, ts$turbidity, type = "l", col = "grey40",
     xlab = "", ylab = "Turbidity index")
lines(ts$date, sma(ts$turbidity, 5), col = "red", lwd = 2)
dev.off()

cat("Least-turbid date:", format(ts$date[which.min(ts$turbidity)]), "\n")
cat("Series written:", nrow(ts), "dates. Outputs in", normalizePath(out_dir), "\n")
