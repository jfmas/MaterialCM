## Modelo mixto con los TRES sensores (Landsat, Sentinel-2, SuperDove), sobre
## el conjunto admitido por el criterio de cobertura estratificada (2-16 m,
## >=15 sondeos/estrato): fig9_admitidas.csv, 148 filas / 37 imágenes.
##
## Sustituye al modelo binario de analisis_estadistico.R (Landsat vs
## Sentinel-2, sensor2 <- ifelse(grepl("Landsat",...), "Landsat","Sentinel-2")),
## que agrupaba incorrectamente a SuperDove dentro de "Sentinel-2" si se le
## aplicaba al catálogo con los tres sensores.
suppressMessages(library(lme4))
sig <- function(p) if (is.na(p)) "" else if (p < 0.001) "***" else
  if (p < 0.01) "**" else if (p < 0.05) "*" else "ns"

d <- read.csv("analisis_satbathy/output/fig9_admitidas.csv", stringsAsFactors = FALSE)
d$sensor3 <- factor(ifelse(grepl("Landsat", d$sensor), "Landsat", d$sensor),
                    levels = c("Landsat", "Sentinel-2", "SuperDove"))  # Landsat = referencia
d$image <- paste(d$sensor, d$date)

cat("Observaciones:", nrow(d), " | imágenes:", length(unique(d$image)), "\n")
print(table(d$sensor3) / 4)  # ~ n de fechas por sensor (algunas fechas <4 combos)
cat("\n")

per_img <- aggregate(cbind(r2, rmse, mae, turbidity) ~ image + sensor3, d, mean)
cat("---- Medias por sensor (nivel imagen, n=", nrow(per_img), ") ----\n", sep = "")
for (v in c("r2", "rmse", "mae", "turbidity")) {
  ag <- aggregate(per_img[[v]], list(per_img$sensor3), function(x) c(m = mean(x), sd = sd(x)))
  cat(sprintf("%-9s Landsat: %.3f±%.3f  Sentinel-2: %.3f±%.3f  SuperDove: %.3f±%.3f\n",
      v, ag$x[1,1], ag$x[1,2], ag$x[2,1], ag$x[2,2], ag$x[3,1], ag$x[3,2]))
}

cat("\n=========================================================\n")
cat("MODELO MIXTO (n=", nrow(d), ", imagen como efecto aleatorio)\n", sep = "")
cat("=========================================================\n")
for (Y in c("r2", "rmse", "mae")) {
  f <- as.formula(paste(Y, "~ sensor3 + turbidity + (1|image)"))
  m <- suppressWarnings(suppressMessages(lmer(f, d)))
  co <- summary(m)$coefficients
  cat(sprintf("\n[%s]\n", toupper(Y)))
  for (i in seq_len(nrow(co))[-1]) {
    p <- 2 * pnorm(-abs(co[i, "t value"]))
    cat(sprintf("  %-20s beta=%+.3f  SE=%.3f  p=%.4f %s\n",
        rownames(co)[i], co[i, "Estimate"], co[i, "Std. Error"], p, sig(p)))
  }
}

cat("\n=========================================================\n")
cat("CONTRASTE SuperDove vs Sentinel-2 (releveling)\n")
cat("=========================================================\n")
d$sensor3_sd <- relevel(d$sensor3, ref = "SuperDove")
for (Y in c("r2", "rmse", "mae")) {
  f <- as.formula(paste(Y, "~ sensor3_sd + turbidity + (1|image)"))
  m <- suppressWarnings(suppressMessages(lmer(f, d)))
  co <- summary(m)$coefficients
  fila <- grep("Sentinel-2", rownames(co))
  p <- 2 * pnorm(-abs(co[fila, "t value"]))
  cat(sprintf("  [%s] Sentinel-2 vs SuperDove: beta=%+.3f  SE=%.3f  p=%.4f %s\n",
      toupper(Y), co[fila, "Estimate"], co[fila, "Std. Error"], p, sig(p)))
}

cat("\n=========================================================\n")
cat("Correlaciones turbidez (n=", nrow(d), ")\n", sep = "")
cat("=========================================================\n")
cat(sprintf("cor(turbidez, R2)   = %.3f\n", cor(d$turbidity, d$r2)))
cat(sprintf("cor(turbidez, RMSE) = %.3f\n", cor(d$turbidity, d$rmse)))
cat(sprintf("cor(turbidez, MAE)  = %.3f\n", cor(d$turbidity, d$mae)))

## rango de turbidez por sensor, para ver si esta confundida con el sensor
cat("\nrango de turbidez por sensor:\n")
print(tapply(per_img$turbidity, per_img$sensor3, function(x) round(range(x), 2)))
