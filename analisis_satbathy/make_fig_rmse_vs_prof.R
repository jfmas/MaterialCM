## Figura complementaria a la Fig 5 (B-52): error en muestra (RMSE) frente a
## la profundidad maxima, para las 4 combinaciones glint/filtro, un panel por
## imagen. Lee las curvas ya calculadas por make_fig6_v2.R (fig6_curvas_*.rds),
## que contienen max_depth, n, r2 y rmse; aqui solo se grafica el RMSE.
## Acompana a la Fig 5 (R2) para mostrar que un R2 global alto no implica un
## error bajo en todo el intervalo de profundidad.
OUT <- "analisis_satbathy/output"
RUN <- c("L8", "L9", "S2a", "S2b", "P1", "P2")
titulos <- c(L8 = "Landsat 8 (8/3/2025)",  L9 = "Landsat 9 (3/5/2025)",
             S2a = "Sentinel-2 (9/3/2025)", S2b = "Sentinel-2 (3/5/2025)",
             P1 = "SuperDove (8/3/2025)",   P2 = "SuperDove (4/5/2025)")
combos <- list(
  list(lab = "Ninguna de las dos",         col = "black",       lty = 1),
  list(lab = "Correccion brillo",          col = "red",         lty = 2),
  list(lab = "Filtro",                     col = "blue",        lty = 3),
  list(lab = "Correccion brillo y filtro", col = "forestgreen", lty = 1))

cur_de <- function(nm) readRDS(file.path(OUT, paste0("fig6_curvas_", nm, ".rds")))
ymax <- max(unlist(lapply(RUN, function(nm)
  unlist(lapply(cur_de(nm), function(d) d$rmse)))), na.rm = TRUE)
ymax <- ceiling(ymax * 10) / 10

png(file.path(OUT, "fig_rmse_vs_profmax.png"),
    width = 18, height = 22, units = "cm", res = 200)
op <- par(mfrow = c(3, 2), mar = c(4, 4, 2.5, 1))
for (nm in RUN) {
  cur <- cur_de(nm)
  plot(NA, xlim = c(-17.5, -4.5), ylim = c(0, ymax),
       xlab = "Profundidad maxima (m)", ylab = "RMSE (m)", main = titulos[[nm]])
  for (cb in combos) {
    d <- cur[[cb$lab]]
    lines(d$max_depth, d$rmse, col = cb$col, lty = cb$lty, lwd = 2)
  }
  if (nm == "L8")
    legend("topright", legend = sapply(combos, `[[`, "lab"),
           col = sapply(combos, `[[`, "col"), lty = sapply(combos, `[[`, "lty"),
           lwd = 2, bty = "n", cex = 0.85)
}
par(op); dev.off()
cat("Figura RMSE vs profundidad:", file.path(OUT, "fig_rmse_vs_profmax.png"), "\n")
