## Figura complementaria a la Fig 8 (B-54): efecto de la turbidez sobre el
## desempeno, en dos paneles: R2 (izquierda, = Fig 8 actual) y RMSE (derecha,
## panel nuevo) frente a la turbidez, para las 37 imagenes admitidas y sus 4
## combinaciones de preprocesamiento. Lee fig9_admitidas.csv (ya calculado por
## aplica_admision.R). Muestra que el error absoluto (RMSE) tambien aumenta con
## la turbidez, de forma complementaria a la caida del R2.
OUT <- "analisis_satbathy/output"
adm <- read.csv(file.path(OUT, "fig9_admitidas.csv"), check.names = FALSE)
adm$family <- ifelse(adm$sensor %in% c("Landsat 8", "Landsat 9"), "Landsat", adm$sensor)
fam_col    <- c("Landsat" = "blue", "Sentinel-2" = "forestgreen", "SuperDove" = "darkorange")
sensor_pch <- c("Landsat 8" = 16, "Landsat 9" = 17, "Sentinel-2" = 15, "SuperDove" = 18)

png(file.path(OUT, "fig_turbidez_r2_rmse.png"),
    width = 22, height = 11, units = "cm", res = 200)
op <- par(mfrow = c(1, 2), mar = c(4.2, 4.3, 1, 1))
## panel a) R2 vs turbidez
plot(adm$turbidity, adm$r2, col = fam_col[adm$family], pch = sensor_pch[adm$sensor],
     cex = 1.1, xlab = "Turbidez", ylab = expression(R^2))
legend("bottomleft", legend = names(sensor_pch), pch = sensor_pch,
       col = fam_col[c("Landsat", "Landsat", "Sentinel-2", "SuperDove")],
       bty = "o", bg = "white", box.col = "grey40", cex = 0.85)
mtext("a)", adj = 0, line = -1.2, font = 2)
## panel b) RMSE vs turbidez (nuevo)
plot(adm$turbidity, adm$rmse, col = fam_col[adm$family], pch = sensor_pch[adm$sensor],
     cex = 1.1, xlab = "Turbidez", ylab = "RMSE (m)")
mtext("b)", adj = 0, line = -1.2, font = 2)
par(op); dev.off()

cat(sprintf("cor(turbidez, R2)   = %.3f\n", cor(adm$turbidity, adm$r2)))
cat(sprintf("cor(turbidez, RMSE) = %.3f\n", cor(adm$turbidity, adm$rmse)))
cat("Figura turbidez R2/RMSE:", file.path(OUT, "fig_turbidez_r2_rmse.png"), "\n")
