## Fig 8: mapa batimétrico + residuales observados (como describe el texto).
## Residual = profundidad observada - estimada (positivo: el modelo la sobrestima
## en magnitud, es decir estima el fondo más profundo de lo medido).
suppressMessages({ library(satbathy); library(terra) })
source("analisis_satbathy/mask_helpers.R")
out <- "analisis_satbathy/output"; N <- 10000; CENTS <- c(0.01, 0.99)

train <- vect("Yuc/training_points.gpkg"); train$depth <- -as.numeric(train$depthc)
test  <- vect("Yuc/test_points.gpkg");     test$depth  <- -as.numeric(test$depthc)
pts   <- rbind(train, test)

S <- list(
 L8 =list(sensor="Landsat 8", date="8/3/2025", path="Landsat03a05/LC08_L2SP_020045_20250308_02_T1.tif", blue=1,green=3,red=2,nir=4, mask="qa", qa=5),
 L9 =list(sensor="Landsat 9", date="3/5/2025", path="Landsat03a05/LC09_L2SP_020045_20250503_02_T1.tif", blue=1,green=3,red=2,nir=4, mask="qa", qa=5),
 S2a=list(sensor="Sentinel-2",date="9/3/2025", path="Sentinel03a05/im2025-03-09.tif", blue=2,green=3,red=4,nir=8, mask="scl", scl="Sentinel03a05/scl2025-03-09.tif"),
 S2b=list(sensor="Sentinel-2",date="3/5/2025", path="Sentinel03a05/im2025-05-03.tif", blue=2,green=3,red=4,nir=8, mask="scl", scl="Sentinel03a05/scl2025-05-03.tif"),
 P1 =list(sensor="SuperDove", date="8/3/2025", path="Yuc/imagenes/cortes/PlanetScope_20250308_165229_70_24f6_L2R.tif", blue=1,green=2,red=3,nir=4, mask="ndwi"),
 P2 =list(sensor="SuperDove", date="4/5/2025", path="Yuc/imagenes/cortes/PlanetScope_2522_2025_05_04_17_01_18_L2R.tif", blue=1,green=2,red=3,nir=4, mask="ndwi"))

brk <- c(-Inf,-2,-1,-0.5,0.5,1,2,Inf)
cols <- c("#67001f","#d6604d","#f4a582","#f7f7f7","#92c5de","#4393c3","#053061")

for (nm in names(S)) {
  cfg <- S[[nm]]
  img <- lowpass_filter(read_masked(cfg))
  r   <- log_ratio(img, "blue", "green", n=N, trim=CENTS)
  tt  <- extract_ratio(r, train, "depth")
  md  <- optimal_max_depth(tt); best <- md$max_depth[which.max(md$r2)]
  fit <- fit_bathymetry(tt, max_depth=best)
  dep <- predict_bathymetry(fit, r)
  ## residuales en los puntos dentro del rango válido
  v <- terra::extract(r, pts, ID=FALSE)[,1]
  ok <- !is.na(v) & pts$depth > best
  pr <- as.numeric(stats::predict(fit$model, newdata=data.frame(ratio=v[ok])))
  res <- pts$depth[ok] - pr
  pv <- pts[ok, ]; pv$res <- res
  ci <- cut(res, brk, labels=FALSE)

  ## sólo el rango de validez del modelo (evita profundidades positivas sin sentido)
  dep[dep > 0 | dep < best] <- NA
  ## los puntos van en lon/lat y el raster puede estar en UTM: proyectar antes de
  ## calcular el encuadre, si no la ventana cae fuera del raster y no se dibuja nada
  pv <- project(pv, crs(dep))
  e <- ext(pv); mx <- 0.35 * (xmax(e) - xmin(e)); my <- 0.20 * (ymax(e) - ymin(e))
  ez <- ext(xmin(e) - mx, xmax(e) + mx, ymin(e) - my, ymax(e) + my)

  png(file.path(out, paste0("fig_depth_", nm, ".png")), width=14, height=12, units="cm", res=200)
  ## escala de profundidad COMÚN a los seis paneles, para que sean comparables
  plot(dep, ext=ez, range=c(-15, 0), main=paste0("Profundidad (m) - ", cfg$sensor, " ", cfg$date),
       col=grDevices::hcl.colors(50, "Blues", rev=TRUE))
  points(pv, bg=cols[ci], col="grey20", pch=21, lwd=0.3, cex=0.75)
  u <- par("usr")   # colocar la leyenda dentro del marco del mapa
  legend(x=u[1] + 0.02*(u[2]-u[1]), y=u[3] + 0.34*(u[4]-u[3]), xpd=NA,
         legend=c("< -2","-2 a -1","-1 a -0.5","-0.5 a 0.5","0.5 a 1","1 a 2","> 2"),
         pt.bg=cols, col="grey20", pch=21, cex=0.5, pt.cex=0.8, ncol=2,
         bty="o", bg="white", box.col="grey60", title="Residual (m)")
  dev.off()
  message("Fig 8 ", nm, " ok (n res=", sum(ok), ", RMSE=", round(sqrt(mean(res^2)),3), ")")
}
message("Fig 8 con residuales lista.")
