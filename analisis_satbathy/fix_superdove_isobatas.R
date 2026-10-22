## Re-genera SOLO los paneles SuperDove (P1, P2) de la Fig 7 con isobatas
## suavizadas (batimetria agregada a ~15 m) y recombina los 6 en la rejilla.
suppressMessages({ library(satbathy); library(terra); library(magick) })
terraOptions(todisk = TRUE, memfrac = 0.4)
source("analisis_satbathy/mask_helpers.R")
out <- "analisis_satbathy/output"; N <- 10000; CENTS <- c(0.01, 0.99)
LAND <- "grey82"; ISO <- seq(-14, -2, by = 2)
train <- vect("Yuc/training_points.gpkg"); train$depth <- -as.numeric(train$depthc)
test  <- vect("Yuc/test_points.gpkg");     test$depth  <- -as.numeric(test$depthc)
pts   <- rbind(train, test)
S <- list(
 P1=list(sensor="SuperDove",date="8/3/2025",path="Yuc/imagenes/cortes/PlanetScope_20250308_165229_70_24f6_L2R.tif",blue=1,green=2,red=3,nir=4,mask="ndwi"),
 P2=list(sensor="SuperDove",date="4/5/2025",path="Yuc/imagenes/cortes/PlanetScope_2522_2025_05_04_17_01_18_L2R.tif",blue=1,green=2,red=3,nir=4,mask="ndwi"))
brk <- c(-Inf,-2,-1,-0.5,0.5,1,2,Inf)
cols <- c("#67001f","#d6604d","#f4a582","#f7f7f7","#92c5de","#4393c3","#053061")
for (nm in names(S)) {
  cfg <- S[[nm]]
  img <- lowpass_filter(read_masked(cfg)); r <- log_ratio(img,"blue","green",n=N,trim=CENTS)
  tt  <- extract_ratio(r,train,"depth"); md <- optimal_max_depth(tt); best <- md$max_depth[which.max(md$r2)]
  fit <- fit_bathymetry(tt,max_depth=best); dep <- predict_bathymetry(fit,r)
  v <- terra::extract(r,pts,ID=FALSE)[,1]; ok <- !is.na(v)&pts$depth>best
  pr <- as.numeric(stats::predict(fit$model,newdata=data.frame(ratio=v[ok]))); res<-pts$depth[ok]-pr
  pv<-pts[ok,]; pv$res<-res; ci<-cut(res,brk,labels=FALSE)
  dep[dep>0 | dep<best] <- NA; pv<-project(pv,crs(dep))
  e<-ext(pv); mx<-0.35*(xmax(e)-xmin(e)); my<-0.20*(ymax(e)-ymin(e))
  ez<-ext(xmin(e)-mx,xmax(e)+mx,ymin(e)-my,ymax(e)+my)
  dep_iso <- aggregate(dep, fact=5, fun="mean", na.rm=TRUE)
  png(file.path(out,paste0("fig7_depth_iso_",nm,".png")),width=14,height=12,units="cm",res=200)
  plot(dep,ext=ez,range=c(-15,0),colNA=LAND,main=paste0("Profundidad (m) - ",cfg$sensor," ",cfg$date),col=grDevices::hcl.colors(50,"Blues",rev=TRUE))
  contour(dep_iso,add=TRUE,levels=ISO,col="grey25",lwd=0.5,labcex=0.45,drawlabels=TRUE,method="edge")
  points(pv,bg=cols[ci],col="grey20",pch=21,lwd=0.3,cex=0.75)
  u<-par("usr"); legend(x=u[1]+0.02*(u[2]-u[1]),y=u[3]+0.34*(u[4]-u[3]),xpd=NA,legend=c("< -2","-2 a -1","-1 a -0.5","-0.5 a 0.5","0.5 a 1","1 a 2","> 2"),pt.bg=cols,col="grey20",pch=21,cex=0.5,pt.cex=0.8,ncol=2,bty="o",bg="white",box.col="grey60",title="Residual (m)")
  dev.off(); terra::tmpFiles(remove=TRUE); gc(full=TRUE); message("panel ",nm," suavizado ok")
}
panels <- file.path(out, paste0("fig7_depth_iso_", c("L8","L9","S2a","S2b","P1","P2"), ".png"))
im <- lapply(panels, image_read); fila <- function(a,b) image_append(c(im[[a]],im[[b]]))
image_write(image_append(c(fila(1,2),fila(3,4),fila(5,6)),stack=TRUE), file.path(out,"fig7_isobatas_combinada.png"))
cat("FIN: recombinada con SuperDove suavizado\n")
