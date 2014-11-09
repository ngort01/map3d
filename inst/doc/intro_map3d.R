### R code from vignette source 'intro_map3d.Rnw'

###################################################
### code chunk number 1: intro_map3d.Rnw:58-59
###################################################
library(map3d)


###################################################
### code chunk number 2: intro_map3d.Rnw:69-73
###################################################
map <- openmap(c(52.05, 7.5), c(51.9, 7.75),12, type="osm")
# Reproject if you want. Takes some time!
map <- openproj(map, CRS("+init=EPSG:4326"))
map3d(map)


###################################################
### code chunk number 3: intro_map3d.Rnw:78-82
###################################################
par3d("userMatrix" = matrix(c(1,0,0,0, 0,0.7,0.7,0, 0,-0.7,0.7,0, 0,0,0,1), 
                            nrow = 4, ncol = 4, byrow = TRUE))
par3d("zoom" = 1)
map3d(map, open3d = FALSE)


###################################################
### code chunk number 4: intro_map3d.Rnw:104-115
###################################################
crs = CRS("+init=epsg:28992 +proj=sterea +lat_0=52.15616055555555 
          +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 
          +y_0=463000 +ellps=bessel 
          +towgs84=565.417,50.3319,465.552,-0.398957,0.343988,-1.8774,4.0725 
          +units=m +no_defs")
## create SpatialPointsDataFrame
data("meuse")
coordinates(meuse) <- ~x+y
proj4string(meuse) <- crs
## plot
spplot3d(meuse, att = "zinc", grid = TRUE)


###################################################
### code chunk number 5: intro_map3d.Rnw:120-124
###################################################
par3d("userMatrix" = matrix(c(1,0,0,0, 0,0.7,0.7,0, 0,-0.7,0.7,0, 0,0,0,1), 
                            nrow = 4, ncol = 4, byrow = TRUE))
par3d("zoom" = 0.95)
spplot3d(meuse, att = "zinc", grid=TRUE, open3d = FALSE)


###################################################
### code chunk number 6: intro_map3d.Rnw:135-136
###################################################
spplot3d(meuse, att = "zinc", col = c("yellow", "red"), type = "bing")


###################################################
### code chunk number 7: intro_map3d.Rnw:141-145
###################################################
par3d("userMatrix" = matrix(c(1,0,0,0, 0,0.7,0.7,0, 0,-0.7,0.7,0, 0,0,0,1), 
                            nrow = 4, ncol = 4, byrow = TRUE))
par3d("zoom" = 0.95)
spplot3d(meuse, att = "zinc", col = c("yellow", "red"), type="bing", open3d = FALSE)


###################################################
### code chunk number 8: intro_map3d.Rnw:155-157
###################################################
data("gewaesserlinien")
spplot3d(gewaesserlinien, "glName", col = c("blue", "cyan"), CRS = CRS("+init=EPSG:4326"))


###################################################
### code chunk number 9: intro_map3d.Rnw:162-166
###################################################
par3d("userMatrix" = matrix(c(1,0,0,0, 0,0.7,0.7,0, 0,-0.7,0.7,0, 0,0,0,1), 
                            nrow = 4, ncol = 4, byrow = TRUE))
par3d("zoom" = 1)
spplot3d(gewaesserlinien, "glName", col = c("blue", "cyan"), CRS = CRS("+init=EPSG:4326"), open3d = FALSE)


###################################################
### code chunk number 10: intro_map3d.Rnw:181-183
###################################################
data("vg2500_bld")
spplot3d(vg2500_bld, "SHAPE_AREA", CRS = CRS("+init=EPSG:4326"))


###################################################
### code chunk number 11: intro_map3d.Rnw:188-192
###################################################
par3d("userMatrix" = matrix(c(1,0,0,0, 0,0.7,0.7,0, 0,-0.7,0.7,0, 0,0,0,1), 
                            nrow = 4, ncol = 4, byrow = TRUE))
par3d("zoom" = 1)
spplot3d(vg2500_bld, "SHAPE_AREA", CRS = CRS("+init=EPSG:4326"), open3d = FALSE)


###################################################
### code chunk number 12: intro_map3d.Rnw:202-208
###################################################
data("meuse.grid")
coordinates(meuse.grid) <- ~x+y
gridded(meuse.grid) <- TRUE
proj4string(meuse.grid) <- crs
spplot3d(meuse.grid, att = "ffreq", col = c("red", "blue", "green"), 
         CRS = CRS("+init=EPSG:4326"))


###################################################
### code chunk number 13: intro_map3d.Rnw:213-217
###################################################
par3d("userMatrix" = matrix(c(1,0,0,0, 0,0.7,0.7,0, 0,-0.7,0.7,0, 0,0,0,1), 
                            nrow = 4, ncol = 4, byrow = TRUE))
par3d("zoom" = 0.9)
spplot3d(meuse.grid, att = "ffreq", col = c("red", "blue", "green"), CRS = CRS("+init=EPSG:4326"), open3d = FALSE)


###################################################
### code chunk number 14: intro_map3d.Rnw:230-239
###################################################
data("meuse.riv")
meuse.riv <- SpatialPolygons(list(Polygons(list(Polygon(meuse.riv)),"meuse.riv")))
proj4string(meuse.riv) <- crs
## First plot, ADD = FALSE
spplot3d(meuse.riv, col = "blue", CRS = CRS("+init=EPSG:4326"))
## Add SpatialPoints and SpatialGrid
spplot3d(meuse, "zinc", ADD = TRUE, CRS = CRS("+init=EPSG:4326"))
spplot3d(meuse.grid, "dist", col=c("black", "white"), ADD = TRUE, 
         CRS = CRS("+init=EPSG:4326"))


###################################################
### code chunk number 15: intro_map3d.Rnw:244-252
###################################################
par3d("userMatrix" = matrix(c(1,0,0,0, 0,0.7,0.7,0, 0,-0.7,0.7,0, 0,0,0,1), 
                            nrow = 4, ncol = 4, byrow = TRUE))
par3d("zoom" = 0.95)
## First plot, ADD = FALSE
spplot3d(meuse.riv, col = "blue", CRS =CRS("+init=EPSG:4326"), open3d = FALSE)
## Add SpatialPoints and SpatialGrid
spplot3d(meuse, "zinc", ADD = TRUE, CRS = CRS("+init=EPSG:4326"), open3d = FALSE)
spplot3d(meuse.grid, "dist", col = c("black", "white"), ADD = TRUE, CRS = CRS("+init=EPSG:4326"), open3d = FALSE)


