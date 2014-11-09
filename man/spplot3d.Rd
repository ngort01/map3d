\docType{methods}
\name{spplot3d}
\alias{spplot3d}
\alias{spplot3d,SpatialGrid-method}
\alias{spplot3d,SpatialGridDataFrame-method}
\alias{spplot3d,SpatialLines-method}
\alias{spplot3d,SpatialLinesDataFrame-method}
\alias{spplot3d,SpatialPixels-method}
\alias{spplot3d,SpatialPixelsDataFrame-method}
\alias{spplot3d,SpatialPoints-method}
\alias{spplot3d,SpatialPointsDataFrame-method}
\alias{spplot3d,SpatialPolygons-method}
\alias{spplot3d,SpatialPolygonsDataFrame-method}
\alias{spplot3d.grid}
\alias{spplot3d.lines}
\alias{spplot3d.points}
\alias{spplot3d.polygons}
\title{Plot a spatial object in 3D.}
\usage{
  spplot3d.points(obj, att = NULL, col = "red",
    height = 0.5, ADD = FALSE,
    type = c("osm", "osm-bw", "maptoolkit-topo", "waze", "mapquest", "mapquest-aerial", 
    "bing", "stamen-toner", "stamen-terrain", "stamen-watercolor", "osm-german", 
    "osm-wanderreitkarte", "mapbox", "esri", "esri-topo", "nps", "apple-iphoto", "skobbler", 
    "cloudmade-<id>", "hillshade", "opencyclemap", "osm-transport", "osm-public-transport", 
    "osm-bbike", "osm-bbike-german"),
    minNumTiles = 10, CRS = NULL, radius = 1, grid = FALSE,
    col_grid = "black", col_axes = "white", open3d = TRUE,
    ...)

  spplot3d.polygons(obj, att = NULL, col = "brown2",
    height = 0.2, ADD = FALSE,
    type = c("osm", "osm-bw", "maptoolkit-topo", "waze", "mapquest", "mapquest-aerial", 
    "bing", "stamen-toner", "stamen-terrain", "stamen-watercolor", "osm-german", 
    "osm-wanderreitkarte", "mapbox", "esri", "esri-topo", "nps", "apple-iphoto", "skobbler", 
    "cloudmade-<id>", "hillshade", "opencyclemap", "osm-transport", "osm-public-transport", 
    "osm-bbike", "osm-bbike-german"),
    minNumTiles = 10, CRS = NULL, grid = FALSE,
    col_grid = "black", col_axes = "white", open3d = TRUE,
    ...)

  spplot3d.grid(obj, att = NULL, col = "red", height = 0.2,
    alpha = 0.5, ADD = FALSE,
    type = c("osm", "osm-bw", "maptoolkit-topo", "waze", "mapquest", "mapquest-aerial", 
    "bing", "stamen-toner", "stamen-terrain", "stamen-watercolor", "osm-german", 
    "osm-wanderreitkarte", "mapbox", "esri", "esri-topo", "nps", "apple-iphoto", "skobbler", 
    "cloudmade-<id>", "hillshade", "opencyclemap", "osm-transport", "osm-public-transport", 
    "osm-bbike", "osm-bbike-german"),
    minNumTiles = 10, CRS = NULL, radius = 1.2,
    grid = FALSE, col_grid = "black", col_axes = "white",
    open3d = TRUE, ...)

  spplot3d.lines(obj, att = NULL, col = "red",
    height = 0.2, ADD = FALSE,
    type = c("osm", "osm-bw", "maptoolkit-topo", "waze", "mapquest", "mapquest-aerial", 
    "bing", "stamen-toner", "stamen-terrain", "stamen-watercolor", "osm-german", 
    "osm-wanderreitkarte", "mapbox", "esri", "esri-topo", "nps", "apple-iphoto", "skobbler", 
    "cloudmade-<id>", "hillshade", "opencyclemap", "osm-transport", "osm-public-transport", 
    "osm-bbike", "osm-bbike-german"),
    minNumTiles = 10, CRS = NULL, grid = FALSE,
    col_grid = "black", col_axes = "white", open3d = TRUE,
    ...)
}
\arguments{
  \item{obj}{spatial object as defined in
  \code{\link[sp]{sp}}.}

  \item{att}{String, name of the attribute that should be
  plotted.}

  \item{col}{vector with one or more colors. If a numeric
  attribute is chosen the function will interpolate the
  colors and create a new color palette which is used to
  color the objects.  If no attribute is chosen objects
  will be colored according to the color vector.}

  \item{height}{height of the spatial objects above the
  map.}

  \item{ADD}{logical, if TRUE CRS must be specified and
  'obj' will be added to an existing plot.}

  \item{type}{the tile server from which to get the map.}

  \item{minNumTiles}{higher numbers increase map detail and
  resolution, but also the time to load the map.}

  \item{CRS}{coordinate reference system, for details see
  \code{\link[sp]{CRS}}.  If NULL the \code{\link{osm}}
  projection is used.}

  \item{radius}{radius of \code{\link{spheres3d}} or
  \code{\link{sprites3d}}.}

  \item{grid}{logical, specifying if a grid should be
  plotted on the map.}

  \item{col_grid}{color of the grid.}

  \item{col_axes}{color of the axes and labels.}

  \item{alpha}{vector of alpha values between 0.0 (fully
  transparent) .. 1.0 (opaque).}

  \item{open3d}{ignore, only needed for snapshot creation
  for the vignette.}

  \item{...}{passes arguments to \code{\link{map3d}}.}
}
\description{
  Plot methods for 3D visulisation of spatial data with and
  without attributes on top of a map.
}
\examples{
crs = CRS("+init=epsg:28992 +proj=sterea +lat_0=52.15616055555555 
+lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel 
+towgs84=565.417,50.3319,465.552,-0.398957,0.343988,-1.8774,4.0725 +units=m +no_defs")

## SpatialPointsDataFrame
data("meuse")
coordinates(meuse) <- ~x+y
proj4string(meuse) <- crs
spplot3d(meuse, att = "zinc", CRS = CRS("+init=EPSG:4326"))
spplot3d(meuse, att = "ffreq", type="bing", grid=TRUE)

\dontrun{
## SpatialGridDataFrame
data("meuse.grid")
coordinates(meuse.grid) <- ~x+y
gridded(meuse.grid) <- TRUE
proj4string(meuse.grid) <- crs
spplot3d(meuse.grid, att = "dist", CRS = CRS("+init=EPSG:4326"))


## using map with high resolution
spplot3d(meuse.grid, att = "ffreq", alpha=0.3, minNumTiles = 30)


## SpatialLinesDataFrame
data("gewaesserlinien")
spplot3d(gewaesserlinien, "glName", col = c("blue", "cyan"))

## SpatiaPolygonsDataFrame
data("vg2500_bld")
spplot3d(vg2500_bld, "SHAPE_AREA", CRS = CRS("+init=EPSG:4326"))}
}
\author{
  Nikolai Gorte <n.gorte@gmail.com>
}

