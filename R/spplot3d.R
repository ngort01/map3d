#' Plot a spatial object in 3D.
#'
#' Plot methods for 3D visulisation of spatial data with and without attributes on top of a map.
#'
#' @param obj spatial object as defined in \code{\link[sp]{sp}}.
#' @param att String, name of the attribute that should be plotted.
#' @param col vector with one or more colors. If a numeric attribute is chosen the function will 
#'            interpolate the colors and create a new color palette which is used to color the 
#'            objects. 
#'            If no attribute is chosen objects will be colored according to the color vector.
#' @param height height of the spatial objects above the map.
#' @param ADD logical, if TRUE CRS must be specified and 'obj' will be added to an existing plot.
#' @param type the tile server from which to get the map. 
#' @param minNumTiles higher numbers increase map detail and resolution, but also the time to load the map. 
#' @param CRS coordinate reference system, for details see \code{\link[sp]{CRS}}.
#'        If NULL the \code{\link{osm}} projection is used.
#' @param radius radius of \code{\link{spheres3d}} or \code{\link{sprites3d}}.
#' @param grid logical, specifying if a grid should be plotted on the map.
#' @param col_grid color of the grid.
#' @param col_axes color of the axes and labels.
#' @param alpha vector of alpha values between 0.0 (fully transparent) .. 1.0 (opaque).
#' @param open3d ignore, only needed for snapshot creation for the vignette.
#' @param ... passes arguments to \code{\link{map3d}}.
#' 
#' @author Nikolai Gorte <n.gorte@@gmail.com>
#' 
#' @examples
#' crs = CRS("+init=epsg:28992 +proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.417,50.3319,465.552,-0.398957,0.343988,-1.8774,4.0725 +units=m +no_defs")
#' 
#' ## SpatialPointsDataFrame
#' data("meuse")
#' coordinates(meuse) <- ~x+y
#' proj4string(meuse) <- crs
#' spplot3d(meuse, att = "zinc", CRS = CRS("+init=EPSG:4326"))
#' spplot3d(meuse, att = "ffreq", type="bing", grid=TRUE)
#' 
#' \dontrun{
#' ## SpatialGridDataFrame
#' data("meuse.grid")
#' coordinates(meuse.grid) <- ~x+y
#' gridded(meuse.grid) <- TRUE
#' proj4string(meuse.grid) <- crs
#' spplot3d(meuse.grid, att = "dist", CRS = CRS("+init=EPSG:4326"))
#' 
#' 
#' ## using map with high resolution
#' spplot3d(meuse.grid, att = "ffreq", alpha=0.3, minNumTiles = 30)
#' 
#' 
#' ## SpatialLinesDataFrame
#' data("gewaesserlinien")
#' spplot3d(gewaesserlinien, "glName", col = c("blue", "cyan"))
#' 
#' ## SpatiaPolygonsDataFrame
#' data("vg2500_bld")
#' spplot3d(vg2500_bld, "SHAPE_AREA", CRS = CRS("+init=EPSG:4326"))}
#' 
#' @export 
#' @docType methods
#' @rdname spplot3d
setGeneric(
  name = "spplot3d",
  def = function(obj, ...) standardGeneric("spplot3d")
)



.grabMap <- function(obj, CRS = NULL, type = NULL, radius = NULL, minNumTiles = NULL){
  bbox <- bbox(obj)
  xmin <- bbox[[1]]
  ymin <- bbox[[2]]
  xmax <- bbox[[3]] 
  ymax <- bbox[[4]]
  if (is.null(CRS)) {
    crs_osm <- osm()
    obj <- spTransform(obj, crs_osm)
    map <- openmap(c(ymax + 0.005, xmin - 0.005), 
                   c(ymin - 0.005, xmax + 0.005), type = type, minNumTiles = minNumTiles)
    bbox <- bbox(obj)
    xmin <- bbox[[1]]
    ymin <- bbox[[2]]
    xmax <- bbox[[3]] 
    ymax <- bbox[[4]]
    xext <- xmax - xmin
    yext <- ymax - ymin
    if (xext > yext)
      radius <- (radius * yext)/100
    else
      radius <- (radius * xext)/100
  } else {
    obj <- spTransform(obj, CRS)
    map <- openmap(c(ymax + 0.005, xmin - 0.005), 
                   c(ymin - 0.005, xmax + 0.005), type = type, minNumTiles = minNumTiles)
    map <- openproj(map, CRS)
    bbox <- bbox(obj)
    xmin <- bbox[[1]]
    ymin <- bbox[[2]]
    xmax <- bbox[[3]] 
    ymax <- bbox[[4]]
    xext <- xmax - xmin
    yext <- ymax - ymin
    if(xext <= 180 && yext <= 180) {
      if (xext > 10 && yext > 10){
        if (xext > yext)
          radius <- (radius * yext)/100
        else
          radius <- (radius * xext)/100 
      } else {
        if (xext > yext)
          radius <- (radius * yext)/5
        else
          radius <- (radius * xext)/5
      }
    } else {
      if (xext > yext)
        radius <- (radius * yext)/100
      else
        radius <- (radius * xext)/100 
    }   
  }
  return(list(obj = obj, map = map, radius = radius))
}
