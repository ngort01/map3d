#' @rdname spplot3d
spplot3d.points <- function(obj, att = NULL, col = "red", height = 0.5, ADD = FALSE,
                            type = c("osm", "osm-bw", "maptoolkit-topo", "waze", "mapquest", 
                                     "mapquest-aerial", "bing", "stamen-toner", "stamen-terrain", 
                                     "stamen-watercolor", "osm-german", "osm-wanderreitkarte", "mapbox",
                                     "esri", "esri-topo", "nps", "apple-iphoto", "skobbler", 
                                     "cloudmade-<id>", "hillshade", "opencyclemap", "osm-transport", 
                                     "osm-public-transport", "osm-bbike", "osm-bbike-german"),
                            minNumTiles = 10, CRS = NULL, radius = 1,  grid = FALSE, 
                            col_grid = "black", col_axes = "white", open3d = TRUE, ...) {
  if (!is(obj, "SpatialPoints")) 
    stop("Not a SpatialPoints object!")
  if (is.na(proj4string(obj)))
    stop("No projection specified!")
  if (ADD && is.null(CRS))
    stop("CRS needed in order to add objects to existing plot!")
  type <- match.arg(type)
  crs_ll <- CRS("+init=EPSG:4326")
  if (!ADD) {
    obj <- spTransform(obj, crs_ll) 
    list <- .grabMap(obj, type = type, radius = radius, CRS = CRS, minNumTiles = minNumTiles)
    obj <- list$obj
    map <- list$map
    radius <- list$radius
    map3d(map, col_axes = col_axes, open3d = open3d, ...)
  } else {
    obj <- spTransform(obj, crs_ll) 
    list <- .grabMap(obj, type = type, radius = radius, CRS = CRS, minNumTiles = minNumTiles)
    radius <- list$radius
    obj <- spTransform(obj, CRS) 
  }
  if (!is.null(att) && att %in% names(obj)) {
    if (!ADD) title3d(main = att, col = col_axes)
    pos <- match(att, names(obj@data))
    att_data <- obj@data[,pos]
    if (is(att_data, "factor")) {
      levels <- levels(att_data)
      if (length(col) < 2) col <- rainbow(length(levels))
      crp <- colorRampPalette(col)(length(levels))
      col <- crp[att_data]
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      legend(1, 1, legend = levels, col = crp,
             pch = 16, xjust = 0.5, yjust = 0.5, title = att) 
    } else {
      if (length(col) < 2) col <- c("blue", "red")
      crp <- colorRampPalette(col)(5)
      col <- crp[cut(att_data, breaks = seq(0, max(att_data), max(att_data)/5), include.lowest=T)]
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      legend(1, 1, col = crp, pch = 16,
             legend = levels(cut(att_data, breaks = seq(0, max(att_data), max(att_data)/5), include.lowest=T)), 
             xjust = 0.5, yjust = 0.5, title = att)
    }
  } else {
    warning("No Attribute chosen or not found in 'obj'!")
  }
  coords <- coordinates(obj)
  spheres3d(coords[,1], coords[,2], height, radius = radius, col = col, ...)
  if (grid) grid3d(c("x", "y", "z+"), col = col_grid)
}


#' @aliases spplot3d,SpatialPoints-method
#' @rdname spplot3d
setMethod("spplot3d", signature("SpatialPoints"), spplot3d.points)
#' @aliases spplot3d,SpatialPointsDataFrame-method
#' @rdname spplot3d
setMethod("spplot3d", signature("SpatialPointsDataFrame"), spplot3d.points)