#' @rdname spplot3d
spplot3d.grid <- function(obj, att = NULL, col = "red", height = 0.2, alpha = 0.5, ADD = FALSE,
                          type = c("osm", "osm-bw", "maptoolkit-topo", "waze", "mapquest", 
                                   "mapquest-aerial", "bing", "stamen-toner", "stamen-terrain", 
                                   "stamen-watercolor", "osm-german", "osm-wanderreitkarte", "mapbox",
                                   "esri", "esri-topo", "nps", "apple-iphoto", "skobbler", 
                                   "cloudmade-<id>", "hillshade", "opencyclemap", "osm-transport", 
                                   "osm-public-transport", "osm-bbike", "osm-bbike-german"),
                          minNumTiles = 10, CRS = NULL, radius = 1.2, grid = FALSE, col_grid = "black", 
                          col_axes = "white", open3d = TRUE, ...) {
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
      legend(1, 1, legend = levels, fill = crp, xjust = 0.5, yjust = 0.5, title = att) 
    } else {
      if (length(col) < 2) col <- c("blue", "red")
      crp <- colorRampPalette(col)(length(unique(att_data)))
      col <- crp[cut(att_data, breaks = seq(0, max(att_data), max(att_data)/length(unique(att_data))), 
                     include.lowest=TRUE)]
      max <- max(att_data)
      min <- min(att_data)
      scale <- (length(unique(att_data)))/(max - min)
      plot(c(0,3), c(min, max), type= "n", axes = FALSE, xlab = "", ylab = "", main = att)
      for (i in 1:length(unique(att_data))){
        y <- i/scale + min
        rect(1.3, y, 1.8, y+1/scale, col = crp[i], border = NA)
      }
      text(rep(2.2, 2), c(max, min), labels = c(max, min), cex = 1.5)
    }
  } else {
    warning("No Attribute chosen or not found in 'obj'!")
  }
  coords <- coordinates(obj)
  sprites3d(coords[,1], coords[,2], height, radius, col = col, lit=F, alpha = alpha, ...)
  if (grid) grid3d(c("x", "y", "z+"), col = col_grid)
}

#' @aliases spplot3d,SpatialPixels-method
#' @rdname spplot3d
setMethod("spplot3d", signature("SpatialPixels"), spplot3d.grid)
#' @aliases spplot3d,SpatialGrid-method
#' @rdname spplot3d
setMethod("spplot3d", signature("SpatialGrid"), spplot3d.grid)
#' @aliases spplot3d,SpatialPixelsDataFrame-method
#' @rdname spplot3d
setMethod("spplot3d", signature("SpatialPixelsDataFrame"), spplot3d.grid)
#' @aliases spplot3d,SpatialGridDataFrame-method
#' @rdname spplot3d
setMethod("spplot3d", signature("SpatialGridDataFrame"), spplot3d.grid)