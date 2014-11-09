#' @rdname spplot3d
spplot3d.polygons <- function(obj, att = NULL, col = "brown2", height = 0.2, ADD = FALSE,
                              type = c("osm", "osm-bw", "maptoolkit-topo", "waze", "mapquest", 
                                       "mapquest-aerial", "bing", "stamen-toner", "stamen-terrain", 
                                       "stamen-watercolor", "osm-german", "osm-wanderreitkarte", "mapbox",
                                       "esri", "esri-topo", "nps", "apple-iphoto", "skobbler", 
                                       "cloudmade-<id>", "hillshade", "opencyclemap", "osm-transport", 
                                       "osm-public-transport", "osm-bbike", "osm-bbike-german"),
                              minNumTiles = 10, CRS = NULL, grid = FALSE, col_grid = "black",
                              col_axes = "white", open3d = TRUE, ...)
{
  if (!is(obj, "SpatialPolygons")) 
    stop("Not a SpatialPolygons object")
  if (is.na(proj4string(obj)))
    stop("No projection specified!")
  if (ADD && is.null(CRS))
    stop("CRS needed in order to add objects to existing plot!")
  type <- match.arg(type)
  crs_ll <- CRS("+init=EPSG:4326")
  if (!ADD) {
    obj <- spTransform(obj, crs_ll) 
    list <- .grabMap(obj, type = type, CRS = CRS, minNumTiles = minNumTiles)
    obj <- list$obj
    map <- list$map
    map3d(map, col_axes = col_axes, open3d = open3d, ...)
  } else {
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
      centroids <- coordinates(obj)
      segments <- mat.or.vec(2 * nrow(centroids), 3)
      for (i in 1:(2 * nrow(centroids))) {
        for (j in 1:2) {
          segments[i,j] <- centroids[ceiling(i/2), j]
        } 
      }
      att_data_scaled <- (att_data/max(att_data))*7
      for (i in 1:length(att_data_scaled)) {
        segments[i*2, 3] <- att_data_scaled[i]
      }
    }
  } else {
    warning("No Attribute chosen or not found in 'obj'!")
  }
  polys <- slot(obj, "polygons")
  po <- slot(obj, "plotOrder")
  for (i in po) {
    polys2 <- slot(polys[[i]], "Polygons")
    po2 <- slot(polys[[i]], "plotOrder")
    for (j in po2) {
      coords <- coordinates(polys2[[j]])
      if (isTRUE(polys2[[j]]@hole)) {
        color <- "black"         
      } else {
        color <- col
      } 
      if (length(col) > 1 && !is.null(att) && att %in% names(obj)) {
        lines3d(coords[,1], coords[,2], height, col = color[i], lit = F, lwd = 3)
      } else {
        lines3d(coords[,1], coords[,2], height, col = color, lit = F, lwd = 3)
      }
      if (!is.null(att) && !is(att_data, "factor")) {
        segments3d(segments[((2*i)-1):(2*i), 1:3], lwd = 5, col = "slateblue1", alpha = 0.8)
      }
    }
  }
  if (!is.null(att) && !is(att_data, "factor")) {
    ticks <- c(min(att_data_scaled), max(att_data_scaled)/2, max(att_data_scaled))
    labels <- c(min(att_data), max(att_data)/2, max(att_data))
    axis3d(c("z-+"), col = col_axes, at = ticks, labels = labels)
  }
  if (grid) grid3d(c("x", "y", "z+"), col = col_grid)
}


#' @aliases spplot3d,SpatialPolygons-method
#' @rdname spplot3d
setMethod("spplot3d", signature("SpatialPolygons"), spplot3d.polygons)
#' @aliases spplot3d,SpatialPolygonsDataFrame-method
#' @rdname spplot3d
setMethod("spplot3d", signature("SpatialPolygonsDataFrame"), spplot3d.polygons)