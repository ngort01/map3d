#' Plot a map object in 3D.
#'
#' This method creates a 3D plot of an OpenStreetMap map object using \code{\link{rgl}}.
#' 
#' @param map map object from the OpenStreetMap package.
#' @param axes logical, specifying if axes are drawn or not.
#' @param col_axes color of the axes.
#' @param col_bg background color.
#' @param lit logical, specifying if lighting calculation should take place on geometry.
#' @param open3d ignore, only needed for snapshot creation for the vignette.
#' @param ... passes arguments to \code{\link{surface3d}}.
#' @author Nikolai Gorte <n.gorte@@gmail.com>

#' @examples
#' map <- openmap(c(52.05, 7.5), c(51.9, 7.75),12, type="osm")
#' # Reproject if you want. Takes some time!  
#' map <- openproj(map, CRS("+init=EPSG:4326"))
#' map3d(map)
#' 
#' @seealso \code{\link{openmap}}, \code{\link{rgl}}
#' @export 
#' @docType methods
#' @rdname map3d
setGeneric(
  name = "map3d",
  def = function(map, ...) standardGeneric("map3d")
)



#' @rdname map3d
map3d.OpenStreetMap <- function(map, axes = TRUE, col_axes = "white", col_bg = "black", lit = FALSE,
                                open3d = TRUE, ...){
  if (!is(map, "OpenStreetMap")) 
    stop("Not an OpenStreetMap object")
  if (length(map$tiles) != 1)
    stop("Please set mergeTiles = TRUE") 
  tile <- map$tiles[[1]]
  xres <- tile$xres
  yres <- tile$yres
  bbox <- tile$bbox
  xmin <- bbox$p1[1]
  xmax <- bbox$p2[1]
  ymin <- bbox$p1[2]
  ymax <- bbox$p2[2]
  xc <- seq(xmin, xmax,len = yres)
  yc <- seq(ymin, ymax,len = xres)
  colors <- matrix(tile$colorData, yres, xres)
  heigths <- matrix(0, yres, xres)
  if (open3d) open3d(windowRect = c(100, 100, 800, 800)) 
  bg3d(col_bg)
  surface3d(xc, yc, heigths, col = colors, lit = lit, ...)
  if (axes) {
    xext <- xmax - xmin
    yext <- ymax - ymin
    y <- abs(yext/xext)
    aspect3d(1, y, 0.05)
    axes3d(c("x", "y"), col = col_axes)
    title3d(xlab = "lon", ylab = "lat", col = col_axes)
  }
}


#' @aliases map3d,OpenStreetMap-method
#' @rdname map3d
setMethod("map3d", signature("OpenStreetMap"), map3d.OpenStreetMap)





## problems with mercator projection
## map will display but with wrong coordiantes
map3d.ggmap <- function(map, axes = TRUE, col_axes = "white", col_bg = "black", lit = FALSE, ...){
  attributes <- attributes(map)
  colors <- as.matrix(map)
  colors <- colors[nrow(colors):1, ]
  colors <- t(colors)
  xmin <- as.numeric(attributes$bb[2])
  xmax <- as.numeric(attributes$bb[4])
  ymin <- as.numeric(attributes$bb[1])
  ymax <- as.numeric(attributes$bb[3])
  xc <- seq(xmin, xmax, len = nrow(colors))
  yc <- seq(ymin, ymax, len = ncol(colors))
  heigths <- matrix(0, nrow(colors), ncol(colors))
  open3d(windowRect = c(100, 100, 1000, 1000)) 
  bg3d(col_bg)
  surface3d(xc, yc, heigths, col=colors, lit = lit, ...)
  if (axes) {
    xext <- xmax - xmin
    yext <- ymax - ymin
    y <- abs(yext/xext)
    aspect3d(1, y, 0.05)
    axes3d(c("x", "y"), col = col_axes)
    title3d(xlab = "lon", ylab = "lat", col = col_axes)
  }
}

## setMethod("map3d", signature("ggmap"),  map3d.ggmap)