\docType{methods}
\name{map3d}
\alias{map3d}
\alias{map3d,OpenStreetMap-method}
\alias{map3d.OpenStreetMap}
\title{Plot a map object in 3D.}
\usage{
  map3d.OpenStreetMap(map, axes = TRUE, col_axes = "white",
    col_bg = "black", lit = FALSE, open3d = TRUE, ...)
}
\arguments{
  \item{map}{map object from the OpenStreetMap package.}

  \item{axes}{logical, specifying if axes are drawn or
  not.}

  \item{col_axes}{color of the axes.}

  \item{col_bg}{background color.}

  \item{lit}{logical, specifying if lighting calculation
  should take place on geometry.}

  \item{open3d}{ignore, only needed for snapshot creation
  for the vignette.}

  \item{...}{passes arguments to \code{\link{surface3d}}.}
}
\description{
  This method creates a 3D plot of an OpenStreetMap map
  object using \code{\link{rgl}}.
}
\examples{
map <- openmap(c(52.05, 7.5), c(51.9, 7.75),12, type="osm")
# Reproject if you want. Takes some time!
map <- openproj(map, CRS("+init=EPSG:4326"))
map3d(map)
}
\author{
  Nikolai Gorte <n.gorte@gmail.com>
}
\seealso{
  \code{\link{openmap}}, \code{\link{rgl}}
}

