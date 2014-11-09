\docType{package}
\name{map3d-package}
\alias{map3d-package}
\title{Maps and spatial data in 3D with rgl.}
\description{
  map3d provides basic methods to create interactive 3D
  plots of spatial data stored in classes defined in
  \code{\link[sp]{sp}} on top of maps from the
  OpenStreetMap package. More information about each
  function can be found in its help documentation.
}
\details{
  Plotting geographic informations in 3D.

  The methods in this package take either map obects or
  spatial objects and plot them in 3D using
  \code{\link{rgl}}. The main function is
  \code{\link{spplot3d}} which takes a spatial object and
  plots it on a map defined by the bounding box of the
  object. The map itself is plotted by \code{\link{map3d}}.

  ...
}
\author{
  Nikolai Gorte <n.gorte@gmail.com>
}
\seealso{
  \code{\link{spplot3d}}, \code{\link{map3d}},
  \code{\link[sp]{sp}}, \code{\link{rgl}},
  \code{\link{openmap}}
}

