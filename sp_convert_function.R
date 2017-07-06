#custom function to convert into sp data

map2SpatialPolygons <- function(df, proj4string = CRS("+proj=longlat")) {
  Plys <- list()
  i <- 1
  mtch <- which(is.na(df$x))
  if (length(mtch) == 0) {
    mtch <- length(df$x) + 1
  } else {
    mtch <- mtch
  }
  shps <- length(mtch)
  # make sure the names are unique
  nms <- df$names
  nms[duplicated(nms)] <- paste(nms[duplicated(nms)], 1:length(nms[duplicated(nms)]))
  for (j in 1:shps) {
    Plys[[j]] <- Polygons(list(Polygon(cbind(df$x[i:(mtch[j] - 1)], df$y[i:(mtch[j] - 1)]))), ID = nms[j])
    i <- mtch[j] + 1
  }
  SpatialPolygons(Plys, proj4string = proj4string)
}