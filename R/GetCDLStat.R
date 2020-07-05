#' Request for summary statistics of the CDL data
#'
#' A function that makes HTTP GET requests for the CDL raster data for an area of interests in a given crop year.
#' This function implements the GetCDLStat services provided by the CropScape \url{https://nassgeodata.gmu.edu/CropScape}.
#'
#' @param aoi Area of interest. Could be a 5-digit FIPS code of a county, three coordinates that defines a triangle,
#' or four corner points that defines a rectangle (or a box), or a single coordinate. The default coordinate system used by CDL is a projected
#' coordinate system called Albers projection (or Albers equal-area conic projection). Users could specify coordinates based on a
#' different coordinate system (defined by the \code{crs} argument), including the geographic coordinate system such as latitude-longitude.
#' @param year  Year of data to request. Should be a 4-digit numerical value.
#' @param type Type of AOI. 'f' for county, 'ps' for triangle with multiple coordinates, 'b' for box with four corner points, 'p' for a single coordinate.
#' @param crs Coordinate system. NULL if use the default coordinate system (e.g., Albers projection); Use '+init=epsg:4326' for longitude/latitude.
#' @param tol_time Number of seconds to wait for a response until giving up. Default is 20 seconds.
#'
#' @return
#' The function returns a data frame that reports summary statistics of the CDL data for an area of interest in a given year.

#' @export
#'
#' @examples
#'\donttest{
#' # Example 1. Retrieve data for the Champaign county in Illinois (FIPS = 17109) in 2018.
#' data <- GetCDLStat(aoi = 17019, year = 2018, type = 'f')
#' head(data, n = 5) # Show top 5 rows of retrieved data
#'
#' # Example 2. Retrieve data for a triangle defined by three coordinates in 2018.
#' data <- GetCDLStat(aoi = c(175207,2219600,175207,2235525,213693,2219600), year = 2018, type = 'ps')
#' head(data, n = 5)
#'
#' # Example 3. Retrieve data for a rectangle box defined by three corner points in 2018.
#' data <- GetCDLStat(aoi = c(130783,2203171,153923,2217961), year = '2018', type = 'b')
#' head(data, n = 5)
#'}

GetCDLStat <- function(aoi = NULL, year = NULL, type = 'f', crs = NULL, tol_time = 20){
  targetCRS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

  if(!type %in% c('f', 'ps', 'b', 'p')) stop('The type value is wrong.')

  if(type == 'f'){
    data <- GetCDLStatF(fips = aoi, year = year, tol_time = tol_time)
  }

  if(type == 'ps'){
    if(length(aoi) < 6) stop('For points, at least 6 values (3 coordinate points) have to be provided for aoi.')
    if(!is.null(crs)){
      numps <- length(aoi) # Number of points
      oldpoints <- sp::SpatialPoints(cbind(aoi[seq(1, numps, by = 2)], aoi[seq(2, numps, by = 2)]), sp::CRS(crs))
      newpoints <- sp::spTransform(oldpoints, targetCRS)
      aoi <- paste0(as.vector(t(newpoints@coords)), collapse = ',')
    }
    data <- GetCDLStatPs(points = aoi, year = year, tol_time = tol_time)
  }

  if(type == 'b'){
    if(length(aoi) != 4) stop('For box, 4 values (2 coordinate points) have to be provided for aoi.')
    if(!is.null(crs)){
      numps <- length(aoi) # Number of points
      oldpoints <- sp::SpatialPoints(cbind(aoi[seq(1, numps, by = 2)], aoi[seq(2, numps, by = 2)]), sp::CRS(crs))
      newpoints <- sp::spTransform(oldpoints, targetCRS)
      aoi <- paste0(as.vector(t(newpoints@coords)), collapse = ',')
    }
    data <- GetCDLStatB(box = aoi, year = year, tol_time = tol_time)
  }
  return(data)
}

