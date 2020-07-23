#' Request summary statistics of the CDL data
#'
#' A function that requests summary statistics of the CDL data for any Area of Interests (AOI) in a given year from the CropScape.
#' This function implements the GetCDLStat services provided by the CropScape \url{https://nassgeodata.gmu.edu/CropScape}.
#'
#' The usage of this function is similar to the \code{GetCDLData} function. Please see the help page of the \code{GetCDLData} function
#' for details. Note that the \code{aoi} cannot be a single point here.
#'
#' @param aoi Area of interest. Can be a 5-digit FIPS code of a county, four corner points that defines a rectangle (or a box) area,
#' multiple coordinates that defines a polygon, or a URL of an compressed ESRI shapefile.
#' @param year  Year of data. Should be a 4-digit numeric value.
#' @param type Type of the selected AOI. 'f' for county, 'b' for box area, 'ps' for polygon, 's' for ESRI shapefile.
#' @param crs Coordinate system. \code{NULL} if use the default coordinate system (i.e., Albers projection); Use '+init=epsg:4326' for longitude/latitude.
#' @param tol_time Number of seconds to wait for a response until giving up. Default is 20 seconds.
#'
#' @return
#' The function returns a data frame that reports summary statistics of the CDL data for an AOI in a given year.

#' @export
#'
#' @examples
#'\donttest{
#' # Example 1. Retrieve data for the Champaign county in Illinois (FIPS = 17109) in 2018.
#' data <- GetCDLStat(aoi = 17019, year = 2018, type = 'f')
#' head(data, n = 5) # Show top 5 rows of retrieved data
#'
#' # Example 2. Retrieve data for a polygon (a triangle) defined by three points in 2018.
#' data <- GetCDLStat(aoi = c(175207,2219600,175207,2235525,213693,2219600), year = 2018, type = 'ps')
#' head(data, n = 5)
#'
#' # Example 3. Retrieve data for a rectangle box defined by three corner points in 2018.
#' data <- GetCDLStat(aoi = c(130783,2203171,153923,2217961), year = '2018', type = 'b')
#' head(data, n = 5)
#'}

GetCDLStat <- function(aoi = NULL, year = NULL, type = NULL, crs = NULL, tol_time = 20){
  targetCRS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

  if(!type %in% c('f', 'ps', 'b', 's')) stop('Invalid type value. See details. \n')

  if(type == 'f'){
    data <- GetCDLStatF(fips = aoi, year = year, tol_time = tol_time)
  }

  if(type == 's'){
    if(!is.null(crs)) stop('The coordinate system must be the Albers projection system. \n')
    data <- GetCDLStatS(poly = aoi, year = year, tol_time = tol_time)
  }

  if(type == 'ps'){
    if(length(aoi) < 6) stop('The aoi must be a numerical vector with at least 6 elements. \n')
    if(!is.null(crs)){
      numps <- length(aoi) # Number of points
      oldpoints <- sp::SpatialPoints(cbind(aoi[seq(1, numps, by = 2)], aoi[seq(2, numps, by = 2)]), sp::CRS(crs))
      newpoints <- sp::spTransform(oldpoints, targetCRS)
      aoi <- paste0(as.vector(t(newpoints@coords)), collapse = ',')
    }
    data <- GetCDLStatPs(points = aoi, year = year, tol_time = tol_time)
  }

  if(type == 'b'){
    if(length(aoi) != 4) stop('The aoi must be a numerical vector with 4 elements. \n')
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

