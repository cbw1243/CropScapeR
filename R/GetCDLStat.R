#' Request for summary statistics of the CDL data 
#'
#' A function that makes HTTP GET requests for the CDL raster data for an area of interests in a given crop year. 
#' This function implements the GetCDLStat services provided by the CropScape~\url{https://nassgeodata.gmu.edu/CropScape}.   
#'
#' @param aoi Area of interest. Could be a 5-digit FIPS code of a county, three coordinates that defines a triangle,
#' or four corner points that defines a rectangle. The default coordinate system (used by CDL) is the Albers equal-area conic projection, or Albers projection. Users can provide
#' coordinates from a different projection method, but user have to specify the coordinate system in the \code{crs} argument.
#' For example, users can provide longitude/latitude coordinates here, while letting \code{crs} be '+init=epsg:4326'.
#' @param year  Year of data to request. Can be a numerical value or a character.
#' @param type Type of aoi. 'f' for county, 'ps' for points, 'b' for box, 'p' for a single point.
#' @param crs Coordinate system, such as '+init=epsg:4326' for longitude/latitude.
#'
#' @export
#'
#' @examples
#'
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
#'
#' @return
#' The function returns a data frame that reports summary statistics of an area of interest in a given year. 

GetCDLStat <- function(aoi = NULL, year = NULL, crs = NULL, type = 'f'){
  targetCRS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  
  if(!type %in% c('f', 'ps', 'b', 'p')) stop('The type value is wrong.')
  
  if(type == 'f'){
    data <- GetCDLStatF(fips = aoi, year = year)
  }
  
  if(type == 'ps'){
    if(length(aoi) < 6) stop('For points, at least 6 values (3 coordinate points) have to be provided for aoi.')
    if(!is.null(crs)){
      numps <- length(aoi) # Number of points
      oldpoints <- sp::SpatialPoints(cbind(aoi[seq(1, numps, by = 2)], aoi[seq(2, numps, by = 2)]), sp::CRS(crs))
      newpoints <- sp::spTransform(oldpoints, targetCRS)
      aoi <- paste0(as.vector(t(newpoints@coords)), collapse = ',')
    }
    data <- GetCDLStatPs(points = aoi, year = year)
  }
  
  if(type == 'b'){
    if(length(aoi) != 4) stop('For box, 4 values (2 coordinate points) have to be provided for aoi.')
    if(!is.null(crs)){
      numps <- length(aoi) # Number of points
      oldpoints <- sp::SpatialPoints(cbind(aoi[seq(1, numps, by = 2)], aoi[seq(2, numps, by = 2)]), sp::CRS(crs))
      newpoints <- sp::spTransform(oldpoints, targetCRS)
      aoi <- paste0(as.vector(t(newpoints@coords)), collapse = ',')
    }
    data <- GetCDLStatB(box = aoi, year = year)
  }
  return(data)
}

