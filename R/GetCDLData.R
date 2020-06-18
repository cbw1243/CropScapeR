#' Request for the CDL raster data
#'
#' A function that makes HTTP GET requests for CDL raster data for any area of interests in a given crop year.
#' This function implements the GetCDLData service provided by the CropScape \url{https://nassgeodata.gmu.edu/CropScape}.
#'
#' @param aoi Area of interest. Could be a 5-digit FIPS code of a county, three coordinates that defines a triangle,
#' or four corner points that defines a rectangle (or a box), or a single coordinate. The default coordinate system used by CDL is a projected
#' coordinate system called Albers projection (or Albers equal-area conic projection). Users could specify coordinates based on a
#' different coordinate system (defined by the \code{crs} argument), including the geographic coordinate system such as latitude-longitude.
#' @param year  Crop year of data to request. Should be a 4-digit numerical value.
#' @param type Type of AOI. 'f' for county, 'ps' for triangle with multiple coordinates, 'b' for box with four corner points, 'p' for a single coordinate.
#' @param mat TRUE/FALSE. If TRUE, return a data table. If FALSE (default), return a raster tif file.
#' @param crs Coordinate system. NULL if use the default coordinate system (e.g., Albers projection); Use '+init=epsg:4326' for longitude/latitude.
#' @param tol_time Number of seconds to wait for a response until giving up. Default is 20 seconds.
#'
#' @return
#' The function returns a raster TIF file or a data table that saves the cropland cover information. There are three columns in the returned data table. The first two are
#' coordinates. The third column reports numerical codes of the land cover category. The CDL provides another EXCEL file that links numerical codes
#' with the land cover names. One can download the EXCEL file from this link~\url{https://www.nass.usda.gov/Research_and_Science/Cropland/docs/cdl_codes_names.xlsx}.
#' One can also use \code{data(linkdata)} to get the data saved in the EXCEL file. However, the linkdata saved in this package is not frequently updated.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1. Retrieve data for the Champaign county in Illinois (FIPS = 17109) in 2018.
#' data <- GetCDLData(aoi = 17019, year = 2018, type = 'f')
#' raster::plot(data) # plot the data.
#'
#' # Example 2. Retrieve data for a single point by long/lat in 2018.
#' data <- GetCDLData(aoi = c(-94.6754,42.1197), year = 2018, type = 'p', crs = '+init=epsg:4326')
#' data
#' # Below uses the same point, but under the default coordinate system
#' data <- GetCDLData(aoi = c(108777,2125055), year = 2018, type = 'p')
#' data
#'
#' # Example 3. Retrieve data for a triangle defined by three coordinates in 2018.
#' data <- GetCDLData(aoi = c(175207,2219600,175207,2235525,213693,2219600), year = 2018, type = 'ps')
#' raster::plot(data)
#'
#' # Example 4. Retrieve data for a rectangle box defined by four corner points in 2018.
#' data <- GetCDLData(aoi = c(130783,2203171,153923,2217961), year = '2018', type = 'b')
#' raster::plot(data)
#'}
GetCDLData <- function(aoi = NULL, year = NULL, type = NULL, mat = FALSE, crs = NULL, tol_time = 20){
  targetCRS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

  if(!type %in% c('f', 'ps', 'b', 'p')) stop('The type value is wrong.')

  if(type == 'f'){
    data <- GetCDLDataF(fips = aoi, year = year, tol_time = tol_time)
  }

  if(type == 'ps'){
    if(length(aoi) < 6) stop('For points, at least 6 values (3 coordinate points) have to be provided for aoi.')
    if(!is.null(crs)){
      numps <- length(aoi) # Number of points
      oldpoints <- sp::SpatialPoints(cbind(aoi[seq(1, numps, by = 2)], aoi[seq(2, numps, by = 2)]), sp::CRS(crs))
      newpoints <- sp::spTransform(oldpoints, targetCRS)
      aoi <- paste0(as.vector(t(newpoints@coords)), collapse = ',')
    }
    data <- GetCDLDataPs(points = aoi, year = year, tol_time = tol_time)
  }

  if(type == 'b'){
    if(length(aoi) != 4) stop('For box, 4 values (2 coordinate points) have to be provided for aoi.')
    if(!is.null(crs)){
      numps <- length(aoi) # Number of points
      oldpoints <- sp::SpatialPoints(cbind(aoi[seq(1, numps, by = 2)], aoi[seq(2, numps, by = 2)]), sp::CRS(crs))
      newpoints <- sp::spTransform(oldpoints, targetCRS)
      aoi <- paste0(as.vector(t(newpoints@coords)), collapse = ',')
    }
    data <- GetCDLDataB(box = aoi, year = year, tol_time = tol_time)
  }

  if(type == 'p'){
    if(!is.null(crs)){
      oldpoints <- sp::SpatialPoints(cbind(aoi[1], aoi[2]), sp::CRS(crs))
      newpoints <- sp::spTransform(oldpoints, targetCRS)
      aoi <- unlist(newpoints@coords)
    }
    data <- GetCDLDataP(point = aoi, year = year, tol_time = tol_time)
  }

  if(isTRUE(mat) & type %in% c('f', 'ps', 'b')){
    data <- raster::rasterToPoints(data)
    data <- data.table::as.data.table(data)
    data.table::setnames(data, c('x', 'y', 'value'))
  }
  return(data)
}

