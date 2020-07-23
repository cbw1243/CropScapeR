#' Request data on land cover changes over time
#'
#' A function that requests data on land cover changes over time from the CropScape. This function implements the GetCDLComp service provided by the CropScape \url{https://nassgeodata.gmu.edu/CropScape}.
#'
#' Land cover changes are obtained by first merging two raster files in two different years (year1 and year 2) together based on the geographic coordinates, and then count
#' the number of pixels (or grids) by types of land cover changes, such as corn to soybeans. The process is done in the CropScape server (the default option) or inside the \code{GetDataComp} function.
#'
#' To obtain land cover chaneg data, the raster objects in two different years must have the same spatial resolutions and identical coordinates to be directly merged together.
#' The CropScape server does internal checks on this and would report an error if the two rasters cannot be directly merged together due to unequal spatial resolution or different
#' coordinates. However, the \code{GetCDLComp} function allows users to obtain land cover changes from two raster files that have different resolutions. This is achieved by resample the raster
#' data using the nearest neighbor resampling technique such that both rasters have the same resolutions (the finer resolution raster is downscaled to lower resolution).
#' Then, the resampled data are processed automatically to get data on land cover changes (this is done by using the \code{manualrotate} function).
#' This feature is useful when dealing with the rasters in 2006 and 2007, which are at 56-meter resolution. While the rasters in other years are at 30-meter resolution.
#' Also note that the resampling process will lead to sampling errors. Whenever the manual calculation of land cover changes is used, a warning message will show up to alert users.
#' If without warning, the data are directly from the CropScape GetCDLComp service.
#'
#' In rare cases, the CropScape server fails to generate land cover change data even without the issue of unequal spatial resolution. A common issue is mismatch in data sizes: the raster objects in two years
#' have different pixel numbers. It is unclear that why this would happen. Nevertheless, when there is data mismatch, the \code{GetCDLComp} function will attempt to calculate for land cover change
#' manually using the \code{manual_rotate} function. Data associated with the unmatched coordinates are
#' discarded at the merging process. Again, a warning message will show up to alert users if \code{manual_rotate} function is used. If no coordinates can be matched, the
#' \code{manual_rotate} function would also fail to get land cover change data. In this case, a warning message will show up to alert users.
#'
#' The usage of this function is similar to the \code{GetCDLData} function. Please see the help page of the \code{GetCDLData} function
#' for details. Note that the \code{aoi} cannot be a single point here.
#'
#' @param aoi Area of interest. Can be a 5-digit FIPS code of a county, four corner points that defines a rectangle (or a box) area,
#' multiple coordinates that defines a polygon, or a URL of an compressed ESRI shapefile.
#' @param year1 Year 1. Should be a 4-digit numeric value.
#' @param year2 Year 2. Should be a 4-digit numeric value.
#' @param type Type of the selected AOI. 'f' for county, 'b' for box area, 'ps' for polygon, 's' for ESRI shapefile.
#' @param mat \code{TRUE}/\code{FALSE}. If \code{TRUE}, return a data frame. If \code{FALSE} (default), return a raster tif file.
#' @param crs Coordinate system. \code{NULL} if use the default coordinate system (i.e., Albers projection); Use '+init=epsg:4326' for longitude/latitude.
#' @param tol_time Number of seconds to wait for a response until giving up. Default is 20 seconds.
#' @param manual_try \code{True} (default) for trying calculating land cover changes using the \code{manualrotate} function. If \code{False}, no attempt is made.
#'
#' @return
#' The function returns a data table or a raster file.
#'
#' @export
#'
#' @examples
#'\donttest{
#' # Example 1. Retrieve data for the Champaign county in Illinois (FIPS = 17109) in 2017-2018.
#' data <- GetCDLComp(aoi = '17019', year1 = 2017, year2 = 2018, type = 'f')
#' head(data, 5)
#'
#' # Example 2. Retrieve data for a polygon (a triangle) defined by three coordinates in 2017-2018.
#' aoi <- c(175207,2219600,175207,2235525,213693,2219600)
#' data <- GetCDLComp(aoi = aoi, year1 = 2017, year2 = 2018, type = 'ps')
#' head(data, 5)
#'
#' # Example 3. Retrieve data for a rectangle box defined by four corner points in 2018.
#' data <- GetCDLComp(aoi = c(130783,2203171,153923,2217961), year1 = 2017, year2 = 2018, type = 'b')
#' head(data, 5)
#'}
GetCDLComp <- function(aoi, year1, year2, type, mat = TRUE, crs = NULL, tol_time = 20, manual_try = TRUE){
  targetCRS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

  if(!type %in% c('f', 'ps', 'b', 'p', 's')) stop('Invalid type value. See details. \n')

  if(type == 'f'){
    data <- GetCDLCompF(fips = aoi, year1 = year1, year2 = year2, mat = mat, tol_time = tol_time, manual_try = manual_try)
  }

  if(type == 's'){
    if(!is.null(crs)) stop('The coordinate system must be the Albers projection system. \n')
    data <- GetCDLCompS(poly = aoi, year1 = year1, year2 = year2, mat = mat, tol_time = tol_time, manual_try = manual_try)
  }

  if(type == 'ps'){
    if(length(aoi) < 6) stop('The aoi must be a numerical vector with at least 6 elements. \n')
    if(!is.null(crs)){
      numps <- length(aoi) # Number of points
      oldpoints <- sp::SpatialPoints(cbind(aoi[seq(1, numps, by = 2)], aoi[seq(2, numps, by = 2)]), sp::CRS(crs))
      newpoints <- sp::spTransform(oldpoints, targetCRS)
      aoi <- paste0(as.vector(t(newpoints@coords)), collapse = ',')
    }
    data <- GetCDLCompPs(points = aoi, year1 = year1, year2 = year2, mat = mat, tol_time = tol_time, manual_try = manual_try)
  }

  if(type == 'b'){
    if(length(aoi) != 4) stop('The aoi must be a numerical vector with 4 elements. \n')
    if(!is.null(crs)){
      numps <- length(aoi) # Number of points
      oldpoints <- sp::SpatialPoints(cbind(aoi[seq(1, numps, by = 2)], aoi[seq(2, numps, by = 2)]), sp::CRS(crs))
      newpoints <- sp::spTransform(oldpoints, targetCRS)
      aoi <- paste0(as.vector(t(newpoints@coords)), collapse = ',')
    }
    data <- GetCDLCompB(box = aoi, year1 = year1, year2 = year2, mat = mat, tol_time = tol_time, manual_try = manual_try)
  }
  return(data)
}



