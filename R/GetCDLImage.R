#' Request images of the CDL data
#'
#' A function that requests images of the CDL data for an area of interests in a given year from the CropScape.
#' This function implements the GetCDLImage service provided by the CropScape \url{https://nassgeodata.gmu.edu/CropScape}.
#'
#' The usage of this function is similar to the \code{GetCDLData} function. Please see the help page of the \code{GetCDLData} function
#' for details. Note that the \code{aoi} cannot be a single point here.
#'
#' @param aoi Area of interest. Can be a 5-digit FIPS code of a county, four corner points that defines a rectangle (or a box) area,
#' multiple coordinates that defines a polygon, or a URL of an compressed ESRI shapefile.
#' @param year  Year of data. Should be a 4-digit numerical value.
#' @param type Type of the selected AOI. 'f' for county, 'b' for box area, 'ps' for polygon, 's' for ESRI shapefile.
#' @param format Format of the image file. Can be png or kml.
#' @param crs Coordinate system. \code{NULL} if use the default coordinate system (i.e., Albers projection); Use '+init=epsg:4326' for longitude/latitude.
#' @param destfile A character string that specifies the directory to save the downloaded image file (e.g., 'C:/image.png'). Note that
#' the name of the image file should be specified as well. If not providing \code{destfile}, the function will create a temporary folder to save the image file.
#' @param verbose \code{TRUE}/\code{FALSE}. Display the directory saving the file or not.
#' @param tol_time Number of seconds to wait for a response until giving up. Default is 20 seconds.
#'
#' @return
#' The function downloads an image file in png or kml format to users' computer. This function is different to \code{GetCDLData} that returns a raster TIF file.
#'
#' @export
#'
GetCDLImage <- function(aoi = NULL, year = NULL, type = NULL, format = 'png',
                        crs = NULL, destfile = NULL, verbose = TRUE, tol_time = 20){
  targetCRS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

  if(!type %in% c('f', 'ps', 'b', 's')) stop('Invalid type value. See details. \n')

  if(type == 'f'){
    GetCDLImageF(fips = aoi, year = year, format = format, verbose = verbose, destfile = destfile, tol_time = tol_time)
  }

  if(type == 'f'){
    GetCDLImageS(poly = aoi, year = year, format = format, verbose = verbose, destfile = destfile, tol_time = tol_time)
  }

  if(type == 'ps'){
    if(length(aoi) < 6) stop('The aoi must be a numerical vector with at least 6 elements. \n')
    if(!is.null(crs)){
      numps <- length(aoi) # Number of points
      oldpoints <- sp::SpatialPoints(cbind(aoi[seq(1, numps, by = 2)], aoi[seq(2, numps, by = 2)]), sp::CRS(crs))
      newpoints <- sp::spTransform(oldpoints, targetCRS)
      aoi <- paste0(as.vector(t(newpoints@coords)), collapse = ',')
    }
    GetCDLImagePs(points = aoi, year = year, format = format, verbose = verbose, destfile = destfile, tol_time = tol_time)
  }

  if(type == 'b'){
    if(length(aoi) != 4) stop('The aoi must be a numerical vector with 4 elements. \n')
    if(!is.null(crs)){
      numps <- length(aoi) # Number of points
      oldpoints <- sp::SpatialPoints(cbind(aoi[seq(1, numps, by = 2)], aoi[seq(2, numps, by = 2)]), sp::CRS(crs))
      newpoints <- sp::spTransform(oldpoints, targetCRS)
      aoi <- paste0(as.vector(t(newpoints@coords)), collapse = ',')
    }
    GetCDLImageB(box = aoi, year = year, format = format, verbose = verbose, destfile = destfile, tol_time = tol_time)
  }
}


