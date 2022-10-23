#' Request images of the CDL data
#'
#' A function that requests images of the CDL data for an area of interests in a given year from the CropScape.
#' This function implements the GetCDLImage service provided by the CropScape \url{https://nassgeodata.gmu.edu/CropScape}.
#'
#' The usage of this function is similar to the \code{GetCDLData} function. Please see the help page of the \code{GetCDLData} function
#' for details. Note that the \code{aoi} cannot be a single point here.
#'
#' @param aoi Area of interest. Can be a 5-digit FIPS code of a county, 2-digit FIPS code of a state, four corner points or an sf object that defines a rectangle (or a box) area,
#' multiple coordinates that defines a polygon, or a URL of an compressed ESRI shapefile.
#' @param year  Year of data. Should be a 4-digit numeric value.
#' @param type Type of the selected AOI. 'f' for state or county, 'b' for box area, 'ps' for polygon, 's' for ESRI shapefile.
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

  if(is.null(aoi)) stop('aoi must be provided. See details. \n')

  if(is.null(year)) stop('year must be provided. See details. \n')

  if(is.null(type)) stop('type must be provided. See details. \n')

  if(type == 'p') stop('Cannot request statistics for a single point. \n')

  if(!type %in% c('f', 'ps', 'b', 's')) stop('Invalid type value. See details. \n')

  if(type == 'f'){
    aoi <- as.character(aoi)
    if ((nchar(aoi) == 1)|(nchar(aoi) == 4)){
      aoi <- paste0('0', aoi)
    }
    GetCDLImageF(fips = aoi, year = year, format = format, verbose = verbose, destfile = destfile, tol_time = tol_time)
  }

  if(type == 's'){
    if(!is.null(crs)) stop('The coordinate system must be the Albers projection system. \n')
    GetCDLImageS(poly = aoi, year = year, format = format, verbose = verbose, destfile = destfile, tol_time = tol_time)
  }

  if(type == 'ps'){
    if(length(aoi) < 6) stop('The aoi must be a numerical vector with at least 6 elements. \n')
    if(!is.null(crs)){ aoi <- convert_crs(aoi, crs)}
    GetCDLImagePs(points = aoi, year = year, format = format, verbose = verbose, destfile = destfile, tol_time = tol_time)
  }

  if(type == 'b'){
    if (!is.numeric(aoi)) {
      if (!(class(aoi)[1] == "sf" | class(aoi)[2] == "sfc")) stop('aoi must be a numerical vector or a sf object. \n')
      if(is.na(sf::st_crs(aoi))) stop('The sf object for aoi does not contain crs. \n')
      aoi_crs <- sf::st_crs(aoi)[[2]]

      if(aoi_crs != targetCRS){aoi <- sf::st_transform(aoi, targetCRS)}

      GetCDLImageB(box = sf::st_bbox(aoi), year = year, format = format, verbose = verbose, destfile = destfile, tol_time = tol_time)
    }else{
      if(length(aoi) != 4) stop('The aoi must be a numerical vector with 4 elements. \n')
      if(!is.null(crs)){ aoi <- convert_crs(aoi, crs)}
      GetCDLImageB(box = aoi, year = year, format = format, verbose = verbose, destfile = destfile, tol_time = tol_time)
    }
  }
}


