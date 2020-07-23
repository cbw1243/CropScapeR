#' Request the CDL data from the CropScape
#'
#' A function that requests the CDL data for any Area of Interests (AOI) in a given year from the CropScape.
#' This function implements the GetCDLData service provided by the CropScape \url{https://nassgeodata.gmu.edu/CropScape}.
#'
#' The \code{GetCDLData} function does the data request in two steps. First, the function sends data requests to the CropScape online server using the \code{GET} function
#' from the \code{httr} package. Second, the function reads the requested data into R using the \code{raster} function from the \code{raster} package. By default, the data
#' returned from the CropScape are in the raster-based GeoTIFF file format. Users can choose to save the raw data in TIF format into their local drives.
#'
#' Users should at least specify \code{aoi}, \code{year}, and \code{type} to make valid data requests. \code{aoi} represents area of interest, and it refers to the
#' area to make data request. An \code{aoi} can be a county defined by a 5-digit FIPS code, a box defined by four corner points,
#' a polygon defined by multiple coordinates, a single point defined by a coordinate, or a custom area defined by an ESRI shapefile.
#'
#' If the type of \code{aoi} is a box, users should specify \code{aoi} as a numeric vector with four elements that represent corner points of the box.
#' The format of the box should be (minimum x, minimum y, maximum x, maximum y). For example, if latitude/longitude is used, users should specify the \code{aoi} as
#' (Lower longitude, Lower latitude, Higher longitude, Higher latitude). If the type of \code{aoi} is a polygon, users should specify \code{aoi} as a
#' numeric vector with at least 6 elements (corresponding to multiple points). The format is (x1, y2, x2, y2, ..., xn, yn). The polygon can take any shape.
#' If the type of \code{aoi} is a custom area, users
#' must specify \code{aoi} as a URL of a compressed ESRI shapefile. The .shp, .shx, .dbf, and .prj files must all be compressed with no subdirectories in a single ZIP file.
#' In cases that the compressed shapefile is saved in the local disk, this shapefile needs to be published to a website URL. See more detailed instructions from here:
#' \url{https://github.com/cbw1243/CropScapeR}
#'
#'
#' The \code{GetCDLData} function provides some additional functionalities that might benefit the users. First, it can recognize data requests made in any coordinate system.
#' The default coordinate system used by the CDL is a projected coordinate system called Albers projection (or Albers equal-area conic projection).
#' Users can specify an alternative coordinate system, such as latitude/longitude, by changing the \code{crs} value. As an exception, this functionality
#' is unavailable when the requested data type is 's' (a shapefile). This is because the zipped shapefile is directly sent to CropScape, and it cannot
#' be processed before sending the request. If a shapefile is used, users must ensure that the shapefile has the Albers projection system. Second, the \code{tol_time} argument specifies
#' the upper time limit for making the data request. This is useful particularly when the CropScape server has issues with responding to the data request (e.g., system maintenance).
#' It is possible that the CropScape server takes a long time before sending back a message saying that the data are not available. The default time limit is 20 seconds.
#' Third, users can choose to save the raster TIF file in their local disks when requesting the CDL data. This can be done simply by specifying a directory name
#' in \code{save_path}. In this case, \code{GetCDLData} will first save the data and then read the saved data into \code{R} using the \code{raster} function
#' from the \code{raster} package. For example, when letting \code{save_path} be 'C:/test.tif', the raster TIF file will be saved at the 'C' disk in the name of
#' 'test.tif'. If \code{save_path} is \code{NULL} (default), the raster TIF file will not be saved but just read into the \code{R} environment through the \code{raster} function.
#' Forth, users can transform the raster data into tabular data by letting \code{mat} be \code{TRUE}. The transformation is done by using the
#' \code{as.data.frame} function from the \code{raster} package. The returned object would be a data frame with the coordinates (first two columns) and
#' numeric codes of the land cover category (third column). The coordinates are centroids of the grid cells.
#'
#' The CDL website provides an EXCEL file that links the numeric codes with the land cover names.
#' Users can download the EXCEL file from this link \url{https://www.nass.usda.gov/Research_and_Science/Cropland/docs/cdl_codes_names.xlsx}.
#' Alternatively, users can also use \code{data(linkdata)} to get the data directly from this package.
#' Yet, be noted that \code{linkdata} saved in this package is not frequently updated.
#'
#' @param aoi Area of interest. Can be a 5-digit FIPS code of a county, four corner points that defines a rectangle (or a box) area,
#' multiple coordinates that defines a polygon, a single coordinate that defines a point, or a URL of an compressed ESRI shapefile. See details.
#' @param year Year of data. Should be a 4-digit numeric value.
#' @param type Type of the selected AOI. 'f' for county, 'b' for box area, 'ps' for polygon, 'p' for a single coordinate, 's' for ESRI shapefile.
#' @param mat \code{TRUE}/\code{FALSE}. If \code{TRUE}, return a data frame. If \code{FALSE} (default), return a raster tif file.
#' @param crs Coordinate system. \code{NULL} if use the default coordinate system (i.e., Albers projection); Use '+init=epsg:4326' for longitude/latitude.
#' @param tol_time Number of seconds to wait for a response until giving up. Default is 20 seconds.
#' @param save_path Path (including the file name with the suffix: '.tif') to save the TIF file.
#' If a path is provided, the TIF file will be saved in the computer in the specified directory. Default: \code{NULL}
#'
#' @return
#' The function returns a raster object or a data frame that records the requested CDL data.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Example. Retrieve data for the Champaign county in Illinois (FIPS = 17109) in 2018.
#' data <- GetCDLData(aoi = 17019, year = 2018, type = 'f')
#' raster::plot(data) # plot the data.
#'
#' # Same request but also save the raster data as a TIF file.
#' # Note: A temporary file is created to save the data using the tempfile function
#' data <- GetCDLData(aoi = 17019, year = 2018, type = 'f', save_path = tempfile(fileext = '.tif'))
#' raster::plot(data) # plot the data.
#'
#' # Example. Retrieve data for a single point by long/lat in 2018.
#' data <- GetCDLData(aoi = c(-94.6754,42.1197), year = 2018, type = 'p', crs = '+init=epsg:4326')
#' data
#' # Below uses the same point, but under the default coordinate system
#' data <- GetCDLData(aoi = c(108777,2125055), year = 2018, type = 'p')
#' data
#'
#' # Example. Retrieve data for a polygon (triangle) area defined by three coordinates in 2018.
#' data <- GetCDLData(aoi = c(175207,2219600,175207,2235525,213693,2219600), year = 2018, type = 'ps')
#' raster::plot(data)
#'
#' # Example. Retrieve data for a box area defined by four corner points in 2018.
#' data <- GetCDLData(aoi = c(130783,2203171,153923,2217961), year = '2018', type = 'b')
#' raster::plot(data)
#'
#' # Example. Retrieve data for a box area defined by four corner points (long/lat)
#' data <- GetCDLData(aoi = c(-88.2, 40.03, -88.1, 40.1), year = '2018', type = 'b',
#' crs = '+init=epsg:4326')
#' raster::plot(data)
#'}
#'
GetCDLData <- function(aoi = NULL, year = NULL, type = NULL, mat = FALSE, crs = NULL, tol_time = 20, save_path = NULL){
  targetCRS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  if(!is.null(save_path)){
    if(substr(save_path, nchar(save_path)-3, nchar(save_path)) != '.tif') stop('The save_path should end with .tif \n')
  }

  if(!type %in% c('f', 'ps', 'b', 'p', 's')) stop('Invalid type value. See details. \n')

  if(type == 'f'){
    data <- GetCDLDataF(fips = aoi, year = year, tol_time = tol_time, save_path = save_path)
  }

  if(type == 's'){
    if(!is.null(crs)) stop('The coordinate system must be the Albers projection system. \n')
    data <- GetCDLDataS(poly = aoi, year = year, tol_time = tol_time, save_path = save_path)
  }

  if(type == 'ps'){
    if(length(aoi) < 6) stop('The aoi must be a numerical vector with at least 6 elements. \n')
    if(!is.null(crs)){
      numps <- length(aoi) # Number of points
      oldpoints <- sp::SpatialPoints(cbind(aoi[seq(1, numps, by = 2)], aoi[seq(2, numps, by = 2)]), sp::CRS(crs))
      newpoints <- sp::spTransform(oldpoints, targetCRS)
      aoi <- paste0(as.vector(t(newpoints@coords)), collapse = ',')
    }
    data <- GetCDLDataPs(points = aoi, year = year, tol_time = tol_time, save_path = save_path)
  }

  if(type == 'b'){
    if(length(aoi) != 4) stop('The aoi must be a numerical vector with 4 elements. \n')
    if(!is.null(crs)){
      numps <- length(aoi) # Number of points
      oldpoints <- sp::SpatialPoints(cbind(aoi[seq(1, numps, by = 2)], aoi[seq(2, numps, by = 2)]), sp::CRS(crs))
      newpoints <- sp::spTransform(oldpoints, targetCRS)
      aoi <- paste0(as.vector(t(newpoints@coords)), collapse = ',')
    }
    data <- GetCDLDataB(box = aoi, year = year, tol_time = tol_time, save_path = save_path)
  }

  if(type == 'p'){
    if(!is.null(crs)){
      oldpoints <- sp::SpatialPoints(cbind(aoi[1], aoi[2]), sp::CRS(crs))
      newpoints <- sp::spTransform(oldpoints, targetCRS)
      aoi <- unlist(newpoints@coords)
    }
    data <- GetCDLDataP(point = aoi, year = year, tol_time = tol_time)
  }

  if(isTRUE(mat) & type %in% c('f', 'ps', 'b', 'poly')){
    data <- raster::rasterToPoints(data)
    data <- data.table::as.data.table(data)
    data.table::setnames(data, c('x', 'y', 'value'))
  }
  return(data)
}

