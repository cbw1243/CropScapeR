#' Calculate for land use changes
#'
#' The \code{manualrotate} function analyzes land use changes based on two raster files. The analysis is done in three steps. At step 1, the two raster files are
#' converted to data tables. At step 2, the two data tables are merged together based on their coordinates. The coordinates without macthes are discarded during the
#' merging process. At step 3, the data are aggregated by counting the number of pixels for each land change group.
#'
#'
#' @param datat1 A raster file.
#' @param datat2 A raster file.
#'
#' @return
#' The function returns a data table.
#'
#' @export
#'
#' @examples
#'\donttest{
#' # Calculate land cover changes for the Champaign county (FIPS = 17109) in 2017-2018.
#' datat1 <- GetCDLData(aoi = '17019', year = 2017, type = 'f')
#' datat2 <- GetCDLData(aoi = '17019', year = 2018, type = 'f')
#' change <- manualrotate(datat1, datat2)
#'}
#'
manualrotate <- function(datat1, datat2){
  if(class(datat1) != c('RasterLayer')) stop('datat1 must be a raster file.')
  if(class(datat2) != c('RasterLayer')) stop('datat2 must be a raster file.')

  res1 <- raster::res(datat1)
  res2 <- raster::res(datat2)

  if(res1[1] > res2[1]){datat2 <- raster::resample(datat2, datat1, method = "ngb")}
  if(res1[1] < res2[1]){datat1 <- raster::resample(datat1, datat2, method = "ngb")}

  stopifnot(raster::res(datat1)[1] == raster::res(datat2)[1])
  conversionfactor <- ifelse(raster::res(datat1) == 56, 0.774922, 0.222394)

  datat1 <- raster::rasterToPoints(datat1)
  datat2 <- raster::rasterToPoints(datat2)

  datat1 <- data.table::as.data.table(datat1)
  datat2 <- data.table::as.data.table(datat2)

  pixelcounts <- merge(datat1, datat2, by = c('x', 'y')) %>%
    as.data.frame() %>%
    'colnames<-'(c('x', 'y', 'value.x', 'value.y')) %>%
    dplyr::filter(value.x > 0, value.y > 0) %>%
    dplyr::group_by(value.x, value.y) %>%
    dplyr::summarise(Count = dplyr::n()) %>%
    dplyr::left_join(., linkdata, by = c('value.x' = 'MasterCat')) %>%
    dplyr::left_join(., linkdata, by = c('value.y' = 'MasterCat')) %>%
    dplyr::ungroup() %>%
    dplyr::select(-value.x, -value.y) %>%
    dplyr::rename(From = Crop.x, To = Crop.y) %>%
    dplyr::mutate(Acreage = Count*conversionfactor[1])

  return(pixelcounts)
}

