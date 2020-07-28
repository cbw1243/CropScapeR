convert_crs <- function(aoi, crs){
  targetCRS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  numps <- length(aoi)
  aoi_dat <- data.frame(x = aoi[seq(1, numps, by = 2)], y = aoi[seq(2, numps, by = 2)])
  aoi_sf <- sf::st_as_sf(aoi_dat, coords = c('x', 'y'), crs = crs)
  aoi_sf_trans <- sf::st_transform(aoi_sf, targetCRS)
  out <- paste0(as.vector(t(sf::st_coordinates(aoi_sf_trans))), collapse = ',')
  return(out)
}

GetCDLDataS <- function(poly, year, tol_time, save_path, readr){
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=', year, '&aoiURL=', poly)
  data <- tryCatch(httr::GET(url, httr::timeout(tol_time)), error = function(x) x)

  if(class(data)[1] != 'response') stop(paste0('No response from the server. Error message from server is:\n', data$message))
  dataX <- httr::content(data, 'text')

  if(grepl('Error', dataX)) stop(dataX)

  num <- gregexpr('returnURL', dataX)
  url2 <- substr(dataX, num[[1]][1]+10, num[[1]][2]-3)

  if(!is.null(save_path)){
    file_info <- httr::GET(url2, httr::write_disk(path = save_path, overwrite = T))
    message(paste0('Data is saved at:', save_path))
    if(isTRUE(readr)){
      outdata <- raster::raster(save_path)
    }else{
      outdata <- NULL
    }
  }else{
    outdata <- raster::raster(url2)
  }
  return(outdata)
}


GetCDLDataF <- function(fips, year, tol_time, save_path, readr){
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=', year, '&fips=', fips)
  data <- tryCatch(httr::GET(url, httr::timeout(tol_time)), error = function(x) x)

  if(class(data)[1] != 'response') stop(paste0('No response from the server. Error message from server is:\n', data$message))
  dataX <- httr::content(data, 'text')
  num <- gregexpr('returnURL', dataX)
  url2 <- substr(dataX, num[[1]][1]+10, num[[1]][2]-3)

  if(!is.null(save_path)){
    file_info <- httr::GET(url2, httr::write_disk(path = save_path, overwrite = T))
    message(paste0('Data is saved at:', save_path))
    if(isTRUE(readr)){
      outdata <- raster::raster(save_path)
    }else{
      outdata <- NULL
    }
  }else{
    if(!isTRUE(readr)) warning('readr focred to be TRUE, because no save_path is provided. \n')
    outdata <- raster::raster(url2)
   }
  return(outdata)
}


GetCDLDataP <- function(point, year, tol_time){
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLValue?year=',
                year, '&x=', point[1],'&y=', point[2])

  data <- tryCatch(httr::GET(url, httr::timeout(tol_time)), error = function(x) x)

  if(class(data)[1] != 'response') stop(paste0('No response from the server. Error message from server is:\n', data$message))
  dataX <- httr::content(data, 'text')
  num <- gregexpr('Result', dataX)[[1]]
  dataX <- substr(dataX, num[1]+8, num[2]-4)

  out <- matrix(NA, 1, 5)
  colnames(out) <- c('x', 'y', 'value', 'category', 'color')
  out[1, ] <- sapply(strsplit(dataX, ',')[[1]], function(x) {
    x <- gsub(".*:","",x)
    x <- gsub("\"","",x)
    x <- trimws(x)
  })
  out <- base::as.data.frame(out)
  return(out)
}


GetCDLDataPs <- function(points, year, tol_time, save_path, readr){
  points <- paste0(points, collapse = ',')
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=',
                year, '&points=', points)

  url2 <- tryCatch(httr::GET(url, httr::timeout(tol_time)), error = function(x) x)
  if(class(url2)[1] != 'response') stop(paste0('No response from the server.\nError message from server is: "', url2$message, '"'))

  url2X <- httr::content(url2, as = 'text')
  num <- gregexpr('returnURL', url2X)[[1]]
  url2 <- substr(url2X, num[1]+10, num[2]-3)

  if(!is.null(save_path)){
    file_info <- httr::GET(url2, httr::write_disk(path = save_path, overwrite = T))
    message(paste0('Data is saved at:', save_path))
    if(isTRUE(readr)){
      outdata <- raster::raster(save_path)
    }else{
      outdata <- NULL
    }
  }else{
    if(!isTRUE(readr)) warning('readr focred to be TRUE, because no save_path is provided. \n')
    outdata <- raster::raster(url2)
  }
  return(outdata)
}


GetCDLDataB <- function(box, year, tol_time, save_path, readr){
  box <- paste0(box, collapse = ',')
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=', year, '&bbox=', box)

  url2 <- tryCatch(httr::GET(url, httr::timeout(tol_time)), error = function(x) x)
  if(class(url2)[1] != 'response') stop(paste0('No response from the server.\nError message from server is: "', url2$message, '"'))

  url2X <- httr::content(url2, as = 'text')
  num <- gregexpr('returnURL', url2X)[[1]]
  url2 <- substr(url2X, num[1]+10, num[2]-3)

  if(!is.null(save_path)){
    message(paste0('Data is saved at:', save_path))
    file_info <- httr::GET(url2, httr::write_disk(path = save_path, overwrite = T))
    if(isTRUE(readr)){
      outdata <- raster::raster(save_path)
    }else{
      outdata <- NULL
    }
  }else{
    if(!isTRUE(readr)) warning('readr focred to be TRUE, because no save_path is provided. \n')
    outdata <- raster::raster(url2)
  }
  return(outdata)
}


GetCDLCompS <- function(poly, year1, year2, mat, tol_time, manual_try){
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLComp?year1=',
                year1,'&year2=',year2,'&aoiURL=', poly, '&format=csv')

  data <- tryCatch(httr::GET(url, httr::timeout(tol_time)), error = function(x) x)

  if(class(data)[1] == 'response'){
    dataX <- httr::content(data, 'text')
    if(grepl('Error', data)) stop(dataX)
  }else{
    dataX <- data$message
    if(grepl('Error', data)) stop(dataX)
    data$status_code <- 999
  }

  dataXtry <- grepl('ERROR 1: TIFFFetchDirectory', dataX) | grepl('Mismatch size of file 1 and file 2', dataX) | grepl('Timeout was reached', dataX)

  if(data$status_code == 200){
    if(isTRUE(mat)){
      num <- gregexpr('returnReportURL', dataX)
      url2 <- substr(dataX, num[[1]][1]+16, num[[1]][2]-3)
      temp_file <- tempfile(fileext = '.csv')
      temp_data <- utils::download.file(url = url2, destfile = temp_file, method = 'wget', quiet = T)
      outdata <- data.table::fread(temp_file)
      ignore_file <- file.remove(temp_file)
      if(nrow(outdata) == 0) stop(paste0('Error: The CDL TIF files are likely corrupted.'))
      outdata$aoi <- poly
    }else{
      num <- gregexpr('returnTIFURL', dataX)
      url2 <- substr(dataX, num[[1]][1]+13, num[[1]][2]-3)
      outdata <- raster::raster(url2)
    }
  }else{
    if(isTRUE(dataXtry) & isTRUE(manual_try)){
      datat1 <- GetCDLData(aoi = poly, year = year1, mat = FALSE, type = 's', tol_time)
      datat2 <- GetCDLData(aoi = poly, year = year2, mat = FALSE, type = 's', tol_time)
      outdata <- manualrotate(datat1, datat2)
      if(nrow(outdata) == 0) stop('Warning: CropScape cannot calculate for crop cover changes. Attempted manual calculation, but there is no match between the raster files.\n')
      outdata$aoi <- poly
      warning(paste0('Warning: CropScape cannot calculate for crop cover changes. The returned data are calculated manually using the manualrotate function.\n Error message from CropScape is :', dataX))
    }else{
      stop(paste0('Error: The requested data might not exist in the CDL database. \nError message from CropScape is :', dataX))
    }
  }
  return(outdata)
}

GetCDLCompF <- function(fips, year1, year2, mat, tol_time, manual_try){
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLComp?year1=',
                year1,'&year2=',year2,'&fips=', fips, '&format=csv')

  data <- tryCatch(httr::GET(url, httr::timeout(tol_time)), error = function(x) x)

  if(class(data)[1] == 'response'){
    dataX <- httr::content(data, 'text')
  }else{
    dataX <- data$message
    data$status_code <- 999
  }

  dataXtry <- grepl('ERROR 1: TIFFFetchDirectory', dataX) | grepl('Mismatch size of file 1 and file 2', dataX) | grepl('Timeout was reached', dataX)

  if(data$status_code == 200){
    if(isTRUE(mat)){
      num <- gregexpr('returnReportURL', dataX)
      url2 <- substr(dataX, num[[1]][1]+16, num[[1]][2]-3)
      temp_file <- tempfile(fileext = '.csv')
      temp_data <- utils::download.file(url = url2, destfile = temp_file, method = 'wget', quiet = T)
      outdata <- data.table::fread(temp_file)
      ignore_file <- file.remove(temp_file)
      if(nrow(outdata) == 0) stop(paste0('Error: The CDL TIF files are likely corrupted.'))
      outdata$aoi <- fips
    }else{
      num <- gregexpr('returnTIFURL', dataX)
      url2 <- substr(dataX, num[[1]][1]+13, num[[1]][2]-3)
      outdata <- raster::raster(url2)
    }
  }else{
    if(isTRUE(dataXtry) & isTRUE(manual_try)){
      datat1 <- GetCDLData(aoi = fips, year = year1, mat = FALSE, type = 'f', tol_time)
      datat2 <- GetCDLData(aoi = fips, year = year2, mat = FALSE, type = 'f', tol_time)
      outdata <- manualrotate(datat1, datat2)
      if(nrow(outdata) == 0) stop('Warning: CropScape cannot calculate for crop cover changes. Attempted manual calculation, but there is no match between the raster files.\n')
      outdata$aoi <- fips
      warning(paste0('Warning: CropScape cannot calculate for crop cover changes. The returned data are calculated manually using the manualrotate function.\n Error message from CropScape is :', dataX))
    }else{
      stop(paste0('Error: The requested data might not exist in the CDL database. \nError message from CropScape is :', dataX))
    }
  }
  return(outdata)
}


GetCDLCompB <- function(box, year1, year2, mat, tol_time, manual_try){
  box <- paste0(box, collapse = ',')
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLComp?year1=',
                year1,'&year2=',year2,'&bbox=', box, '&format=json')
  data <- tryCatch(httr::GET(url, httr::timeout(tol_time)),
                   error = function(x) x)

  if(class(data)[1] == 'response'){
    dataX <- httr::content(data, 'text')
  }else{
    dataX <- data$message
    data$status_code <- 999
  }

  dataXtry <- grepl('ERROR 1: TIFFFetchDirectory', dataX) | grepl('Mismatch size of file 1 and file 2', dataX) | grepl('Timeout was reached', dataX)

  if(data$status_code == 200){
    if(isTRUE(mat)){
      num <- gregexpr('returnReportURL', dataX)
      url2 <- substr(dataX, num[[1]][1]+16, num[[1]][2]-3)
      outdataX <- RJSONIO::fromJSON(url2)$ow
      outdata <- lapply(outdataX, function(x) data.frame(matrix(unlist(x), nrow = 1), stringsAsFactors = F))
      outdata <- dplyr::bind_rows(outdata)[,-1]
      colnames(outdata) <- c('From', 'To', 'Count', 'Acreage')
      outdata$Count <- as.numeric(outdata$Count)
      outdata$Acreage <- as.numeric(outdata$Acreage)
      outdata$aoi <- box
    }else{
      num <- gregexpr('returnTIFURL', dataX)
      url2 <- substr(dataX, num[[1]][1]+13, num[[1]][2]-3)
      outdata <- raster::raster(url2)
    }
  }else{
    if(isTRUE(dataXtry) & isTRUE(manual_try)){
      datat1 <- GetCDLData(aoi = box, year = year1, mat = FALSE, type = 'b', tol_time)
      datat2 <- GetCDLData(aoi = box, year = year2, mat = FALSE, type = 'b', tol_time)
      outdata <- manualrotate(datat1, datat2)
      if(nrow(outdata) == 0) stop('Warning: CropScape cannot calculate for crop cover changes. Attempted manual calculation, but there is no match between the raster files.\n')
      outdata$aoi <- box
      warning(paste0('Warning: CropScape cannot calculate for crop cover changes. The returned data are calculated manually using the manualrotate function.\n Error message from CropScape is :', dataX))
    }else{
      stop(paste0('Error: No data is found in the CDL database with the request. \n Error message from CropScape is :', dataX))
    }
  }
  return(outdata)
}


GetCDLCompPs <- function(points, year1, year2, mat, tol_time, manual_try){
  points <- paste0(points, collapse = ',')
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLComp?year1=',
                year1,'&year2=',year2,'&points=', points, '&format=csv')
  data <- tryCatch(httr::GET(url, httr::timeout(tol_time)),
                   error = function(x) x)

  if(class(data)[1] == 'response'){
    dataX <- httr::content(data, 'text')
  }else{
    dataX <- data$message
    data$status_code <- 999
  }

  dataXtry <- grepl('ERROR 1: TIFFFetchDirectory', dataX) | grepl('Mismatch size of file 1 and file 2', dataX) | grepl('Timeout was reached', dataX)

  if(data$status_code == 200){
    if(isTRUE(mat)){
      num <- gregexpr('returnReportURL', dataX)
      url2 <- substr(dataX, num[[1]][1]+16, num[[1]][2]-3)
      temp_file <- tempfile(fileext = '.csv')
      temp_data <- utils::download.file(url = url2, destfile = temp_file, method = 'wget', quiet = T)
      outdata <- data.table::fread(temp_file)
      ignore_file <- file.remove(temp_file)
      if(nrow(outdata) == 0) stop(paste0('Error: The CDL TIF files are likely corrupted.'))
      outdata$aoi <- points
    }else{
      num <- gregexpr('returnTIFURL', dataX)
      url2 <- substr(dataX, num[[1]][1]+13, num[[1]][2]-3)
      outdata <- raster::raster(url2)
    }
  }else{
    if(isTRUE(dataXtry) & isTRUE(manual_try)){
      datat1 <- GetCDLData(aoi = points, year = year1, mat = FALSE, type = 'ps', tol_time)
      datat2 <- GetCDLData(aoi = points, year = year2, mat = FALSE, type = 'ps', tol_time)
      outdata <- manualrotate(datat1, datat2)
      if(nrow(outdata) == 0) stop('Warning: CropScape cannot calculate for crop cover changes. Attempted manual calculation, but there is no match between the raster files.\n')
      outdata$aoi <- points
      warning(paste0('Warning: CropScape cannot calculate for crop cover changes. The returned data are calculated manually using the manualrotate function.\n Error message from CropScape is :', dataX))
    }else{
      stop(paste0('Error: The requested data might not exist in the CDL database. \nError message from CropScape is :', dataX))
    }
  }
  return(outdata)
}

GetCDLImageS <- function(poly, year, format, destfile, verbose, tol_time){

  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=', year, '&aoiURL=', poly)

  url2 <- httr::GET(url, httr::timeout(tol_time))
  url2X <- httr::content(url2, as = 'text')
  num <- gregexpr('returnURL', url2X)[[1]]
  url2 <- substr(url2X, num[1]+10, num[2]-3)

  url3 <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLImage?files=',
                 url2, '&format=', format)
  data <- httr::GET(url3)
  dataX <- httr::content(data, 'text')

  num <- gregexpr('returnURLArray', dataX)
  url4 <- substr(dataX, num[[1]][1]+15, num[[1]][2]-3)

  if(is.null(destfile)) destfile <- tempfile()
  if(isTRUE(verbose)) message('The ', format, ' file is saved at ', destfile, '\n')
  utils::download.file(url4, destfile = destfile, mode = 'wb', method = 'wget', quiet = ifelse(isTRUE(verbose), FALSE, TRUE))
}

GetCDLImageF <- function(fips, year, format, destfile, verbose, tol_time){

  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=', year, '&fips=', fips)

  url2 <- httr::GET(url, httr::timeout(tol_time))
  url2X <- httr::content(url2, as = 'text')
  num <- gregexpr('returnURL', url2X)[[1]]
  url2 <- substr(url2X, num[1]+10, num[2]-3)

  url3 <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLImage?files=',
                 url2, '&format=', format)
  data <- httr::GET(url3)
  dataX <- httr::content(data, 'text')

  num <- gregexpr('returnURLArray', dataX)
  url4 <- substr(dataX, num[[1]][1]+15, num[[1]][2]-3)

  if(is.null(destfile)) destfile <- tempfile()
  if(isTRUE(verbose)) message('The ', format, ' file is saved at ', destfile, '\n')
  utils::download.file(url4, destfile = destfile, mode = 'wb', method = 'wget', quiet = ifelse(isTRUE(verbose), FALSE, TRUE))
}

GetCDLImageB <- function(box, year, format, destfile, verbose, tol_time){
  box <- paste0(box, collapse = ',')
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=', year, '&bbox=', box)

  url2 <- httr::GET(url, httr::timeout(tol_time))
  url2X <- httr::content(url2, as = 'text')
  num <- gregexpr('returnURL', url2X)[[1]]
  url2 <- substr(url2X, num[1]+10, num[2]-3)

  url3 <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLImage?files=',
                 url2, '&format=', format)
  data <- httr::GET(url3, httr::timeout(tol_time))
  dataX <- httr::content(data, 'text')

  num <- gregexpr('returnURLArray', dataX)
  url4 <- substr(dataX, num[[1]][1]+15, num[[1]][2]-3)

  if(is.null(destfile)) destfile <- tempfile()
  if(isTRUE(verbose)) message('The', format, 'file is saved at ', destfile, '\n')
  utils::download.file(url4, destfile = destfile, mode = 'wb', method = 'wget', quiet = ifelse(isTRUE(verbose), FALSE, TRUE))
}

GetCDLImagePs <- function(points, year, format, destfile, verbose, tol_time){
  points <- paste0(points, collapse = ',')
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=',
                year, '&points=', points)

  url2 <- httr::GET(url, httr::timeout(tol_time))
  url2X <- httr::content(url2, as = 'text')
  num <- gregexpr('returnURL', url2X)[[1]]
  url2 <- substr(url2X, num[1]+10, num[2]-3)

  url3 <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLImage?files=',
                 url2, '&format=', format)
  data <- httr::GET(url3, httr::timeout(tol_time))
  dataX <- httr::content(data, 'text')

  num <- gregexpr('returnURLArray', dataX)
  url4 <- substr(dataX, num[[1]][1]+15, num[[1]][2]-3)

  if(is.null(destfile)) destfile <- tempfile()
  if(isTRUE(verbose)) message('The', format, 'file is saved at ', destfile, '\n')
  utils::download.file(url4, destfile = destfile, mode = 'wb', method = 'wget', quiet = ifelse(isTRUE(verbose), FALSE, TRUE))
}

GetCDLStatS <- function(poly, year, tol_time){
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLStat?year=',
                year, '&aoiURL=', poly, '&format=txt')
  data <- httr::GET(url, httr::timeout(tol_time))
  dataX <- httr::content(data, 'text')
  if(grepl('Error', dataX)) stop(dataX)

  num <- gregexpr('returnURL', dataX)
  url2 <- substr(dataX, num[[1]][1]+10, num[[1]][2]-3)

  temp_file <- tempfile(fileext = '.csv')
  temp_data <- utils::download.file(url = url2, destfile = temp_file, method = 'wget', quiet = T)
  data <- data.table::fread(temp_file)
  ignore_file <- file.remove(temp_file)
  data <- data[,-c(2,4)]

  return(data)
}


GetCDLStatF <- function(fips, year, tol_time){
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLStat?year=',
                year, '&fips=', fips, '&format=txt')
  data <- httr::GET(url, httr::timeout(tol_time))
  dataX <- httr::content(data, 'text')

  num <- gregexpr('returnURL', dataX)
  url2 <- substr(dataX, num[[1]][1]+10, num[[1]][2]-3)

  temp_file <- tempfile(fileext = '.csv')
  temp_data <- utils::download.file(url = url2, destfile = temp_file, method = 'wget', quiet = T)
  data <- data.table::fread(temp_file)
  ignore_file <- file.remove(temp_file)
  data <- data[,-c(2,4)]

  return(data)
}


GetCDLStatPs <- function(points, year, tol_time){
  points <- paste0(points, collapse = ',')
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLStat?year=',
                year, '&points=', points, '&format=csv')

  url2 <- httr::GET(url, httr::timeout(tol_time))
  url2X <- httr::content(url2, as = 'text')
  num <- gregexpr('returnURL', url2X)[[1]]
  url2 <- substr(url2X, num[1]+10, num[2]-3)

  temp_file <- tempfile(fileext = '.csv')
  temp_data <- utils::download.file(url = url2, destfile = temp_file, method = 'wget', quiet = T)
  data <- data.table::fread(temp_file)
  ignore_file <- file.remove(temp_file)

  return(data)
}

GetCDLStatB <- function(box, year, tol_time){
  box <- paste0(box, collapse = ',')
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLStat?year=',
                year, '&bbox=', box, '&format=json')

  url2 <- httr::GET(url, httr::timeout(tol_time))
  url2X <- httr::content(url2, as = 'text')
  num <- gregexpr('returnURL', url2X)[[1]]
  url2 <- substr(url2X, num[1]+10, num[2]-3)

  data <- RJSONIO::fromJSON(url2)$ow

  data <- lapply(data, function(x) data.frame(matrix(unlist(x), nrow = 1), stringsAsFactors = F))
  data <- dplyr::bind_rows(data)
  colnames(data) <- c('Value', 'Counts', 'Category', 'Color', 'Acreage')
  return(data)
}
