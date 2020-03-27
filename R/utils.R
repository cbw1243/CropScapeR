GetCDLDataF <- function(fips, year){
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=',
                year, '&fips=', fips)
  data <- httr::GET(url)
  dataX <- httr::content(data, 'text')

  num <- gregexpr('returnURL', dataX)
  url2 <- substr(dataX, num[[1]][1]+10, num[[1]][2]-3)

  outdata <- raster::raster(url2)
  return(outdata)
}


GetCDLDataP <- function(point, year){
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLValue?year=',
                year, '&x=', point[1],'&y=', point[2])
  data <- httr::GET(url)
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
  return(out)
}


GetCDLDataPs <- function(points, year, mat = F){
  points <- paste0(points, collapse = ',')
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=',
                year, '&points=', points)

  url2 <- httr::GET(url)
  url2X <- httr::content(url2, as = 'text')
  num <- gregexpr('returnURL', url2X)[[1]]
  url2 <- substr(url2X, num[1]+10, num[2]-3)

  outdata <- raster::raster(url2)
  return(outdata)
}

GetCDLDataB <- function(box, year, mat = F){
  box <- paste0(box, collapse = ',')
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=', year, '&bbox=', box)

  url2 <- httr::GET(url)
  url2X <- httr::content(url2, as = 'text')
  num <- gregexpr('returnURL', url2X)[[1]]
  url2 <- substr(url2X, num[1]+10, num[2]-3)

  outdata <- raster::raster(url2)
  return(outdata)
}

manualrotate <- function(aoi, year1, year2, type = NULL, crs = NULL){
  datat1 <- GetCDLData(aoi = aoi, year = year1, mat = FALSE, type = type, crs = crs)
  datat2 <- GetCDLData(aoi = aoi, year = year2, mat = FALSE, type = type, crs = crs)

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
    dplyr::mutate(Acreage = Count*conversionfactor)

  return(pixelcounts)
}

GetCDLCompF <- function(fips, year1, year2, mat = TRUE){
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLComp?year1=',
                year1,'&year2=',year2,'&fips=', fips, '&format=csv')
  data <- httr::GET(url)
  dataX <- httr::content(data, 'text')

  if(data$status_code == 200){
    if(isTRUE(mat)){
    num <- gregexpr('returnReportURL', dataX)
    url2 <- substr(dataX, num[[1]][1]+16, num[[1]][2]-3)
    outdata <- data.table::fread(url2)
    if(nrow(outdata) == 0) stop(paste0('Error: The CDL TIF files are likely corrupted.'))
    outdata$aoi <- fips
    }else{
    num <- gregexpr('returnTIFURL', dataX)
    url2 <- substr(dataX, num[[1]][1]+13, num[[1]][2]-3)
    outdata <- raster::raster(url2)
    }
  }else{
    if(grepl('ERROR 1: TIFFFetchDirectory', dataX) | grepl('Mismatch size of file 1 and file 2', dataX)){
      outdata <- manualrotate(aoi = fips, year1 = year1, year2 = year2, type = 'f')
      if(nrow(outdata) == 0) stop('The manually calculated crop cover change data has no observation. Something is wrong with the CDL data.')
      outdata$aoi <- fips
      warning(paste0('Warning: CropScape cannot calculate the crop cover changes. So use manual calculations. Error message from CropScape is :', dataX))
    }else{
      stop(paste0('Error: The requested data might not exist in the CDL database. Error message from CropScape is :', dataX))
    }
  }
  return(outdata)
}


GetCDLCompB <- function(box, year1, year2, mat = TRUE){
  box <- paste0(box, collapse = ',')
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLComp?year1=',
                year1,'&year2=',year2,'&bbox=', box, '&format=json')
  data <- httr::GET(url)
  dataX <- httr::content(data, 'text')

  if(data$status_code == 200){
    if(isTRUE(mat)){
      num <- gregexpr('returnReportURL', dataX)
      url2 <- substr(dataX, num[[1]][1]+16, num[[1]][2]-3)
      outdataX <- RJSONIO::fromJSON(url2)$ow

      outdata <- lapply(outdataX, function(x) data.frame(matrix(unlist(x), nrow = 1), stringsAsFactors = F))
      outdata <- dplyr::bind_rows(outdata)[,-1]
      colnames(outdata) <- c('From', 'To', 'Count', 'Acreage')
      outdata$aoi <- box

    }else{
      num <- gregexpr('returnTIFURL', dataX)
      url2 <- substr(dataX, num[[1]][1]+13, num[[1]][2]-3)
      outdata <- raster::raster(url2)
   }
  }else{
    if(grepl('ERROR 1: TIFFFetchDirectory', dataX) | grepl('Mismatch size of file 1 and file 2', dataX)){
      outdata <- manualrotate(aoi = box, year1 = year1, year2 = year2, type = 'b')
      if(nrow(outdata) == 0) stop('The manually calculated crop cover change data has no observation. Something is wrong with the CDL data.')
      outdata$aoi <- box
      warning(paste0('Warning: CropScape cannot calculate the crop cover changes. So use manual calculations. Error message from CropScape is :', dataX))
    }else{
      stop(paste0('Error: The requested data might not exist in the CDL database. Error message from CropScape is :', dataX))
    }
  }
  return(outdata)
}


GetCDLCompPs <- function(points, year1, year2, mat = TRUE){
  points <- paste0(points, collapse = ',')
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLComp?year1=',
                year1,'&year2=',year2,'&points=', points, '&format=csv')
  data <- httr::GET(url)
  dataX <- httr::content(data, 'text')

  if(data$status_code == 200){
    if(isTRUE(mat)){
      num <- gregexpr('returnReportURL', dataX)
      url2 <- substr(dataX, num[[1]][1]+16, num[[1]][2]-3)
      outdata <- data.table::fread(url2)
      outdata$aoi <- points
    }else{
      num <- gregexpr('returnTIFURL', dataX)
      url2 <- substr(dataX, num[[1]][1]+13, num[[1]][2]-3)
      outdata <- raster::raster(url2)
    }
  }else{
    if(grepl('ERROR 1: TIFFFetchDirectory', dataX) | grepl('Mismatch size of file 1 and file 2', dataX)){
      outdata <- manualrotate(aoi = points, year1 = year1, year2 = year2, type = 'ps')
      if(nrow(outdata) == 0) stop('The manually calculated crop cover change data has no observation. Something is wrong with the CDL data.')
      outdata$aoi <- points
      warning(paste0('Warning: CropScape cannot calculate the crop cover changes. So use manual calculations. Error message from CropScape is :', dataX))
    }else{
      stop(paste0('Error: The requested data might not exist in the CDL database. Error message from CropScape is :', dataX))
    }
  }
  return(outdata)
}

GetCDLImageF <- function(fips, year, format = 'png', destfile = NULL, verbose = TRUE){

  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=', year, '&fips=', fips)

  url2 <- httr::GET(url)
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
  if(isTRUE(verbose)) cat('The', format, 'file is saved at ', destfile)
  utils::download.file(url4, destfile = destfile, mode = 'wb')
}

GetCDLImageB <- function(box, year, format = 'png', destfile = NULL, verbose = TRUE){
  box <- paste0(box, collapse = ',')
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=', year, '&bbox=', box)

  url2 <- httr::GET(url)
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
  if(isTRUE(verbose)) cat('The', format, 'file is saved at ', destfile)
  utils::download.file(url4, destfile = destfile, mode = 'wb')
}

GetCDLImagePs <- function(points, year, format = 'png', destfile = NULL, verbose = TRUE){
  points <- paste0(points, collapse = ',')
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=',
                year, '&points=', points)

  url2 <- httr::GET(url)
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
  if(isTRUE(verbose)) cat('The', format, 'file is saved at ', destfile)
  utils::download.file(url4, destfile = destfile, mode = 'wb')
}


GetCDLStatF <- function(fips, year, mat = FALSE){
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLStat?year=',
                year, '&fips=', fips, '&format=txt')
  data <- httr::GET(url)
  dataX <- httr::content(data, 'text')

  num <- gregexpr('returnURL', dataX)
  url2 <- substr(dataX, num[[1]][1]+10, num[[1]][2]-3)

  data <- data.table::fread(url2)
  data <- data[,-c(2,4)]
  return(data)
}


GetCDLStatPs <- function(points, year, mat = F){
  points <- paste0(points, collapse = ',')
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLStat?year=',
                year, '&points=', points, '&format=csv')

  url2 <- httr::GET(url)
  url2X <- httr::content(url2, as = 'text')
  num <- gregexpr('returnURL', url2X)[[1]]
  url2 <- substr(url2X, num[1]+10, num[2]-3)

  data <- data.table::fread(url2)
  return(data)
}

GetCDLStatB <- function(box, year, mat = F){
  box <- paste0(box, collapse = ',')
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLStat?year=',
                year, '&bbox=', box, '&format=json')

  url2 <- httr::GET(url)
  url2X <- httr::content(url2, as = 'text')
  num <- gregexpr('returnURL', url2X)[[1]]
  url2 <- substr(url2X, num[1]+10, num[2]-3)

  data <- RJSONIO::fromJSON(url2)$ow

  data <- lapply(data, function(x) data.frame(matrix(unlist(x), nrow = 1), stringsAsFactors = F))
  data <- dplyr::bind_rows(data)
  colnames(data) <- c('Value', 'Counts', 'Category', 'Color', 'Acreage')
  return(data)
}



