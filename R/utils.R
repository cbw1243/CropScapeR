GetCDLDataF <- function(fips, year, tol_time){
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=',
                year, '&fips=', fips)

  data <- tryCatch(httr::GET(url, httr::timeout(tol_time)),
                   error = function(x) x)
  if(class(data)[1] != 'response') stop('Request time limit is reached. Try to increase tol_time, or try later.\n')

  dataX <- httr::content(data, 'text')
  num <- gregexpr('returnURL', dataX)
  url2 <- substr(dataX, num[[1]][1]+10, num[[1]][2]-3)
  outdata <- raster::raster(url2)

  return(outdata)
}


GetCDLDataP <- function(point, year, tol_time){
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLValue?year=',
                year, '&x=', point[1],'&y=', point[2])

  data <- tryCatch(httr::GET(url, httr::timeout(tol_time)),
                   error = function(x) x)

  if(class(data)[1] != 'response') stop('Request time limit is reached. Try to increase tol_time, or try later.\n')

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


GetCDLDataPs <- function(points, year, mat = F, tol_time){
  points <- paste0(points, collapse = ',')
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=',
                year, '&points=', points)

  url2 <- tryCatch(httr::GET(url, httr::timeout(tol_time)),
                   error = function(x) x)
  if(class(url2)[1] != 'response') stop('Request time limit is reached. Try to increase tol_time, or try later.\n')

  url2X <- httr::content(url2, as = 'text')
  num <- gregexpr('returnURL', url2X)[[1]]
  url2 <- substr(url2X, num[1]+10, num[2]-3)

  outdata <- raster::raster(url2)
  return(outdata)
}

GetCDLDataB <- function(box, year, mat = F, tol_time){
  box <- paste0(box, collapse = ',')
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=', year, '&bbox=', box)

  url2 <- tryCatch(httr::GET(url, httr::timeout(tol_time)),
                   error = function(x) x)
  if(class(url2)[1] != 'response') stop('Request time limit is reached. Try to increase tol_time, or try later.\n')

  url2X <- httr::content(url2, as = 'text')
  num <- gregexpr('returnURL', url2X)[[1]]
  url2 <- substr(url2X, num[1]+10, num[2]-3)

  outdata <- raster::raster(url2)
  return(outdata)
}



GetCDLCompF <- function(fips, year1, year2, mat = TRUE, tol_time, manual_try){
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLComp?year1=',
                year1,'&year2=',year2,'&fips=', fips, '&format=csv')

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
    outdata <- data.table::fread(url2)
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


GetCDLCompB <- function(box, year1, year2, mat = TRUE, tol_time, manual_try){
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


GetCDLCompPs <- function(points, year1, year2, mat = TRUE, tol_time, manual_try){
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
      outdata <- data.table::fread(url2)
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

GetCDLImageF <- function(fips, year, format = 'png', destfile = NULL, verbose = TRUE, tol_time){

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
  if(isTRUE(verbose)) cat('The', format, 'file is saved at ', destfile)
  utils::download.file(url4, destfile = destfile, mode = 'wb')
}

GetCDLImageB <- function(box, year, format = 'png', destfile = NULL, verbose = TRUE, tol_time){
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
  if(isTRUE(verbose)) cat('The', format, 'file is saved at ', destfile)
  utils::download.file(url4, destfile = destfile, mode = 'wb')
}

GetCDLImagePs <- function(points, year, format = 'png', destfile = NULL, verbose = TRUE, tol_time){
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
  if(isTRUE(verbose)) cat('The', format, 'file is saved at ', destfile)
  utils::download.file(url4, destfile = destfile, mode = 'wb')
}


GetCDLStatF <- function(fips, year, mat = FALSE, tol_time){
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLStat?year=',
                year, '&fips=', fips, '&format=txt')
  data <- httr::GET(url, httr::timeout(tol_time))
  dataX <- httr::content(data, 'text')

  num <- gregexpr('returnURL', dataX)
  url2 <- substr(dataX, num[[1]][1]+10, num[[1]][2]-3)

  data <- data.table::fread(url2)
  data <- data[,-c(2,4)]
  return(data)
}


GetCDLStatPs <- function(points, year, mat = F, tol_time){
  points <- paste0(points, collapse = ',')
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLStat?year=',
                year, '&points=', points, '&format=csv')

  url2 <- httr::GET(url, httr::timeout(tol_time))
  url2X <- httr::content(url2, as = 'text')
  num <- gregexpr('returnURL', url2X)[[1]]
  url2 <- substr(url2X, num[1]+10, num[2]-3)

  data <- data.table::fread(url2)
  return(data)
}

GetCDLStatB <- function(box, year, mat = F, tol_time){
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



