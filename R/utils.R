GetCDLDataF <- function(fips, year){
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=',
                year, '&fips=', fips)
  data <- httr::GET(url)
  dataX <- httr::content(data, 'text')

  num <- gregexpr('returnURL', dataX)
  url2 <- substr(dataX, num[[1]][1]+10, num[[1]][2]-3)

  data <- raster::raster(url2)
  return(data)
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

  data <- raster::raster(url2)
  return(data)
}

GetCDLDataB <- function(box, year, mat = F){
  box <- paste0(box, collapse = ',')
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=', year, '&bbox=', box)

  url2 <- httr::GET(url)
  url2X <- httr::content(url2, as = 'text')
  num <- gregexpr('returnURL', url2X)[[1]]
  url2 <- substr(url2X, num[1]+10, num[2]-3)

  data <- raster::raster(url2)
  return(data)
}

GetCDLCompF <- function(fips, year1, year2, mat = TRUE){
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLComp?year1=',
                year1,'&year2=',year2,'&fips=', fips, '&format=csv')
  data <- httr::GET(url)
  dataX <- httr::content(data, 'text')

  if(isTRUE(mat)){
    num <- gregexpr('returnReportURL', dataX)
    url2 <- substr(dataX, num[[1]][1]+16, num[[1]][2]-3)
    data <- data.table::fread(url2)
    data$aoi <- fips
  }else{
    num <- gregexpr('returnTIFURL', dataX)
    url2 <- substr(dataX, num[[1]][1]+13, num[[1]][2]-3)
    data <- raster::raster(url2)
  }
  return(data)
}


GetCDLCompB <- function(box, year1, year2, mat = TRUE){
  box <- paste0(box, collapse = ',')
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLComp?year1=',
                year1,'&year2=',year2,'&bbox=', box, '&format=json')
  data <- httr::GET(url)
  dataX <- httr::content(data, 'text')

  if(isTRUE(mat)){
    num <- gregexpr('returnReportURL', dataX)
    url2 <- substr(dataX, num[[1]][1]+16, num[[1]][2]-3)
    data <- RJSONIO::fromJSON(url2)$ow

    data <- lapply(data, function(x) data.frame(matrix(unlist(x), nrow = 1), stringsAsFactors = F))
    data <- dplyr::bind_rows(data)[,-1]
    colnames(data) <- c('From', 'To', 'Count', 'Acreage')
    data$aoi <- box

  }else{
    num <- gregexpr('returnTIFURL', dataX)
    url2 <- substr(dataX, num[[1]][1]+13, num[[1]][2]-3)
    data <- raster::raster(url2)
  }
  return(data)
}


GetCDLCompPs <- function(points, year1, year2, mat = TRUE){
  points <- paste0(points, collapse = ',')
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLComp?year1=',
                year1,'&year2=',year2,'&points=', points, '&format=csv')
  data <- httr::GET(url)
  dataX <- httr::content(data, 'text')

  if(isTRUE(mat)){
    num <- gregexpr('returnReportURL', dataX)
    url2 <- substr(dataX, num[[1]][1]+16, num[[1]][2]-3)
    data <- data.table::fread(url2)
    data$aoi <- points
  }else{
    num <- gregexpr('returnTIFURL', dataX)
    url2 <- substr(dataX, num[[1]][1]+13, num[[1]][2]-3)
    data <- raster::raster(url2)
  }
  return(data)
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
