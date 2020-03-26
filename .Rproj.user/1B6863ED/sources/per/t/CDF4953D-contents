countydata <- function(fips, year, mat = FALSE){
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=',
                year, '&fips=', fips)
  data <- httr::GET(url)
  dataX <- httr::content(data, 'text')

  num <- gregexpr('returnURL', dataX)
  url2 <- substr(dataX, num[[1]][1]+10, num[[1]][2]-3)

  data <- raster::raster(url2)
  return(data)
}


pointdata <- function(point, year){
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


pointSdata <- function(points, year, mat = F){
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

boxdata <- function(box, year, mat = F){
  box <- paste0(box, collapse = ',')
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=', year, '&bbox=', box)

  url2 <- httr::GET(url)
  url2X <- httr::content(url2, as = 'text')
  num <- gregexpr('returnURL', url2X)[[1]]
  url2 <- substr(url2X, num[1]+10, num[2]-3)

  data <- raster::raster(url2)
  return(data)
}
