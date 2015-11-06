#' Imports EOBS data
#' @param variable String from ('TG', 'TN', 'TX, ...) / TODO: vector 
#' @param period Temporal object
#' @param area Spatial object
#' @param grid String from ('0.25', '0.5', 'R0.25', ...)
#' @param removeNA Boolean indicating if rows with NA can be deleted / TODO: This has to be fine tuned
#' @param download Boolean indicating whether to download
#' @note Fixed file is loaded instead of OPeNDAP request, i.e. variable,
#'  period, and grid are deprecated
importEOBS <- function(variable, period, area, grid, removeNA=TRUE,
                       download=TRUE) {
  data <- importEOBS.bbox(variable, period, sp::bbox(area), grid)
  if ( !is.matrix(area) ) data <- removeOutsiders(data, area)
  if ( removeNA ) data <- removeNAvalues(data) 
  return(data)
}

#' Cuts a bbox from the ncdf file and returns the values as data.table
#' Not for external use
importEOBS.bbox <- function(variable, period, bbox, grid) {
  variable = 'tg'
  # This functionality could be replaced with OPeNDAP
  filename <- '~/Desktop/tg_0.50deg_reg_1995-2015_v12.0.nc'
  tmpFile  <- ncdf4::nc_open(filename, readunlim = FALSE)
  
  values <- list()
  values$lat         <- ncdf4::ncvar_get(tmpFile, varid = 'latitude',  start = 1, count = -1)
  values$lon         <- ncdf4::ncvar_get(tmpFile, varid = 'longitude', start = 1, count = -1)
  values$time        <- ncdf4::ncvar_get(tmpFile, varid = 'time',      start = 1, count = -1)
  values[[variable]] <- ncdf4::ncvar_get(tmpFile, varid = variable,    start = c(1, 1, 1), count = c(-1,-1,-1))
  
  ncdf4::nc_close(tmpFile)  
  
  validRange <- list()
  validRange$time <- 6941 : 7305 # Here we would like to have the period 
  validRange$lat <- which(findInterval(values$lat, bbox[2,])==1)
  validRange$lon <- which(findInterval(values$lon, bbox[1,])==1)
  
  validValues <- list()
  validValues$lat         <- values$lat[validRange$lat]
  validValues$lon         <- values$lon[validRange$lon]
  validValues$time        <- as.Date(values$time[validRange$time], origin="1950-01-01")
  validValues[[variable]] <- values[[variable]][validRange$lon, validRange$lat, validRange$time]
  
  return(createDataTableFromNCDF(variable, validValues))
}

#' Creates a data.table from the ncdf4 input
#' Not for external use
createDataTableFromNCDF <- function(variable, validValues) {
  dataFrame <- plyr::adply(validValues[[variable]], c(1,2,3))
  dataTable <- data.table(dataFrame)
  dataTable[, lat := validValues$lat[X2]]
  dataTable[, lon := validValues$lon[X1]]
  dataTable[, time := validValues$time[X3]]
  dataTable[, paste(variable) := V1]
  dataTable[, X1 := NULL]
  dataTable[, X2 := NULL]
  dataTable[, X3 := NULL]
  dataTable[, V1 := NULL]
  #setkey(list(lat, lon, time))
  return(dataTable)
}

#' Removes points outside of the SpatialPolygons
#' Not for external use
removeOutsiders <- function(data, area) {
  setkey(data, lon, lat)
  data[, pointID:=.GRP, by=key(data)]
  coords <- data[, .(lon = unique(lon),
                     lat = unique(lat)), by = pointID][, .(lon, lat)]
  points <- sp::SpatialPoints(coords, area@proj4string)
  index  <- data[, unique(pointID)][which(!is.na(sp::over(points,
                                            as(area, 'SpatialPolygons'))))]
  data <- data[pointID %in% index]
  setkey(data, lon, lat)
  return(data[, pointID:=.GRP, by=key(data)])
}

#' Removes all NA rows 
#' Not for external use
removeNAvalues <- function(data) {
  data <- data[which(!is.na(tg)), ]
  setkey(data, lon, lat)
  data[, pointID:=.GRP, by=key(data)]
}
