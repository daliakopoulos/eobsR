#' Imports EOBS data
#' @param variable String from ('tg', 'tn', 'tx, ...)
#' @param period Either numeric, timeBased or ISO-8601 style (see package xts.)
#' @param area Either SpatialPolygons or SpatialPolygonsDataFrame object (see
#' package sp)
#' @param grid String from ('0.25reg', '0.50reg', '0.25rot', '0.50rot')
#' @param na.rm Boolean indicating if rows with NA can be deleted 
#' @param download Boolean indicating whether to download
#' @export
importEOBS <- function(variable, period, area, grid, na.rm=TRUE,
                       download=TRUE) {
  SanitizeInput(variable, period, area, grid)
  url  <- specifyURL(variable, grid)
  data <- getOpenDapValues(url, variable, sp::bbox(area), period)
  if ( !is.matrix(area) ) data <- removeOutsiders(data, area)
  if ( na.rm ) data <- removeNAvalues(data) 
  return(data)
}

# Checks whether the input to importEOBS is valid or not
# @param variable Variable name
# @param period Period
# @param area Area
# @param grid Grid
SanitizeInput <- function(variable, period, area, grid) {
  if (variable %in% c('tg_stderr', 'tn_stderr', 'tx_stderr', 'pp_stderr',
                      'rr_stderr')) {
    stop("Standard error of variables not yet implemented.")
  }
  else if (!variable %in% c('tg', 'tn', "tx", "pp", 'rr', 'tg_stderr', 'tn_stderr',
                       'tx_stderr', 'pp_stderr', 'rr_stderr')) {
    stop(paste("Variable", variable, "not known."))
  }
  tryCatch(xts::.parseISO8601(period),
           warning = function(e) {stop()},
           error = function(e) {
             stop("Period should be either Numeric, timeBased or ISO-8601 style.")
           })
  if (!class(area) %in% c("SpatialPolygons","SpatialPolygonsDataFrame")) {
    stop("Area should be of class SpatialPolygons or SpatialPolygonsDataFrame.")
  }
  if (!grid %in% c("0.25reg", "0.50reg", "0.25rot", "0.50rot")) {
    stop("Grid should be specified correctly.")
  }
}

# Specifies the url based on the variableName and the grid
# @param variableName Variable name
# @param grid Grid
specifyURL <- function(variableName, grid) {
  url <- 'http://opendap.knmi.nl/knmi/thredds/dodsC/e-obs_'
  if (grid=="0.50reg") {
    url <- paste(url, '0.50regular/', sep="")
    ending <- '_0.50deg_reg_v12.0.nc'
  }
  if (grid=="0.25reg") {
    url <- paste(url, '0.25regular/', sep="")
    ending <- '_0.25deg_reg_v12.0.nc'
  }
  url <- paste(url, variableName, ending, sep="")
  return(url)
}

# Accesses the OPeNDAB server
# @param opendapURL String
# @param variableName String which variable to get
# @param bbox Bounding box of spatial object
# @param period Time period
# @note This function is based on the script by Maarten Plieger
# https://publicwiki.deltares.nl/display/OET/OPeNDAP+subsetting+with+R
getOpenDapValues = function(opendapURL, variableName, bbox, period){
  print(paste("Loading opendapURL",opendapURL));
  
  # Open the dataset
  dataset = ncdf4::nc_open(opendapURL)
  
  # Get lon and lat variables, which are the dimensions of depth.
  values <- list()
  values$lat         <- ncdf4::ncvar_get(dataset, varid = 'latitude',  start = 1, count = -1)
  values$lon         <- ncdf4::ncvar_get(dataset, varid = 'longitude', start = 1, count = -1)
  values$time        <- ncdf4::ncvar_get(dataset, varid = 'time',      start = 1, count = -1)
  
  # Determine the valid range of the dimensions based on the period and
  # the bounding box
  validRange <- list()
  validRange$time <- which(findInterval(values$time, periodBoundaries(values$time, period))==1)  
  validRange$lat  <- which(findInterval(values$lat, bbox[2,])==1)
  validRange$lon  <- which(findInterval(values$lon, bbox[1,])==1)
  
  # Make a selection of indices which fall in our subsetting window
  # E.g. translate degrees to indices of arrays.
  determineCount <- function(x) {return(c(x[1], tail(x,1) - x[1] + 1))}
  count <- rbind(determineCount(validRange$lon),
                 determineCount(validRange$lat),
                 determineCount(validRange$time))
  
  
  # Prepare a list with the valued values of the dimensions and the variable
  validValues <- list()
  validValues$lat             <- values$lat[validRange$lat]
  validValues$lon             <- values$lon[validRange$lon]
  validValues$time            <- as.Date(values$time[validRange$time],
                                         origin="1950-01-01")
  validValues[[variableName]] <- ncdf4::ncvar_get(dataset, variableName,
                                                  start=count[, 1],
                                                  count=count[, 2])
  
  # Close the data set and return data.table created from the valid values
  ncdf4::nc_close(dataset)
  return(createDataTableFromNCDF(variableName, validValues))
}

# Creates a data.table from the ncdf4 input
# Not for external use
# @param variable Variable name
# @param validValues Valid vlaues
createDataTableFromNCDF <- function(variable, validValues) {
  dataFrame <- plyr::adply(validValues[[variable]], c(1,2,3))
  dataTable <- data.table(dataFrame)
  dataTable[, time  := as.Date(validValues$time[X3])]
  dataTable[, year  := as.numeric(format(time, "%Y"))]
  dataTable[, month := as.numeric(format(time, "%m"))]
  dataTable[, day   := as.numeric(format(time, "%d"))]
  dataTable[, lat := validValues$lat[X2]]
  dataTable[, lon := validValues$lon[X1]]
  dataTable[, paste(variable) := V1]
  dataTable[, X1 := NULL]
  dataTable[, X2 := NULL]
  dataTable[, X3 := NULL]
  dataTable[, V1 := NULL]
  #setkey(list(lat, lon, time))
  return(dataTable)
}

# Removes points outside of the SpatialPolygons
# Not for external use
# @param data Data.table
# @param area Valid area
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

# Removes all rows with NAs
# Not for external use
# @param data data.table
removeNAvalues <- function(data) {
  # We don't check if time is NA (it should not) but date * 0 is not defined
  data <- data[complete.cases(data[,!"time", with=FALSE]*0)]
  setkey(data, lon, lat)
  data[, pointID:=.GRP, by=key(data)]
}

# To define the valid range 
# @param time Time
# @param period Period
periodBoundaries <- function(time, period) {
  xts <- xts::xts(time, as.Date(time, origin="1950-01-01")) 
  interval <- range(as.numeric(xts[period]))
  interval[2] <- interval[2] + 1
  return(interval)
}
