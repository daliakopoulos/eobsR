#' Imports EOBS data
#' @param variable String from ('tg', 'tn', 'tx, ...)
#' @param period Either numeric, timeBased or ISO-8601 style (see \code{\link[xts]{.subset.xts}})
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
  data[, year  := as.numeric(format(time, "%Y"))]
  data[, month := as.numeric(format(time, "%m"))]
  data[, day   := as.numeric(format(time, "%d"))]
  setcolorder(data, c("time", "year", "month", "day", "lat", "lon", variable, "pointID"))
  return(data)
}

#' Import EOBS data from local file
#' @note Should be merged with \code{\link{importEOBS}}
#' @inheritParams importEOBS
#' @param filename String containing the path to the ncdf file
#' @export
LocalImportEOBS <- function(variable, filename, period = NULL, area = NULL,
                            na.rm=TRUE) {
  # Local sanitizing
  data <- GetEOBS(filename, variable, area, period, na.rm)
  return(data)
}

# TODO: Merge with getOpenDapValues 
# add bbox and period than this should be possible
GetEOBS <- function(filename, variable, area, period, na.rm) {
  if(!is.null(area)) bbox <- sp::bbox(area)
  ncdfConnection          <- ncdf4::nc_open(filename)
  validValues             <- GetEobsDimensions(ncdfConnection)
  validValues$time        <- as.Date(validValues$time, origin="1950-01-01")
  validValues[[variable]] <- ncdf4::ncvar_get(ncdfConnection, variable)
  ncdf4::nc_close(ncdfConnection)
  result <- CreateDataTableMelt(variable, validValues)
  if ( !is.null(area) & !is.matrix(area)) {
    result <- removeOutsiders(result, area)
  }
  if ( na.rm ) result <- removeNAvalues(result) 
  result[, year  := as.numeric(format(time, "%Y"))]
  result[, month := as.numeric(format(time, "%m"))]
  result[, day   := as.numeric(format(time, "%d"))]
  setcolorder(result, c("time", "year", "month", "day", "lat", "lon", variable, "pointID"))
  return(result)
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

# Get the EOBS netcdf dimensions
GetEobsDimensions <- function(ncdfConnection) {
  values <- list()
  values$lat         <- ncdf4::ncvar_get(ncdfConnection, varid = 'latitude')
  values$lon         <- ncdf4::ncvar_get(ncdfConnection, varid = 'longitude')
  values$time        <- ncdf4::ncvar_get(ncdfConnection, varid = 'time')
  return(values)
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
  values <- GetEobsDimensions(dataset) 
  
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
  return(CreateDataTableMelt(variableName, validValues))
}

# Creates a data.table from the ncdf4 input
# Not for external use
# @param variable Variable name
# @param validValues Valid vlaues
#' @import foreach
CreateDataTableFromNCDF <- function(variable, validValues) {
  result <- foreach(i=seq.int(1, length(validValues$lon)), .combine = "rbind") %dopar% { 
    foreach(j = seq.int(1, length(validValues$lat)), .combine = "rbind") %do% {
      if (all(is.na(validValues[[variable]][i, j, ]))) {}
      else {
        data.table(lon   = validValues$lon[i],
                   lat   = validValues$lat[j],
                   time  = validValues$time,
                   value = validValues[[variable]][i, j, ])
      }
    }
  }
  #result <- removeNAvalues(result) # For this function time has to be time ...
  #result[, year  := as.numeric(format(time, "%Y"))]
  #result[, month := as.numeric(format(time, "%m"))]
  #result[, day   := as.numeric(format(time, "%d"))]
  #setcolorder(result, c("time", "year", "month", "day", "lat", "lon", "value"))
  setnames(result, "value", paste(variable))
  return(result)
}

CreateDataTableMelt <- function(variable, validValues) {
  meltedValues <- reshape2::melt(validValues[[variable]],
                                 varnames=c("lon", "lat", "time"))
  result <- as.data.table(meltedValues)
  setkey(result, lon, lat)
  result[, pointID:=.GRP, by = key(result)]
  setkey(result, pointID)
  index <- result[, !all(is.na(value)), by = pointID][V1==TRUE, pointID]
  result <- result[pointID %in% index, ]
  result[, pointID := NULL]
  #setnames(result, "Var1", "lon")
  #setnames(result, "Var2", "lat")
  #setnames(result, "Var3", "time")
  result[, lon := validValues$lon[lon]]
  result[, lat := validValues$lat[lat]]
  result[, time := validValues$time[time]]
  #result[, year  := as.numeric(format(time, "%Y"))]
  #result[, month := as.numeric(format(time, "%m"))]
  #result[, day   := as.numeric(format(time, "%d"))]
  #setcolorder(result, c("time", "year", "month", "day", "lat", "lon", "value"))
  setnames(result, "value", paste(variable))
  return(result)
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
