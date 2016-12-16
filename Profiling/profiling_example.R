period <- '2014'
adm0 <- raster::getData('GADM', country='NL', level=0)
fadm0 = fortify(adm0)
analysis <- function() {
 data  <- importEOBS('tg', period, adm0, "0.50reg")
}
