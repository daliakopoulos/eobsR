library(microbenchmark)
library(doParallel)
registerDoParallel()

fileName <- "../STExt/STExtDoc/data/tg_0.50deg_reg_v12.0_plus_2015_annual_avg.nc"
file.exists(fileName)
fileCon  <- ncdf4::nc_open(fileName)
valVal1   <- eobsR:::GetEobsDimensions(fileCon)
valVal1$time <- as.Date(valVal1$time, origin="1950-01-01")
valVal1[['tg']] <- ncdf4::ncvar_get(fileCon, 'tg')
ncdf4::nc_close(fileCon)

fileName <- "../data/spatialSubset.nc"
file.exists(fileName)
fileCon  <- ncdf4::nc_open(fileName)
valVal2   <- eobsR:::GetEobsDimensions(fileCon)
valVal2$time <- as.Date(valVal2$time, origin="1950-01-01")
valVal2[['tg']] <- ncdf4::ncvar_get(fileCon, 'tg')
ncdf4::nc_close(fileCon)

test1a <- eobsR:::CreateDataTableFromNCDF('tg', valVal1)
test1b <- eobsR:::CreateDataTableMelt('tg', valVal1)
system.time(test2a <- eobsR:::CreateDataTableFromNCDF('tg', valVal2))
system.time(test2b <- eobsR:::CreateDataTableMelt('tg', valVal2))

all(test1a == test1b, na.rm=TRUE)
all(test2a == test2b, na.rm=TRUE)

microbenchmark(
  eobsR:::CreateDataTableFromNCDF('tg', valVal1),
  eobsR:::CreateDataTableMelt('tg', valVal1),
  eobsR:::CreateDataTableFromNCDF('tg', valVal2),
  eobsR:::CreateDataTableMelt('tg', valVal2),
  times=10)

#Unit: milliseconds
#expr                                                 min       lq       mean   median         uq       max neval
#eobsR:::CreateDataTableFromNCDF("tg", valVal1) 8041.1097 8058.2322 8251.0487 8259.0604 8353.7787 8631.4224    10
#eobsR:::CreateDataTableMelt("tg", valVal1)      210.7354  214.4493  234.5822  220.5432  222.8285  377.9285    10
#eobsR:::CreateDataTableFromNCDF("tg", valVal2)  426.4912  440.6237  493.9500  447.7269  460.1749  793.2833    10
#eobsR:::CreateDataTableMelt("tg", valVal2)      315.0291  320.6840  378.9841  329.7833  458.1113  476.5579    10

adm0 <- raster::getData('GADM', country='NL', level=0)
library(lineprof)
source("./R/importEOBS.R")
l <- lineprof(importEOBS("tg", "2014", adm0, "0.50reg"))
