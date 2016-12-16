library(doParallel)
registerDoParallel(1)
library(eobsR)
adm0 <- raster::getData('GADM', country='NL', level=0)
context("Input testing:")

expect_error(importEOBS("foo"), "Variable foo not known.")
expect_error(importEOBS('tg', 'A', "foo"), "Period should be either Numeric, timeBased or ISO-8601 style.")
expect_error(importEOBS('tg', '2014', "foo"), "Area should be of class SpatialPolygons or SpatialPolygonsDataFrame.")
expect_error(importEOBS('tg', '2014', adm0, "foo"), "Grid should be specified correctly.")

context("Output testing:")
expect_equal_to_reference(importEOBS('tg', '2014', adm0, '0.50reg'), file="output.rds")
expect_equal_to_reference(importEOBS('rr', '2014', adm0, '0.50reg'), file="output_rr.rds")
expect_equal_to_reference(LocalImportEOBS("tg", "tg_0.50deg_reg_v12.0_plus_2015_ANN_avg.nc",
                                          "2000/2015", adm0),
                          file = "output_local.rds")

stopImplicitCluster()
